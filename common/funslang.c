#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "funslang.h"
#include "LibFunslang_stub.h"

#define MAX_PACKING_SIZE 4

bool fsCompileGLSL(FSprogram* p)
{
	GLuint glvs, glfs, glp;
	int i;

	glvs = glCreateShader(GL_VERTEX_SHADER);
	glShaderSource(glvs, 1, (const GLchar**)&p->glsl_vertex_shader_source, NULL);
	glCompileShader(glvs);

	glfs = glCreateShader(GL_FRAGMENT_SHADER);
	glShaderSource(glfs, 1, (const GLchar**)&p->glsl_fragment_shader_source, NULL);
	glCompileShader(glfs);

	glp = glCreateProgram();
	glAttachShader(glp, glvs);
	glAttachShader(glp, glfs);

	for (i = 0; i < p->num_vertex_varyings; i += MAX_PACKING_SIZE)
	{
		char vbuf[128];
		snprintf(vbuf, 128, "VertexVaryings%d", i);
		glBindAttribLocation(glp, i / MAX_PACKING_SIZE, vbuf);
	}

	glLinkProgram(glp);
	
	glValidateProgram(glp);
	GLint validate_status;
	glGetProgramiv(glp, GL_VALIDATE_STATUS, &validate_status);
	if (GL_TRUE != validate_status) return false;

#if 1
#define LOG_SIZE 8096
	char logbuf[LOG_SIZE];
	glGetShaderInfoLog(glvs, LOG_SIZE, NULL, logbuf);
	printf("%s\n", logbuf);
	glGetShaderInfoLog(glfs, LOG_SIZE, NULL, logbuf);
	printf("%s\n", logbuf);
	glGetProgramInfoLog(glp, LOG_SIZE, NULL, logbuf);
	printf("%s\n", logbuf);
#endif
	
	p->glsl_program = glp;
	p->loc_vertex_uniforms = glGetUniformLocation(glp, "VertexUniforms");
	p->loc_fragment_uniforms = glGetUniformLocation(glp, "FragmentUniforms");
	
	return true;
}

bool fsCompile(FSprogram* p)
{
	bool success;
	char* c;
	
	fsCompileVertex((char*)p->vertex_shader_path, &success, &p->num_vertex_uniforms, &p->num_vertex_varyings, &c);
	
	if (!success)
	{
		printf("error in <%s>:\n%s", p->vertex_shader_path, c);
		fsFree(c);
		return false;
	}
	
	printf("##### emitted vertex shader:\n\n%s\n\n", c);
	p->glsl_vertex_shader_source = strdup(c);
	fsFree(c);
	
	fsCompileFragment((char*)p->fragment_shader_path, &success, &p->num_fragment_uniforms, &p->num_fragment_varyings, &c);

	if (!success)
	{
		printf("error in <%s>:\n%s", p->fragment_shader_path, c);
		fsFree(c);
		return false;
	}
	
	printf("##### emitted fragment shader:\n\n%s\n\n", c);
	p->glsl_fragment_shader_source = strdup(c);
	fsFree(c);
	
	return fsCompileGLSL(p);
}

void fsSetVertexUniforms(FSprogram* p, const GLfloat* data)
{
	if (p->num_vertex_uniforms > 0)
	{
		glUniform1fv(p->loc_vertex_uniforms, p->num_vertex_uniforms, data);
	}
}

void fsSetFragmentUniforms(FSprogram* p, const GLfloat* data)
{
	if (p->num_fragment_uniforms > 0)
	{
		glUniform1fv(p->loc_fragment_uniforms, p->num_fragment_uniforms, data);
	}
}

void fsSetVertexVaryings(FSprogram* p, const GLfloat* data)
{
	int num_packed;
	int num_total = p->num_vertex_varyings;
	
	for (num_packed = 0; num_packed < num_total; num_packed += MAX_PACKING_SIZE)
	{
		GLint loc = num_packed / MAX_PACKING_SIZE;
		int num_left = num_total - num_packed;
		int num_now = num_left < MAX_PACKING_SIZE ? num_left : MAX_PACKING_SIZE;
		glVertexAttribPointer(loc, num_now, GL_FLOAT, 0, num_total * sizeof(GLfloat), &data[num_packed]);
		glEnableVertexAttribArray(loc);
	}
}

void fsInit(int* argc, char*** argv)
{
	hs_init(argc, argv);
}

void fsExit(void)
{
	hs_exit();
}
