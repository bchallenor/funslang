#include <stdio.h>
#include <assert.h>

#include <GL/glew.h>
#include <GL/glut.h>

#include "LibFunslang_stub.h"

#define DEBUG

#define MAX_PACKING_SIZE 4

typedef struct
{
	const char* vertex_shader_source;
	const char* fragment_shader_source;
	
	int num_vertex_uniforms;
	int num_vertex_varyings;
	int num_fragment_uniforms;
	
	GLchar* glsl_vertex_shader_source;
	GLchar* glsl_fragment_shader_source;
	GLuint glsl_program;
	GLint loc_vertex_uniforms;
	GLint loc_fragment_uniforms;
} FSprogram;

void fsCompileGLSL(FSprogram* p)
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

#ifdef DEBUG
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
}

void fsSetVertexUniforms(FSprogram* p, const void* data)
{
	if (p->num_vertex_uniforms > 0)
	{
		glUniform1fv(p->loc_vertex_uniforms, p->num_vertex_uniforms, data);
	}
}

void fsSetFragmentUniforms(FSprogram* p, const void* data)
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

void fsInit(int argc, char** argv)
{
	hs_init(&argc, &argv);
}















#define WINDOW_W 500
#define WINDOW_H 500


GLchar* g_FragProgramCode =
"varying vec4 FragmentVaryings0;"

"void main()"
"{"
"float t0, t1, t2, t3;"

"t3 = FragmentVaryings0[3];"
"t2 = FragmentVaryings0[2];"
"t1 = FragmentVaryings0[1];"
"t0 = FragmentVaryings0[0];"

"gl_FragColor = vec4(t0, t1, t2, t3);"
//"gl_FragColor = vec4(1,0,0,1);"
"}";

GLchar* g_VertProgramCode =
"uniform float VertexUniforms[16];"
"attribute vec4 VertexVaryings4;"
"attribute vec4 VertexVaryings0;"
"varying vec4 FragmentVaryings0;"

"void main()"
"{"
"float t0, t3, t7, t11, t48, t49, t50, t51, t1, t4, t8, t12, t15, t17, t20, t23, t26, t28, t31, t34, t37, t39, t42, t45, t14, t25, t36, t47, t10, t22, t33, t44, t6, t19, t30, t41, t2, t16, t27, t38, t5, t18, t29, t40, t9, t21, t32, t43, t13, t24, t35, t46;"

"t51 = VertexVaryings4[3];"
"t50 = VertexVaryings4[2];"
"t49 = VertexVaryings4[1];"
"t48 = VertexVaryings4[0];"
"t45 = VertexUniforms[15];"
"t42 = VertexUniforms[14];"
"t39 = VertexUniforms[13];"
"t37 = VertexUniforms[12];"
"t34 = VertexUniforms[11];"
"t31 = VertexUniforms[10];"
"t28 = VertexUniforms[9];"
"t26 = VertexUniforms[8];"
"t23 = VertexUniforms[7];"
"t20 = VertexUniforms[6];"
"t17 = VertexUniforms[5];"
"t15 = VertexUniforms[4];"
"t12 = VertexUniforms[3];"
"t11 = VertexVaryings0[3];"
"t46 = t11 * t45;"
"t35 = t11 * t34;"
"t24 = t11 * t23;"
"t13 = t11 * t12;"
"t8 = VertexUniforms[2];"
"t7 = VertexVaryings0[2];"
"t43 = t7 * t42;"
"t32 = t7 * t31;"
"t21 = t7 * t20;"
"t9 = t7 * t8;"
"t4 = VertexUniforms[1];"
"t3 = VertexVaryings0[1];"
"t40 = t3 * t39;"
"t29 = t3 * t28;"
"t18 = t3 * t17;"
"t5 = t3 * t4;"
"t1 = VertexUniforms[0];"
"t0 = VertexVaryings0[0];"
"t38 = t0 * t37;"
"t41 = t38 + t40;"
"t44 = t41 + t43;"
"t47 = t44 + t46;"
"t27 = t0 * t26;"
"t30 = t27 + t29;"
"t33 = t30 + t32;"
"t36 = t33 + t35;"
"t16 = t0 * t15;"
"t19 = t16 + t18;"
"t22 = t19 + t21;"
"t25 = t22 + t24;"
"t2 = t0 * t1;"
"t6 = t2 + t5;"
"t10 = t6 + t9;"
"t14 = t10 + t13;"

"gl_Position = vec4(t14, t25, t36, t47);"
//"gl_Position = vec4(t0, t3, t7, t11);"
"FragmentVaryings0[0] = t48;"
"FragmentVaryings0[1] = t49;"
"FragmentVaryings0[2] = t50;"
"FragmentVaryings0[3] = t51;"
"}";


void render(void)
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	
	glLoadIdentity();
	glTranslatef(-1.5f,0.0f,-6.0f);
	glDrawArrays(GL_TRIANGLES, 0, 3);

	glutSwapBuffers();
}

int main(int argc, char** argv)
{
	// Create window.
	glutInit(&argc,argv);
	glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE | GLUT_DEPTH);
	glutInitWindowSize(WINDOW_W, WINDOW_H);
	glutCreateWindow("demo");

	// Check for the required extensions.
	if (GLEW_OK != glewInit() || !GLEW_VERSION_2_0)
	{
		printf("OpenGL 2.0 is required!");
		return 1;
	}

	// Set up GLUT callbacks.
	glutDisplayFunc(render);


	// Set up shaders.
	FSprogram p;
	p.glsl_vertex_shader_source = g_VertProgramCode;
	p.glsl_fragment_shader_source = g_FragProgramCode;
	p.num_vertex_uniforms = 16;
	p.num_vertex_varyings = 8;
	p.num_fragment_uniforms = 0;
	fsCompileGLSL(&p);
	
	glUseProgram(p.glsl_program);


	{
		const GLfloat vertexUniforms[16] =
		{
			1.0, 0.0, 0.0, 0.0,
			0.0, 1.0, 0.0, 0.0,
			0.0, 0.0, 1.0, 0.0,
			0.0, 0.0, 0.0, 1.0
		};
		const GLfloat fragmentUniforms[1] = {1.0};
		const GLfloat vertexVaryings[24] =
		{
			0.0, 1.0, 0.0, 1.0,   1.0, 0.0, 0.0, 1.0,
			-1.0, -1.0, 0.0, 1.0,   0.0, 1.0, 0.0, 1.0,
			1.0, -1.0, 0.0, 1.0,   0.0, 0.0, 1.0, 1.0
		};
		fsSetVertexUniforms(&p, (void*)vertexUniforms);
		fsSetFragmentUniforms(&p, (void*)fragmentUniforms);
		fsSetVertexVaryings(&p, vertexVaryings);
	}

	// Enter main loop.
	glutMainLoop();

	return 0;
}
