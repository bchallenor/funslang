#ifndef FUNSLANG_H
#define FUNSLANG_H

#include <GL/glew.h>

typedef struct
{
	const char* vertex_shader_path;
	const char* fragment_shader_path;
	
	int num_vertex_uniforms;
	int num_vertex_varyings;
	int num_fragment_uniforms;
	int num_fragment_varyings;
	
	GLchar* glsl_vertex_shader_source;
	GLchar* glsl_fragment_shader_source;
	GLuint glsl_program;
	GLint loc_vertex_uniforms;
	GLint loc_fragment_uniforms;
} FSprogram;

void fsInit(int* argc, char*** argv);
bool fsCompile(FSprogram* p);
void fsSetVertexUniforms(FSprogram* p, const GLfloat* data);
void fsSetFragmentUniforms(FSprogram* p, const GLfloat* data);
void fsSetVertexVaryings(FSprogram* p, const GLfloat* data);
void fsExit(void);

#endif // FUNSLANG_H
