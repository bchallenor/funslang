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
	
	int num_textures;
	
	GLchar* glsl_vertex_shader_source;
	GLchar* glsl_fragment_shader_source;
	GLuint glsl_program;
	GLint loc_vertex_uniforms;
	GLint loc_fragment_uniforms;
} FSprogram;

void fsInit(int* argc, char*** argv);
void fsExit(void);

// Funslang compilation.
// Requires p->vertex_shader_path and p->fragment_shader_path to be set.
// The rest of p can be uninitialized.
bool fsCompile(FSprogram* p);

// Shader data initialization.
void fsSetVertexUniforms(FSprogram* p, const GLfloat* data);
void fsSetFragmentUniforms(FSprogram* p, const GLfloat* data);
void fsSetVertexVaryings(FSprogram* p, const GLfloat* data);

// Shader texture image unit assignment.
void fsSetTextureImageUnits(FSprogram* p);

// Loads texture from file and assigns it new name (using glGenTextures).
// Creates mipmap and enables trilinear filtering.
// Currently only supports JPG.
// Returns name, or 0 on error (note 0 is not a valid GL texture name).
GLuint fsLoadTexture2D(const char* fn);

#endif // FUNSLANG_H
