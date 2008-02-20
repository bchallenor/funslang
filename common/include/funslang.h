#ifndef FUNSLANG_H
#define FUNSLANG_H

#include <stdlib.h>
#include <GL/glew.h>

#ifdef FUNSLANG_BUILDING_DLL
// don't need dllexport as we're using a .def file
#define FS_API
#else
#define FS_API __declspec(dllimport)
#endif

#define FS_BOOL int
#define FS_TRUE 1
#define FS_FALSE 0

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

FS_API void fsInit(int* argc, char*** argv);
FS_API void fsExit(void);

// Funslang compilation.
// Requires p->vertex_shader_path and p->fragment_shader_path to be set.
// The rest of p can be uninitialized.
FS_API FS_BOOL fsCompile(FSprogram* p);

// Shader data initialization.
// You _must_ ensure that p is active by calling glUseProgram(p->glsl_program) first.
FS_API void fsSetVertexUniforms(FSprogram* p, const GLfloat* data);
FS_API void fsSetFragmentUniforms(FSprogram* p, const GLfloat* data);
FS_API void fsSetVertexVaryings(FSprogram* p, const GLfloat* data);

// Shader texture image unit assignment.
// You _must_ ensure that p is active by calling glUseProgram(p->glsl_program) first.
FS_API void fsSetTextureImageUnits(FSprogram* p);

// Loads texture from file and assigns it new name (using glGenTextures).
// Creates mipmap and enables trilinear filtering.
// Currently only supports JPG.
// Returns name, guaranteeing that the new texture is currently bound.
// On error, returns 0 (note 0 is not a valid GL texture name).
FS_API GLuint fsLoadTexture2D(const char* fn, unsigned int* width, unsigned int* height);

// Loads JPG pixel data from file to byte array in RGB order.
// Uses the given function to allocate memory.
FS_API unsigned char* fsLoadJPG(void* (*alloc_pixel_buffer)(size_t), const char* fn, unsigned int* width, unsigned int* height);

#endif // FUNSLANG_H
