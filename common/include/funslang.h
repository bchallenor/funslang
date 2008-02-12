#ifndef FUNSLANG_H
#define FUNSLANG_H

#include <GL/glew.h>

#ifdef FUNSLANG_BUILDING_DLL
// don't need dllexport as we're using a .def file
#define FUNSLANG_API
#else
#define FUNSLANG_API __declspec(dllimport)
#endif

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

FUNSLANG_API void fsInit(int* argc, char*** argv);
FUNSLANG_API void fsExit(void);

// Funslang compilation.
// Requires p->vertex_shader_path and p->fragment_shader_path to be set.
// The rest of p can be uninitialized.
FUNSLANG_API bool fsCompile(FSprogram* p);

// Shader data initialization.
FUNSLANG_API void fsSetVertexUniforms(FSprogram* p, const GLfloat* data);
FUNSLANG_API void fsSetFragmentUniforms(FSprogram* p, const GLfloat* data);
FUNSLANG_API void fsSetVertexVaryings(FSprogram* p, const GLfloat* data);

// Shader texture image unit assignment.
FUNSLANG_API void fsSetTextureImageUnits(FSprogram* p);

// Loads texture from file and assigns it new name (using glGenTextures).
// Creates mipmap and enables trilinear filtering.
// Currently only supports JPG.
// Returns name, or 0 on error (note 0 is not a valid GL texture name).
FUNSLANG_API GLuint fsLoadTexture2D(const char* fn);

#endif // FUNSLANG_H
