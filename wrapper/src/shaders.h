#ifndef SHADERS_H
#define SHADERS_H

#include <GL/glew.h>

// len is that returned by strlen, i.e. without terminator.
GLuint compileARBShader(GLenum type, const char* code, const GLsizei len);

#endif // SHADERS_H