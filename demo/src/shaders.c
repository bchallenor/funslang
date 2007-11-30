#include <stdio.h>

#include "shaders.h"

GLuint compileARBShader(GLenum type, const char* code, const GLsizei len)
{
	GLuint id;
	GLint err;

	glGenProgramsARB(1, &id);
	glBindProgramARB(type, id);
	glProgramStringARB(type, GL_PROGRAM_FORMAT_ASCII_ARB, len, code);
	
	glGetIntegerv(GL_PROGRAM_ERROR_POSITION_ARB, &err);
	if (err >= 0)
	{
		fprintf(stderr, glGetString(GL_PROGRAM_ERROR_STRING_ARB));
		glDeleteProgramsARB(1, &id);
		return 0;
	}

    return id;
}