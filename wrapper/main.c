#include <stdbool.h>
#include <stdio.h>

#include <GL/glew.h>
#include <GL/glut.h>

#include "funslang.h"

#define WINDOW_W 500
#define WINDOW_H 500

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
	// Init funslang compiler and the Haskell runtime.
	fsInit(&argc, &argv);
	
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

	// Compile shaders.
	FSprogram p;
	p.vertex_shader_path = "test.vp";
	p.fragment_shader_path = "test.fp";
	if (!fsCompile(&p)) return 1;
	
	// Activate shaders.
	glUseProgram(p.glsl_program);

	// Set shader data.
	const GLfloat vertexUniforms[16] =
	{
		1.0, 0.0, 0.0, 0.0,
		0.0, 1.0, 0.0, 0.0,
		0.0, 0.0, 1.0, 0.0,
		0.0, 0.0, 0.0, 1.0
	};
	const GLfloat fragmentUniforms[1] = {(float)true};
	const GLfloat vertexVaryings[24] =
	{
		0.0, 1.0, 0.0, 1.0,   1.0, 0.0, 0.0, 1.0,
		-1.0, -1.0, 0.0, 1.0,   0.0, 1.0, 0.0, 1.0,
		1.0, -1.0, 0.0, 1.0,   0.0, 0.0, 1.0, 1.0
	};
	fsSetVertexUniforms(&p, vertexUniforms);
	fsSetFragmentUniforms(&p, fragmentUniforms);
	fsSetVertexVaryings(&p, vertexVaryings);

	// Enter main loop.
	glutMainLoop();

	return 0;
}
