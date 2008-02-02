#include <stdbool.h>
#include <stdio.h>

#include <GL/glew.h>
#include <GL/glut.h>

#include "funslang.h"

#define WINDOW_W 500
#define WINDOW_H 500

const GLfloat brickVertexUniforms[44] =
{
	0.0, 0.0, 5.0, // LightPosition
	
	1.0, 0.0, 0.0, 0.0, // mvm
	0.0, 1.0, 0.0, 0.0,
	0.0, 0.0, 1.0, 0.0,
	0.0, 0.0, 0.0, 1.0,
	
	1.0, 0.0, 0.0, 0.0, // pm
	0.0, 1.0, 0.0, 0.0,
	0.0, 0.0, 1.0, 0.0,
	0.0, 0.0, 0.0, 1.0,
	
	1.0, 0.0, 0.0, // nm
	0.0, 1.0, 0.0,
	0.0, 0.0, 1.0
};
const GLfloat brickFragmentUniforms[10] =
{
	1.0, 0.3, 0.2, // BrickColor
	0.85, 0.86, 0.84, // MortarColor
	0.3, 0.15, // BrickSize
	0.85, 0.90 // BrickPct
};
const GLfloat brickVertexVaryings[7*4] =
{
	0.0, 0.0, 0.0, 1.0,   0.0, 0.0, 1.0,
	1.0, 0.0, 0.0, 1.0,   0.0, 0.0, 1.0,
	0.5, 1.0, 0.0, 1.0,   0.0, 0.0, 1.0,
};

void render(void)
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	
	glLoadIdentity();
	glDrawArrays(GL_TRIANGLES, 0, 3);

	glutSwapBuffers();
}

// Assumes paths already set.
bool initShaders(FSprogram* p, const GLfloat* vertexUniforms, const GLfloat* fragmentUniforms, const GLfloat* vertexVaryings)
{
	// Compile shaders.
	if (!fsCompile(p)) return false;
	
	// Activate shaders.
	glUseProgram(p->glsl_program);

	// Set shader data.
	fsSetVertexUniforms(p, vertexUniforms);
	fsSetFragmentUniforms(p, fragmentUniforms);
	fsSetVertexVaryings(p, vertexVaryings);
	
	return true;
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

	// Init shaders.
	FSprogram p;
	p.vertex_shader_path = "brick.vp";
	p.fragment_shader_path = "brick.fp";
	if (!initShaders(&p, brickVertexUniforms, brickFragmentUniforms, brickVertexVaryings)) return 1;

	// Enter main loop.
	glutMainLoop();

	return 0;
}
