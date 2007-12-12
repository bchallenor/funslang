#include <stdio.h>
#include <assert.h>

#include <GL/glew.h>
#include <GL/glut.h>

#include "shaders.h"

#define WINDOW_W 500
#define WINDOW_H 500

GLuint g_VertProgramId = 0;
GLuint g_FragProgramId = 0;

const char g_FragProgramCode[] =
//"!!NVfp4.0\n"
//"FLOAT PARAM color[3] = {program.local[0..2]};\n"
//"INT PARAM idx = program.local[3];\n"
//"INT TEMP idx2;\n"
//"MOV.S idx2, idx;\n"
//"MOV.F result.color, color[idx2.x];\n"
//"END";
"!!ARBfp1.0\n"
"PARAM color = {0.0,0.0,1.0,1.0};\n"
"MOV result.color, color;\n"
"END";

void render(void)
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	
	glutSolidTeapot(0.5f);

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
	if (GLEW_OK != glewInit() || !glewIsSupported("GL_ARB_vertex_program GL_ARB_fragment_program"))
	{
		//printf("GL_NV_gpu_program4 is required!");
		return 1;
	}
	
	// Set up GLUT callbacks.
	glutDisplayFunc(render);

	// Set up shaders.
	g_FragProgramId = compileARBShader(GL_FRAGMENT_PROGRAM_ARB, g_FragProgramCode, sizeof(g_FragProgramCode)-1);
	if (!g_FragProgramId) return 1;

	// Enable shader.
	glProgramLocalParameter4fARB(GL_FRAGMENT_PROGRAM_ARB, 0, 1.0, 0.0, 0.0, 0.0);
	glProgramLocalParameter4fARB(GL_FRAGMENT_PROGRAM_ARB, 1, 0.0, 1.0, 0.0, 0.0);
	glProgramLocalParameter4fARB(GL_FRAGMENT_PROGRAM_ARB, 2, 0.0, 0.0, 1.0, 0.0);
	glProgramLocalParameterI4iNV(GL_FRAGMENT_PROGRAM_ARB, 3, 2, 0, 0, 0);
	glEnable(GL_FRAGMENT_PROGRAM_ARB);

	// Enter main loop.
	glutMainLoop();

	return 0;
}