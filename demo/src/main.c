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
//"!!ARBfp1.0                                        \n"
//"TEMP texcoord;                                   \n"
//"MOV texcoord, fragment.texcoord[0];              \n"
//"FLR texcoord.z, texcoord;                        \n"
//"TEX result.color, texcoord, texture[0], ARRAY2D; \n"
//"END";
"!!NVfp4.0\n"
"PARAM red = { 1.0, 0.0, 0.0, 1.0 };\n"
"MOV result.color, red;\n"
"END";


void render(void)
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	
	glutSolidTeapot(0.5f);

	glutSwapBuffers();
}

int main(int argc, char** argv)
{
	glutInit(&argc,argv);
	glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE | GLUT_DEPTH);
	glutInitWindowSize(WINDOW_W, WINDOW_H);
	glutCreateWindow("demo");

	glewInit();
	
	if (!glewIsSupported("GL_NV_gpu_program4"))
	{
		printf("GL_NV_gpu_program4 is required!");
		return 1;
	}
	
	glutDisplayFunc(render);


	// Set up shaders.
	glEnable(GL_FRAGMENT_PROGRAM_ARB);
	g_FragProgramId = compileARBShader(GL_FRAGMENT_PROGRAM_ARB, g_FragProgramCode, sizeof(g_FragProgramCode)-1);
	assert(g_FragProgramId);


	glutMainLoop();

	return 0;
}