#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define _USE_MATH_DEFINES
#include <math.h>

#include <GL/glew.h>
#include <GL/glut.h>

#include "funslang.h"

#define WINDOW_W 1000
#define WINDOW_H 1000

typedef enum { false, true } bool;

GLfloat g_ModelMatrixFaceA[4][4] =
{
	{ 1,  0,  0,  0},
	{ 0,  1,  0,  0},
	{ 0,  0,  1,  0},
	{ 0,  0,  0,  1},
};
GLfloat g_ModelMatrixFaceB[4][4] =
{
	{ 0,  0, -1,  0},
	{ 0,  1,  0,  0},
	{ 1,  0,  0,  0},
	{ 0,  0,  0,  1},
};
GLfloat g_ModelMatrixFaceC[4][4] =
{
	{-1,  0,  0,  0},
	{ 0,  1,  0,  0},
	{ 0,  0, -1,  0},
	{ 0,  0,  0,  1},
};
GLfloat g_ModelMatrixFaceD[4][4] =
{
	{ 0,  0,  1,  0},
	{ 0,  1,  0,  0},
	{-1,  0,  0,  0},
	{ 0,  0,  0,  1},
};

GLfloat g_ModelMatrixFaceE[4][4] =
{
	{ 1,  0,  0,  0},
	{ 0,  0,  1,  0},
	{ 0, -1,  0,  0},
	{ 0,  0,  0,  1},
};
GLfloat g_ModelMatrixFaceF[4][4] =
{
	{ 1,  0,  0,  0},
	{ 0,  0, -1,  0},
	{ 0,  1,  0,  0},
	{ 0,  0,  0,  1},
};

const GLfloat g_vv[4*3] =
{
	-1, -1, +1,
	+1, -1, +1,
	+1, +1, +1,
	-1, +1, +1,
};


typedef struct
{
	float proj[4][4];
	float model[4][4];
	float rotx;
	float roty;
	float rotz;
	float from[3];
	float to[3];
	float up[3];
} VertexUniforms;

VertexUniforms g_vu =
{
	{
		{0, 0, 0, 0},
		{0, 0, 0, 0},
		{0, 0, 0, 0},
		{0, 0, 0, 0},
	},
	{
		{0, 0, 0, 0},
		{0, 0, 0, 0},
		{0, 0, 0, 0},
		{0, 0, 0, 0},
	},
	0,
	0,
	0,
	{2,2,2},
	{0,0,0},
	{0,1,0},
};

typedef struct
{
	float Zoom;
	float Xcenter;
	float Ycenter;
	float InnerColor[3];
	float OuterColor1[3];
	float OuterColor2[3];
} FragmentUniforms;

FragmentUniforms g_fu =
{
	1.36,
	-0.64,
	0,
	{0,0,0},
	{1,1,1},
	{0,0,1},
};


int g_FrameNumThisTick = 0, g_TickTime = 0, g_Time, g_TimeDelta;
double g_PhaseDelta;

bool g_IsRotatingX = false;
bool g_IsRotatingY = false;
bool g_IsRotatingZ = false;

bool g_RenderCube = false;

FSprogram g_Program;


void updateFPS(void)
{
	int t, timeThisTick;

	g_FrameNumThisTick++;
	
	t = glutGet(GLUT_ELAPSED_TIME);
	g_TimeDelta = g_Time - t;
	g_PhaseDelta = 2 * M_PI * g_TimeDelta / 1000.0;
	g_Time = t;
	
	timeThisTick = g_Time - g_TickTime;
	
	if (timeThisTick > 1000)
	{
		printf("FPS:%4.2f\n", (g_FrameNumThisTick * 1000.0) / timeThisTick);
		
		g_TickTime = g_Time;
		g_FrameNumThisTick = 0;
	}
}

void key(unsigned char key, int x, int y)
{
	switch (key)
	{
		case 'i':
			g_IsRotatingX = !g_IsRotatingX;
			return;
		case 'j':
			g_IsRotatingY = !g_IsRotatingY;
			return;
		case 'k':
			g_IsRotatingZ = !g_IsRotatingZ;
			return;
	}
}

void render(void)
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	
	if (g_RenderCube)
	{
		memcpy(g_vu.model, g_ModelMatrixFaceA, 16 * sizeof(GLfloat));
		fsSetVertexUniforms(&g_Program, (GLfloat*)&g_vu);
		fsSetFragmentUniforms(&g_Program, (GLfloat*)&g_fu);
		glDrawArrays(GL_QUADS, 0, 4);
		
		memcpy(g_vu.model, g_ModelMatrixFaceB, 16 * sizeof(GLfloat));
		fsSetVertexUniforms(&g_Program, (GLfloat*)&g_vu);
		fsSetFragmentUniforms(&g_Program, (GLfloat*)&g_fu);
		glDrawArrays(GL_QUADS, 0, 4);
		
		memcpy(g_vu.model, g_ModelMatrixFaceC, 16 * sizeof(GLfloat));
		fsSetVertexUniforms(&g_Program, (GLfloat*)&g_vu);
		fsSetFragmentUniforms(&g_Program, (GLfloat*)&g_fu);
		glDrawArrays(GL_QUADS, 0, 4);
		
		memcpy(g_vu.model, g_ModelMatrixFaceD, 16 * sizeof(GLfloat));
		fsSetVertexUniforms(&g_Program, (GLfloat*)&g_vu);
		fsSetFragmentUniforms(&g_Program, (GLfloat*)&g_fu);
		glDrawArrays(GL_QUADS, 0, 4);
		
		memcpy(g_vu.model, g_ModelMatrixFaceE, 16 * sizeof(GLfloat));
		fsSetVertexUniforms(&g_Program, (GLfloat*)&g_vu);
		fsSetFragmentUniforms(&g_Program, (GLfloat*)&g_fu);
		glDrawArrays(GL_QUADS, 0, 4);
		
		memcpy(g_vu.model, g_ModelMatrixFaceF, 16 * sizeof(GLfloat));
		fsSetVertexUniforms(&g_Program, (GLfloat*)&g_vu);
		fsSetFragmentUniforms(&g_Program, (GLfloat*)&g_fu);
		glDrawArrays(GL_QUADS, 0, 4);
	}
	else
	{
		memcpy(g_vu.model, g_ModelMatrixFaceA, 16 * sizeof(GLfloat));
		g_vu.from[0] = 0; g_vu.from[1] = 0; g_vu.from[2] = 0;
		g_vu.to[0] = 0; g_vu.to[1] = 0; g_vu.to[2] = -1;
		g_vu.up[0] = 0; g_vu.up[1] = 1; g_vu.up[2] = 0;
		fsSetVertexUniforms(&g_Program, (GLfloat*)&g_vu);
		fsSetFragmentUniforms(&g_Program, (GLfloat*)&g_fu);
		glDrawArrays(GL_QUADS, 0, 4);
	}
	
	glutSwapBuffers();
}

void frame(void)
{
	updateFPS();
	
	if (g_IsRotatingX)
	{
		static double phase = 0;
		phase += g_PhaseDelta / 4;
		g_vu.rotx = phase;
	}
	if (g_IsRotatingY)
	{
		static double phase = 0;
		phase += g_PhaseDelta / 4;
		g_vu.roty = phase;
	}
	if (g_IsRotatingZ)
	{
		static double phase = 0;
		phase += g_PhaseDelta / 4;
		g_vu.rotz = phase;
	}
	
	render();
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
		exit(1);
	}
	
	// Enable back-face culling.
	glEnable(GL_CULL_FACE);

	// Steal projection matrix from GL.
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	if (g_RenderCube) gluPerspective(60.0, 1.0, 1, 5);
	glGetFloatv(GL_PROJECTION_MATRIX, (GLfloat*)&g_vu.proj);
	
	// Init shaders.
	g_Program.vertex_shader_path = "../../funslang/Mandelbrot.vp";
	g_Program.fragment_shader_path = "../../funslang/Mandelbrot.fp";
	if (!fsCompile(&g_Program)) exit(1);
	glUseProgram(g_Program.glsl_program);
	fsSetVertexVaryings(&g_Program, (GLfloat*)&g_vv);
	
	// Set up GLUT callbacks.
	glutDisplayFunc(render);
	glutIdleFunc(frame);
	glutKeyboardFunc(key);

	// Enter main loop.
	glutMainLoop();

	return 0;
}
