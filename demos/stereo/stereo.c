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

typedef struct
{
	float windowW;
	float tileW;
	float zNear, zFar;
	float numDepthLevels;
} FragmentUniforms;

FragmentUniforms g_FragmentUniforms =
{
	WINDOW_W,
	0, // to be set later
	2, 7,
	0, // to be set later
};

typedef struct
{
	float x, y;
} VertexVaryings;

int g_FrameNumThisTick = 0, g_TickTime = 0, g_Time, g_TimeDelta;
double g_PhaseDelta;
bool g_IsRotating = false, g_ShowDepthMap = false;

FSprogram g_Program;

unsigned char* g_TileData;
unsigned int g_TileW;
unsigned int g_TileH;
GLuint g_DepthTexture, g_OutputTexture;


void updateFPS(void)
{
	int t, timeThisTick;

	g_FrameNumThisTick++;
	
	t = glutGet(GLUT_ELAPSED_TIME);
	g_TimeDelta = g_Time - t;
	g_PhaseDelta = (2 * M_PI * g_TimeDelta) / 1000.0;
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
		case ' ':
			g_IsRotating = !g_IsRotating;
			return;
		case 'd':
			g_ShowDepthMap = !g_ShowDepthMap;
			return;
	}
}

void render(void)
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// Rotate and render model.
	{
		static double phase = 0;

		glUseProgram(0);

		glPushMatrix();
		if (g_IsRotating) phase += g_PhaseDelta/4;
		glRotated(phase / M_PI * 180, 0,1,0);
		glutSolidTeapot(1.0);
		//glutSolidCube(1.0);
		glPopMatrix();
	}

	// Create depth texture from rendered scene.
	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D, g_DepthTexture);
	glCopyTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, 0, 0, WINDOW_W, WINDOW_H);

	// Load output texture (note that strip zero is already filled).
	glActiveTexture(GL_TEXTURE1);
	glBindTexture(GL_TEXTURE_2D, g_OutputTexture);

	// Render stereogram in strips of width "g_TileW".
	glUseProgram(g_Program.glsl_program);
	{
		float xmin, xmax;
		float w = (float)g_TileW / (float)WINDOW_W;
		int s;

		for (xmin = w, xmax = xmin + w; xmin < 1.0; xmin = xmax, xmax += w)
		{
			VertexVaryings vs[4] =
			{
				{xmin, 0},
				{xmax, 0},
				{xmax, 1},
				{xmin, 1},
			};

			s = xmin*WINDOW_W;

			fsSetVertexVaryings(&g_Program, (GLfloat*)vs);
			glDrawArrays(GL_QUADS, 0, 4);
			glCopyTexSubImage2D(GL_TEXTURE_2D, 0, s, 0, s, 0, WINDOW_W - s < g_TileW ? WINDOW_W - s : g_TileW, WINDOW_H);
		}
	}

	// Render the final texture.
	{
		glUseProgram(0);
		glActiveTexture(GL_TEXTURE0);
		if (g_ShowDepthMap)
		{
			glBindTexture(GL_TEXTURE_2D, g_DepthTexture);
		}
		else
		{
			glBindTexture(GL_TEXTURE_2D, g_OutputTexture);
		}
		glEnable(GL_TEXTURE_2D);

		glMatrixMode(GL_MODELVIEW);
		glPushMatrix();
		glLoadIdentity();
		glMatrixMode(GL_PROJECTION);
		glPushMatrix();
		glLoadIdentity();
		glBegin(GL_QUADS);
		{
			glTexCoord2f(0.0, 0.0);
			glVertex2d(-1.0, -1.0);
			glTexCoord2f(1.0, 0.0);
			glVertex2d(1.0, -1.0);
			glTexCoord2f(1.0, 1.0);
			glVertex2d(1.0, 1.0);
			glTexCoord2f(0.0, 1.0);
			glVertex2d(-1.0, 1.0);
		}
		glEnd();
		glPopMatrix();
		glMatrixMode(GL_MODELVIEW);
		glPopMatrix();
	}
	
	glutSwapBuffers();
}

void frame(void)
{
	updateFPS();
	
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
	
	// Enable the depth buffer!
	glEnable(GL_DEPTH_TEST);
	glDepthFunc(GL_LEQUAL);

	// Set projection matrix for non-Funslang render pass.
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(60.0, WINDOW_W / WINDOW_H, g_FragmentUniforms.zNear, g_FragmentUniforms.zFar);

	// Set view matrix.
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	gluLookAt(3,3,3, 0,0,0, 0,1,0);
	
	// Init depth texture.
	glGenTextures(1, &g_DepthTexture);
	glBindTexture(GL_TEXTURE_2D, g_DepthTexture);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, WINDOW_W, WINDOW_H, 0, GL_DEPTH_COMPONENT, GL_FLOAT, NULL);
	
	// Init output texture.
	glGenTextures(1, &g_OutputTexture);
	glBindTexture(GL_TEXTURE_2D, g_OutputTexture);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, WINDOW_W, WINDOW_H, 0, GL_RGB, GL_FLOAT, NULL);

	// Load tile image into strip zero of output texture.
	//g_TileData = fsLoadJPG("com.bencloward.textures.brick15.jpg", malloc, &g_TileW, &g_TileH);
	g_TileData = fsLoadJPG("com.rhythm.randomTile128.jpg", malloc, &g_TileW, &g_TileH);
	{
		unsigned int y, r, h;
		for (y = 0; y < WINDOW_H; y += g_TileH)
		{
			r = WINDOW_H - y;
			h = r > g_TileH ? g_TileH : r;
			glTexSubImage2D(GL_TEXTURE_2D, 0, 0, y, g_TileW, h, GL_RGB, GL_UNSIGNED_BYTE, g_TileData);
		}
	}

	// Init shaders.
	g_Program.vertex_shader_path = "../../funslang/stereo.vp";
	g_Program.fragment_shader_path = "../../funslang/stereo.fp";
	if (!fsCompile(&g_Program)) exit(1);
	glUseProgram(g_Program.glsl_program);
	fsSetVertexUniforms(&g_Program, NULL);
	g_FragmentUniforms.tileW = g_TileW;
	g_FragmentUniforms.numDepthLevels = g_TileW/3;
	fsSetFragmentUniforms(&g_Program, (GLfloat*)&g_FragmentUniforms);
	fsSetTextureImageUnits(&g_Program);

	// Set up GLUT callbacks.
	glutDisplayFunc(render);
	glutIdleFunc(frame);
	glutKeyboardFunc(key);

	// Enter main loop.
	glutMainLoop();

	return 0;
}
