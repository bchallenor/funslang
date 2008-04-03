#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define _USE_MATH_DEFINES
#include <math.h>

#include <GL/glew.h>
#include <GL/glut.h>

#include "funslang.h"

#define WINDOW_W 768
#define WINDOW_H 432

typedef enum { false, true } bool;

typedef struct
{
	float windowW;
	float tileW;
	float zNear, zFar;
	float numDepthLevels;
} StereoFragmentUniforms;

StereoFragmentUniforms g_StereoFragmentUniforms =
{
	WINDOW_W,
	0, // to be set later
	2, 7,
	0, // to be set later
};

typedef struct
{
	float x, y;
} StereoVertexVaryings;

typedef struct
{
	float p[4];
	float c[2];
} TexmapVertexVaryings;

int g_FrameNumThisTick = 0, g_TickTime = 0, g_Time, g_TimeDelta;
double g_PhaseDelta;
bool g_IsRotating = false, g_ShowDepthMap = false, g_IsAnimated = false;

FSprogram g_ProgramStereo, g_ProgramTexmap;

unsigned int g_TileW;
unsigned int g_TileH;
float g_RelativeTileW, g_RelativeStripW;
GLuint g_FrameBufferObject;
GLuint g_DepthTexture, g_TileTexture;
GLuint g_ColorTexture[2];
GLenum g_ColorTextureAttachment[2] = {GL_COLOR_ATTACHMENT0_EXT, GL_COLOR_ATTACHMENT1_EXT};
unsigned int g_ColorTextureRead = 0, g_ColorTextureWrite = 1;

void swapColorTextures(void)
{
	//g_ColorTextureRead = !g_ColorTextureRead;
	//g_ColorTextureWrite = !g_ColorTextureWrite;
	//glDrawBuffer(g_ColorTextureAttachment[g_ColorTextureWrite]);
}

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
		case 'a':
			g_IsAnimated = !g_IsAnimated;
			return;
	}
}

void render(void)
{
	// Switch to offscreen render textures.
	glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, g_FrameBufferObject);

	// Enable and clear the depth texture.
	glEnable(GL_DEPTH_TEST);
	glClear(GL_DEPTH_BUFFER_BIT);// | GL_COLOR_BUFFER_BIT);

	// Render scene to depth texture.
	// Note some arbitrary colors will be written to a color texture, but ignore them.
	{
		static double phase = 0;

		glUseProgram(0);

		glPushMatrix();
		if (g_IsRotating) phase += g_PhaseDelta/4;
		glRotated(phase / M_PI * 180, 0,1,0);
		glutSolidTeapot(1.0);
		glPopMatrix();
	}

	// Shaders should not read from and write to the same texture.
	// For the color textures, this was fixed by double buffering.
	// There's no need to update the depth texture though, so just disable writes entirely.
	glDisable(GL_DEPTH_TEST);

	// Render strip zero of tile pattern.
	{
		static double tile_animation_offset = 0;
		const double MAGIC_SPEED = 10;
		if (g_IsAnimated) tile_animation_offset += (g_PhaseDelta*MAGIC_SPEED)/(2*M_PI);
		{
			TexmapVertexVaryings vs[4] =
			{
				{{-1+2*0,              -1,0,1}, {0+tile_animation_offset,0}},
				{{-1+2*g_RelativeTileW,-1,0,1}, {1+tile_animation_offset,0}},
				{{-1+2*g_RelativeTileW,+1,0,1}, {1+tile_animation_offset,(double)WINDOW_H/(double)g_TileH}},
				{{-1+2*0,              +1,0,1}, {0+tile_animation_offset,(double)WINDOW_H/(double)g_TileH}},
			};
			glActiveTexture(GL_TEXTURE0);
			glBindTexture(GL_TEXTURE_2D, g_TileTexture);
			glUseProgram(g_ProgramTexmap.glsl_program);
			fsSetVertexVaryings(&g_ProgramTexmap, (GLfloat*)vs);
			glDrawArrays(GL_QUADS, 0, 4);
			swapColorTextures();
		}
	}

	// Render stereogram in strips of width "g_RelativeStripW".
	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D, g_DepthTexture);
	glUseProgram(g_ProgramStereo.glsl_program);
	{
		float xmin = g_RelativeTileW;
		float xmax = xmin + g_RelativeStripW;
		

		//xmin += g_RelativeStripW;
		//xmax+= g_RelativeStripW;

		for ( ; xmin < 1.0; xmin = xmax, xmax += g_RelativeStripW)
		{
			StereoVertexVaryings vs[4] =
			{
				//{xmin, 0},
				//{xmax, 0},
				//{xmax, 1},
				//{xmin, 1},
				{xmin, 1},
				{xmax, 1},
				{xmax, 0},
				{xmin, 0},
			};

			// Read from the correct color texture.
			glActiveTexture(GL_TEXTURE1);
			glBindTexture(GL_TEXTURE_2D, g_ColorTexture[g_ColorTextureRead]);

			fsSetVertexVaryings(&g_ProgramStereo, (GLfloat*)vs);
			glDrawArrays(GL_QUADS, 0, 4);
			swapColorTextures();
		}
	}

	// Switch back to window buffers.
	glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);

	// No need for depth testing here.
	glDisable(GL_DEPTH_TEST);

	// Blit the final color texture to the screen.
	{
		TexmapVertexVaryings vs[4] =
		{
			{{-1,-1,0,1}, {0,0}},
			{{+1,-1,0,1}, {1,0}},
			{{+1,+1,0,1}, {1,1}},
			{{-1,+1,0,1}, {0,1}},
		};
		glActiveTexture(GL_TEXTURE0);
		glBindTexture(GL_TEXTURE_2D, g_ShowDepthMap ? g_DepthTexture : g_ColorTexture[g_ColorTextureRead]);
		glUseProgram(g_ProgramTexmap.glsl_program);
		fsSetVertexVaryings(&g_ProgramTexmap, (GLfloat*)vs);
		glDrawArrays(GL_QUADS, 0, 4);
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
	unsigned int i;

	// Init funslang compiler and the Haskell runtime.
	fsInit(&argc, &argv);
	
	// Create window.
	glutInit(&argc,argv);
	glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE | GLUT_DEPTH);
	glutInitWindowSize(WINDOW_W, WINDOW_H);
	glutCreateWindow("demo");

	// Check for the required extensions.
	if (GLEW_OK != glewInit() || !GLEW_VERSION_2_0 || !GLEW_EXT_framebuffer_object)
	{
		printf("OpenGL 2.0 and EXT_framebuffer_object are required!");
		exit(1);
	}

	// Set projection matrix for non-Funslang render pass.
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(30.0, ((double)WINDOW_W) / ((double)WINDOW_H), g_StereoFragmentUniforms.zNear, g_StereoFragmentUniforms.zFar);

	// Set view matrix.
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	gluLookAt(3,3,3, 0,0,0, 0,1,0);
	
	// Create framebuffer object for intermediate rendering.
	glGenFramebuffersEXT(1, &g_FrameBufferObject);
	glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, g_FrameBufferObject);

	// Attach texture to use as the depth buffer.
	glGenTextures(1, &g_DepthTexture);
	glBindTexture(GL_TEXTURE_2D, g_DepthTexture);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, WINDOW_W, WINDOW_H, 0, GL_DEPTH_COMPONENT, GL_FLOAT, NULL);
	glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_TEXTURE_2D, g_DepthTexture, 0);
	
	// Attach textures to use as double color buffers.
	glGenTextures(2, g_ColorTexture);
	for (i = 0; i < 2; i++)
	{
		glBindTexture(GL_TEXTURE_2D, g_ColorTexture[i]);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
		glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, WINDOW_W, WINDOW_H, 0, GL_RGB, GL_FLOAT, NULL);
		glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, g_ColorTextureAttachment[i], GL_TEXTURE_2D, g_ColorTexture[i], 0);
	}

	// Init tile texture.
	g_TileTexture = fsLoadTexture2D("textures/crystal-spores-128.jpg", &g_TileW, &g_TileH);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

	// Init shaders.
	g_ProgramStereo.vertex_shader_path = "../../funslang/Stereo.vp";
	g_ProgramStereo.fragment_shader_path = "../../funslang/Stereo.fp";
	if (!fsCompile(&g_ProgramStereo)) exit(1);
	glUseProgram(g_ProgramStereo.glsl_program);
	fsSetVertexUniforms(&g_ProgramStereo, NULL);
	g_StereoFragmentUniforms.tileW = g_TileW;
	g_StereoFragmentUniforms.numDepthLevels = g_TileW/3;
	fsSetFragmentUniforms(&g_ProgramStereo, (GLfloat*)&g_StereoFragmentUniforms);
	fsSetTextureImageUnits(&g_ProgramStereo);
	g_RelativeTileW = (float)g_TileW / (float)WINDOW_W;
	g_RelativeStripW = (float)(g_TileW - g_StereoFragmentUniforms.numDepthLevels) / (float)WINDOW_W;
	//g_RelativeStripW = (float)(10) / (float)WINDOW_W;// todo

	g_ProgramTexmap.vertex_shader_path = "../../funslang/Texmap.vp";
	g_ProgramTexmap.fragment_shader_path = "../../funslang/Texmap.fp";
	if (!fsCompile(&g_ProgramTexmap)) exit(1);
	glUseProgram(g_ProgramTexmap.glsl_program);
	fsSetVertexUniforms(&g_ProgramTexmap, NULL);
	fsSetFragmentUniforms(&g_ProgramTexmap, NULL);
	fsSetTextureImageUnits(&g_ProgramTexmap);
	
	// Set up GLUT callbacks.
	glutDisplayFunc(render);
	glutIdleFunc(frame);
	glutKeyboardFunc(key);

	// Enter main loop.
	glutMainLoop();

	return 0;
}
