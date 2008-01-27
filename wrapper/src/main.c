#include <stdio.h>
#include <assert.h>

#include <GL/glew.h>
#include <GL/glut.h>

#include "shaders.h"

#define WINDOW_W 500
#define WINDOW_H 500

GLuint g_VertProgramId = 0;
GLuint g_FragProgramId = 0;

const GLchar* g_FragProgramCode =
"uniform float FragmentUniforms[1];\n"
"varying vec4 FragmentVaryings0;\n"

"void main()\n"
"{\n"
"float t1, t2, t3, t4;\n"
"bool t0;\n"
"// no vec4\n"
"\n"
"t4 = FragmentVaryings0[3];\n"
"t3 = FragmentVaryings0[2];\n"
"t2 = FragmentVaryings0[1];\n"
"t1 = FragmentVaryings0[0];\n"
"t0 = bool(FragmentUniforms[0]);\n"
"\n"
"if (!t0) discard;\n"
"gl_FragColor = vec4(t1, t2, t3, t4);\n"
"}\n";

const GLchar* g_VertProgramCode =
"uniform float VertexUniforms[16];\n"
"attribute vec4 VertexVaryings4;\n"
"attribute vec4 VertexVaryings0;\n"
"varying vec4 FragmentVaryings0;\n"

"void main()\n"
"{\n"
"float t0;float t3;float t7;float t11;float t48;float t49;float t50;float t51;float t1;float t4;float t8;float t12;float t15;float t17;float t20;float t23;float t26;float t28;float t31;float t34;float t37;float t39;float t42;float t45;float t14;float t25;float t36;float t47;float t10;float t22;float t33;float t44;float t6;float t19;float t30;float t41;float t2;float t16;float t27;float t38;float t5;float t18;float t29;float t40;float t9;float t21;float t32;float t43;float t13;float t24;float t35;float t46;\n"
"t51 = VertexVaryings4[3];\n"
"t50 = VertexVaryings4[2];\n"
"t49 = VertexVaryings4[1];\n"
"t48 = VertexVaryings4[0];\n"
"t45 = VertexUniforms[15];\n"
"t42 = VertexUniforms[14];\n"
"t39 = VertexUniforms[13];\n"
"t37 = VertexUniforms[12];\n"
"t34 = VertexUniforms[11];\n"
"t31 = VertexUniforms[10];\n"
"t28 = VertexUniforms[9];\n"
"t26 = VertexUniforms[8];\n"
"t23 = VertexUniforms[7];\n"
"t20 = VertexUniforms[6];\n"
"t17 = VertexUniforms[5];\n"
"t15 = VertexUniforms[4];\n"
"t12 = VertexUniforms[3];\n"
"t11 = VertexVaryings0[3];\n"
"t46 = t11 * t45;\n"
"t35 = t11 * t34;\n"
"t24 = t11 * t23;\n"
"t13 = t11 * t12;\n"
"t8 = VertexUniforms[2];\n"
"t7 = VertexVaryings0[2];\n"
"t43 = t7 * t42;\n"
"t32 = t7 * t31;\n"
"t21 = t7 * t20;\n"
"t9 = t7 * t8;\n"
"t4 = VertexUniforms[1];\n"
"t3 = VertexVaryings0[1];\n"
"t40 = t3 * t39;\n"
"t29 = t3 * t28;\n"
"t18 = t3 * t17;\n"
"t5 = t3 * t4;\n"
"t1 = VertexUniforms[0];\n"
"t0 = VertexVaryings0[0];\n"
"t38 = t0 * t37;\n"
"t41 = t38 + t40;\n"
"t44 = t41 + t43;\n"
"t47 = t44 + t46;\n"
"t27 = t0 * t26;\n"
"t30 = t27 + t29;\n"
"t33 = t30 + t32;\n"
"t36 = t33 + t35;\n"
"t16 = t0 * t15;\n"
"t19 = t16 + t18;\n"
"t22 = t19 + t21;\n"
"t25 = t22 + t24;\n"
"t2 = t0 * t1;\n"
"t6 = t2 + t5;\n"
"t10 = t6 + t9;\n"
"t14 = t10 + t13;\n"

"gl_Position = vec4(t14, t25, t36, t47);\n"
"FragmentVaryings0[0] = t48;\n"
"FragmentVaryings0[1] = t49;\n"
"FragmentVaryings0[2] = t50;\n"
"FragmentVaryings0[3] = t51;\n"
"}\n";

#define LOG_SIZE 1024
GLchar logbuf[LOG_SIZE];


void render(void)
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	
	glutSolidTeapot(0.5f);

	glutSwapBuffers();
}

int main(int argc, char** argv)
{
	GLuint vs, fs, p;

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


	// Set up shaders.

	vs = glCreateShader(GL_VERTEX_SHADER);
	glShaderSource(vs, 1, &g_VertProgramCode, NULL);
	glCompileShader(vs);

	glGetShaderInfoLog(vs, LOG_SIZE, NULL, logbuf);
	printf("%s\n", logbuf);

	fs = glCreateShader(GL_FRAGMENT_SHADER);
	glShaderSource(fs, 1, &g_FragProgramCode, NULL);
	glCompileShader(fs);

	glGetShaderInfoLog(fs, LOG_SIZE, NULL, logbuf);
	printf("%s\n", logbuf);

	p = glCreateProgram();
	glAttachShader(p, vs);
	glAttachShader(p, fs);
	glLinkProgram(p);

	glUseProgram(p);

	{
		const GLfloat vertexUniforms[4][4] = {{1.0, 0.0, 0.0, 0.0}, {0.0, 1.0, 0.0, 0.0}, {0.0, 0.0, 1.0, 0.0}, {0.0, 0.0, 0.0, 1.0}};
		const GLfloat fragmentUniforms[1] = {1.0};
		const GLfloat vertexVaryings[24] =
		{
			1.0, 0.0, 0.0, 1.0,   1.0, 0.0, 0.0, 1.0,
			0.0, 1.0, 0.0, 1.0,   1.0, 0.0, 0.0, 1.0,
			0.0, 0.0, 1.0, 1.0,   1.0, 0.0, 0.0, 1.0
		};
		glUniform1fv(glGetUniformLocation(p, "VertexUniforms"), 16, vertexUniforms);
		glUniform1fv(glGetUniformLocation(p, "FragmentUniforms"), 1, fragmentUniforms);
		glVertexAttribPointer(glGetAttribLocation(p, "VertexVaryings0"), 4, GL_FLOAT, 0, 8, vertexVaryings);
		glEnableVertexAttribArray(glGetAttribLocation(p, "VertexVaryings0"));
		glVertexAttribPointer(glGetAttribLocation(p, "VertexVaryings4"), 4, GL_FLOAT, 0, 8, &vertexVaryings[4]);
		glEnableVertexAttribArray(glGetAttribLocation(p, "VertexVaryings4"));
		glDrawArrays(GL_TRIANGLES, 0, 3);
	}

	// Enter main loop.
	glutMainLoop();

	return 0;
}