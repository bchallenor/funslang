#include <stdio.h>

#include <GL/glew.h>
#include <GL/glut.h>

void renderScene(void)
{
	glClear(GL_COLOR_BUFFER_BIT);
	glBegin(GL_TRIANGLES);

	glVertex3f(-0.5,-0.5,0.0);
	glVertex3f(0.5,0.0,0.0);
	glVertex3f(0.0,0.5,0.0);

	glEnd();
	glFlush();
}

int main(int argc, char** argv)
{
	glutInit(&argc,argv);
	glutCreateWindow("GLEW Test");

	glewInit();
	
	if (GLEW_NV_gpu_program4)
	{
		printf("Yay!");
	}
	
	glutDisplayFunc(renderScene);

	glutMainLoop();

	return 0;
}