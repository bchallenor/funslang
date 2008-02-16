#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define FUNSLANG_BUILDING_DLL
#include "funslang.h"

#ifdef __GLASGOW_HASKELL__
#include "LibFunslang_stub.h"
extern void __stginit_LibFunslang(void);
#endif

//#include "png.h"
#include "jpeglib.h"

#define FS_TRACE 1
#define MAX_PACKING_SIZE 4

FS_BOOL fsCompileGLSL(FSprogram* p)
{
	GLuint glvs, glfs, glp;
	int i;

	glvs = glCreateShader(GL_VERTEX_SHADER);
	glShaderSource(glvs, 1, (const GLchar**)&p->glsl_vertex_shader_source, NULL);
	glCompileShader(glvs);

	glfs = glCreateShader(GL_FRAGMENT_SHADER);
	glShaderSource(glfs, 1, (const GLchar**)&p->glsl_fragment_shader_source, NULL);
	glCompileShader(glfs);

	glp = glCreateProgram();
	glAttachShader(glp, glvs);
	glAttachShader(glp, glfs);

	for (i = 0; i < p->num_vertex_varyings; i += MAX_PACKING_SIZE)
	{
		char vbuf[128];
		snprintf(vbuf, 128, "VertexVaryings%d", i);
		glBindAttribLocation(glp, i / MAX_PACKING_SIZE, vbuf);
	}

	glLinkProgram(glp);
	
	glValidateProgram(glp);
	GLint validate_status;
	glGetProgramiv(glp, GL_VALIDATE_STATUS, &validate_status);
	if (GL_TRUE != validate_status) return FS_FALSE;

#ifdef FS_TRACE
#define LOG_SIZE 8096
	char logbuf[LOG_SIZE];
	glGetShaderInfoLog(glvs, LOG_SIZE, NULL, logbuf);
	printf("%s\n", logbuf);
	glGetShaderInfoLog(glfs, LOG_SIZE, NULL, logbuf);
	printf("%s\n", logbuf);
	glGetProgramInfoLog(glp, LOG_SIZE, NULL, logbuf);
	printf("%s\n", logbuf);
#endif
	
	p->glsl_program = glp;
	p->loc_vertex_uniforms = glGetUniformLocation(glp, "VertexUniforms");
	p->loc_fragment_uniforms = glGetUniformLocation(glp, "FragmentUniforms");
	
	return FS_TRUE;
}

FS_BOOL fsCompile(FSprogram* p)
{
	char* err;
	char* v_type;
	char* f_type;
	char* v_glsl_src;
	char* f_glsl_src;
	
	_fsCompile(
		(char*)p->vertex_shader_path,
		(char*)p->fragment_shader_path,
		&err,
		&v_type, &p->num_vertex_uniforms, &p->num_vertex_varyings, &v_glsl_src,
		&f_type, &p->num_fragment_uniforms, &p->num_fragment_varyings, &f_glsl_src,
		&p->num_textures
	);
	
	if (err)
	{
		fprintf(stderr, "%s", err);
		_fsFree(err);
		return FS_FALSE;
	}
	else
	{
		printf("##### emitted vertex shader:\n\n%s\n\n%s\n\n", v_type, v_glsl_src);
		p->glsl_vertex_shader_source = strdup(v_glsl_src);
		_fsFree(v_type);
		_fsFree(v_glsl_src);
		
		printf("##### emitted fragment shader:\n\n%s\n\n%s\n\n", f_type, f_glsl_src);
		p->glsl_fragment_shader_source = strdup(f_glsl_src);
		_fsFree(f_type);
		_fsFree(f_glsl_src);
		
		return fsCompileGLSL(p);
	}
}

void fsSetVertexUniforms(FSprogram* p, const GLfloat* data)
{
	if (!data) return;

	if (p->num_vertex_uniforms > 0)
	{
		glUniform1fv(p->loc_vertex_uniforms, p->num_vertex_uniforms, data);
	}
}

void fsSetFragmentUniforms(FSprogram* p, const GLfloat* data)
{
	if (!data) return;

	if (p->num_fragment_uniforms > 0)
	{
		glUniform1fv(p->loc_fragment_uniforms, p->num_fragment_uniforms, data);
	}
}

void fsSetVertexVaryings(FSprogram* p, const GLfloat* data)
{
	int num_packed;
	int num_total = p->num_vertex_varyings;
	
	if (!data) return;
	
	for (num_packed = 0; num_packed < num_total; num_packed += MAX_PACKING_SIZE)
	{
		GLint loc = num_packed / MAX_PACKING_SIZE;
		int num_left = num_total - num_packed;
		int num_now = num_left < MAX_PACKING_SIZE ? num_left : MAX_PACKING_SIZE;
		glVertexAttribPointer(loc, num_now, GL_FLOAT, 0, num_total * sizeof(GLfloat), &data[num_packed]);
		glEnableVertexAttribArray(loc);
	}
}

void fsSetTextureImageUnits(FSprogram* p)
{
	int tex_image_unit;

	for (tex_image_unit = 0; tex_image_unit < p->num_textures; tex_image_unit++)
	{
		GLint loc;
		char tbuf[128];
		
		snprintf(tbuf, 128, "Tex%d", tex_image_unit);
		loc = glGetUniformLocation(p->glsl_program, tbuf);
		if (loc < 0)
		{
			// This is OK.
			// The texture was declared by the compiler,
			// but the GLSL program doesn't actually use it.
#ifdef FS_TRACE
			printf("<%s> not used by GLSL program\n");
#endif
		}
		else
		{
			glUniform1i(loc, tex_image_unit);
		}
	}
}

// Loads JPG pixel data from file to byte array in RGB order.
unsigned char* _fsLoadJPG(const char* fn, unsigned int* width, unsigned int* height)
{
	struct jpeg_decompress_struct cinfo;
	struct jpeg_error_mgr jerr;
	FILE* f;
	unsigned char* data;
	int row_stride;
	
	cinfo.err = jpeg_std_error(&jerr);
	jpeg_create_decompress(&cinfo);
	
	f = fopen(fn, "rb");
	if (!f)
	{
		fprintf(stderr, "can't open %s\n", fn);
		return NULL;
	}
	jpeg_stdio_src(&cinfo, f);
	
	jpeg_read_header(&cinfo, TRUE);
	cinfo.out_color_space = JCS_RGB; // force RGB output, even for grayscale images
	
	jpeg_start_decompress(&cinfo);
	
	row_stride = cinfo.output_width * cinfo.output_components;
	
	data = malloc(row_stride * cinfo.output_height * sizeof(data[0]));
	if (!data)
	{
		fprintf(stderr, "can't allocate enough memory for %s\n", fn);
		return NULL;
	}
	
	while (cinfo.output_scanline < cinfo.output_height)
	{
		JSAMPROW row_pointer[1];
		row_pointer[0] = &data[cinfo.output_scanline * row_stride];
		jpeg_read_scanlines(&cinfo, row_pointer, 1);
	}
	
	jpeg_finish_decompress(&cinfo);
	
	jpeg_destroy_decompress(&cinfo);
	
	fclose(f);
	
	*width = cinfo.output_width;
	*height = cinfo.output_height;
	
	return data;
}

#if 0 // this is not ready
unsigned char* read_png(FILE *fp)  /* file is already open */
{
   png_structp png_ptr;
   png_infop info_ptr;
   png_uint_32 width, height;
   int bit_depth, color_type, interlace_type;

   /* Create and initialize the png_struct with the desired error handler
    * functions.  If you want to use the default stderr and longjump method,
    * you can supply NULL for the last three parameters.  We also supply the
    * the compiler header file version, so that we know if the application
    * was compiled with a compatible version of the library.  REQUIRED
    */
   png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);

   if (png_ptr == NULL)
   {
      fclose(fp);
      return NULL;
   }

   /* Allocate/initialize the memory for image information.  REQUIRED. */
   info_ptr = png_create_info_struct(png_ptr);
   if (info_ptr == NULL)
   {
      fclose(fp);
      png_destroy_read_struct(&png_ptr, NULL, NULL);
      return NULL;
   }

   /* Set error handling if you are using the setjmp/longjmp method (this is
    * the normal method of doing things with libpng).  REQUIRED unless you
    * set up your own error handlers in the png_create_read_struct() earlier.
    */

   if (setjmp(png_jmpbuf(png_ptr)))
   {
      /* Free all of the memory associated with the png_ptr and info_ptr */
      png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
      fclose(fp);
      /* If we get here, we had a problem reading the file */
      return NULL;
   }

   /* One of the following I/O initialization methods is REQUIRED */
   /* Set up the input control if you are using standard C streams */
   png_init_io(png_ptr, fp);

   /*
    * If you have enough memory to read in the entire image at once,
    * and you need to specify only transforms that can be controlled
    * with one of the PNG_TRANSFORM_* bits (this presently excludes
    * dithering, filling, setting background, and doing gamma
    * adjustment), then you can read the entire image (including
    * pixels) into the info structure with this call:
    */
   png_read_png(png_ptr, info_ptr, PNG_TRANSFORM_PACKING | PNG_TRANSFORM_EXPAND, NULL);

   /* At this point you have read the entire image */

   /* clean up after the read, and free any memory allocated - REQUIRED */
   png_destroy_read_struct(&png_ptr, &info_ptr, NULL);

   /* close the file */
   fclose(fp);

   /* that's it */
   return (OK);
}
#endif

GLuint fsLoadTexture2D(const char* fn)
{	
	unsigned char* data;
	unsigned int width;
	unsigned int height;
	GLuint tex_name;
	
	data = _fsLoadJPG(fn, &width, &height);
	if (!data) return 0;
	
	glGenTextures(1, &tex_name);
	
	glBindTexture(GL_TEXTURE_2D, tex_name);
	glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB8, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, data);
	
	free(data);
	
	return tex_name;
}

void fsInit(int* argc, char*** argv)
{
	hs_init(argc, argv);
#ifdef __GLASGOW_HASKELL__
	hs_add_root(__stginit_LibFunslang);
#endif
}

void fsExit(void)
{
	hs_exit();
}
