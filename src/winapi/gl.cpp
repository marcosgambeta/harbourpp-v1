//
// WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI
//
// Copyright (c) 2025 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

// MIT License
//
// Copyright (c) 2025 Marcos Antonio Gambeta
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// NOTE: source code generated with the help of a code generator

#include <windows.h>
#include <GL\gl.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "hbwinuni.hpp"
#include "winapi.hpp"

#if 0
// typedef unsigned int GLenum;
#define wa_par_GLenum(n) static_cast<GLenum>(hb_parni(n))
#define wa_ret_GLenum(x) hb_retni(x)

// typedef unsigned char GLboolean;
#define wa_par_GLboolean(n) static_cast<GLboolean>(hb_parni(n))
#define wa_ret_GLboolean(x) hb_retni(x)

// typedef unsigned int GLbitfield;
#define wa_par_GLbitfield(n) static_cast<GLbitfield>(hb_parni(n))
#define wa_ret_GLbitfield(x) hb_retni(x)

// typedef signed char GLbyte;
#define wa_par_GLbyte(n) static_cast<GLbyte>(hb_parni(n))
#define wa_ret_GLbyte(x) hb_retni(x)

// typedef short GLshort;
#define wa_par_GLshort(n) static_cast<GLshort>(hb_parni(n))
#define wa_ret_GLshort(x) hb_retni(x)

// typedef int GLint;
#define wa_par_GLint(n) static_cast<GLint>(hb_parni(n))
#define wa_ret_GLint(x) hb_retni(x)

// typedef int GLsizei;
#define wa_par_GLsizei(n) static_cast<GLsizei>(hb_parni(n))
#define wa_ret_GLsizei(x) hb_retni(x)

// typedef unsigned char GLubyte;
#define wa_par_GLubyte(n) static_cast<GLubyte>(hb_parni(n))
#define wa_ret_GLubyte(x) hb_retni(x)

// typedef unsigned short GLushort;
#define wa_par_GLushort(n) static_cast<GLushort>(hb_parni(n))
#define wa_ret_GLushort(x) hb_retni(x)

// typedef unsigned int GLuint;
#define wa_par_GLuint(n) static_cast<GLuint>(hb_parni(n))
#define wa_ret_GLuint(x) hb_retni(x)

// typedef float GLfloat;
#define wa_par_GLfloat(n) static_cast<GLfloat>(hb_parnd(n))
#define wa_ret_GLfloat(x) hb_retnd(x)

// typedef float GLclampf;
#define wa_par_GLclampf(n) static_cast<GLclampf>(hb_parnd(n))
#define wa_ret_GLclampf(x) hb_retnd(x)

// typedef double GLdouble;
#define wa_par_GLdouble(n) static_cast<GLdouble>(hb_parnd(n))
#define wa_ret_GLdouble(x) hb_retnd(x)

// typedef double GLclampd;
#define wa_par_GLclampd(n) static_cast<GLclampd>(hb_parnd(n))
#define wa_ret_GLclampd(x) hb_retnd(x)

// typedef void GLvoid;
#endif

// WINGDIAPI void APIENTRY glAccum(GLenum op,GLfloat value)
HB_FUNC(WAGLACCUM)
{
  glAccum(wa_par_GLenum(1), wa_par_GLfloat(2));
}

// WINGDIAPI void APIENTRY glAlphaFunc(GLenum func,GLclampf ref)
HB_FUNC(WAGLALPHAFUNC)
{
  glAlphaFunc(wa_par_GLenum(1), wa_par_GLclampf(2));
}

// WINGDIAPI GLboolean APIENTRY glAreTexturesResident(GLsizei n,const GLuint *textures,GLboolean *residences)
#if 0
HB_FUNC(WAGLARETEXTURESRESIDENT)
{
  wa_ret_GLboolean(glAreTexturesResident(wa_par_GLsizei(1), const GLuint *textures, GLboolean *residences));
}
#endif

// WINGDIAPI void APIENTRY glArrayElement(GLint i)
HB_FUNC(WAGLARRAYELEMENT)
{
  glArrayElement(wa_par_GLint(1));
}

// WINGDIAPI void APIENTRY glBegin(GLenum mode)
HB_FUNC(WAGLBEGIN)
{
  glBegin(wa_par_GLenum(1));
}

// WINGDIAPI void APIENTRY glBindTexture(GLenum target,GLuint texture)
HB_FUNC(WAGLBINDTEXTURE)
{
  glBindTexture(wa_par_GLenum(1), wa_par_GLuint(2));
}

// WINGDIAPI void APIENTRY glBitmap(GLsizei width,GLsizei height,GLfloat xorig,GLfloat yorig,GLfloat xmove,GLfloat ymove,const GLubyte *bitmap)
HB_FUNC(WAGLBITMAP)
{
  glBitmap(wa_par_GLsizei(1), wa_par_GLsizei(2), wa_par_GLfloat(3), wa_par_GLfloat(4), wa_par_GLfloat(5), wa_par_GLfloat(6), static_cast<const GLubyte *>(hb_parptr(7)));
}

// WINGDIAPI void APIENTRY glBlendFunc(GLenum sfactor,GLenum dfactor)
HB_FUNC(WAGLBLENDFUNC)
{
  glBlendFunc(wa_par_GLenum(1), wa_par_GLenum(2));
}

// WINGDIAPI void APIENTRY glCallList(GLuint list)
HB_FUNC(WAGLCALLLIST)
{
  glCallList(wa_par_GLuint(1));
}

// WINGDIAPI void APIENTRY glCallLists(GLsizei n,GLenum type,const GLvoid *lists)
#if 0
HB_FUNC(WAGLCALLLISTS)
{
  glCallLists(wa_par_GLsizei(1), wa_par_GLenum(2), const GLvoid *lists);
}
#endif

// WINGDIAPI void APIENTRY glClear(GLbitfield mask)
HB_FUNC(WAGLCLEAR)
{
  glClear(wa_par_GLbitfield(1));
}

// WINGDIAPI void APIENTRY glClearAccum(GLfloat red,GLfloat green,GLfloat blue,GLfloat alpha)
HB_FUNC(WAGLCLEARACCUM)
{
  glClearAccum(wa_par_GLfloat(1), wa_par_GLfloat(2), wa_par_GLfloat(3), wa_par_GLfloat(4));
}

// WINGDIAPI void APIENTRY glClearColor(GLclampf red,GLclampf green,GLclampf blue,GLclampf alpha)
HB_FUNC(WAGLCLEARCOLOR)
{
  glClearColor(wa_par_GLclampf(1), wa_par_GLclampf(2), wa_par_GLclampf(3), wa_par_GLclampf(4));
}

// WINGDIAPI void APIENTRY glClearDepth(GLclampd depth)
HB_FUNC(WAGLCLEARDEPTH)
{
  glClearDepth(wa_par_GLclampd(1));
}

// WINGDIAPI void APIENTRY glClearIndex(GLfloat c)
HB_FUNC(WAGLCLEARINDEX)
{
  glClearIndex(wa_par_GLfloat(1));
}

// WINGDIAPI void APIENTRY glClearStencil(GLint s)
HB_FUNC(WAGLCLEARSTENCIL)
{
  glClearStencil(wa_par_GLint(1));
}

// WINGDIAPI void APIENTRY glClipPlane(GLenum plane,const GLdouble *equation)
#if 0
HB_FUNC(WAGLCLIPPLANE)
{
  glClipPlane(wa_par_GLenum(1), const GLdouble *equation);
}
#endif

// WINGDIAPI void APIENTRY glColor3b(GLbyte red,GLbyte green,GLbyte blue)
HB_FUNC(WAGLCOLOR3B)
{
  glColor3b(wa_par_GLbyte(1), wa_par_GLbyte(2), wa_par_GLbyte(3));
}

// WINGDIAPI void APIENTRY glColor3bv(const GLbyte *v)
#if 0
HB_FUNC(WAGLCOLOR3BV)
{
  glColor3bv(const GLbyte *v);
}
#endif

// WINGDIAPI void APIENTRY glColor3d(GLdouble red,GLdouble green,GLdouble blue)
HB_FUNC(WAGLCOLOR3D)
{
  glColor3d(wa_par_GLdouble(1), wa_par_GLdouble(2), wa_par_GLdouble(3));
}

// WINGDIAPI void APIENTRY glColor3dv(const GLdouble *v)
#if 0
HB_FUNC(WAGLCOLOR3DV)
{
  glColor3dv(const GLdouble *v);
}
#endif

// WINGDIAPI void APIENTRY glColor3f(GLfloat red,GLfloat green,GLfloat blue)
HB_FUNC(WAGLCOLOR3F)
{
  glColor3f(wa_par_GLfloat(1), wa_par_GLfloat(2), wa_par_GLfloat(3));
}

// WINGDIAPI void APIENTRY glColor3fv(const GLfloat *v)
#if 0
HB_FUNC(WAGLCOLOR3FV)
{
  glColor3fv(const GLfloat *v);
}
#endif

// WINGDIAPI void APIENTRY glColor3i(GLint red,GLint green,GLint blue)
HB_FUNC(WAGLCOLOR3I)
{
  glColor3i(wa_par_GLint(1), wa_par_GLint(2), wa_par_GLint(3));
}

// WINGDIAPI void APIENTRY glColor3iv(const GLint *v)
#if 0
HB_FUNC(WAGLCOLOR3IV)
{
  glColor3iv(const GLint *v);
}
#endif

// WINGDIAPI void APIENTRY glColor3s(GLshort red,GLshort green,GLshort blue)
HB_FUNC(WAGLCOLOR3S)
{
  glColor3s(wa_par_GLshort(1), wa_par_GLshort(2), wa_par_GLshort(3));
}

// WINGDIAPI void APIENTRY glColor3sv(const GLshort *v)
#if 0
HB_FUNC(WAGLCOLOR3SV)
{
  glColor3sv(const GLshort *v);
}
#endif

// WINGDIAPI void APIENTRY glColor3ub(GLubyte red,GLubyte green,GLubyte blue)
HB_FUNC(WAGLCOLOR3UB)
{
  glColor3ub(wa_par_GLubyte(1), wa_par_GLubyte(2), wa_par_GLubyte(3));
}

// WINGDIAPI void APIENTRY glColor3ubv(const GLubyte *v)
#if 0
HB_FUNC(WAGLCOLOR3UBV)
{
  glColor3ubv(const GLubyte *v);
}
#endif

// WINGDIAPI void APIENTRY glColor3ui(GLuint red,GLuint green,GLuint blue)
HB_FUNC(WAGLCOLOR3UI)
{
  glColor3ui(wa_par_GLuint(1), wa_par_GLuint(2), wa_par_GLuint(3));
}

// WINGDIAPI void APIENTRY glColor3uiv(const GLuint *v)
#if 0
HB_FUNC(WAGLCOLOR3UIV)
{
  glColor3uiv(const GLuint *v);
}
#endif

// WINGDIAPI void APIENTRY glColor3us(GLushort red,GLushort green,GLushort blue)
HB_FUNC(WAGLCOLOR3US)
{
  glColor3us(wa_par_GLushort(1), wa_par_GLushort(2), wa_par_GLushort(3));
}

// WINGDIAPI void APIENTRY glColor3usv(const GLushort *v)
#if 0
HB_FUNC(WAGLCOLOR3USV)
{
  glColor3usv(const GLushort *v);
}
#endif

// WINGDIAPI void APIENTRY glColor4b(GLbyte red,GLbyte green,GLbyte blue,GLbyte alpha)
HB_FUNC(WAGLCOLOR4B)
{
  glColor4b(wa_par_GLbyte(1), wa_par_GLbyte(2), wa_par_GLbyte(3), wa_par_GLbyte(4));
}

// WINGDIAPI void APIENTRY glColor4bv(const GLbyte *v)
#if 0
HB_FUNC(WAGLCOLOR4BV)
{
  glColor4bv(const GLbyte *v);
}
#endif

// WINGDIAPI void APIENTRY glColor4d(GLdouble red,GLdouble green,GLdouble blue,GLdouble alpha)
HB_FUNC(WAGLCOLOR4D)
{
  glColor4d(wa_par_GLdouble(1), wa_par_GLdouble(2), wa_par_GLdouble(3), wa_par_GLdouble(4));
}

// WINGDIAPI void APIENTRY glColor4dv(const GLdouble *v)
#if 0
HB_FUNC(WAGLCOLOR4DV)
{
  glColor4dv(const GLdouble *v);
}
#endif

// WINGDIAPI void APIENTRY glColor4f(GLfloat red,GLfloat green,GLfloat blue,GLfloat alpha)
HB_FUNC(WAGLCOLOR4F)
{
  glColor4f(wa_par_GLfloat(1), wa_par_GLfloat(2), wa_par_GLfloat(3), wa_par_GLfloat(4));
}

// WINGDIAPI void APIENTRY glColor4fv(const GLfloat *v)
#if 0
HB_FUNC(WAGLCOLOR4FV)
{
  glColor4fv(const GLfloat *v);
}
#endif

// WINGDIAPI void APIENTRY glColor4i(GLint red,GLint green,GLint blue,GLint alpha)
HB_FUNC(WAGLCOLOR4I)
{
  glColor4i(wa_par_GLint(1), wa_par_GLint(2), wa_par_GLint(3), wa_par_GLint(4));
}

// WINGDIAPI void APIENTRY glColor4iv(const GLint *v)
#if 0
HB_FUNC(WAGLCOLOR4IV)
{
  glColor4iv(const GLint *v);
}
#endif

// WINGDIAPI void APIENTRY glColor4s(GLshort red,GLshort green,GLshort blue,GLshort alpha)
HB_FUNC(WAGLCOLOR4S)
{
  glColor4s(wa_par_GLshort(1), wa_par_GLshort(2), wa_par_GLshort(3), wa_par_GLshort(4));
}

// WINGDIAPI void APIENTRY glColor4sv(const GLshort *v)
#if 0
HB_FUNC(WAGLCOLOR4SV)
{
  glColor4sv(const GLshort *v);
}
#endif

// WINGDIAPI void APIENTRY glColor4ub(GLubyte red,GLubyte green,GLubyte blue,GLubyte alpha)
HB_FUNC(WAGLCOLOR4UB)
{
  glColor4ub(wa_par_GLubyte(1), wa_par_GLubyte(2), wa_par_GLubyte(3), wa_par_GLubyte(4));
}

// WINGDIAPI void APIENTRY glColor4ubv(const GLubyte *v)
#if 0
HB_FUNC(WAGLCOLOR4UBV)
{
  glColor4ubv(const GLubyte *v)
}
#endif

// WINGDIAPI void APIENTRY glColor4ui(GLuint red,GLuint green,GLuint blue,GLuint alpha)
HB_FUNC(WAGLCOLOR4UI)
{
  glColor4ui(wa_par_GLuint(1), wa_par_GLuint(2), wa_par_GLuint(3), wa_par_GLuint(4));
}

// WINGDIAPI void APIENTRY glColor4uiv(const GLuint *v)
#if 0
HB_FUNC(WAGLCOLOR4UIV)
{
  glColor4uiv(const GLuint *v);
}
#endif

// WINGDIAPI void APIENTRY glColor4us(GLushort red,GLushort green,GLushort blue,GLushort alpha)
HB_FUNC(WAGLCOLOR4US)
{
  glColor4us(wa_par_GLushort(1), wa_par_GLushort(2), wa_par_GLushort(3), wa_par_GLushort(4));
}

// WINGDIAPI void APIENTRY glColor4usv(const GLushort *v)
#if 0
HB_FUNC(WAGLCOLOR4USV)
{
  glColor4usv(const GLushort *v);
}
#endif

// WINGDIAPI void APIENTRY glColorMask(GLboolean red,GLboolean green,GLboolean blue,GLboolean alpha)
HB_FUNC(WAGLCOLORMASK)
{
  glColorMask(wa_par_GLboolean(1), wa_par_GLboolean(2), wa_par_GLboolean(3), wa_par_GLboolean(4));
}

// WINGDIAPI void APIENTRY glColorMaterial(GLenum face,GLenum mode)
HB_FUNC(WAGLCOLORMATERIAL)
{
  glColorMaterial(wa_par_GLenum(1), wa_par_GLenum(2));
}

// WINGDIAPI void APIENTRY glColorPointer(GLint size,GLenum type,GLsizei stride,const GLvoid *pointer)
#if 0
HB_FUNC(WAGLCOLORPOINTER)
{
  glColorPointer(wa_par_GLint(1), wa_par_GLenum(2), wa_par_GLsizei(3), const GLvoid *pointer);
}
#endif

// WINGDIAPI void APIENTRY glCopyPixels(GLint x,GLint y,GLsizei width,GLsizei height,GLenum type)
HB_FUNC(WAGLCOPYPIXELS)
{
  glCopyPixels(wa_par_GLint(1), wa_par_GLint(2), wa_par_GLsizei(3), wa_par_GLsizei(4), wa_par_GLenum(5));
}

// WINGDIAPI void APIENTRY glCopyTexImage1D(GLenum target,GLint level,GLenum internalFormat,GLint x,GLint y,GLsizei width,GLint border)
HB_FUNC(WAGLCOPYTEXIMAGE1D)
{
  glCopyTexImage1D(wa_par_GLenum(1), wa_par_GLint(2), wa_par_GLenum(3), wa_par_GLint(4), wa_par_GLint(5), wa_par_GLsizei(6), wa_par_GLint(7));
}

// WINGDIAPI void APIENTRY glCopyTexImage2D(GLenum target,GLint level,GLenum internalFormat,GLint x,GLint y,GLsizei width,GLsizei height,GLint border)
HB_FUNC(WAGLCOPYTEXIMAGE2D)
{
  glCopyTexImage2D(wa_par_GLenum(1), wa_par_GLint(2), wa_par_GLenum(3), wa_par_GLint(4), wa_par_GLint(5), wa_par_GLsizei(6), wa_par_GLsizei(7), wa_par_GLint(8));
}

// WINGDIAPI void APIENTRY glCopyTexSubImage1D(GLenum target,GLint level,GLint xoffset,GLint x,GLint y,GLsizei width)
HB_FUNC(WAGLCOPYTEXSUBIMAGE1D)
{
  glCopyTexSubImage1D(wa_par_GLenum(1), wa_par_GLint(2), wa_par_GLint(3), wa_par_GLint(4), wa_par_GLint(5), wa_par_GLsizei(6));
}

// WINGDIAPI void APIENTRY glCopyTexSubImage2D(GLenum target,GLint level,GLint xoffset,GLint yoffset,GLint x,GLint y,GLsizei width,GLsizei height)
HB_FUNC(WAGLCOPYTEXSUBIMAGE2D)
{
  glCopyTexSubImage2D(wa_par_GLenum(1), wa_par_GLint(2), wa_par_GLint(3), wa_par_GLint(4), wa_par_GLint(5), wa_par_GLint(6), wa_par_GLsizei(7), wa_par_GLsizei(8));
}

// WINGDIAPI void APIENTRY glCullFace(GLenum mode)
HB_FUNC(WAGLCULLFACE)
{
  glCullFace(wa_par_GLenum(1));
}

// WINGDIAPI void APIENTRY glDeleteLists(GLuint list,GLsizei range)
HB_FUNC(WAGLDELETELISTS)
{
  glDeleteLists(wa_par_GLuint(1), wa_par_GLsizei(2));
}

// WINGDIAPI void APIENTRY glDeleteTextures(GLsizei n,const GLuint *textures)
#if 0
HB_FUNC(WAGLDELETETEXTURES)
{
  glDeleteTextures(wa_par_GLsizei(1), const GLuint *textures);
}
#endif

// WINGDIAPI void APIENTRY glDepthFunc(GLenum func)
HB_FUNC(WAGLDEPTHFUNC)
{
  glDepthFunc(wa_par_GLenum(1));
}

// WINGDIAPI void APIENTRY glDepthMask(GLboolean flag)
HB_FUNC(WAGLDEPTHMASK)
{
  glDepthMask(wa_par_GLboolean(1));
}

// WINGDIAPI void APIENTRY glDepthRange (GLclampd zNear,GLclampd zFar)
HB_FUNC(WAGLDEPTHRANGE)
{
  glDepthRange(wa_par_GLclampd(1), wa_par_GLclampd(2));
}

// WINGDIAPI void APIENTRY glDisable(GLenum cap)
HB_FUNC(WAGLDISABLE)
{
  glDisable(wa_par_GLenum(1));
}

// WINGDIAPI void APIENTRY glDisableClientState(GLenum array)
HB_FUNC(WAGLDISABLECLIENTSTATE)
{
  glDisableClientState(wa_par_GLenum(1));
}

// WINGDIAPI void APIENTRY glDrawArrays(GLenum mode,GLint first,GLsizei count)
HB_FUNC(WAGLDRAWARRAYS)
{
  glDrawArrays(wa_par_GLenum(1), wa_par_GLint(2), wa_par_GLsizei(3));
}

// WINGDIAPI void APIENTRY glDrawBuffer(GLenum mode)
HB_FUNC(WAGLDRAWBUFFER)
{
  glDrawBuffer(wa_par_GLenum(1));
}

// WINGDIAPI void APIENTRY glDrawElements(GLenum mode,GLsizei count,GLenum type,const GLvoid *indices)
#if 0
HB_FUNC(WAGLDRAWELEMENTS)
{
  glDrawElements(wa_par_GLenum(1), wa_par_GLsizei(2), wa_par_GLenum(3), const GLvoid *indices);
}
#endif

// WINGDIAPI void APIENTRY glDrawPixels(GLsizei width,GLsizei height,GLenum format,GLenum type,const GLvoid *pixels)
#if 0
HB_FUNC(WAGLDRAWPIXELS)
{
  glDrawPixels(wa_par_GLsizei(1), wa_par_GLsizei(2), wa_par_GLenum(3), wa_par_GLenum(4), const GLvoid *pixels);
}
#endif

// WINGDIAPI void APIENTRY glEdgeFlag(GLboolean flag)
HB_FUNC(WAGLEDGEFLAG)
{
  glEdgeFlag(wa_par_GLboolean(1));
}

// WINGDIAPI void APIENTRY glEdgeFlagPointer(GLsizei stride,const GLvoid *pointer)
#if 0
HB_FUNC(WAGLEDGEFLAGPOINTER)
{
  glEdgeFlagPointer(wa_par_GLsizei(1), const GLvoid *pointer);
}
#endif

// WINGDIAPI void APIENTRY glEdgeFlagv(const GLboolean *flag)
#if 0
HB_FUNC(WAGLEDGEFLAGV)
{
  glEdgeFlagv(const GLboolean *flag);
}
#endif

// WINGDIAPI void APIENTRY glEnable(GLenum cap)
HB_FUNC(WAGLENABLE)
{
  glEnable(wa_par_GLenum(1));
}

// WINGDIAPI void APIENTRY glEnableClientState(GLenum array)
HB_FUNC(WAGLENABLECLIENTSTATE)
{
  glEnableClientState(wa_par_GLenum(1));
}

// WINGDIAPI void APIENTRY glEnd(void)
HB_FUNC(WAGLEND)
{
  glEnd();
}

// WINGDIAPI void APIENTRY glEndList(void)
HB_FUNC(WAGLENDLIST)
{
  glEndList();
}

// WINGDIAPI void APIENTRY glEvalCoord1d(GLdouble u)
HB_FUNC(WAGLEVALCOORD1D)
{
  glEvalCoord1d(wa_par_GLdouble(1));
}

// WINGDIAPI void APIENTRY glEvalCoord1dv(const GLdouble *u)
#if 0
HB_FUNC(WAGLEVALCOORD1DV)
{
  glEvalCoord1dv(const GLdouble *u);
}
#endif

// WINGDIAPI void APIENTRY glEvalCoord1f(GLfloat u)
HB_FUNC(WAGLEVALCOORD1F)
{
  glEvalCoord1f(wa_par_GLfloat(1));
}

// WINGDIAPI void APIENTRY glEvalCoord1fv(const GLfloat *u)
#if 0
HB_FUNC(WAGLEVALCOORD1FV)
{
  glEvalCoord1fv(const GLfloat *u);
}
#endif

// WINGDIAPI void APIENTRY glEvalCoord2d(GLdouble u,GLdouble v)
HB_FUNC(WAGLEVALCOORD2D)
{
  glEvalCoord2d(wa_par_GLdouble(1), wa_par_GLdouble(2));
}

// WINGDIAPI void APIENTRY glEvalCoord2dv(const GLdouble *u)
#if 0
HB_FUNC(WAGLEVALCOORD2DV)
{
  glEvalCoord2dv(const GLdouble *u);
}
#endif

// WINGDIAPI void APIENTRY glEvalCoord2f(GLfloat u,GLfloat v)
HB_FUNC(WAGLEVALCOORD2F)
{
  glEvalCoord2f(wa_par_GLfloat(1), wa_par_GLfloat(2));
}

// WINGDIAPI void APIENTRY glEvalCoord2fv(const GLfloat *u)
#if 0
HB_FUNC(WAGLEVALCOORD2FV)
{
  glEvalCoord2fv(const GLfloat *u);
}
#endif

// WINGDIAPI void APIENTRY glEvalMesh1(GLenum mode,GLint i1,GLint i2)
HB_FUNC(WAGLEVALMESH1)
{
  glEvalMesh1(wa_par_GLenum(1), wa_par_GLint(2), wa_par_GLint(3));
}

// WINGDIAPI void APIENTRY glEvalMesh2(GLenum mode,GLint i1,GLint i2,GLint j1,GLint j2)
HB_FUNC(WAGLEVALMESH2)
{
  glEvalMesh2(wa_par_GLenum(1), wa_par_GLint(2), wa_par_GLint(3), wa_par_GLint(4), wa_par_GLint(5));
}

// WINGDIAPI void APIENTRY glEvalPoint1(GLint i)
HB_FUNC(WAGLEVALPOINT1)
{
  glEvalPoint1(wa_par_GLint(1));
}

// WINGDIAPI void APIENTRY glEvalPoint2(GLint i,GLint j)
HB_FUNC(WAGLEVALPOINT2)
{
  glEvalPoint2(wa_par_GLint(1), wa_par_GLint(2));
}

// WINGDIAPI void APIENTRY glFeedbackBuffer(GLsizei size,GLenum type,GLfloat *buffer)
#if 0
HB_FUNC(WAGLFEEDBACKBUFFER)
{
  glFeedbackBuffer(wa_par_GLsizei(1), wa_par_GLenum(2), GLfloat *buffer);
}
#endif

// WINGDIAPI void APIENTRY glFinish(void)
HB_FUNC(WAGLFINISH)
{
  glFinish();
}

// WINGDIAPI void APIENTRY glFlush(void)
HB_FUNC(WAGLFLUSH)
{
  glFlush();
}

// WINGDIAPI void APIENTRY glFogf(GLenum pname,GLfloat param)
HB_FUNC(WAGLFOGF)
{
  glFogf(wa_par_GLenum(1), wa_par_GLfloat(2));
}

// WINGDIAPI void APIENTRY glFogfv(GLenum pname,const GLfloat *params)
#if 0
HB_FUNC(WAGLFOGFV)
{
  glFogfv(wa_par_GLenum(1), const GLfloat *params);
}
#endif

// WINGDIAPI void APIENTRY glFogi(GLenum pname,GLint param)
HB_FUNC(WAGLFOGI)
{
  glFogi(wa_par_GLenum(1), wa_par_GLint(2));
}

// WINGDIAPI void APIENTRY glFogiv(GLenum pname,const GLint *params)
#if 0
HB_FUNC(WAGLFOGIV)
{
  glFogiv(wa_par_GLenum(1), const GLint *params);
}
#endif

// WINGDIAPI void APIENTRY glFrontFace(GLenum mode)
HB_FUNC(WAGLFRONTFACE)
{
  glFrontFace(wa_par_GLenum(1));
}

// WINGDIAPI void APIENTRY glFrustum(GLdouble left,GLdouble right,GLdouble bottom,GLdouble top,GLdouble zNear,GLdouble zFar)
HB_FUNC(WAGLFRUSTUM)
{
  glFrustum(wa_par_GLdouble(1), wa_par_GLdouble(2), wa_par_GLdouble(3), wa_par_GLdouble(4), wa_par_GLdouble(5), wa_par_GLdouble(6));
}

// WINGDIAPI GLuint APIENTRY glGenLists(GLsizei range)
HB_FUNC(WAGLGENLISTS)
{
  wa_ret_GLuint(glGenLists(wa_par_GLsizei(1)));
}

// WINGDIAPI void APIENTRY glGenTextures(GLsizei n,GLuint *textures)
#if 0
HB_FUNC(WAGLGENTEXTURES)
{
  glGenTextures(wa_par_GLsizei(1), GLuint *textures);
}
#endif

// WINGDIAPI void APIENTRY glGetBooleanv(GLenum pname,GLboolean *params)
#if 0
HB_FUNC(WAGLGETBOOLEANV)
{
  glGetBooleanv(wa_par_GLenum(1), GLboolean *params);
}
#endif

// WINGDIAPI void APIENTRY glGetClipPlane(GLenum plane,GLdouble *equation)
#if 0
HB_FUNC(WAGLGETCLIPPLANE)
{
  glGetClipPlane(wa_par_GLenum(1), GLdouble *equation);
}
#endif

// WINGDIAPI void APIENTRY glGetDoublev(GLenum pname,GLdouble *params)
#if 0
HB_FUNC(WAGLGETDOUBLEV)
{
  glGetDoublev(wa_par_GLenum(1), GLdouble *params);
}
#endif

// WINGDIAPI GLenum APIENTRY glGetError(void)
HB_FUNC(WAGLGETERROR)
{
  glGetError();
}

// WINGDIAPI void APIENTRY glGetFloatv(GLenum pname,GLfloat *params)
#if 0
HB_FUNC(WAGLGETFLOATV)
{
  glGetFloatv(wa_par_GLenum(1), GLfloat *params);
}
#endif

// WINGDIAPI void APIENTRY glGetIntegerv(GLenum pname,GLint *params)
#if 0
HB_FUNC(WAGLGETINTEGERV)
{
  glGetIntegerv(wa_par_GLenum(1), GLint *params);
}
#endif

// WINGDIAPI void APIENTRY glGetLightfv(GLenum light,GLenum pname,GLfloat *params)
#if 0
HB_FUNC(WAGLGETLIGHTFV)
{
  glGetLightfv(wa_par_GLenum(1), wa_par_GLenum(2), GLfloat *params);
}
#endif

// WINGDIAPI void APIENTRY glGetLightiv(GLenum light,GLenum pname,GLint *params)
#if 0
HB_FUNC(WAGLGETLIGHTIV)
{
  glGetLightiv(wa_par_GLenum(1), wa_par_GLenum(2), GLint *params);
}
#endif

// WINGDIAPI void APIENTRY glGetMapdv(GLenum target,GLenum query,GLdouble *v)
#if 0
HB_FUNC(WAGLGETMAPDV)
{
  glGetMapdv(wa_Par_GLenum(1), wa_par_GLenum(2), GLdouble *v);
}
#endif

// WINGDIAPI void APIENTRY glGetMapfv(GLenum target,GLenum query,GLfloat *v)
#if 0
HB_FUNC(WAGLGETMAPFV)
{
  glGetMapfv(wa_par_GLenum(1), wa_par_GLenum(2), GLfloat *v);
}
#endif

// WINGDIAPI void APIENTRY glGetMapiv(GLenum target,GLenum query,GLint *v)
#if 0
HB_FUNC(WAGLGETMAPIV)
{
  glGetMapiv(wa_par_GLenum(1), wa_par_GLenum(2), GLint *v);
}
#endif

// WINGDIAPI void APIENTRY glGetMaterialfv(GLenum face,GLenum pname,GLfloat *params)
#if 0
HB_FUNC(WAGLGETMATERIALFV)
{
  glGetMaterialfv(wa_par_GLenum(1), wa_par_GLenum(2), GLfloat *params);
}
#endif

// WINGDIAPI void APIENTRY glGetMaterialiv(GLenum face,GLenum pname,GLint *params)
#if 0
HB_FUNC(WAGLGETMATERIALIV)
{
  glGetMaterialiv(wa_par_GLenum(1), wa_par_GLenum(2), GLint *params);
}
#endif

// WINGDIAPI void APIENTRY glGetPixelMapfv(GLenum map,GLfloat *values)
#if 0
HB_FUNC(WAGLGETPIXELMAPFV)
{
  glGetPixelMapfv(wa_par_GLenum(1), GLfloat *values);
}
#endif

// WINGDIAPI void APIENTRY glGetPixelMapuiv(GLenum map,GLuint *values)
#if 0
HB_FUNC(WAGLGETPIXELMAPUIV)
{
  glGetPixelMapuiv(wa_par_GLenum(1), GLuint *values);
}
#endif

// WINGDIAPI void APIENTRY glGetPixelMapusv(GLenum map,GLushort *values)
#if 0
HB_FUNC(WAGLGETPIXELMAPUSV)
{
  glGetPixelMapusv(wa_par_GLenum(1), GLushort *values);
}
#endif

// WINGDIAPI void APIENTRY glGetPointerv(GLenum pname,GLvoid **params)
#if 0
HB_FUNC(WAGLGETPOINTERV)
{
  glGetPointerv(wa_par_GLenum(1), GLvoid **params);
}
#endif

// WINGDIAPI void APIENTRY glGetPolygonStipple(GLubyte *mask)
#if 0
HB_FUNC(WAGLGETPOLYGONSTIPPLE)
{
  glGetPolygonStipple(GLubyte *mask);
}
#endif

// WINGDIAPI const GLubyte *APIENTRY glGetString(GLenum name)
#if 0
HB_FUNC(WAGLGETSTRING)
{
  const GLubyte * glGetString(wa_par_GLenum(1));
}
#endif

// WINGDIAPI void APIENTRY glGetTexEnvfv(GLenum target,GLenum pname,GLfloat *params)
#if 0
HB_FUNC(WAGLGETTEXENVFV)
{
  glGetTexEnvfv(wa_par_GLenum(1), wa_par_GLenum(2), GLfloat *params);
}
#endif

// WINGDIAPI void APIENTRY glGetTexEnviv(GLenum target,GLenum pname,GLint *params)
#if 0
HB_FUNC(WAGLGETTEXENVIV)
{
  glGetTexEnviv(wa_par_GLenum(1), wa_par_GLenum(2), GLint *params);
}
#endif

// WINGDIAPI void APIENTRY glGetTexGendv(GLenum coord,GLenum pname,GLdouble *params)
#if 0
HB_FUNC(WAGLGETTEXGENDV)
{
  glGetTexGendv(wa_par_GLenum(1), wa_par_GLenum(2), GLdouble *params);
}
#endif

// WINGDIAPI void APIENTRY glGetTexGenfv(GLenum coord,GLenum pname,GLfloat *params)
#if 0
HB_FUNC(WAGLGETTEXGENFV)
{
  glGetTexGenfv(wa_par_GLenum(1), wa_par_GLenum(2), GLfloat *params);
}
#endif

// WINGDIAPI void APIENTRY glGetTexGeniv(GLenum coord,GLenum pname,GLint *params)
#if 0
HB_FUNC(WAGLGETTEXGENIV)
{
  glGetTexGeniv(wa_par_GLenum(1), wa_par_GLenum(2), GLint *params);
}
#endif

// WINGDIAPI void APIENTRY glGetTexImage(GLenum target,GLint level,GLenum format,GLenum type,GLvoid *pixels)
#if 0
HB_FUNC(WAGLGETTEXIMAGE)
{
  glGetTexImage(wa_par_GLenum(1), wa_par_GLint(2), wa_par_GLenum(3), wa_par_GLenum(4), GLvoid *pixels);
}
#endif

// WINGDIAPI void APIENTRY glGetTexLevelParameterfv(GLenum target,GLint level,GLenum pname,GLfloat *params)
#if 0
HB_FUNC(WAGLGETTEXLEVELPARAMETERFV)
{
  glGetTexLevelParameterfv(wa_par_GLenum(1), wa_par_GLint(2), wa_par_GLenum(3), GLfloat *params);
}
#endif

// WINGDIAPI void APIENTRY glGetTexLevelParameteriv(GLenum target,GLint level,GLenum pname,GLint *params)
#if 0
HB_FUNC(WAGLGETTEXLEVELPARAMETERIV)
{
  glGetTexLevelParameteriv(wa_par_GLenum(1), wa_par_GLint(2), wa_par_GLenum(3), GLint *params);
}
#endif

// WINGDIAPI void APIENTRY glGetTexParameterfv(GLenum target,GLenum pname,GLfloat *params)
#if 0
HB_FUNC(WAGLGETTEXPARAMETERFV)
{
  glGetTexParameterfv(wa_par_GLenum(1), wa_par_GLenum(2), GLfloat *params);
}
#endif

// WINGDIAPI void APIENTRY glGetTexParameteriv(GLenum target,GLenum pname,GLint *params)
#if 0
HB_FUNC(WAGLGETTEXPARAMETERIV)
{
  glGetTexParameteriv(wa_par_GLenum(1), wa_par_GLenum(2), GLint *params);
}
#endif

// WINGDIAPI void APIENTRY glHint(GLenum target,GLenum mode)
HB_FUNC(WAGLHINT)
{
  glHint(wa_par_GLenum(1), wa_par_GLenum(2));
}

// WINGDIAPI void APIENTRY glIndexMask(GLuint mask)
HB_FUNC(WAGLINDEXMASK)
{
  glIndexMask(wa_par_GLuint(1));
}

// WINGDIAPI void APIENTRY glIndexPointer(GLenum type,GLsizei stride,const GLvoid *pointer)
#if 0
HB_FUNC(WAGLINDEXPOINTER)
{
  glIndexPointer(wa_par_GLenum(1), wa_par_GLsizei(2), const GLvoid *pointer);
}
#endif

// WINGDIAPI void APIENTRY glIndexd(GLdouble c)
HB_FUNC(WAGLINDEXD)
{
  glIndexd(wa_par_GLdouble(1));
}

// WINGDIAPI void APIENTRY glIndexdv(const GLdouble *c)
#if 0
HB_FUNC(WAGLINDEXDV)
{
  glIndexdv(const GLdouble *c);
}
#endif

// WINGDIAPI void APIENTRY glIndexf(GLfloat c)
HB_FUNC(WAGLINDEXF)
{
  glIndexf(wa_par_GLfloat(1));
}

// WINGDIAPI void APIENTRY glIndexfv(const GLfloat *c)
#if 0
HB_FUNC(WAGLINDEXFV)
{
  glIndexfv(const GLfloat *c);
}
#endif

// WINGDIAPI void APIENTRY glIndexi(GLint c)
HB_FUNC(WAGLINDEXI)
{
  glIndexi(wa_par_GLint(1));
}

// WINGDIAPI void APIENTRY glIndexiv(const GLint *c)
#if 0
HB_FUNC(WAGLINDEXIV)
{
  glIndexiv(const GLint *c);
}
#endif

// WINGDIAPI void APIENTRY glIndexs(GLshort c)
HB_FUNC(WAGLINDEXS)
{
  glIndexs(wa_par_GLshort(1));
}

// WINGDIAPI void APIENTRY glIndexsv(const GLshort *c)
#if 0
HB_FUNC(WAGLINDEXSV)
{
  glIndexsv(const GLshort *c);
}
#endif

// WINGDIAPI void APIENTRY glIndexub(GLubyte c)
HB_FUNC(WAGLINDEXUB)
{
  glIndexub(wa_par_GLubyte(1));
}

// WINGDIAPI void APIENTRY glIndexubv(const GLubyte *c)
#if 0
HB_FUNC(WAGLINDEXUBV)
{
  glIndexubv(const GLubyte *c);
}
#endif

// WINGDIAPI void APIENTRY glInitNames(void)
HB_FUNC(WAGLINITNAMES)
{
  glInitNames();
}

// WINGDIAPI void APIENTRY glInterleavedArrays(GLenum format,GLsizei stride,const GLvoid *pointer)
#if 0
HB_FUNC(WAGLINTERLEAVEDARRAYS)
{
  glInterleavedArrays(wa_par_GLenum(1), wa_par_GLsizei(2), const GLvoid *pointer);
}
#endif

// WINGDIAPI GLboolean APIENTRY glIsEnabled(GLenum cap)
HB_FUNC(WAGLISENABLED)
{
  wa_ret_GLboolean(glIsEnabled(wa_par_GLenum(1)));
}

// WINGDIAPI GLboolean APIENTRY glIsList(GLuint list)
HB_FUNC(WAGLISLIST)
{
  wa_ret_GLboolean(glIsList(wa_par_GLuint(1)));
}

// WINGDIAPI GLboolean APIENTRY glIsTexture(GLuint texture)
HB_FUNC(WAGLISTEXTURE)
{
  wa_ret_GLboolean(glIsTexture(wa_par_GLuint(1)));
}

// WINGDIAPI void APIENTRY glLightModelf(GLenum pname,GLfloat param)
HB_FUNC(WAGLLIGHTMODELF)
{
  glLightModelf(wa_par_GLenum(1), wa_par_GLfloat(2));
}

// WINGDIAPI void APIENTRY glLightModelfv(GLenum pname,const GLfloat *params)
#if 0
HB_FUNC(WAGLLIGHTMODELFV)
{
  glLightModelfv(wa_par_GLenum(1), const GLfloat *params);
}
#endif

// WINGDIAPI void APIENTRY glLightModeli(GLenum pname,GLint param)
HB_FUNC(WAGLLIGHTMODELI)
{
  glLightModeli(wa_par_GLenum(1), wa_par_GLint(2));
}

// WINGDIAPI void APIENTRY glLightModeliv(GLenum pname,const GLint *params)
#if 0
HB_FUNC(WAGLLIGHTMODELIV)
{
  glLightModeliv(wa_par_GLenum(1), const GLint *params);
}
#endif

// WINGDIAPI void APIENTRY glLightf(GLenum light,GLenum pname,GLfloat param)
HB_FUNC(WAGLLIGHTF)
{
  glLightf(wa_par_GLenum(1), wa_par_GLenum(2), wa_par_GLfloat(3));
}

// WINGDIAPI void APIENTRY glLightfv(GLenum light,GLenum pname,const GLfloat *params)
#if 0
HB_FUNC(WAGLLIGHTFV)
{
  glLightfv(wa_par_GLenum(1), wa_par_GLenum(2), const GLfloat *params);
}
#endif

// WINGDIAPI void APIENTRY glLighti(GLenum light,GLenum pname,GLint param)
HB_FUNC(WAGLLIGHTI)
{
  glLighti(wa_par_GLenum(1), wa_par_GLenum(2), wa_par_GLint(3));
}

// WINGDIAPI void APIENTRY glLightiv(GLenum light,GLenum pname,const GLint *params)
#if 0
HB_FUNC(WAGLLIGHTIV)
{
  glLightiv(wa_par_GLenum(1), wa_par_GLenum(2), const GLint *params);
}
#endif

// WINGDIAPI void APIENTRY glLineStipple(GLint factor,GLushort pattern)
HB_FUNC(WAGLLINESTIPPLE)
{
  glLineStipple(wa_par_GLint(1), wa_par_GLushort(2));
}

// WINGDIAPI void APIENTRY glLineWidth(GLfloat width)
HB_FUNC(WAGLLINEWIDTH)
{
  glLineWidth(wa_par_GLfloat(1));
}

// WINGDIAPI void APIENTRY glListBase(GLuint base)
HB_FUNC(WAGLLISTBASE)
{
  glListBase(wa_par_GLuint(1));
}

// WINGDIAPI void APIENTRY glLoadIdentity(void)
HB_FUNC(WAGLLOADIDENTITY)
{
  glLoadIdentity();
}

// WINGDIAPI void APIENTRY glLoadMatrixd(const GLdouble *m)
#if 0
HB_FUNC(WAGLLOADMATRIXD)
{
  glLoadMatrixd(const GLdouble *m);
}
#endif

// WINGDIAPI void APIENTRY glLoadMatrixf(const GLfloat *m)
#if 0
HB_FUNC(WAGLLOADMATRIXF)
{
  glLoadMatrixf(const GLfloat *m);
}
#endif

// WINGDIAPI void APIENTRY glLoadName(GLuint name)
HB_FUNC(WAGLLOADNAME)
{
  glLoadName(wa_par_GLuint(1));
}

// WINGDIAPI void APIENTRY glLogicOp(GLenum opcode)
HB_FUNC(WAGLLOGICOP)
{
  glLogicOp(wa_par_GLenum(1));
}

// WINGDIAPI void APIENTRY glMap1d(GLenum target,GLdouble u1,GLdouble u2,GLint stride,GLint order,const GLdouble *points)
#if 0
HB_FUNC(WAGLMAP1D)
{
  glMap1d(wa_par_GLenum(1), wa_par_GLdouble(2), wa_par_GLdouble(3), wa_par_GLint(4), wa_par_GLint(5), const GLdouble *points);
}
#endif

// WINGDIAPI void APIENTRY glMap1f(GLenum target,GLfloat u1,GLfloat u2,GLint stride,GLint order,const GLfloat *points)
#if 0
HB_FUNC(WAGLMAP1F)
{
  glMap1f(wa_par_GLenum(1), wa_par_GLfloat(2), wa_par_GLfloat(3), wa_par_GLint(4), wa_par_GLint(5), const GLfloat *points);
}
#endif

// WINGDIAPI void APIENTRY glMap2d(GLenum target,GLdouble u1,GLdouble u2,GLint ustride,GLint uorder,GLdouble v1,GLdouble v2,GLint vstride,GLint vorder,const GLdouble *points)
#if 0
HB_FUNC(WAGLMAP2D)
{
  glMap2d(wa_par_GLenum(1), wa_par_GLdouble(2), wa_par_GLdouble(3), wa_par_GLint(4), wa_par_GLint(5), wa_par_GLdouble(6), wa_par_GLdouble(7), wa_par_GLint(8), wa_par_GLint(9), const GLdouble *points);
}
#endif

// WINGDIAPI void APIENTRY glMap2f(GLenum target,GLfloat u1,GLfloat u2,GLint ustride,GLint uorder,GLfloat v1,GLfloat v2,GLint vstride,GLint vorder,const GLfloat *points)
#if 0
HB_FUNC(WAGLMAP2F)
{
  glMap2f(wa_par_GLenum(1), wa_par_GLfloat(2), wa_par_GLfloat(3), wa_par_GLint(4), wa_par_GLint(5), wa_par_GLfloat(6), wa_par_GLfloat(7), wa_par_GLint(8), wa_par_GLint(9), const GLfloat *points);
}
#endif

// WINGDIAPI void APIENTRY glMapGrid1d(GLint un,GLdouble u1,GLdouble u2)
HB_FUNC(WAGLMAPGRID1D)
{
  glMapGrid1d(wa_par_GLint(1), wa_par_GLdouble(2), wa_par_GLdouble(3));
}

// WINGDIAPI void APIENTRY glMapGrid1f(GLint un,GLfloat u1,GLfloat u2)
HB_FUNC(WAGLMAPGRID1F)
{
  glMapGrid1f(wa_par_GLint(1), wa_par_GLfloat(2), wa_par_GLfloat(3));
}

// WINGDIAPI void APIENTRY glMapGrid2d(GLint un,GLdouble u1,GLdouble u2,GLint vn,GLdouble v1,GLdouble v2)
HB_FUNC(WAGLMAPGRID2D)
{
  glMapGrid2d(wa_par_GLint(1), wa_par_GLdouble(2), wa_par_GLdouble(3), wa_par_GLint(4), wa_par_GLdouble(5), wa_par_GLdouble(6));
}

// WINGDIAPI void APIENTRY glMapGrid2f(GLint un,GLfloat u1,GLfloat u2,GLint vn,GLfloat v1,GLfloat v2)
HB_FUNC(WAGLMAPGRID2F)
{
  glMapGrid2f(wa_par_GLint(1), wa_par_GLfloat(2), wa_par_GLfloat(3), wa_par_GLint(4), wa_par_GLfloat(5), wa_par_GLfloat(6));
}

// WINGDIAPI void APIENTRY glMaterialf(GLenum face,GLenum pname,GLfloat param)
HB_FUNC(WAGLMATERIALF)
{
  glMaterialf(wa_par_GLenum(1), wa_par_GLenum(2), wa_par_GLfloat(3));
}

// WINGDIAPI void APIENTRY glMaterialfv(GLenum face,GLenum pname,const GLfloat *params)
#if 0
HB_FUNC(WAGLMATERIALFV)
{
  glMaterialfv(wa_par_GLenum(1), wa_par_GLenum(2), const GLfloat *params);
}
#endif

// WINGDIAPI void APIENTRY glMateriali(GLenum face,GLenum pname,GLint param)
HB_FUNC(WAGLMATERIALI)
{
  glMateriali(wa_par_GLenum(1), wa_par_GLenum(2), wa_par_GLint(3));
}

// WINGDIAPI void APIENTRY glMaterialiv(GLenum face,GLenum pname,const GLint *params)
#if 0
HB_FUNC(WAGLMATERIALIV)
{
  glMaterialiv(wa_par_GLenum(1), wa_par_GLenum(2), const GLint *params);
}
#endif

// WINGDIAPI void APIENTRY glMatrixMode(GLenum mode)
HB_FUNC(WAGLMATRIXMODE)
{
  glMatrixMode(wa_par_GLenum(1));
}

// WINGDIAPI void APIENTRY glMultMatrixd(const GLdouble *m)
#if 0
HB_FUNC(WAGLMULTMATRIXD)
{
  glMultMatrixd(const GLdouble *m);
}
#endif

// WINGDIAPI void APIENTRY glMultMatrixf(const GLfloat *m)
#if 0
HB_FUNC(WAGLMULTMATRIXF)
{
  glMultMatrixf(const GLfloat *m);
}
#endif

// WINGDIAPI void APIENTRY glNewList(GLuint list,GLenum mode)
HB_FUNC(WAGLNEWLIST)
{
  glNewList(wa_par_GLuint(1), wa_par_GLenum(2));
}

// WINGDIAPI void APIENTRY glNormal3b (GLbyte nx,GLbyte ny,GLbyte nz)
HB_FUNC(WAGLNORMAL3B)
{
  glNormal3b(wa_par_GLbyte(1), wa_par_GLbyte(2), wa_par_GLbyte(3));
}

// WINGDIAPI void APIENTRY glNormal3bv(const GLbyte *v)
#if 0
HB_FUNC(WAGLNORMAL3BV)
{
  glNormal3bv(const GLbyte *v);
}
#endif

// WINGDIAPI void APIENTRY glNormal3d(GLdouble nx,GLdouble ny,GLdouble nz)
HB_FUNC(WAGLNORMAL3D)
{
  glNormal3d(wa_par_GLdouble(1), wa_par_GLdouble(2), wa_par_GLdouble(3));
}

// WINGDIAPI void APIENTRY glNormal3dv(const GLdouble *v)
#if 0
HB_FUNC(WAGLNORMAL3DV)
{
  glNormal3dv(const GLdouble *v);
}
#endif

// WINGDIAPI void APIENTRY glNormal3f(GLfloat nx,GLfloat ny,GLfloat nz)
HB_FUNC(WAGLNORMAL3F)
{
  glNormal3f(wa_par_GLfloat(1), wa_par_GLfloat(2), wa_par_GLfloat(3));
}

// WINGDIAPI void APIENTRY glNormal3fv(const GLfloat *v)
#if 0
HB_FUNC(WAGLNORMAL3FV)
{
  glNormal3fv(const GLfloat *v);
}
#endif

// WINGDIAPI void APIENTRY glNormal3i(GLint nx,GLint ny,GLint nz)
HB_FUNC(WAGLNORMAL3I)
{
  glNormal3i(wa_par_GLint(1), wa_par_GLint(2), wa_par_GLint(3));
}

// WINGDIAPI void APIENTRY glNormal3iv(const GLint *v)
#if 0
HB_FUNC(WAGLNORMAL3IV)
{
  glNormal3iv(const GLint *v);
}
#endif

// WINGDIAPI void APIENTRY glNormal3s(GLshort nx,GLshort ny,GLshort nz)
HB_FUNC(WAGLNORMAL3S)
{
  glNormal3s(wa_par_GLshort(1), wa_par_GLshort(2), wa_par_GLshort(3));
}

// WINGDIAPI void APIENTRY glNormal3sv(const GLshort *v)
#if 0
HB_FUNC(WAGLNORMAL3SV)
{
  glNormal3sv(const GLshort *v);
}
#endif

// WINGDIAPI void APIENTRY glNormalPointer(GLenum type,GLsizei stride,const GLvoid *pointer)
#if 0
HB_FUNC(WAGLNORMALPOINTER)
{
  glNormalPointer(wa_par_GLenum(1), wa_par_GLsizei(2), const GLvoid *pointer);
}
#endif

// WINGDIAPI void APIENTRY glOrtho(GLdouble left,GLdouble right,GLdouble bottom,GLdouble top,GLdouble zNear,GLdouble zFar)
HB_FUNC(WAGLORTHO)
{
  glOrtho(wa_par_GLdouble(1), wa_par_GLdouble(2), wa_par_GLdouble(3), wa_par_GLdouble(4), wa_par_GLdouble(5), wa_par_GLdouble(6));
}

// WINGDIAPI void APIENTRY glPassThrough(GLfloat token)
HB_FUNC(WAGLPASSTHROUGH)
{
  glPassThrough(wa_par_GLfloat(1));
}

// WINGDIAPI void APIENTRY glPixelMapfv(GLenum map,GLsizei mapsize,const GLfloat *values)
#if 0
HB_FUNC(WAGLPIXELMAPFV)
{
  glPixelMapfv(wa_par_GLenum(1), wa_par_GLsizei(2), const GLfloat *values);
}
#endif

// WINGDIAPI void APIENTRY glPixelMapuiv(GLenum map,GLsizei mapsize,const GLuint *values)
#if 0
HB_FUNC(WAGLPIXELMAPUIV)
{
  glPixelMapuiv(wa_par_GLenum(1), wa_par_GLsizei(2), const GLuint *values);
}
#endif

// WINGDIAPI void APIENTRY glPixelMapusv(GLenum map,GLsizei mapsize,const GLushort *values)
#if 0
HB_FUNC(WAGLPIXELMAPUSV)
{
  glPixelMapusv(wa_par_GLenum(1), wa_par_GLsizei(2), const GLushort *values);
}
#endif

// WINGDIAPI void APIENTRY glPixelStoref(GLenum pname,GLfloat param)
HB_FUNC(WAGLPIXELSTOREF)
{
  glPixelStoref(wa_par_GLenum(1), wa_par_GLfloat(2));
}

// WINGDIAPI void APIENTRY glPixelStorei(GLenum pname,GLint param)
HB_FUNC(WAGLPIXELSTOREI)
{
  glPixelStorei(wa_par_GLenum(1), wa_par_GLint(2));
}

// WINGDIAPI void APIENTRY glPixelTransferf(GLenum pname,GLfloat param)
HB_FUNC(WAGLPIXELTRANSFERF)
{
  glPixelTransferf(wa_par_GLenum(1), wa_par_GLfloat(2));
}

// WINGDIAPI void APIENTRY glPixelTransferi(GLenum pname,GLint param)
HB_FUNC(WAGLPIXELTRANSFERI)
{
  glPixelTransferi(wa_par_GLenum(1), wa_par_GLint(2));
}

// WINGDIAPI void APIENTRY glPixelZoom(GLfloat xfactor,GLfloat yfactor)
HB_FUNC(WAGLPIXELZOOM)
{
  glPixelZoom(wa_par_GLfloat(1), wa_par_GLfloat(2));
}

// WINGDIAPI void APIENTRY glPointSize(GLfloat size)
HB_FUNC(WAGLPOINTSIZE)
{
  glPointSize(wa_par_GLfloat(1));
}

// WINGDIAPI void APIENTRY glPolygonMode(GLenum face,GLenum mode)
HB_FUNC(WAGLPOLYGONMODE)
{
  glPolygonMode(wa_par_GLenum(1), wa_par_GLenum(2));
}

// WINGDIAPI void APIENTRY glPolygonOffset(GLfloat factor,GLfloat units)
HB_FUNC(WAGLPOLYGONOFFSET)
{
  glPolygonOffset(wa_par_GLfloat(1), wa_par_GLfloat(2));
}

// WINGDIAPI void APIENTRY glPolygonStipple(const GLubyte *mask)
#if 0
HB_FUNC(WAGLPOLYGONSTIPPLE)
{
  glPolygonStipple(const GLubyte *mask);
}
#endif

// WINGDIAPI void APIENTRY glPopAttrib(void)
HB_FUNC(WAGLPOPATTRIB)
{
  glPopAttrib();
}

// WINGDIAPI void APIENTRY glPopClientAttrib(void)
HB_FUNC(WAGLPOPCLIENTATTRIB)
{
  glPopClientAttrib();
}

// WINGDIAPI void APIENTRY glPopMatrix(void)
HB_FUNC(WAGLPOPMATRIX)
{
  glPopMatrix();
}

// WINGDIAPI void APIENTRY glPopName(void)
HB_FUNC(WAGLPOPNAME)
{
  glPopName();
}

// WINGDIAPI void APIENTRY glPrioritizeTextures(GLsizei n,const GLuint *textures,const GLclampf *priorities)
#if 0
HB_FUNC(WAGLPRIORITIZETEXTURES)
{
  glPrioritizeTextures(wa_par_GLsizei(1), const GLuint *textures, const GLclampf *priorities);
}
#endif

// WINGDIAPI void APIENTRY glPushAttrib(GLbitfield mask)
HB_FUNC(WAGLPUSHATTRIB)
{
  glPushAttrib(wa_par_GLbitfield(1));
}

// WINGDIAPI void APIENTRY glPushClientAttrib(GLbitfield mask)
HB_FUNC(WAGLPUSHCLIENTATTRIB)
{
  glPushClientAttrib(wa_par_GLbitfield(1));
}

// WINGDIAPI void APIENTRY glPushMatrix(void)
HB_FUNC(WAGLPUSHMATRIX)
{
  glPushMatrix();
}

// WINGDIAPI void APIENTRY glPushName(GLuint name)
HB_FUNC(WAGLPUSHNAME)
{
  glPushName(wa_par_GLuint(1));
}

// WINGDIAPI void APIENTRY glRasterPos2d(GLdouble x,GLdouble y)
HB_FUNC(WAGLRASTERPOS2D)
{
  glRasterPos2d(wa_par_GLdouble(1), wa_par_GLdouble(2));
}

// WINGDIAPI void APIENTRY glRasterPos2dv(const GLdouble *v)
#if 0
HB_FUNC(WAGLRASTERPOS2DV)
{
  glRasterPos2dv(const GLdouble *v);
}
#endif

// WINGDIAPI void APIENTRY glRasterPos2f(GLfloat x,GLfloat y)
HB_FUNC(WAGLRASTERPOS2F)
{
  glRasterPos2f(wa_par_GLfloat(1), wa_par_GLfloat(2));
}

// WINGDIAPI void APIENTRY glRasterPos2fv(const GLfloat *v)
#if 0
HB_FUNC(WAGLRASTERPOS2FV)
{
  glRasterPos2fv(const GLfloat *v);
}
#endif

// WINGDIAPI void APIENTRY glRasterPos2i(GLint x,GLint y)
HB_FUNC(WAGLRASTERPOS2I)
{
  glRasterPos2i(wa_par_GLint(1), wa_par_GLint(2));
}

// WINGDIAPI void APIENTRY glRasterPos2iv(const GLint *v)
#if 0
HB_FUNC(WAGLRASTERPOS2IV)
{
  glRasterPos2iv(const GLint *v);
}
#endif

// WINGDIAPI void APIENTRY glRasterPos2s(GLshort x,GLshort y)
HB_FUNC(WAGLRASTERPOS2S)
{
  glRasterPos2s(wa_par_GLshort(1), wa_par_GLshort(2));
}

// WINGDIAPI void APIENTRY glRasterPos2sv(const GLshort *v)
#if 0
HB_FUNC(WAGLRASTERPOS2SV)
{
  glRasterPos2sv(const GLshort *v);
}
#endif

// WINGDIAPI void APIENTRY glRasterPos3d(GLdouble x,GLdouble y,GLdouble z)
HB_FUNC(WAGLRASTERPOS3D)
{
  glRasterPos3d(wa_par_GLdouble(1), wa_par_GLdouble(2), wa_par_GLdouble(3));
}

// WINGDIAPI void APIENTRY glRasterPos3dv(const GLdouble *v)
#if 0
HB_FUNC(WAGLRASTERPOS3DV)
{
  glRasterPos3dv(const GLdouble *v);
}
#endif

// WINGDIAPI void APIENTRY glRasterPos3f(GLfloat x,GLfloat y,GLfloat z)
HB_FUNC(WAGLRASTERPOS3F)
{
  glRasterPos3f(wa_par_GLfloat(1), wa_par_GLfloat(2), wa_par_GLfloat(3));
}

// WINGDIAPI void APIENTRY glRasterPos3fv(const GLfloat *v)
#if 0
HB_FUNC(WAGLRASTERPOS3FV)
{
  glRasterPos3fv(const GLfloat *v);
}
#endif

// WINGDIAPI void APIENTRY glRasterPos3i(GLint x,GLint y,GLint z)
HB_FUNC(WAGLRASTERPOS3I)
{
  glRasterPos3i(wa_par_GLint(1), wa_par_GLint(2), wa_par_GLint(3));
}

// WINGDIAPI void APIENTRY glRasterPos3iv(const GLint *v)
#if 0
HB_FUNC(WAGLRASTERPOS3IV)
{
  glRasterPos3iv(const GLint *v);
}
#endif

// WINGDIAPI void APIENTRY glRasterPos3s(GLshort x,GLshort y,GLshort z)
HB_FUNC(WAGLRASTERPOS3S)
{
  glRasterPos3s(wa_par_GLshort(1), wa_par_GLshort(2), wa_par_GLshort(3));
}

// WINGDIAPI void APIENTRY glRasterPos3sv(const GLshort *v)
#if 0
HB_FUNC(WAGLRASTERPOS3SV)
{
  glRasterPos3sv(const GLshort *v);
}
#endif

// WINGDIAPI void APIENTRY glRasterPos4d(GLdouble x,GLdouble y,GLdouble z,GLdouble w)
HB_FUNC(WAGLRASTERPOS4D)
{
  glRasterPos4d(wa_par_GLdouble(1), wa_par_GLdouble(2), wa_par_GLdouble(3), wa_par_GLdouble(4));
}

// WINGDIAPI void APIENTRY glRasterPos4dv(const GLdouble *v)
#if 0
HB_FUNC(WAGLRASTERPOS4DV)
{
  glRasterPos4dv(const GLdouble *v);
}
#endif

// WINGDIAPI void APIENTRY glRasterPos4f(GLfloat x,GLfloat y,GLfloat z,GLfloat w)
HB_FUNC(WAGLRASTERPOS4F)
{
  glRasterPos4f(wa_par_GLfloat(1), wa_par_GLfloat(2), wa_par_GLfloat(3), wa_par_GLfloat(4));
}

// WINGDIAPI void APIENTRY glRasterPos4fv(const GLfloat *v)
#if 0
HB_FUNC(WAGLRASTERPOS4FV)
{
  glRasterPos4fv(const GLfloat *v);
}
#endif

// WINGDIAPI void APIENTRY glRasterPos4i(GLint x,GLint y,GLint z,GLint w)
HB_FUNC(WAGLRASTERPOS4I)
{
  glRasterPos4i(wa_par_GLint(1), wa_par_GLint(2), wa_par_GLint(3), wa_par_GLint(4));
}

// WINGDIAPI void APIENTRY glRasterPos4iv(const GLint *v)
#if 0
HB_FUNC(WAGLRASTERPOS4IV)
{
  glRasterPos4iv(const GLint *v);
}
#endif

// WINGDIAPI void APIENTRY glRasterPos4s(GLshort x,GLshort y,GLshort z,GLshort w)
HB_FUNC(WAGLRASTERPOS4S)
{
  glRasterPos4s(wa_par_GLshort(1), wa_par_GLshort(2), wa_par_GLshort(3), wa_par_GLshort(4));
}

// WINGDIAPI void APIENTRY glRasterPos4sv(const GLshort *v)
#if 0
HB_FUNC(WAGLRASTERPOS4SV)
{
  glRasterPos4sv(const GLshort *v);
}
#endif

// WINGDIAPI void APIENTRY glReadBuffer(GLenum mode)
HB_FUNC(WAGLREADBUFFER)
{
  glReadBuffer(wa_par_GLenum(1));
}

// WINGDIAPI void APIENTRY glReadPixels(GLint x,GLint y,GLsizei width,GLsizei height,GLenum format,GLenum type,GLvoid *pixels)
#if 0
HB_FUNC(WAGLREADPIXELS)
{
  glReadPixels(wa_par_GLint(1), wa_par_GLint(2), wa_par_GLsizei(3), wa_par_GLsizei(4), wa_par_GLenum(5), wa_par_GLenum(6), GLvoid *pixels);
}
#endif

// WINGDIAPI void APIENTRY glRectd(GLdouble x1,GLdouble y1,GLdouble x2,GLdouble y2)
HB_FUNC(WAGLRECTD)
{
  glRectd(wa_par_GLdouble(1), wa_par_GLdouble(2), wa_par_GLdouble(3), wa_par_GLdouble(4));
}

// WINGDIAPI void APIENTRY glRectdv(const GLdouble *v1,const GLdouble *v2)
#if 0
HB_FUNC(WAGLRECTDV)
{
  glRectdv(const GLdouble *v1, const GLdouble *v2);
}
#endif

// WINGDIAPI void APIENTRY glRectf(GLfloat x1,GLfloat y1,GLfloat x2,GLfloat y2)
HB_FUNC(WAGLRECTF)
{
  glRectf(wa_par_GLfloat(1), wa_par_GLfloat(2), wa_par_GLfloat(3), wa_par_GLfloat(4));
}

// WINGDIAPI void APIENTRY glRectfv(const GLfloat *v1,const GLfloat *v2)
#if 0
HB_FUNC(WAGLRECTFV)
{
  glRectfv(const GLfloat *v1, const GLfloat *v2);
}
#endif

// WINGDIAPI void APIENTRY glRecti(GLint x1,GLint y1,GLint x2,GLint y2)
HB_FUNC(WAGLRECTI)
{
  glRecti(wa_par_GLint(1), wa_par_GLint(2), wa_par_GLint(3), wa_par_GLint(4));
}

// WINGDIAPI void APIENTRY glRectiv(const GLint *v1,const GLint *v2)
#if 0
HB_FUNC(WAGLRECTIV)
{
  glRectiv(const GLint *v1, const GLint *v2);
}
#endif

// WINGDIAPI void APIENTRY glRects(GLshort x1,GLshort y1,GLshort x2,GLshort y2)
HB_FUNC(WAGLRECTS)
{
  glRects(wa_par_GLshort(1), wa_par_GLshort(2), wa_par_GLshort(3), wa_par_GLshort(4));
}

// WINGDIAPI void APIENTRY glRectsv(const GLshort *v1,const GLshort *v2)
#if 0
HB_FUNC(WAGLRECTSV)
{
  glRectsv(const GLshort *v1, const GLshort *v2);
}
#endif

// WINGDIAPI GLint APIENTRY glRenderMode(GLenum mode)
HB_FUNC(WAGLRENDERMODE)
{
  wa_ret_GLint(glRenderMode(wa_par_GLenum(1)));
}

// WINGDIAPI void APIENTRY glRotated(GLdouble angle,GLdouble x,GLdouble y,GLdouble z)
HB_FUNC(WAGLROTATED)
{
  glRotated(wa_par_GLdouble(1), wa_par_GLdouble(2), wa_par_GLdouble(3), wa_par_GLdouble(4));
}

// WINGDIAPI void APIENTRY glRotatef(GLfloat angle,GLfloat x,GLfloat y,GLfloat z)
HB_FUNC(WAGLROTATEF)
{
  glRotatef(wa_par_GLfloat(1), wa_par_GLfloat(2), wa_par_GLfloat(3), wa_par_GLfloat(4));
}

// WINGDIAPI void APIENTRY glScaled(GLdouble x,GLdouble y,GLdouble z)
HB_FUNC(WAGLSCALED)
{
  glScaled(wa_par_GLdouble(1), wa_par_GLdouble(2), wa_par_GLdouble(3));
}

// WINGDIAPI void APIENTRY glScalef(GLfloat x,GLfloat y,GLfloat z)
HB_FUNC(WAGLSCALEF)
{
  glScalef(wa_par_GLfloat(1), wa_par_GLfloat(2), wa_par_GLfloat(3));
}

// WINGDIAPI void APIENTRY glScissor(GLint x,GLint y,GLsizei width,GLsizei height)
HB_FUNC(WAGLSCISSOR)
{
  glScissor(wa_par_GLint(1), wa_par_GLint(2), wa_par_GLsizei(3), wa_par_GLsizei(4));
}

// WINGDIAPI void APIENTRY glSelectBuffer(GLsizei size,GLuint *buffer)
#if 0
HB_FUNC(WAGLSELECTBUFFER)
{
  glSelectBuffer(wa_par_GLsizei(1), GLuint *buffer);
}
#endif

// WINGDIAPI void APIENTRY glShadeModel(GLenum mode)
HB_FUNC(WAGLSHADEMODEL)
{
  glShadeModel(wa_par_GLenum(1));
}

// WINGDIAPI void APIENTRY glStencilFunc(GLenum func,GLint ref,GLuint mask)
HB_FUNC(WAGLSTENCILFUNC)
{
  glStencilFunc(wa_par_GLenum(1), wa_par_GLint(2), wa_par_GLuint(3));
}

// WINGDIAPI void APIENTRY glStencilMask(GLuint mask)
HB_FUNC(WAGLSTENCILMASK)
{
  glStencilMask(wa_par_GLuint(1));
}

// WINGDIAPI void APIENTRY glStencilOp(GLenum fail,GLenum zfail,GLenum zpass)
HB_FUNC(WAGLSTENCILOP)
{
  glStencilOp(wa_par_GLenum(1), wa_par_GLenum(2), wa_par_GLenum(3));
}

// WINGDIAPI void APIENTRY glTexCoord1d(GLdouble s)
HB_FUNC(WAGLTEXCOORD1D)
{
  glTexCoord1d(wa_par_GLdouble(1));
}

// WINGDIAPI void APIENTRY glTexCoord1dv(const GLdouble *v)
#if 0
HB_FUNC(WAGLTEXCOORD1DV)
{
  glTexCoord1dv(const GLdouble *v);
}
#endif

// WINGDIAPI void APIENTRY glTexCoord1f(GLfloat s)
HB_FUNC(WAGLTEXCOORD1F)
{
  glTexCoord1f(wa_par_GLfloat(1));
}

// WINGDIAPI void APIENTRY glTexCoord1fv(const GLfloat *v)
#if 0
HB_FUNC(WAGLTEXCOORD1FV)
{
  glTexCoord1fv(const GLfloat *v);
}
#endif

// WINGDIAPI void APIENTRY glTexCoord1i(GLint s)
HB_FUNC(WAGLTEXCOORD1I)
{
  glTexCoord1i(wa_par_GLint(1));
}

// WINGDIAPI void APIENTRY glTexCoord1iv(const GLint *v)
#if 0
HB_FUNC(WAGLTEXCOORD1IV)
{
  glTexCoord1iv(const GLint *v);
}
#endif

// WINGDIAPI void APIENTRY glTexCoord1s(GLshort s)
HB_FUNC(WAGLTEXCOORD1S)
{
  glTexCoord1s(wa_par_GLshort(1));
}

// WINGDIAPI void APIENTRY glTexCoord1sv(const GLshort *v)
#if 0
HB_FUNC(WAGLTEXCOORD1SV)
{
  glTexCoord1sv(const GLshort *v);
}
#endif

// WINGDIAPI void APIENTRY glTexCoord2d(GLdouble s,GLdouble t)
HB_FUNC(WAGLTEXCOORD2D)
{
  glTexCoord2d(wa_par_GLdouble(1), wa_par_GLdouble(2));
}

// WINGDIAPI void APIENTRY glTexCoord2dv(const GLdouble *v)
#if 0
HB_FUNC(WAGLTEXCOORD2DV)
{
  glTexCoord2dv(const GLdouble *v);
}
#endif

// WINGDIAPI void APIENTRY glTexCoord2f(GLfloat s,GLfloat t)
HB_FUNC(WAGLTEXCOORD2F)
{
  glTexCoord2f(wa_par_GLfloat(1), wa_par_GLfloat(2));
}

// WINGDIAPI void APIENTRY glTexCoord2fv(const GLfloat *v)
#if 0
HB_FUNC(WAGLTEXCOORD2FV)
{
  glTexCoord2fv(const GLfloat *v);
}
#endif

// WINGDIAPI void APIENTRY glTexCoord2i(GLint s,GLint t)
HB_FUNC(WAGLTEXCOORD2I)
{
  glTexCoord2i(wa_par_GLint(1), wa_par_GLint(2));
}

// WINGDIAPI void APIENTRY glTexCoord2iv(const GLint *v)
#if 0
HB_FUNC(WAGLTEXCOORD2IV)
{
  glTexCoord2iv(const GLint *v);
}
#endif

// WINGDIAPI void APIENTRY glTexCoord2s(GLshort s,GLshort t)
HB_FUNC(WAGLTEXCOORD2S)
{
  glTexCoord2s(wa_par_GLshort(1), wa_par_GLshort(2));
}

// WINGDIAPI void APIENTRY glTexCoord2sv(const GLshort *v)
#if 0
HB_FUNC(WAGLTEXCOORD2SV)
{
  glTexCoord2sv(const GLshort *v);
}
#endif

// WINGDIAPI void APIENTRY glTexCoord3d(GLdouble s,GLdouble t,GLdouble r)
HB_FUNC(WAGLTEXCOORD3D)
{
  glTexCoord3d(wa_par_GLdouble(1), wa_par_GLdouble(2), wa_par_GLdouble(3));
}

// WINGDIAPI void APIENTRY glTexCoord3dv(const GLdouble *v)
#if 0
HB_FUNC(WAGLTEXCOORD3DV)
{
  glTexCoord3dv(const GLdouble *v);
}
#endif

// WINGDIAPI void APIENTRY glTexCoord3f(GLfloat s,GLfloat t,GLfloat r)
HB_FUNC(WAGLTEXCOORD3F)
{
  glTexCoord3f(wa_par_GLfloat(1), wa_par_GLfloat(2), wa_par_GLfloat(3));
}

// WINGDIAPI void APIENTRY glTexCoord3fv(const GLfloat *v)
#if 0
HB_FUNC(WAGLTEXCOORD3FV)
{
  glTexCoord3fv(const GLfloat *v);
}
#endif

// WINGDIAPI void APIENTRY glTexCoord3i(GLint s,GLint t,GLint r)
HB_FUNC(WAGLTEXCOORD3I)
{
  glTexCoord3i(wa_par_GLint(1), wa_par_GLint(2), wa_par_GLint(3));
}

// WINGDIAPI void APIENTRY glTexCoord3iv(const GLint *v)
#if 0
HB_FUNC(WAGLTEXCOORD3IV)
{
  glTexCoord3iv(const GLint *v);
}
#endif

// WINGDIAPI void APIENTRY glTexCoord3s(GLshort s,GLshort t,GLshort r)
HB_FUNC(WAGLTEXCOORD3S)
{
  glTexCoord3s(wa_par_GLshort(1), wa_par_GLshort(2), wa_par_GLshort(3));
}

// WINGDIAPI void APIENTRY glTexCoord3sv(const GLshort *v)
#if 0
HB_FUNC(WAGLTEXCOORD3SV)
{
  glTexCoord3sv(const GLshort *v);
}
#endif

// WINGDIAPI void APIENTRY glTexCoord4d(GLdouble s,GLdouble t,GLdouble r,GLdouble q)
HB_FUNC(WAGLTEXCOORD4D)
{
  glTexCoord4d(wa_par_GLdouble(1), wa_par_GLdouble(2), wa_par_GLdouble(3), wa_par_GLdouble(4));
}

// WINGDIAPI void APIENTRY glTexCoord4dv(const GLdouble *v)
#if 0
HB_FUNC(WAGLTEXCOORD4DV)
{
  glTexCoord4dv(const GLdouble *v);
}
#endif

// WINGDIAPI void APIENTRY glTexCoord4f(GLfloat s,GLfloat t,GLfloat r,GLfloat q)
HB_FUNC(WAGLTEXCOORD4F)
{
  glTexCoord4f(wa_par_GLfloat(1), wa_par_GLfloat(2), wa_par_GLfloat(3), wa_par_GLfloat(4));
}

// WINGDIAPI void APIENTRY glTexCoord4fv(const GLfloat *v)
#if 0
HB_FUNC(WAGLTEXCOORD4FV)
{
  glTexCoord4fv(const GLfloat *v);
}
#endif

// WINGDIAPI void APIENTRY glTexCoord4i(GLint s,GLint t,GLint r,GLint q)
HB_FUNC(WAGLTEXCOORD4I)
{
  glTexCoord4i(wa_par_GLint(1), wa_par_GLint(2), wa_par_GLint(3), wa_par_GLint(4));
}

// WINGDIAPI void APIENTRY glTexCoord4iv(const GLint *v)
#if 0
HB_FUNC(WAGLTEXCOORD4IV)
{
  glTexCoord4iv(const GLint *v);
}
#endif

// WINGDIAPI void APIENTRY glTexCoord4s(GLshort s,GLshort t,GLshort r,GLshort q)
HB_FUNC(WAGLTEXCOORD4S)
{
  glTexCoord4s(wa_par_GLshort(1), wa_par_GLshort(2), wa_par_GLshort(3), wa_par_GLshort(4));
}

// WINGDIAPI void APIENTRY glTexCoord4sv(const GLshort *v)
#if 0
HB_FUNC(WAGLTEXCOORD4SV)
{
  glTexCoord4sv(const GLshort *v);
}
#endif

// WINGDIAPI void APIENTRY glTexCoordPointer(GLint size,GLenum type,GLsizei stride,const GLvoid *pointer)
#if 0
HB_FUNC(WAGLTEXCOORDPOINTER)
{
  glTexCoordPointer(wa_par_GLint(1), wa_par_GLenum(2), wa_par_GLsizei(3), const GLvoid *pointer);
}
#endif

// WINGDIAPI void APIENTRY glTexEnvf(GLenum target,GLenum pname,GLfloat param)
HB_FUNC(WAGLTEXENVF)
{
  glTexEnvf(wa_par_GLenum(1), wa_par_GLenum(2), wa_par_GLfloat(3));
}

// WINGDIAPI void APIENTRY glTexEnvfv(GLenum target,GLenum pname,const GLfloat *params)
#if 0
HB_FUNC(WAGLTEXENVFV)
{
  glTexEnvfv(wa_par_GLenum(1), wa_par_GLenum(2), const GLfloat *params);
}
#endif

// WINGDIAPI void APIENTRY glTexEnvi(GLenum target,GLenum pname,GLint param)
HB_FUNC(WAGLTEXENVI)
{
  glTexEnvi(wa_par_GLenum(1), wa_par_GLenum(2), wa_par_GLint(3));
}

// WINGDIAPI void APIENTRY glTexEnviv(GLenum target,GLenum pname,const GLint *params)
#if 0
HB_FUNC(WAGLTEXENVIV)
{
  glTexEnviv(wa_par_GLenum(1), wa_par_GLenum(2), const GLint *params);
}
#endif

// WINGDIAPI void APIENTRY glTexGend(GLenum coord,GLenum pname,GLdouble param)
HB_FUNC(WAGLTEXGEND)
{
  glTexGend(wa_par_GLenum(1), wa_par_GLenum(2), wa_par_GLdouble(3));
}

// WINGDIAPI void APIENTRY glTexGendv(GLenum coord,GLenum pname,const GLdouble *params)
#if 0
HB_FUNC(WAGLTEXGENDV)
{
  glTexGendv(wa_par_GLenum(1), wa_par_GLenum(2), const GLdouble *params);
}
#endif

// WINGDIAPI void APIENTRY glTexGenf(GLenum coord,GLenum pname,GLfloat param)
HB_FUNC(WAGLTEXGENF)
{
  glTexGenf(wa_par_GLenum(1), wa_par_GLenum(2), wa_par_GLfloat(3));
}

// WINGDIAPI void APIENTRY glTexGenfv(GLenum coord,GLenum pname,const GLfloat *params)
#if 0
HB_FUNC(WAGLTEXGENFV)
{
  glTexGenfv(wa_par_GLenum(1), wa_par_GLenum(2), const GLfloat *params);
}
#endif

// WINGDIAPI void APIENTRY glTexGeni(GLenum coord,GLenum pname,GLint param)
HB_FUNC(WAGLTEXGENI)
{
  glTexGeni(wa_par_GLenum(1), wa_par_GLenum(2), wa_par_GLint(3));
}

// WINGDIAPI void APIENTRY glTexGeniv(GLenum coord,GLenum pname,const GLint *params)
#if 0
HB_FUNC(WAGLTEXGENIV)
{
  glTexGeniv(wa_par_GLenum(1), wa_par_GLenum(2), const GLint *params);
}
#endif

// WINGDIAPI void APIENTRY glTexImage1D(GLenum target,GLint level,GLint internalformat,GLsizei width,GLint border,GLenum format,GLenum type,const GLvoid *pixels)
#if 0
HB_FUNC(WAGLTEXIMAGE1D)
{
  glTexImage1D(wa_par_GLenum(1), wa_par_GLint(2), wa_par_GLint(3), wa_par_GLsizei(4), wa_par_GLint(5), wa_par_GLenum(6), wa_par_GLenum(7), const GLvoid *pixels);
}
#endif

// WINGDIAPI void APIENTRY glTexImage2D(GLenum target,GLint level,GLint internalformat,GLsizei width,GLsizei height,GLint border,GLenum format,GLenum type,const GLvoid *pixels)
#if 0
HB_FUNC(WAGLTEXIMAGE2D)
{
  glTexImage2D(wa_par_GLenum(1), wa_par_GLint(2), wa_par_GLint(3), wa_par_GLsizei(4), wa_par_GLsizei(5), wa_par_GLint(6), wa_par_GLenum(7), wa_par_GLenum(8), const GLvoid *pixels);
}
#endif

// WINGDIAPI void APIENTRY glTexParameterf(GLenum target,GLenum pname,GLfloat param)
HB_FUNC(WAGLTEXPARAMETERF)
{
  glTexParameterf(wa_par_GLenum(1), wa_par_GLenum(2), wa_par_GLfloat(3));
}

// WINGDIAPI void APIENTRY glTexParameterfv(GLenum target,GLenum pname,const GLfloat *params)
#if 0
HB_FUNC(WAGLTEXPARAMETERFV)
{
  glTexParameterfv(wa_par_GLenum(1), wa_par_GLenum(2), const GLfloat *params);
}
#endif

// WINGDIAPI void APIENTRY glTexParameteri(GLenum target,GLenum pname,GLint param)
HB_FUNC(WAGLTEXPARAMETERI)
{
  glTexParameteri(wa_par_GLenum(1), wa_par_GLenum(2), wa_par_GLint(3));
}

// WINGDIAPI void APIENTRY glTexParameteriv(GLenum target,GLenum pname,const GLint *params)
#if 0
HB_FUNC(WAGLTEXPARAMETERIV)
{
  glTexParameteriv(wa_par_GLenum(1), wa_par_GLenum(2), const GLint *params);
}
#endif

// WINGDIAPI void APIENTRY glTexSubImage1D(GLenum target,GLint level,GLint xoffset,GLsizei width,GLenum format,GLenum type,const GLvoid *pixels)
#if 0
HB_FUNC(WAGLTEXSUBIMAGE1D)
{
  glTexSubImage1D(wa_par_GLenum(1), wa_par_GLint(2), wa_par_GLint(3), wa_par_GLsizei(4), wa_par_GLenum(5), wa_par_GLenum(6), const GLvoid *pixels);
}
#endif

// WINGDIAPI void APIENTRY glTexSubImage2D(GLenum target,GLint level,GLint xoffset,GLint yoffset,GLsizei width,GLsizei height,GLenum format,GLenum type,const GLvoid *pixels)
#if 0
HB_FUNC(WAGLTEXSUBIMAGE2D)
{
  glTexSubImage2D(wa_par_GLenum(1), wa_par_GLint(2), wa_par_GLint(3), wa_par_GLint(4), wa_par_GLsizei(5), wa_par_GLsizei(6), wa_par_GLenum(7), wa_par_GLenum(8), const GLvoid *pixels);
}
#endif

// WINGDIAPI void APIENTRY glTranslated(GLdouble x,GLdouble y,GLdouble z)
HB_FUNC(WAGLTRANSLATED)
{
  glTranslated(wa_par_GLdouble(1), wa_par_GLdouble(2), wa_par_GLdouble(3));
}

// WINGDIAPI void APIENTRY glTranslatef(GLfloat x,GLfloat y,GLfloat z)
HB_FUNC(WAGLTRANSLATEF)
{
  glTranslatef(wa_par_GLfloat(1), wa_par_GLfloat(2), wa_par_GLfloat(3));
}

// WINGDIAPI void APIENTRY glVertex2d(GLdouble x,GLdouble y)
HB_FUNC(WAGLVERTEX2D)
{
  glVertex2d(wa_par_GLdouble(1), wa_par_GLdouble(2));
}

// WINGDIAPI void APIENTRY glVertex2dv(const GLdouble *v)
#if 0
HB_FUNC(WAGLVERTEX2DV)
{
  glVertex2dv(const GLdouble *v);
}
#endif

// WINGDIAPI void APIENTRY glVertex2f(GLfloat x,GLfloat y)
HB_FUNC(WAGLVERTEX2F)
{
  glVertex2f(wa_par_GLfloat(1), wa_par_GLfloat(2));
}

// WINGDIAPI void APIENTRY glVertex2fv(const GLfloat *v)
#if 0
HB_FUNC(WAGLVERTEX2FV)
{
  glVertex2fv(const GLfloat *v);
}
#endif

// WINGDIAPI void APIENTRY glVertex2i(GLint x,GLint y)
HB_FUNC(WAGLVERTEX2I)
{
  glVertex2i(wa_par_GLint(1), wa_par_GLint(2));
}

// WINGDIAPI void APIENTRY glVertex2iv(const GLint *v)
#if 0
HB_FUNC(WAGLVERTEX2IV)
{
  glVertex2iv(const GLint *v);
}
#endif

// WINGDIAPI void APIENTRY glVertex2s(GLshort x,GLshort y)
HB_FUNC(WAGLVERTEX2S)
{
  glVertex2s(wa_par_GLshort(1), wa_par_GLshort(2));
}

// WINGDIAPI void APIENTRY glVertex2sv(const GLshort *v)
#if 0
HB_FUNC(WAGLVERTEX2SV)
{
  glVertex2sv(const GLshort *v);
}
#endif

// WINGDIAPI void APIENTRY glVertex3d(GLdouble x,GLdouble y,GLdouble z)
HB_FUNC(WAGLVERTEX3D)
{
  glVertex3d(wa_par_GLdouble(1), wa_par_GLdouble(2), wa_par_GLdouble(3));
}

// WINGDIAPI void APIENTRY glVertex3dv(const GLdouble *v)
#if 0
HB_FUNC(WAGLVERTEX3DV)
{
  glVertex3dv(const GLdouble *v);
}
#endif

// WINGDIAPI void APIENTRY glVertex3f(GLfloat x,GLfloat y,GLfloat z)
HB_FUNC(WAGLVERTEX3F)
{
  glVertex3f(wa_par_GLfloat(1), wa_par_GLfloat(2), wa_par_GLfloat(3));
}

// WINGDIAPI void APIENTRY glVertex3fv(const GLfloat *v)
#if 0
HB_FUNC(WAGLVERTEX3FV)
{
  glVertex3fv(const GLfloat *v);
}
#endif

// WINGDIAPI void APIENTRY glVertex3i(GLint x,GLint y,GLint z)
HB_FUNC(WAGLVERTEX3I)
{
  glVertex3i(wa_par_GLint(1), wa_par_GLint(2), wa_par_GLint(3));
}

// WINGDIAPI void APIENTRY glVertex3iv(const GLint *v)
#if 0
HB_FUNC(WAGLVERTEX3IV)
{
  glVertex3iv(const GLint *v);
}
#endif

// WINGDIAPI void APIENTRY glVertex3s(GLshort x,GLshort y,GLshort z)
HB_FUNC(WAGLVERTEX3S)
{
  glVertex3s(wa_par_GLshort(1), wa_par_GLshort(2), wa_par_GLshort(3));
}

// WINGDIAPI void APIENTRY glVertex3sv(const GLshort *v)
#if 0
HB_FUNC(WAGLVERTEX3SV)
{
  glVertex3sv(const GLshort *v);
}
#endif

// WINGDIAPI void APIENTRY glVertex4d(GLdouble x,GLdouble y,GLdouble z,GLdouble w)
HB_FUNC(WAGLVERTEX4D)
{
  glVertex4d(wa_par_GLdouble(1), wa_par_GLdouble(2), wa_par_GLdouble(3), wa_par_GLdouble(4));
}

// WINGDIAPI void APIENTRY glVertex4dv(const GLdouble *v)
#if 0
HB_FUNC(WAGLVERTEX4DV)
{
  glVertex4dv(const GLdouble *v);
}
#endif

// WINGDIAPI void APIENTRY glVertex4f(GLfloat x,GLfloat y,GLfloat z,GLfloat w)
HB_FUNC(WAGLVERTEX4F)
{
  glVertex4f(wa_par_GLfloat(1), wa_par_GLfloat(2), wa_par_GLfloat(3), wa_par_GLfloat(4));
}

// WINGDIAPI void APIENTRY glVertex4fv(const GLfloat *v)
#if 0
HB_FUNC(WAGLVERTEX4FV)
{
  glVertex4fv(const GLfloat *v);
}
#endif

// WINGDIAPI void APIENTRY glVertex4i(GLint x,GLint y,GLint z,GLint w)
HB_FUNC(WAGLVERTEX4I)
{
  glVertex4i(wa_par_GLint(1), wa_par_GLint(2), wa_par_GLint(3), wa_par_GLint(4));
}

// WINGDIAPI void APIENTRY glVertex4iv(const GLint *v)
#if 0
HB_FUNC(WAGLVERTEX4IV)
{
  glVertex4iv(const GLint *v);
}
#endif

// WINGDIAPI void APIENTRY glVertex4s(GLshort x,GLshort y,GLshort z,GLshort w)
HB_FUNC(WAGLVERTEX4S)
{
  glVertex4s(wa_par_GLshort(1), wa_par_GLshort(2), wa_par_GLshort(3), wa_par_GLshort(4));
}

// WINGDIAPI void APIENTRY glVertex4sv(const GLshort *v)
#if 0
HB_FUNC(WAGLVERTEX4SV)
{
  glVertex4sv(const GLshort *v);
}
#endif

// WINGDIAPI void APIENTRY glVertexPointer(GLint size,GLenum type,GLsizei stride,const GLvoid *pointer)
#if 0
HB_FUNC(WAGLVERTEXPOINTER)
{
  glVertexPointer(wa_par_GLint(1), wa_par_GLenum(2), wa_par_GLsizei(3), const GLvoid *pointer);
}
#endif

// WINGDIAPI void APIENTRY glViewport(GLint x,GLint y,GLsizei width,GLsizei height)
HB_FUNC(WAGLVIEWPORT)
{
  glViewport(wa_par_GLint(1), wa_par_GLint(2), wa_par_GLsizei(3), wa_par_GLsizei(4));
}

// typedef void (APIENTRY *PFNGLARRAYELEMENTEXTPROC)(GLint i);
// typedef void (APIENTRY *PFNGLDRAWARRAYSEXTPROC)(GLenum mode,GLint first,GLsizei count);
// typedef void (APIENTRY *PFNGLVERTEXPOINTEREXTPROC)(GLint size,GLenum type,GLsizei stride,GLsizei count,const GLvoid *pointer);
// typedef void (APIENTRY *PFNGLNORMALPOINTEREXTPROC)(GLenum type,GLsizei stride,GLsizei count,const GLvoid *pointer);
// typedef void (APIENTRY *PFNGLCOLORPOINTEREXTPROC)(GLint size,GLenum type,GLsizei stride,GLsizei count,const GLvoid *pointer);
// typedef void (APIENTRY *PFNGLINDEXPOINTEREXTPROC)(GLenum type,GLsizei stride,GLsizei count,const GLvoid *pointer);
// typedef void (APIENTRY *PFNGLTEXCOORDPOINTEREXTPROC)(GLint size,GLenum type,GLsizei stride,GLsizei count,const GLvoid *pointer);
// typedef void (APIENTRY *PFNGLEDGEFLAGPOINTEREXTPROC)(GLsizei stride,GLsizei count,const GLboolean *pointer);
// typedef void (APIENTRY *PFNGLGETPOINTERVEXTPROC)(GLenum pname,GLvoid **params);
// typedef void (APIENTRY *PFNGLARRAYELEMENTARRAYEXTPROC)(GLenum mode,GLsizei count,const GLvoid *pi);
// typedef void (APIENTRY *PFNGLDRAWRANGEELEMENTSWINPROC)(GLenum mode,GLuint start,GLuint end,GLsizei count,GLenum type,const GLvoid *indices);
// typedef void (APIENTRY *PFNGLADDSWAPHINTRECTWINPROC)(GLint x,GLint y,GLsizei width,GLsizei height);
// typedef void (APIENTRY *PFNGLCOLORTABLEEXTPROC)(GLenum target,GLenum internalFormat,GLsizei width,GLenum format,GLenum type,const GLvoid *data);
// typedef void (APIENTRY *PFNGLCOLORSUBTABLEEXTPROC)(GLenum target,GLsizei start,GLsizei count,GLenum format,GLenum type,const GLvoid *data);
// typedef void (APIENTRY *PFNGLGETCOLORTABLEEXTPROC)(GLenum target,GLenum format,GLenum type,GLvoid *data);
// typedef void (APIENTRY *PFNGLGETCOLORTABLEPARAMETERIVEXTPROC)(GLenum target,GLenum pname,GLint *params);
// typedef void (APIENTRY *PFNGLGETCOLORTABLEPARAMETERFVEXTPROC)(GLenum target,GLenum pname,GLfloat *params);
