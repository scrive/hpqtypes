##############################################################
# Project: libpqtypes
# Makefile for Microsoft Visual Studio 6, 7 or 8
#
# nmake -f win32.mak [options] [targets]
#
# For further build instructions, see the package's INSTALL file.
#
# Authors: Andrew Chernow, Merlin Moncure
# Contact: libpqtypes@esilo.com
##############################################################

PROJNAME = libpqtypes

OBJECTS = src\array.obj src\datetime.obj src\error.obj \
	src\events.obj src\exec.obj src\geo.obj src\handler.obj \
	src\misc.obj src\network.obj src\numerics.obj \
	src\param.obj src\port.obj src\record.obj src\spec.obj \
	src\utils.obj src\varlena.obj


CFLAGS = $(CFLAGS) /nologo /W4 /MD /GF /Ob2 /O2 /Oi- /D_WIN32_WINNT=0x0501

!IFDEF MT
CFLAGS = $(CFLAGS) /DPQT_THREAD_SAFE
!ENDIF

!IFDEF PQT_LONG_LONG
CFLAGS = $(CFLAGS) /DPQT_LONG_LONG=$(PQT_LONG_LONG)
!ENDIF

# set the libpath for libpq.dll and libpqdll.lib (same thing as gcc -L)
!IFDEF LPATH
LIBPATH = /LIBPATH:$(LPATH)
!ENDIF

INC2 = $(INC) /Isrc
LIBS = ws2_32.lib libpqdll.lib
LINKOPTS = /nologo /subsystem:windows /incremental:no
LINKIGNORE = /IGNORE:4089 /IGNORE:4006 /IGNORE:4068
LIBOPTS = /nologo /subsystem:windows $(LINKIGNORE)

all: $(OBJECTS)
	lib $(LIBOPTS) /OUT:$(PROJNAME).lib $(OBJECTS)
	link $(LINKOPTS) /dll $(OBJECTS) /out:$(PROJNAME).dll \
		/IMPLIB:$(PROJNAME)dll.lib $(LIBS) $(LIBPATH)

test:
	$(CC) $(CFLAGS) $(INC2) src\regression-test.c /Feregtest.exe \
		$(LIBS) $(PROJNAME)dll.lib /link $(LIBPATH) $(LINKIGNORE)
	-@del regression-test.obj

$(OBJECTS):
	$(CC) $(CFLAGS) $(INC2) /c $** /Fo$*.obj

clean:
	-@del /Q /F \
		$(OBJECTS) \
		$(PROJNAME).dll \
		$(PROJNAME).dll.manifest \
		$(PROJNAME).lib \
		$(PROJNAME)dll.lib \
		$(PROJNAME)dll.exp \
		regtest.exe \
		regtest.exe.manifest 2>NUL 1>NUL

