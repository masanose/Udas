To use the functions external/IDL_GEOPACK
you will need to install Haje Korth's
IDL wrapper for N.A. Tsyganenko's magnetic
fields model package.

To do this you must:
#1 Download the IDL geopack for your 
architecture from:
http://dysprosium.jhuapl.edu/idl_geopack/
or from the themis site:
http://themis.ssl.berkeley.edu/beta/software.shtml

#2 Unzip the package to any directory

#3 Copy the binary and the dlm file from the package
you just unzipped into your idl dlm directory.


The dlm file will be named:
idl_geopack.dlm
The binary will be named:
idl_geopack.{extension}
{extension} will vary from
OS to OS
on windows it will be called:
idl_geopack.dll
on linux and unix it will be called:
idl_geopack.so

Your idl dlm directory will vary depending
on where you installed IDL
but it can be found by typing:
print,!DLM_PATH
in an IDL terminal

#4 after the files are copied just restart your idl
session and idl should automatically detect the geopack 
functions.

The boolean function igp_test()
in the external/IDL_GEOPACK tests to
be sure the package is in place.
Typing
print,igp_test()
will check that it is installed correctly

or you can type: 
geopack_help
in IDL to test as well.