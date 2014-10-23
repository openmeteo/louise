L O U I S E    v. 3
===================
Delphi library for user interface
(Library of User Interface Special Edition :-)
for the development of applications such as
hydrognomon, etc.


Readme date: 2010-02-11
file version: 0.01

This file contains the required steps to compile
louise from source files.



I. Check Dependencies and versions.

CodeGear Delphi 2009, version:
  12.0.3420.21218
with TeeChart Standard, version:
  8.03.11068


All components are bundled with Delphi 2009, should
be updated to match the above versions.



II. Build Instructions:

1. Check out thelma library (version 3).
Check library revisions required by the hydrognomon
version.
Build / Install libraries in this order:

  a. Thelma runtime (thelma.bpl)
  b. Thelma designtime (thelmadsgn.bpl)

2. Ensure that louise\lib directory exist to output
*.dcu files (compiled object files).


3. Build / install louise library in this order:
  louise3.dproj     (build or compile only)
  louise3dsgn.dproj (build and install)

4. Is Helpcompiler is available, compile developers
help. Install help for delphi.

URL's
http://www.hydrognomon.org/
http://www.itia.ntua.gr/

Contact people:

Stefanos Kozanis
S.Kozanis@itia.ntua.gr
Antonis Christofides
anthony@itia.ntua.gr
