C/=====================================================================/
C/ TypesConstantsVariables.f90 /
C/=====================================================================/
C/ A fortran code with examples of types, constants, and variables. /
C/=====================================================================/
C/ Date: 8/27/08 /
C/ Version: 1.0.0 /
C/ Executing: /
C/ ./TypesConstantsVariables /
C/====|================================================================/
CC
C Starting up a program
C
      program TypesConstantsVariables
CC
C A varaible must be defined to be used
C
      implicit none
CC
C Define the two parameters values single and double. These values
CC never change in the program.
C
      integer*4, parameter :: single = 4
      integer*4, parameter :: double = 8
CC
C Define a bunch of variables with different types, some redundant.
C
      integer*4 intvarstar4
      integer*8 intvarstar8
      integer(kind=double) :: intvardouble ! same as above
      real*4 floatvarstar4
      real(kind=single) :: floatvarsingle
      real*8 floatvarstar8
      real(kind=double) :: floatvardouble
      character(len=256) :: filename
CC
C Variable assignments followed by write statements to the screen.
C
      floatvarstar4 = 1
      intvarstar4 = 1
      write(*,"(f9.0, I9)") floatvarstar4, intvarstar4
      floatvarstar4 = 1.6
      intvarstar4 = 1.6
      write(*,"(f9.0, I9)") floatvarstar4, intvarstar4
      
      floatvarstar4 = 1.6
      intvarstar4 = 1.6
      write(*,"(f9.1, I9)") floatvarstar4, intvarstar4
      floatvarstar4 = 3.14159265
      floatvarstar8 = 3.14159265
      write(*,"(f18.10,f18.10)") floatvarstar4, floatvarstar8
      floatvarstar4 = 3.14159265D0
      floatvarstar8 = 3.14159265D0
      write(*,"(f18.10,f18.10)") floatvarstar4, floatvarstar8
      floatvarsingle = 3.14159265D0
      floatvardouble = 3.14159265D0
      write(*,"(f18.10,f18.10)") floatvarstar4, floatvarstar8
      intvarstar4 = 2147483647
      write(*,"(I16)") intvarstar4
      intvarstar4 = 2147483649
      write(*,"(I16)") intvarstar4
      intvardouble = 2147483649
      write(*,"(I16)") intvardouble
      filename = 'wrfout_d01'
      write(*,"(A30)") filename
CC
C Ending the program
C
      end
C/=====================================================================/
C/ End of program /
C/=====================================================================/
