C/=====================================================================/
C/ FileIO.f90                                                          /
C/=====================================================================/
C/ A fortran code for file input and output.                           /
C/=====================================================================/
C/ Date: 9/12/08                                                       /
C/ Version: 1.0.0                                                      /
C/ Executing:                                                          /
C/ ./FileIO                                                            /
C/====|================================================================/
      PROGRAM FileIO
      implicit none
C
CC Initialize the variables.
C
      integer*4 a, b, c, u, v
      integer*4 ra, rb, rc, rra, rrb, rrc
C
CC Define the variables.
C
      a = 5
      b = 12
      c = a + b
C
CC Write the vriables to the screen.
C
      write(*,'(A,I2)') 'a = ',a ,'b = ', b, 'c = ', c
C
CC Open a file nicknamed u, which will be named read.dat
C
      open(unit=u, file= 'read.dat')
C
CC Write to the file
C
      write(u,'(I4,I4,I4)') a, b, c
      write(u,'(I4)') a, b, c
C
CC Close to save the data of the file
C
      close(u)
C
CC We will begin to read the file read.dat. In order to do that
CC we need to reopen the file, and then use the read function.
C
      open(unit=u, file= 'read.dat')
C
CC Read in the file read.dat
C
      read(u,*, end = 10) ra, rb, rc, rra, rrb, rrc
C
CC Print read variables onto the screen, to make sure we got it.
C
10 write(*,110) ra, rb, rc, rra, rrb, rrc
110 format(I2)
C
CC Openeing a new file call reed.dat and make a copy of read.dat,
CC but adding 100 to each variable.
C
      open(unit=v, file ='reed.dat')
      ra = ra + 100
      rb = rb + 100
      rc = rc + 100
      rra = rra + 100
      rrb = rrb + 100
      rrc = rrc + 100
      write(v,120) ra, rb, rc, rra, rrb, rrc
120 format(I3)
C
CC Close file.
C
close(u)
close(v)
      END
C/=====================================================================/
C/ End of program                                                      /
C/=====================================================================/
