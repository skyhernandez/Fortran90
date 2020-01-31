C/=====================================================================/
C/ Formulas.f90 /
C/=====================================================================/
C/ A fortran code with examples of types, constants, and variables in /
C/ arrays using formulas. /
C/=====================================================================/
C/ Date: 9/03/08 /
C/ Version: 1.0.0 /
C/ Executing: /
C/ ./Formulas /
C/====|================================================================/
      PROGRAM Formulas
      implicit none
CC
C Define the two paramters values single and double. These values
CC never change in the program.
C
      integer*4, parameter :: single = 4
      integer*4, parameter :: double = 8
      integer*4, parameter :: arrsize = 100
      integer*4, nlats, nlons, status
CC
C Define a fixed length arrays.
C
      real(KIND=double), dimension(100,100) :: floatarraydouble
CC
C Define one allocatable array.
C
      real(KIND=single), ALLOCATABLE, dimension(:,:) :: floatarray
CC
C Set the size of the allocatable arrays.
C
      nlats = 35
      nlons = 70
CC
C Allcoate the array.
C
      ALLOCATE(floatarray(nlats,nlons), STAT=status)
CC
C Write to standard output the value of status.
C
      write(*,*) 'ALLOCATION STATUS = ', status
CC
C Write to standard output some of the array values.
C
      floatarray(35,70) = 2.0 * 5.0
      floatarray(35,69) = 2.0 * nlats
      floatarray(35,68) = nlons/nlats
      floatarray(35,67) = nlons/80
      floatarray(35,66) = nlons/80.
      floatarray(35,65) = 2. ** 3.
      floatarray(35,64) = cos(180.)
      floatarray(35,63) = cos(3.14159)
      floatarray(35,62) = LOG10(100.)
      floatarray(35,61) = exp(4.605170)
      floatarray(35,60) = alog(100.)
      floatarray(35,59) = sqrt(100.)
      floatarray(35,58) = acos(-1.)
      floatarraydouble(1,1) = log(100.)
      floatarraydouble(1,2) = dlog(100.) ! 'd' before function for
! double percision
      floatarraydouble(1,3) = dexp(floatarraydouble(1,2))
CC
C Write to standard output some of the array values.
C
      write(*,*) floatarray(35,70)
      write(*,*) floatarray(35,69)
      write(*,*) floatarray(35,68)
      write(*,*) floatarray(35,67)
      write(*,*) floatarray(35,66)
      write(*,*) floatarray(35,65)
      write(*,*) floatarray(35,64)
      write(*,*) floatarray(35,63)
      write(*,*) floatarray(35,62)
      write(*,*) floatarray(35,61)
      write(*,*) floatarray(35,60)
      write(*,*) floatarray(35,59)
      write(*,*) floatarray(35,58)
      write(*,*) floatarraydouble(1,1)
      write(*,*) floatarraydouble(1,2)
      write(*,*) floatarraydouble(1,3)
      END
C/=====================================================================/
C/ End of program /
C/=====================================================================/