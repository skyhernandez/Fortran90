C/=====================================================================/
C/ Arrays.f90                                                          /
C/=====================================================================/
C/ A fortran code with examples of types, constants, and variables in ?/
C/ arrays.                                                             /
C/=====================================================================/
C/ Date: 9/03/08                                                       /
C/ Version: 1.0.0                                                      /
C/ Executing:                                                          /
C/ ./Arrays                                                            /
C/====|================================================================/
      PROGRAM Arrays
      implicit none
C
CC Define the two paramters values single and double. These values
CC never change in the program.
C
      integer*4, parameter :: single = 4
      integer*4, parameter :: double = 8
      integer*4, parameter :: arrsize = 100
      integer*4, nlats, nlons, status
C
CC Define a bunch of fixed length arrays.
C
      integer*4, dimension(arrsize) :: intarraysinglea
      integer*4 :: intarraysingleb(-arrsize/2:arrsize/2)
      real(KIND=double), dimension(100,100) :: floatarraydouble
C
CC Define one allocatable array.
C
      real(KIND=single), ALLOCATABLE, dimension(:,:) :: floatarray
C
CC Set the size of the allocatable arrays.
C
      nlats = 35
      nlons = 70
C
CC Allcoate the array.
C
      ALLOCATE(floatarray(nlats,nlons), STAT=status)
C
CC Write to standard output the value of status.
C
      write(*,*) 'ALLOCATION STATUS = ', status
C
CC Write to standard output some of the array values.
C
      write(*,*) intarraysinglea(1)
      write(*,*) intarraysinglea(101)
      write(*,*) intarraysingleb(-50)
      write(*,*) floatarraydouble(100,100)
      write(*,*) floatarray(35,70)
C
CC Change the array values.
C
      intarraysinglea(1) = 100
      intarraysinglea(100) = 201
      intarraysingleb(-50) = -151
      floatarraydouble(100,100) = 10000
      floatarray(36,71) = 23.6 ! out of the bounds many
! things can happen (BAD)
      floatarray(35,70) = 23.6
C
CC Write to standard output the values of status.
C
      write(*,*) 'New Values'
C
CC Write to standard output some of the array values.
C
      write(*,*) intarraysinglea(1)
      write(*,*) intarraysinglea(100)
      write(*,*) intarraysingleb(-50)
      write(*,*) floatarraydouble(100,100)
      write(*,*) floatarray(35,70)
      END
C/=====================================================================/
C/ End of program                                                      /
C/=====================================================================/
