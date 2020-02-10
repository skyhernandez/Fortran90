C/=====================================================================/
C/ Pointers.f90                                                        /
C/=====================================================================/
C/ A fortran code on how to use pointers. Pointers allow you to        /
C/ allow you to allocate and deallocate within a subroutine and not in /
C/ the main code.                                                      /
C/=====================================================================/
C/ Date: 11/5/08                                                       /
C/ Version: 1.0.0                                                      /
C/ Executing:                                                          /
C/ ./Pointers                                                          /
C/====|================================================================/
      PROGRAM Pointers
C
CC To make use of pointers you must use an Interface before assigning
CC variables/arrays.
C
      INTERFACE
      SUBROUTINE AllocateAPointer(ptr2array)
      REAL*4, DIMENSION(:), POINTER :: ptr2array
      END SUBROUTINE AllocateAPointer
      END INTERFACE
C
CC Assigning an allocatable array as a pointer.
C
      REAL*4, DIMENSION(:), POINTER :: ptr2array
CALL AllocateAPointer(ptr2array)
C
CC Setting the array without having to allocate it before hand.
C
      ptr2array(1) = 1
      ptr2array(10) = 10
C
CC Print out the array values.
C
      WRITE(*,*) ptr2array
      END PROGRAM
C/=====================================================================/
C/ SUBROUTINE AllocateAPointer                                         /
C/=====================================================================/
      SUBROUTINE AllocateAPointer(array)
        INTEGER*4, istatus
        REAL*4, DIMENSION(:), POINTER :: array
        ALLOCATE(array(10), STAT = istatus)
      END SUBROUTINE
C/=====================================================================/
