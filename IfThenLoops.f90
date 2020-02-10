C/=====================================================================/
C/ IfThenLoops.f90                                                     /
C/=====================================================================/
C/ A fortran code with if then statements.                             /
C/=====================================================================/
C/ Date: 8/29/08                                                       /
C/ Version: 1.0.0                                                      /
C/ Executing:                                                          /
C/ ./IfThenLoops                                                       /
C/====|================================================================/
      PROGRAM IfThenLoops
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
CC Define a fixed length arrays.
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
      nlats = 2000000
      nlons = 7000000
C
CC Allcoate the array.
C
      ALLOCATE(floatarray(nlats,nlons), STAT=status)
C
CC Write to standard output the value of status.
C
      write(*,*) 'ALLOCATION STATUS = ', status
C
CC Set of IF-then statements to test the value of status.
C
      if (status .eq. 0) then
         write(*,*) 'Successful Allocation - Yeah'
C stop
      end if      
C status = -1
      if (status == 0) then
         write(*,*) 'Successful Allocation - YiYeah'
      else
         write(*,*) 'Unsucessful Aloocation - Dough'
      end if
      status = -1
      if (status == 0) then
         write(*,*) 'Successful Allocation - YiYeah'
      else if (status < 0) then
         write(*,*) 'Status is less than 0 - Dough'
      else if (status > 0) then
         write(*,*) 'Status is greater than 0 - Dough'
      else
         write(*,*) 'Status is Bizarre - Dough'
      end if
      if (allocated (floatarray)) then
         write(*,*) 'Successful allocation-- I think!'
         floatarray(1,1)=-1.
         write(*,*) floatarray(1,1)
         floatarray(nlats,nlons) = 1.
         write(*,*) floatarray(nlats,nlons)
      else
         write(*,*) 'Unsuccessful Allocation - Argh! Stop!!'
         stop
      end if
C
CC Deallocate the array.
C
      deallocate(floatarray, stat= status) ! gains memmory for other
! parts of the same
! program
      END
C/=====================================================================/
C/ End of program                                                      /
C/=====================================================================/
