C/=====================================================================/
C/ Loops.f90 /
C/=====================================================================/
C/ A fortran code with loops. /
C/=====================================================================/
C/ Date: 9/05/08 /
C/ Version: 1.0.0 /
C/ Executing: /
C/ ./Loops /
C/====|================================================================/
      PROGRAM Loops
      implicit none
C
CC Define the parameter values single. This value never changes in
CC the program.
C
      integer*4, parameter :: single = 4
C
CC Define a bunch of variables
C
      integer i, npts, status
      real*4 d, pi, theta, thetastep
C
CC Define one allocatable array.
C
      real*4, allocatable, dimension(:) :: x,y
C
CC Set the size of the allocatable array
C
      write(*,*) 'Enter the number of points'
      read(*,*) npts
      write(*,*) 'Number of points entered: ', npts
C
CC Allocate the allocatable array
C
      allocate(x(npts), stat= status)
      write(*,*) 'Allocation status for x = ', status
      allocate(y(npts), stat= status)
      write(*,*) 'Allocation status for y = ', status
C
CC Compute the (x,y) locations along a circle of radius 1.
C
      pi = acos(-1.0)
      thetastep = (2.0*pi)/(npts-1.)
C
CC Your first do loop
C
      do i = 1, npts, 1
         theta = (i-1)*thetastep
         x(i) = cos(theta)
         y(i) = sin(theta)
         d = sqrt(x(i)**2+y(i)**2)
         write(*,*) 'x = ', x(i), 'y = ', y(i), 'distance = ', d
      end do
C
CC Deallocate th eallocatable arrays
C
      deallocate (x, stat = status)
      deallocate (y, stat = status)
      END
C/=====================================================================/
C/ End of program /
C/=====================================================================/
