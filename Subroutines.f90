C/=====================================================================/
C/ Subroutines.f90 /
C/=====================================================================/
C/ A fortran code with subroutines. /
C/=====================================================================/
C/ Date: 9/05/08 /
C/ Version: 1.0.0 /
C/ Executing: /
C/ ./Subroutines /
C/====|================================================================/
      PROGRAM Subroutines
      implicit none
C
CC Define the two parameter values single and double. These values
CC never change in the program
C
      integer*4, parameter :: single = 4, double = 8
C
CC Define a bunch of variables.
C
      integer i, k, npts, status
      integer InOutFcn, InOutSub, InFcn, InSub
      integer FcnMyInCircle
      real(kind=single) D
      real(kind=double) PiFcn, PiSub
C
CC Define three allocatable arrays.
C
      integer(kind=single), allocatable, dimension(:) :: seedints
      real(kind=single), allocatable, dimension(:) :: x, y
C
CC Set the size of the allocatable array.
C
      write(*,*) 'Enter the number of points'
      read(*,*) npts
      write(*,*) 'Number of points entered: ', npts
C
CC Allocate the allocatable array.
C
      allocate(x(npts), stat= status)
      write(*,*) 'Allocation status for x = ', status
      allocate(y(npts), stat= status)
      write(*,*) 'Allocation status for y = ', status
C
CC Processor sets the random seed using intrinsic subroutine random_seed
C
      call random_seed
C
CC Get number of integers used to set the seed using Intrinsic Subroutine
CC RANDOM_SEED.
C
      call random_seed (size = k)
C
CC Allocate seedints.
C
      allocate(seedints(k), STAT=status)
C
CC Get integers used to set the see using Intrinsic subroutine
CC RANDOM_SEED.
C
      call random_seed (get = seedints(1:k))
C
CC Print the seed information.
C
C print *,k
      DO i = 1, k, 1
C print *, seedints(i)
      END DO
C
CC Retrieve a pile of random numbers using the intrinsic subroutine
CC RANDOM_NUMBER.
C
      call random_number(x)
      call random_number(y)
C
CC Subtract 0.5 from both sets of numbers x andy. Here we use an
CC advanced feature of fortran 90.
C
      x = x - 0.5
C
CC and here we do it with a brute force loop.
C
      DO i = 1, npts, 1
         y(i) = y(i) - 0.5
C write(*,*) y(i), x(i)
      END DO
C
CC Estimate the value of pi using a function and a subroutine.
C
      DO i = 1, npts, 1
C write(*,*) '**********************************'
C write(*,*) x(i), y(i), InOutFcn
         InOutFcn = FcnMyInCircle(x(i),y(i))
C write(*,*) x(i), y(i), InOutFcn
C write(*,*) 'Moving from function to subroutine'
C write(*,*) x(i), y(i), InOutSub, d
call SubMyInCircle(x(i), y(i), InOutSub, d)
C write(*,*) x(i), y(i), InOutSub, d
C write(*,*) '**********************************'
         InFcn = InFcn + InOutFcn
         InSub = InSub + InOutSub
      END DO
      PiFcn = ((1.0D0*InFcn)/npts)/(0.5D0*0.5D0)
      PiSub = ((1.0D0*InSub)/npts)/(0.5D0*0.5D0)
      write(*,*) 'Pi = ', PiFcn, PiSub
C
CC Deallocate the arrays.
C
      deallocate(x, STAT=status)
      deallocate(y, STAT=status)
      END
C/=====================================================================/
C/ End of program /
C/=====================================================================/
C/=====================================================================/
C/ Function FcnMyInCircle /
C/=====================================================================/
      integer function FcnMyInCircle(xin, yin)
        implicit none
        integer*4, parameter :: single = 4
        real(kind = single) xin, yin, d
        d = sqrt(xin**2+yin**2)
        if (d <= 0.5) then
           FcnMyInCircle = 1
        else
           FcnMyInCircle = 0
        endif
        return
      end function FcnMyInCircle
C/=====================================================================/
C/ Subroutine SubMyInCircle /
C/=====================================================================/
      subroutine SubMyInCircle(xin, yin, inout, d)
        implicit none
        integer*4 inout
        real*4 xin, yin, d
        d = sqrt(xin**2+yin**2)
        if (d <= 0.5) then
           inout = 1
        else
           inout = 0
        end if
        return
      end subroutine SubMyInCircle
C/=====================================================================/
