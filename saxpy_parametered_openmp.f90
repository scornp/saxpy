
program saxpy
! setup type later real, double etc
use utility
!use omp_lib
use omp_sub
implicit none
character*100                   :: run

! allocate space for data
integer(kind=8)			:: vecLength = 1000
real(kind=4), allocatable       :: y(:), x(:), b(:)
real(kind=4)                    :: a

! counters
integer                         :: i, j, k

! timing
real                            :: t1
integer                         :: c1
 
real(kind=4)                    :: numCalcs

real(kind=8)                    :: It, TA(2)

integer                         :: FLOPSPERCALC 
integer,parameter               :: MAXFLOPS_ITERS = 100000!000000
integer, parameter              :: LOOP_COUNT = 512
integer, parameter              :: lotsOfRuns = 1 
real                            :: tFlops = 1.0e12
integer                         :: numthreads
integer                         :: tid
	      call getarg(0, run) 
          run = trim(adjustl(run))

!          lotsOfRuns = 100
!      MAXFLOPS_ITERS = 100000000
!          LOOP_COUNT = 128

! allocate space for the vetcors
	allocate(y(vecLength), x(vecLength), b(vecLength))

! set data
! at some point need to check results
	a = 10.0
	call random_seed()
	call random_number(x)
 	call random_number(y) 
		a = 1.0
		x = 1.0
		y = 1.0
!$omp parallel
	numthreads = omp_get_num_threads()
!$omp end parallel

!$omp master
    write(6, *) "num of threads = ", numthreads
!$omp end master 

! number of calculations
    
    FLOPSPERCALC = 2 
        numCalcs = lotsOfRuns*FLOPSPERCALC*LOOP_COUNT*(MAXFLOPS_ITERS/tFlops)
    
!--------------------------------------------------------------------------

    call clockStartPoint(t1, c1) 
      do i = 1, lotsOfRuns
        do j = 1, MAXFLOPS_ITERS 
!$omp parallel do schedule(guided)
            do k = 1, LOOP_COUNT 
                y(k) = a * x(k) + y(k)
            end do
!$omp end parallel do
	end do
      end do
    call calcTimeAndFlops(t1, c1, numCalcs, run) 

    write(6, *) "Sanity check sum is: ", sum(y)

    deallocate(y, x, b)
end program saxpy

!!DIR$ VECTOR ALIGNED
!!dir$ simd vectorlength(16)
