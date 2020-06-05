
program saxpy
! setup type later real, double etc
use utility
!use omp_lib
use omp_lib
implicit none
character*100                   :: run

! allocate space for data
integer(kind=8)			:: maxVecLength = 1000
real(kind=4), allocatable       :: y(:), x(:), b(:)
real(kind=4)                    :: a

! counters
integer                         :: i, j, k

! timing
real                            :: t1
integer                         :: c1
 
real(kind=4)                    :: numCalcs

integer                         :: FLOPSPERCALC 
integer, parameter              :: vecLength = 512
integer, parameter              :: numOfRuns = 100000 
real                            :: tFlops = 1.0e12
integer                         :: numthreads
integer                         :: tid
	      call getarg(0, run) 
          run = trim(adjustl(run))

!          numOfRuns = 100
!          numOfRuns = 100000000
!          vecLength = 128

! allocate space for the vetcors
	allocate(y(maxVecLength), x(maxVecLength), b(maxVecLength))

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
        numCalcs = numOfRuns*FLOPSPERCALC*vecLength*(numOfRuns/tFlops)
    
!--------------------------------------------------------------------------

    call clockStartPoint(t1, c1) 
      do i = 1, numOfRuns
!$omp parallel do 
            do k = 1, vecLength 
                y(k) = a * x(k) + y(k)
            end do
!$omp end parallel do
      end do
    call calcTimeAndFlops(t1, c1, numCalcs, run) 

    write(6, *) "Sanity check sum is: ", sum(y)

    deallocate(y, x, b)
end program saxpy

!!DIR$ VECTOR ALIGNED
!!dir$ simd vectorlength(16)
