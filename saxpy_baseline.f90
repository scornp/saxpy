
program saxpy
! setup type later real, double etc
use utility
!use omp_lib
!use omp_sub

implicit none
character*100                   :: run

! allocate space for data
real(kind=4), allocatable       :: y(:), x(:)
real(kind=4)                    :: a

! counters
integer                         :: i, j, k

! timing
real                            :: t1
integer                         :: c1
 
real(kind=4)                    :: numCalcs

integer                         :: flopsPerCalc 

integer                         :: vecLength = 512
integer(kind=8)			:: maxVecLength = 4000

integer                         :: numOfRuns = 100000 
real                            :: gFlops = 1.0e9
real   				:: wallTime
integer                         :: numOfThreads, offset, istart(1000), iStop(1000)
integer                         :: tid
integer                         :: numOfBlocks, blockSize
integer				:: numSubBlocks, subBlockSize
integer                         :: iBegin, iEnd
	      call getarg(0, run) 
          run = trim(adjustl(run))

!          numOfRuns = 100
!          numOfRuns = 100000000
!          vecLength = 128

! number of calculations

! the max number of threads is 8 on aarch64
! need to create vectors of sya 
	 vecLength = vecLength*8
         blockSize = vecLength/numOfThreads ! assume 4 lanes
       numOfBlocks = numOfThreads

      subBlockSize = 4
      numSubBlocks = blockSize/subBlockSize

!	write(6, *) numSubBlocks, subBlockSize	

! allocate space for the vetcors
	allocate(y(vecLength), x(vecLength))

! set data
! at some point need to check results

!	call random_seed()
!	call random_number(x)
!	call random_number(y) 
		a = 1.0
		x = 1.0
		y = 1.0
    
     flopsPerCalc = 2 
         numCalcs = flopsPerCalc*vecLength*numOfRuns

    call clockStartPoint(t1, c1) 
       do i = 1, numOfRuns
            do k = 1, vecLength
             y(k) = a * x(k) + y(k)
           end do
       end do  

    call calcTimeAndFlops(t1, c1, wallTime) 

    write(6, '(a12, a11, f10.6, a11, f12.0, a11, f8.6, a11, f12.1)')	&				
              & " Baseline :",        					&
              & " Wall time ", wallTime,             			&
              & " numCalcs  ", numCalcs,             			& 
              & " GFlops    ", numCalcs/wallTime/gFlops,   		&
              & " Check sum ", sum(y)


    deallocate(y, x)
end program saxpy

!!DIR$ VECTOR ALIGNED
!!dir$ simd vectorlength(16)
