
program saxpy
! setup type later real, double etc
use utility
use omp_lib
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

!$omp parallel
    !$omp master
	numOfThreads = omp_get_num_threads()
!   write(6, *) numOfThreads
    !$omp end master 
!$omp end parallel

! the max number of threads is 8 on aarch64
! need to create vectors of sya 

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
         numCalcs = flopsPerCalc*vecLength*(numOfRuns/gFlops)
    
!--------------------------------------------------------------------------

       do i = 1, numOfThreads
        istart(i) = 1 + (i - 1)*blockSize
         iStop(i) = istart(i) + blockSize - 1         
       end do
       
!!$omp master
!    do i = 1, numOfThreads
!     write(6, '(4(a15, i4))') " numOfThreads  ", i, &
!           & " istart  ", istart(i),              &
!           & " iend  ", iStop(i),                 &
!           & " size  ", (iStop(i) - istart(i)) + 1
!    end do
!!$omp end master 

    call clockStartPoint(t1, c1) 
 
!$omp parallel private (tid, j, iBegin, iEnd) shared(a, x, y, istart, iStop)
            tid = omp_get_thread_num() + 1
           
!     do k = istart(tid), iend(tid), blockSize
!	write(6, '(3(a15, i4))') "core :", tid,   &
!          & " istart(tid) ", istart(tid),        &
!           & " iend(tid) ", iStop(tid)

	iBegin = istart(tid)
	  iEnd = iStop(tid)

!	write(6, *) tid, iBegin, iEnd

      do i = 1, numOfRuns
!!dir$ simd
!                y(iBegin:iEnd) = a * x(iBegin:iEnd) + y(iBegin:iEnd) 
!dir$ simd               
           do k = iBegin, iEnd, subBlockSize
             y(k:k + subBlockSize - 1) = a * x(k+ subBlockSize - 1) + y(k + subBlockSize - 1)
           end do
     end do
!$omp end parallel
     
    call calcTimeAndFlops(t1, c1, wallTime) 

    write(6, '(a12, i2, a11, f8.6, a11, f8.6, a11, f8.6, a11, f12.1)')	&				
              & " Threads   ", numOfThreads,     			&
              & " Wall time ", wallTime,             			&
              & " numCalcs  ", numCalcs,             			& 
              & " Gflops    ", numCalcs/wallTime,   			&
              & " Check sum ", sum(y)

    deallocate(y, x)
end program saxpy

!!DIR$ VECTOR ALIGNED
!!dir$ simd vectorlength(16)
