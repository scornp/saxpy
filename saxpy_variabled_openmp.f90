
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

real(kind=8)                    :: It, TA(2)

integer                         :: FLOPSPERCALC 
!integer                         :: MAXFLOPS_ITERS = 100000000
integer                         :: MAXFLOPS_ITERS = 10000000!000000
integer                         :: LOOP_COUNT = 1536
integer(kind=8)			:: vecLength = 4000

integer                         :: lotsOfRuns = 1 
real                            :: tFlops = 1.0e12
integer                         :: numOfCores, offset, istart(1000), iStop(1000)
integer                         :: tid
integer                         :: numOfBlocks, blockSize
integer                         :: iBegin, iEnd
	      call getarg(0, run) 
          run = trim(adjustl(run))

!          lotsOfRuns = 100
!      MAXFLOPS_ITERS = 100000000
!          LOOP_COUNT = 128

! number of calculations

!$omp parallel
    !$omp master
	numOfCores = omp_get_num_threads()
    write(6, *) " number of cores = ", numOfCores
    !$omp end master 
!$omp end parallel

         blockSize = 16
       numOfBlocks = numOfCores

	LOOP_COUNT = blockSize*numOfCores

! allocate space for the vetcors
	allocate(y(LOOP_COUNT), x(LOOP_COUNT))

! set data
! at some point need to check results

	call random_seed()
	call random_number(x)
 	call random_number(y) 
		a = 1.0
		x = 1.0
		y = 1.0
    
     FLOPSPERCALC = 2 
         numCalcs = FLOPSPERCALC*LOOP_COUNT*(MAXFLOPS_ITERS/tFlops)
    
!--------------------------------------------------------------------------
    write(6, *) " number of cores = ", numOfCores

       do i = 1, numOfCores
        istart(i) = 1 + (i - 1)*blockSize
         iStop(i) = istart(i) + blockSize - 1         
       end do
       
!!$omp master
!    do i = 1, numOfCores
!     write(6, '(4(a15, i4))') " numOfCores  ", i, &
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

        do j = 1, MAXFLOPS_ITERS 
!!dir$ simd
!                y(iBegin:iEnd) = a * x(iBegin:iEnd) + y(iBegin:iEnd)                
           do i = iBegin, iEnd
             y(i) = a * x(i) + y(i)
           end do
         end do
!     end do
!$omp end parallel
     
    call calcTimeAndFlops(t1, c1, numCalcs, run) 

    write(6, *) "Sanity check sum is: ", sum(y)

    deallocate(y, x)
end program saxpy

!!DIR$ VECTOR ALIGNED
!!dir$ simd vectorlength(16)
