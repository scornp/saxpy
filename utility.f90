!
! 
! utility routines to calculate the length of an ascii data file 
!  and return its contents in real (kind=4)
!
!
module utility
implicit none
contains

    subroutine clockStartPoint(t1, c1) 
    real, intent(out)            :: t1
    integer, intent(out)         :: c1
    call cpu_time(t1)
    call system_clock(c1)  
    return
    end subroutine clockStartPoint

    subroutine calcTimeAndFlops(t1, c1, sysClockTime) 
    real, intent(in)             :: t1
    integer, intent(in)          :: c1
!    real(kind=4), intent(in)     :: numCalcs
!    character*(*), intent(in)    :: method
    real, intent(out)            :: sysClockTime
!    real, intent(out)            :: sysClockTime, Flops
    real                         :: t2
    integer                      :: c2
    real                         :: rate 
    integer                      :: cr, cm

    call system_clock(count_rate=cr)
    call system_clock(count_max=cm)

    rate = REAL(cr)
    call cpu_time(t2)
    call system_clock(c2)
 
    sysClockTime = (c2 - c1)/rate
!          Flops = numCalcs/sysClockTime
	!Flops = numCalcs/(t2 - t1)
!    write(6,'(a10, a80)')   " Method    : ", trim(adjustl(method))
!    write(6,'(a10, e12.3)') " Wall_time : ", sysClockTime
!    write(6,'(a10, e12.3)') " TFlops    : ", Flops
!    write(6,'(a10, e12.3)') " cpu_time  : ", t2 - t1 
!    write(6,'(a10, e12.3)') " rate      : ", rate
    return
    end subroutine calcTimeAndFlops 

end module utility


