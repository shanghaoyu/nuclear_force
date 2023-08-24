function EMN500(lp,l,kp,k,J,S,Tz)
! matrix <lp J S | V(kp,k) | l J S >
! Tz = -1 pp
! Tz = 0 np
! Tz = 1 nn
! Follow the unit conventions of the miyagii:
! kp,k ------ fm^-1
! V    ------ Mev fm^-3
implicit none 
real*8 :: EMN500
real*8,intent(in) :: kp,k
integer,intent(in) :: lp,l,J,S,Tz

integer :: jp,inn
real*8 :: v,xmev,ymev
logical :: heform,sing,trip,coup
common /cpot/   v(6),xmev,ymev
common /cstate/ jp,heform,sing,trip,coup
common /cnn/ inn
real*8 :: convert=197.32705d0
external n3lo500new

heform=.false.
sing=.true.
trip=.true.
coup=.true.

jp=J
inn=Tz+2
xmev=kp*convert
ymev=k*convert

call n3lo500new
v=v*convert**3

if(j .eq. 0)then 
    if( (S .eq. 0) .and. (lp .eq. 0) .and. (l .eq. 0) ) then 
        EMN500=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. 1) .and. (l .eq. 1) ))then
        EMN500=v(3)
    else
        write(*,*) "error"
        return
    end if
else if(j .gt. 0)then
    if( (S .eq. 0) .and. (lp .eq. J) .and. (l .eq. J) ) then 
        EMN500=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. J) .and. (l .eq. J) ))then
        EMN500=v(2)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J+1) ))then
        EMN500=v(3)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J-1) ))then
        EMN500=v(4)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J-1) ))then
        EMN500=v(5)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J+1) ))then
        EMN500=v(6)
    else
        write(*,*) "error"
        return
    end if
else
    write(*,*) "error"
    return
end if

return
end function

function cdbonnpot(lp,l,kp,k,J,S,Tz)
    ! matrix <lp J S | V(kp,k) | l J S >
    ! Tz = -1 pp
    ! Tz = 0 np
    ! Tz = 1 nn
    ! Follow the unit conventions of the miyagii:
    ! kp,k ------ fm^-1
    ! V    ------ Mev fm^-3
    implicit none 
    real*8 :: cdbonnpot
    real*8,intent(in) :: kp,k
    integer,intent(in) :: lp,l,J,S,Tz
    
    integer :: jp,inn
    real*8 :: v,xmev,ymev
    logical :: heform,sing,trip,coup
    common /cpot/   v(6),xmev,ymev
    common /cstate/ jp,heform,sing,trip,coup
    common /cnn/ inn
    real*8 :: convert=197.32705d0
    external cdbonn
    
    heform=.false.
    sing=.true.
    trip=.true.
    coup=.true.
    
    jp=J
    inn=Tz+2
    xmev=kp*convert
    ymev=k*convert
    
    call cdbonn
    v=v*convert**3
    
    if(j .eq. 0)then 
        if( (S .eq. 0) .and. (lp .eq. 0) .and. (l .eq. 0) ) then 
            cdbonnpot=v(1)
        else if(( (S .eq. 1) .and. (lp .eq. 1) .and. (l .eq. 1) ))then
            cdbonnpot=v(3)
        else
            write(*,*) "error"
            return
        end if
    else if(j .gt. 0)then
        if( (S .eq. 0) .and. (lp .eq. J) .and. (l .eq. J) ) then 
            cdbonnpot=v(1)
        else if(( (S .eq. 1) .and. (lp .eq. J) .and. (l .eq. J) ))then
            cdbonnpot=v(2)
        else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J+1) ))then
            cdbonnpot=v(3)
        else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J-1) ))then
            cdbonnpot=v(4)
        else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J-1) ))then
            cdbonnpot=v(5)
        else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J+1) ))then
            cdbonnpot=v(6)
        else
            write(*,*) "error"
            return
        end if
    else
        write(*,*) "error"
        return
    end if
    
    return
    end function



