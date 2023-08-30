

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




function loemn450(lp,l,kp,k,J,S,Tz)
use lo450m,only:lo450
implicit none
real*8 :: loemn450
real*8,intent(in) :: kp,k
integer,intent(in) :: lp,l,J,S,Tz

integer :: jp,inn
real*8 :: v,xmev,ymev
logical :: heform,sing,trip,coup
common /cpot/   v(6),xmev,ymev
common /cstate/ jp,heform,sing,trip,coup
common /cnn/ inn
real*8 :: convert=197.32705d0

heform=.false.
sing=.true.
trip=.true.
coup=.true.

jp=J
inn=Tz+2
xmev=kp*convert
ymev=k*convert

call lo450
v=v*convert**3

if(j .eq. 0)then
    if( (S .eq. 0) .and. (lp .eq. 0) .and. (l .eq. 0) ) then
        loemn450=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. 1) .and. (l .eq. 1) ))then
        loemn450=v(3)
    else
        write(*,*) "error"
        return
    end if
else if(j .gt. 0)then
    if( (S .eq. 0) .and. (lp .eq. J) .and. (l .eq. J) ) then
        loemn450=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. J) .and. (l .eq. J) ))then
        loemn450=v(2)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J+1) ))then
        loemn450=v(3)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J-1) ))then
        loemn450=v(4)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J-1) ))then
        loemn450=v(5)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J+1) ))then
        loemn450=v(6)
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


function loemn500(lp,l,kp,k,J,S,Tz)
use lo500m,only:lo500
implicit none
real*8 :: loemn500
real*8,intent(in) :: kp,k
integer,intent(in) :: lp,l,J,S,Tz

integer :: jp,inn
real*8 :: v,xmev,ymev
logical :: heform,sing,trip,coup
common /cpot/   v(6),xmev,ymev
common /cstate/ jp,heform,sing,trip,coup
common /cnn/ inn
real*8 :: convert=197.32705d0

heform=.false.
sing=.true.
trip=.true.
coup=.true.

jp=J
inn=Tz+2
xmev=kp*convert
ymev=k*convert

call lo500
v=v*convert**3

if(j .eq. 0)then
    if( (S .eq. 0) .and. (lp .eq. 0) .and. (l .eq. 0) ) then
        loemn500=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. 1) .and. (l .eq. 1) ))then
        loemn500=v(3)
    else
        write(*,*) "error"
        return
    end if
else if(j .gt. 0)then
    if( (S .eq. 0) .and. (lp .eq. J) .and. (l .eq. J) ) then
        loemn500=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. J) .and. (l .eq. J) ))then
        loemn500=v(2)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J+1) ))then
        loemn500=v(3)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J-1) ))then
        loemn500=v(4)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J-1) ))then
        loemn500=v(5)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J+1) ))then
        loemn500=v(6)
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


function loemn550(lp,l,kp,k,J,S,Tz)
use lo550m,only:lo550
implicit none
real*8 :: loemn550
real*8,intent(in) :: kp,k
integer,intent(in) :: lp,l,J,S,Tz

integer :: jp,inn
real*8 :: v,xmev,ymev
logical :: heform,sing,trip,coup
common /cpot/   v(6),xmev,ymev
common /cstate/ jp,heform,sing,trip,coup
common /cnn/ inn
real*8 :: convert=197.32705d0

heform=.false.
sing=.true.
trip=.true.
coup=.true.

jp=J
inn=Tz+2
xmev=kp*convert
ymev=k*convert

call lo550
v=v*convert**3

if(j .eq. 0)then
    if( (S .eq. 0) .and. (lp .eq. 0) .and. (l .eq. 0) ) then
        loemn550=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. 1) .and. (l .eq. 1) ))then
        loemn550=v(3)
    else
        write(*,*) "error"
        return
    end if
else if(j .gt. 0)then
    if( (S .eq. 0) .and. (lp .eq. J) .and. (l .eq. J) ) then
        loemn550=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. J) .and. (l .eq. J) ))then
        loemn550=v(2)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J+1) ))then
        loemn550=v(3)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J-1) ))then
        loemn550=v(4)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J-1) ))then
        loemn550=v(5)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J+1) ))then
        loemn550=v(6)
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


function nloemn450(lp,l,kp,k,J,S,Tz)
use nlo450m,only:nlo450
implicit none
real*8 :: nloemn450
real*8,intent(in) :: kp,k
integer,intent(in) :: lp,l,J,S,Tz

integer :: jp,inn
real*8 :: v,xmev,ymev
logical :: heform,sing,trip,coup
common /cpot/   v(6),xmev,ymev
common /cstate/ jp,heform,sing,trip,coup
common /cnn/ inn
real*8 :: convert=197.32705d0

heform=.false.
sing=.true.
trip=.true.
coup=.true.

jp=J
inn=Tz+2
xmev=kp*convert
ymev=k*convert

call nlo450
v=v*convert**3

if(j .eq. 0)then
    if( (S .eq. 0) .and. (lp .eq. 0) .and. (l .eq. 0) ) then
        nloemn450=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. 1) .and. (l .eq. 1) ))then
        nloemn450=v(3)
    else
        write(*,*) "error"
        return
    end if
else if(j .gt. 0)then
    if( (S .eq. 0) .and. (lp .eq. J) .and. (l .eq. J) ) then
        nloemn450=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. J) .and. (l .eq. J) ))then
        nloemn450=v(2)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J+1) ))then
        nloemn450=v(3)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J-1) ))then
        nloemn450=v(4)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J-1) ))then
        nloemn450=v(5)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J+1) ))then
        nloemn450=v(6)
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


function nloemn500(lp,l,kp,k,J,S,Tz)
use nlo500m,only:nlo500
implicit none
real*8 :: nloemn500
real*8,intent(in) :: kp,k
integer,intent(in) :: lp,l,J,S,Tz

integer :: jp,inn
real*8 :: v,xmev,ymev
logical :: heform,sing,trip,coup
common /cpot/   v(6),xmev,ymev
common /cstate/ jp,heform,sing,trip,coup
common /cnn/ inn
real*8 :: convert=197.32705d0

heform=.false.
sing=.true.
trip=.true.
coup=.true.

jp=J
inn=Tz+2
xmev=kp*convert
ymev=k*convert

call nlo500
v=v*convert**3

if(j .eq. 0)then
    if( (S .eq. 0) .and. (lp .eq. 0) .and. (l .eq. 0) ) then
        nloemn500=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. 1) .and. (l .eq. 1) ))then
        nloemn500=v(3)
    else
        write(*,*) "error"
        return
    end if
else if(j .gt. 0)then
    if( (S .eq. 0) .and. (lp .eq. J) .and. (l .eq. J) ) then
        nloemn500=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. J) .and. (l .eq. J) ))then
        nloemn500=v(2)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J+1) ))then
        nloemn500=v(3)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J-1) ))then
        nloemn500=v(4)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J-1) ))then
        nloemn500=v(5)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J+1) ))then
        nloemn500=v(6)
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


function nloemn550(lp,l,kp,k,J,S,Tz)
use nlo550m,only:nlo550
implicit none
real*8 :: nloemn550
real*8,intent(in) :: kp,k
integer,intent(in) :: lp,l,J,S,Tz

integer :: jp,inn
real*8 :: v,xmev,ymev
logical :: heform,sing,trip,coup
common /cpot/   v(6),xmev,ymev
common /cstate/ jp,heform,sing,trip,coup
common /cnn/ inn
real*8 :: convert=197.32705d0


heform=.false.
sing=.true.
trip=.true.
coup=.true.

jp=J
inn=Tz+2
xmev=kp*convert
ymev=k*convert

call nlo550
v=v*convert**3

if(j .eq. 0)then
    if( (S .eq. 0) .and. (lp .eq. 0) .and. (l .eq. 0) ) then
        nloemn550=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. 1) .and. (l .eq. 1) ))then
        nloemn550=v(3)
    else
        write(*,*) "error"
        return
    end if
else if(j .gt. 0)then
    if( (S .eq. 0) .and. (lp .eq. J) .and. (l .eq. J) ) then
        nloemn550=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. J) .and. (l .eq. J) ))then
        nloemn550=v(2)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J+1) ))then
        nloemn550=v(3)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J-1) ))then
        nloemn550=v(4)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J-1) ))then
        nloemn550=v(5)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J+1) ))then
        nloemn550=v(6)
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


function n2loemn450(lp,l,kp,k,J,S,Tz)
use n2lo450m,only:n2lo450
implicit none
real*8 :: n2loemn450
real*8,intent(in) :: kp,k
integer,intent(in) :: lp,l,J,S,Tz

integer :: jp,inn
real*8 :: v,xmev,ymev
logical :: heform,sing,trip,coup
common /cpot/   v(6),xmev,ymev
common /cstate/ jp,heform,sing,trip,coup
common /cnn/ inn
real*8 :: convert=197.32705d0

heform=.false.
sing=.true.
trip=.true.
coup=.true.

jp=J
inn=Tz+2
xmev=kp*convert
ymev=k*convert

call n2lo450
v=v*convert**3

if(j .eq. 0)then
    if( (S .eq. 0) .and. (lp .eq. 0) .and. (l .eq. 0) ) then
        n2loemn450=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. 1) .and. (l .eq. 1) ))then
        n2loemn450=v(3)
    else
        write(*,*) "error"
        return
    end if
else if(j .gt. 0)then
    if( (S .eq. 0) .and. (lp .eq. J) .and. (l .eq. J) ) then
        n2loemn450=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. J) .and. (l .eq. J) ))then
        n2loemn450=v(2)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J+1) ))then
        n2loemn450=v(3)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J-1) ))then
        n2loemn450=v(4)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J-1) ))then
        n2loemn450=v(5)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J+1) ))then
        n2loemn450=v(6)
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


function n2loemn500(lp,l,kp,k,J,S,Tz)
use n2lo500m,only:n2lo500
implicit none
real*8 :: n2loemn500
real*8,intent(in) :: kp,k
integer,intent(in) :: lp,l,J,S,Tz

integer :: jp,inn
real*8 :: v,xmev,ymev
logical :: heform,sing,trip,coup
common /cpot/   v(6),xmev,ymev
common /cstate/ jp,heform,sing,trip,coup
common /cnn/ inn
real*8 :: convert=197.32705d0

heform=.false.
sing=.true.
trip=.true.
coup=.true.

jp=J
inn=Tz+2
xmev=kp*convert
ymev=k*convert

call n2lo500
v=v*convert**3

if(j .eq. 0)then
    if( (S .eq. 0) .and. (lp .eq. 0) .and. (l .eq. 0) ) then
        n2loemn500=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. 1) .and. (l .eq. 1) ))then
        n2loemn500=v(3)
    else
        write(*,*) "error"
        return
    end if
else if(j .gt. 0)then
    if( (S .eq. 0) .and. (lp .eq. J) .and. (l .eq. J) ) then
        n2loemn500=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. J) .and. (l .eq. J) ))then
        n2loemn500=v(2)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J+1) ))then
        n2loemn500=v(3)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J-1) ))then
        n2loemn500=v(4)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J-1) ))then
        n2loemn500=v(5)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J+1) ))then
        n2loemn500=v(6)
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


function n2loemn550(lp,l,kp,k,J,S,Tz)
use n2lo550m,only:n2lo550
implicit none
real*8 :: n2loemn550
real*8,intent(in) :: kp,k
integer,intent(in) :: lp,l,J,S,Tz

integer :: jp,inn
real*8 :: v,xmev,ymev
logical :: heform,sing,trip,coup
common /cpot/   v(6),xmev,ymev
common /cstate/ jp,heform,sing,trip,coup
common /cnn/ inn
real*8 :: convert=197.32705d0
heform=.false.
sing=.true.
trip=.true.
coup=.true.

jp=J
inn=Tz+2
xmev=kp*convert
ymev=k*convert

call n2lo550
v=v*convert**3

if(j .eq. 0)then
    if( (S .eq. 0) .and. (lp .eq. 0) .and. (l .eq. 0) ) then
        n2loemn550=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. 1) .and. (l .eq. 1) ))then
        n2loemn550=v(3)
    else
        write(*,*) "error"
        return
    end if
else if(j .gt. 0)then
    if( (S .eq. 0) .and. (lp .eq. J) .and. (l .eq. J) ) then
        n2loemn550=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. J) .and. (l .eq. J) ))then
        n2loemn550=v(2)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J+1) ))then
        n2loemn550=v(3)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J-1) ))then
        n2loemn550=v(4)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J-1) ))then
        n2loemn550=v(5)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J+1) ))then
        n2loemn550=v(6)
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


function n3loemn450(lp,l,kp,k,J,S,Tz)
use n3lo450m,only:n3lo450new
implicit none
real*8 :: n3loemn450
real*8,intent(in) :: kp,k
integer,intent(in) :: lp,l,J,S,Tz

integer :: jp,inn
real*8 :: v,xmev,ymev
logical :: heform,sing,trip,coup
common /cpot/   v(6),xmev,ymev
common /cstate/ jp,heform,sing,trip,coup
common /cnn/ inn
real*8 :: convert=197.32705d0

heform=.false.
sing=.true.
trip=.true.
coup=.true.

jp=J
inn=Tz+2
xmev=kp*convert
ymev=k*convert

call n3lo450new
v=v*convert**3

if(j .eq. 0)then
    if( (S .eq. 0) .and. (lp .eq. 0) .and. (l .eq. 0) ) then
        n3loemn450=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. 1) .and. (l .eq. 1) ))then
        n3loemn450=v(3)
    else
        write(*,*) "error"
        return
    end if
else if(j .gt. 0)then
    if( (S .eq. 0) .and. (lp .eq. J) .and. (l .eq. J) ) then
        n3loemn450=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. J) .and. (l .eq. J) ))then
        n3loemn450=v(2)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J+1) ))then
        n3loemn450=v(3)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J-1) ))then
        n3loemn450=v(4)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J-1) ))then
        n3loemn450=v(5)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J+1) ))then
        n3loemn450=v(6)
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


function n3loemn500(lp,l,kp,k,J,S,Tz)
use n3lo500m,only:n3lo500new
implicit none
real*8 :: n3loemn500
real*8,intent(in) :: kp,k
integer,intent(in) :: lp,l,J,S,Tz

integer :: jp,inn
real*8 :: v,xmev,ymev
logical :: heform,sing,trip,coup
common /cpot/   v(6),xmev,ymev
common /cstate/ jp,heform,sing,trip,coup
common /cnn/ inn
real*8 :: convert=197.32705d0

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
        n3loemn500=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. 1) .and. (l .eq. 1) ))then
        n3loemn500=v(3)
    else
        write(*,*) "error"
        return
    end if
else if(j .gt. 0)then
    if( (S .eq. 0) .and. (lp .eq. J) .and. (l .eq. J) ) then
        n3loemn500=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. J) .and. (l .eq. J) ))then
        n3loemn500=v(2)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J+1) ))then
        n3loemn500=v(3)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J-1) ))then
        n3loemn500=v(4)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J-1) ))then
        n3loemn500=v(5)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J+1) ))then
        n3loemn500=v(6)
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


function n3loemn550(lp,l,kp,k,J,S,Tz)
use n3lo550m,only:n3lo550new
implicit none
real*8 :: n3loemn550
real*8,intent(in) :: kp,k
integer,intent(in) :: lp,l,J,S,Tz

integer :: jp,inn
real*8 :: v,xmev,ymev
logical :: heform,sing,trip,coup
common /cpot/   v(6),xmev,ymev
common /cstate/ jp,heform,sing,trip,coup
common /cnn/ inn
real*8 :: convert=197.32705d0

heform=.false.
sing=.true.
trip=.true.
coup=.true.

jp=J
inn=Tz+2
xmev=kp*convert
ymev=k*convert

call n3lo550new
v=v*convert**3

if(j .eq. 0)then
    if( (S .eq. 0) .and. (lp .eq. 0) .and. (l .eq. 0) ) then
        n3loemn550=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. 1) .and. (l .eq. 1) ))then
        n3loemn550=v(3)
    else
        write(*,*) "error"
        return
    end if
else if(j .gt. 0)then
    if( (S .eq. 0) .and. (lp .eq. J) .and. (l .eq. J) ) then
        n3loemn550=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. J) .and. (l .eq. J) ))then
        n3loemn550=v(2)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J+1) ))then
        n3loemn550=v(3)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J-1) ))then
        n3loemn550=v(4)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J-1) ))then
        n3loemn550=v(5)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J+1) ))then
        n3loemn550=v(6)
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


function n4loemn450(lp,l,kp,k,J,S,Tz)
use n4lo450m,only:n4lo450
implicit none
real*8 :: n4loemn450
real*8,intent(in) :: kp,k
integer,intent(in) :: lp,l,J,S,Tz

integer :: jp,inn
real*8 :: v,xmev,ymev
logical :: heform,sing,trip,coup
common /cpot/   v(6),xmev,ymev
common /cstate/ jp,heform,sing,trip,coup
common /cnn/ inn
real*8 :: convert=197.32705d0
heform=.false.
sing=.true.
trip=.true.
coup=.true.

jp=J
inn=Tz+2
xmev=kp*convert
ymev=k*convert

call n4lo450
v=v*convert**3

if(j .eq. 0)then
    if( (S .eq. 0) .and. (lp .eq. 0) .and. (l .eq. 0) ) then
        n4loemn450=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. 1) .and. (l .eq. 1) ))then
        n4loemn450=v(3)
    else
        write(*,*) "error"
        return
    end if
else if(j .gt. 0)then
    if( (S .eq. 0) .and. (lp .eq. J) .and. (l .eq. J) ) then
        n4loemn450=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. J) .and. (l .eq. J) ))then
        n4loemn450=v(2)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J+1) ))then
        n4loemn450=v(3)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J-1) ))then
        n4loemn450=v(4)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J-1) ))then
        n4loemn450=v(5)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J+1) ))then
        n4loemn450=v(6)
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


function n4loemn500(lp,l,kp,k,J,S,Tz)
use n4lo500m,only:n4lo500
implicit none
real*8 :: n4loemn500
real*8,intent(in) :: kp,k
integer,intent(in) :: lp,l,J,S,Tz

integer :: jp,inn
real*8 :: v,xmev,ymev
logical :: heform,sing,trip,coup
common /cpot/   v(6),xmev,ymev
common /cstate/ jp,heform,sing,trip,coup
common /cnn/ inn
real*8 :: convert=197.32705d0

heform=.false.
sing=.true.
trip=.true.
coup=.true.

jp=J
inn=Tz+2
xmev=kp*convert
ymev=k*convert

call n4lo500
v=v*convert**3

if(j .eq. 0)then
    if( (S .eq. 0) .and. (lp .eq. 0) .and. (l .eq. 0) ) then
        n4loemn500=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. 1) .and. (l .eq. 1) ))then
        n4loemn500=v(3)
    else
        write(*,*) "error"
        return
    end if
else if(j .gt. 0)then
    if( (S .eq. 0) .and. (lp .eq. J) .and. (l .eq. J) ) then
        n4loemn500=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. J) .and. (l .eq. J) ))then
        n4loemn500=v(2)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J+1) ))then
        n4loemn500=v(3)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J-1) ))then
        n4loemn500=v(4)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J-1) ))then
        n4loemn500=v(5)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J+1) ))then
        n4loemn500=v(6)
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


function n4loemn550(lp,l,kp,k,J,S,Tz)
use n4lo550m,only:n4lo550
implicit none
real*8 :: n4loemn550
real*8,intent(in) :: kp,k
integer,intent(in) :: lp,l,J,S,Tz

integer :: jp,inn
real*8 :: v,xmev,ymev
logical :: heform,sing,trip,coup
common /cpot/   v(6),xmev,ymev
common /cstate/ jp,heform,sing,trip,coup
common /cnn/ inn
real*8 :: convert=197.32705d0
heform=.false.
sing=.true.
trip=.true.
coup=.true.

jp=J
inn=Tz+2
xmev=kp*convert
ymev=k*convert

call n4lo550
v=v*convert**3

if(j .eq. 0)then
    if( (S .eq. 0) .and. (lp .eq. 0) .and. (l .eq. 0) ) then
        n4loemn550=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. 1) .and. (l .eq. 1) ))then
        n4loemn550=v(3)
    else
        write(*,*) "error"
        return
    end if
else if(j .gt. 0)then
    if( (S .eq. 0) .and. (lp .eq. J) .and. (l .eq. J) ) then
        n4loemn550=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. J) .and. (l .eq. J) ))then
        n4loemn550=v(2)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J+1) ))then
        n4loemn550=v(3)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J-1) ))then
        n4loemn550=v(4)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J-1) ))then
        n4loemn550=v(5)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J+1) ))then
        n4loemn550=v(6)
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


function n2LOopt(lp,l,kp,k,J,S,Tz)
use nnlo_optm,only:nnlo_opt
implicit none
real*8 :: n2LOopt
real*8,intent(in) :: kp,k
integer,intent(in) :: lp,l,J,S,Tz

integer :: jp,inn
real*8 :: v,xmev,ymev
logical :: heform,sing,trip,coup
common /cpot/   v(6),xmev,ymev
common /cstate/ jp,heform,sing,trip,coup
common /cnn/ inn
real*8 :: convert=197.32705d0

heform=.false.
sing=.true.
trip=.true.
coup=.true.

jp=J
inn=Tz+2
xmev=kp*convert
ymev=k*convert

call nnlo_opt
v=v*convert**3

if(j .eq. 0)then
    if( (S .eq. 0) .and. (lp .eq. 0) .and. (l .eq. 0) ) then
        n2LOopt=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. 1) .and. (l .eq. 1) ))then
        n2LOopt=v(3)
    else
        write(*,*) "error"
        return
    end if
else if(j .gt. 0)then
    if( (S .eq. 0) .and. (lp .eq. J) .and. (l .eq. J) ) then
        n2LOopt=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. J) .and. (l .eq. J) ))then
        n2LOopt=v(2)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J+1) ))then
        n2LOopt=v(3)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J-1) ))then
        n2LOopt=v(4)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J-1) ))then
        n2LOopt=v(5)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J+1) ))then
        n2LOopt=v(6)
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


function n2LOsat(lp,l,kp,k,J,S,Tz)
use nnlo_satm,only:nnlo_sat
implicit none
real*8 :: n2LOsat
real*8,intent(in) :: kp,k
integer,intent(in) :: lp,l,J,S,Tz

integer :: jp,inn
real*8 :: v,xmev,ymev
logical :: heform,sing,trip,coup
common /cpot/   v(6),xmev,ymev
common /cstate/ jp,heform,sing,trip,coup
common /cnn/ inn
real*8 :: convert=197.32705d0
heform=.false.
sing=.true.
trip=.true.
coup=.true.

jp=J
inn=Tz+2
xmev=kp*convert
ymev=k*convert

call nnlo_sat
v=v*convert**3

if(j .eq. 0)then
    if( (S .eq. 0) .and. (lp .eq. 0) .and. (l .eq. 0) ) then
        n2LOsat=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. 1) .and. (l .eq. 1) ))then
        n2LOsat=v(3)
    else
        write(*,*) "error"
        return
    end if
else if(j .gt. 0)then
    if( (S .eq. 0) .and. (lp .eq. J) .and. (l .eq. J) ) then
        n2LOsat=v(1)
    else if(( (S .eq. 1) .and. (lp .eq. J) .and. (l .eq. J) ))then
        n2LOsat=v(2)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J+1) ))then
        n2LOsat=v(3)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J-1) ))then
        n2LOsat=v(4)
    else if(( (S .eq. 1) .and. (lp .eq. J+1) .and. (l .eq. J-1) ))then
        n2LOsat=v(5)
    else if(( (S .eq. 1) .and. (lp .eq. J-1) .and. (l .eq. J+1) ))then
        n2LOsat=v(6)
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