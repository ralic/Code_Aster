subroutine usunew(type, para, crit, epsi, x1,&
                  x2, resu, iret)
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
    include 'asterfort/usufon.h'
    real(kind=8) :: para(*)
    character(len=*) :: type, crit
!-----------------------------------------------------------------------
    integer :: i, iret, maxit
    real(kind=8) :: da, dfr, dl, dx, dxold, epsi, fr
    real(kind=8) :: resu, temp, x1, x2, xh, xl, zero
!
!-----------------------------------------------------------------------
    parameter     ( maxit = 100 )
!     ------------------------------------------------------------------
!
    iret = 0
    zero = 0.d0
!
    if (type(1:8) .eq. 'TUBE_BAV') then
        dl = para(2)
        da = para(3)
    endif
    xl = x1
    xh = x2
    resu = ( x1 + x2 ) * 0.5d0
    dxold = abs( x2 - x1 )
    dx = dxold
    call usufon(type, para, resu, fr, dfr)
    do 10 i = 1, maxit
        dxold = dx
        if (((resu-xh)*dfr-fr)*((resu-xl)*dfr-fr) .ge. zero .or. abs( fr*2.d0) .gt.&
            abs(dxold*dfr)) then
            dx = ( xh - xl ) * 0.5d0
            resu = dx * xl
!            IF ( XL .EQ. RESU ) GOTO 9999
            if (crit(1:4) .eq. 'RELA') then
                if (abs(xl-resu) .le. epsi * abs(resu)) goto 9999
            else
                if (abs(xl - resu) .le. epsi) goto 9999
            endif
        else
            dx = fr / dfr
            temp = resu
            resu = resu - dx
!            IF ( TEMP .EQ. RESU ) GOTO 9999
            if (crit(1:4) .eq. 'RELA') then
                if (abs(temp-resu) .le. epsi * abs(resu)) goto 9999
            else
                if (abs(temp - resu) .le. epsi) goto 9999
            endif
        endif
        if (abs(dx) .lt. epsi) goto 9999
        if (type(1:8) .eq. 'TUBE_BAV') then
            if (( resu - dl*da ) .lt. 0.d0) then
!            WRITE(8,*)'--->> USUNEW, NOMBRE NEGATIF ',( RESU - DL*DA )
!            WRITE(8,*)'              ON AJUSTE'
                resu = dl*da
            endif
        endif
        call usufon(type, para, resu, fr, dfr)
        if (fr .lt. zero) then
            xl = resu
        else
            xh = resu
        endif
10  end do
    iret = 10
    goto 9999
!
9999  continue
end subroutine
