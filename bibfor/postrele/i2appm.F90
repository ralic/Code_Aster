subroutine i2appm(xp, yp, xso, yso, xin,&
                  yin, cdroi, nbc, dedans)
    implicit   none
    real(kind=8) :: xp, yp, xso(*), yso(*), xin(*), yin(*)
    logical :: dedans, cdroi(*)
    integer :: nbc
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    integer :: c, k, l, nt, i
    real(kind=8) :: ps, xd, yd, xf, yf
!
    dedans = .false.
    xf = xso(1)
    yf = yso(1)
    k = 0
    l = 0
!
    nt = nbc
    do 10 c = 1, nbc
!
        if (.not. cdroi(c)) then
!
            nt = nt + 1
            do 12 i = 1, 2
                xd = xf
                yd = yf
                if (i .eq. 1) then
                    xf = xin(c)
                    yf = yin(c)
                else
                    xf = xso(c+1)
                    yf = yso(c+1)
                endif
!
                ps = (xp-xd)*(yd-yf) + (yp-yd)*(xf-xd)
!
                if (ps .ge. 0.0d0) then
                    k = k + 1
                else
                    l = l + 1
                endif
!
12          continue
!
        else
!
            xd = xf
            yd = yf
            xf = xso(c+1)
            yf = yso(c+1)
!
            ps = (xp-xd)*(yd-yf) + (yp-yd)*(xf-xd)
!
            if (ps .ge. 0.0d0) then
                k = k + 1
            else
                l = l + 1
            endif
!
        endif
!
10  end do
!
    if (k .eq. nt .or. l .eq. nt) dedans = .true.
!
end subroutine
