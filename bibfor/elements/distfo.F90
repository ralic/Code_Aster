function distfo(zimat, kfonc, xx, yy, normx,&
                normy)
!
    implicit  none
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!======================================================================
!
!     CALCUL
!
! IN  ZIMAT :
! IN  KFONC :
! IN  XX :
! IN  YY :
!
! OUT NORMX :
! OUT NORMY :
!
    include 'asterfort/cdnfon.h'
    include 'asterfort/rcvalb.h'
    integer :: i, itmax, ier, zimat, kpg, spt
!
    real(kind=8) :: distfo, xx, yy, normx, normy, x0, y0
    real(kind=8) :: xi, yi, xm1, res, ym1, rp, dym1, tol
    real(kind=8) :: xm2, ym2, dyi
!
    integer :: codres
    character(len=8) :: kfonc, fami, poum
    character(len=16) :: phenom
!
    phenom = 'GLRC_DAMAGE'
!
    tol = 1.0d-3
    x0 = xx / normx
    y0 = yy / normy
    res = 1.0d6
    xi = x0
!
    ym1 = 0.0d0
    xi = 0.0d0
    itmax = 1000
    xm1 = 1.0d20
    ym1 = 1.0d20
!
    xi = xi* normx
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, zimat,&
                ' ', phenom, 1, 'X ', xi,&
                1, kfonc, yi, codres, 1)
    call cdnfon(zimat, kfonc, xi, 1, dyi,&
                ier)
    yi = yi/ normy
    xi = xi/ normx
    dyi = dyi * normx / normy
!
    do 20, i = 1,itmax
    xm2 = xm1
    ym2 = ym1
    xm1 = xi
    ym1 = yi
    dym1 = dyi
!
    rp = (xm2 - xm1)*(xm2 - xm1) + (ym2 - ym1)*(ym2 - ym1)
    res = sqrt(rp*rp)
!
    if (res .lt. tol) goto 30
!
    rp = dym1/(dym1*dym1 + 1.0d0)
    xi = rp*(y0 - ym1 + dym1*xm1) + x0/(dym1*dym1 + 1.0d0)
    xi = xi* normx
!
    call rcvalb(fami, kpg, spt, poum, zimat,&
                ' ', phenom, 1, 'X ', xi,&
                1, kfonc, yi, codres, 1)
    call cdnfon(zimat, kfonc, xi, 1, dyi,&
                ier)
!
    yi = yi/ normy
    xi = xi/ normx
    dyi = dyi * normx / normy
!
    20 end do
!
30  continue
!
    rp = (xm1 - x0)*(xm1 - x0)
    rp = rp + (ym1 - y0)*(ym1 - y0)
    distfo = sqrt(rp)
!
end function
