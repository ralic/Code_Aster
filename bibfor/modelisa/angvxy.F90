subroutine angvxy(gx, gn, angl)
    implicit none
!       ----------------------------------------------------------------
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
! ======================================================================
!       ----------------------------------------------------------------
!       CALCUL DES 3 ANGLES NAUTIQUES A PARTIR DU VECTEUR GX
!       ET D UN VECTEUR GN DONT LA PROJECTION NORMALE SUR LE PLAN NORMAL
!       A GX DONNE LE VECTEUR GY
!       IN      GX , GN
!       OUT     ALPHA , BETA , GAMMA
!       ----------------------------------------------------------------
    include 'asterc/r8miem.h'
    include 'asterfort/angvx.h'
    include 'asterfort/matrot.h'
    include 'asterfort/pmavec.h'
    real(kind=8) :: mro(3, 3), gx(3), gy(3), gn(3), angl(*)
!
!-----------------------------------------------------------------------
    real(kind=8) :: alpha, beta, tst
!-----------------------------------------------------------------------
    tst = r8miem()
!
    call angvx(gx, alpha, beta)
    angl(1) = alpha
    angl(2) = beta
    angl(3) = 0.d0
    call matrot(angl, mro)
    call pmavec('ZERO', 3, mro, gn, gy)
    if (abs(gy(3)) .le. tst .and. abs(gy(2)) .le. tst) then
        angl(3) = 0.d0
    else
        angl(3) = atan2(gy(3),gy(2))
    endif
!
end subroutine
