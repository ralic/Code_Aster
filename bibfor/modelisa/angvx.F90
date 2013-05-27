subroutine angvx(gx, alpha, beta)
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
!       CALCUL DE 2 ANGLES NAUTIQUES A PARTIR DU VECTEUR GX
!       IN      GX
!       OUT     ALPHA , BETA
!       ----------------------------------------------------------------
    include 'asterc/r8miem.h'
    real(kind=8) :: gx(3), alpha, beta, p, tst
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    tst = r8miem()
!
    if (abs(gx(2)) .le. tst .and. abs(gx(1)) .le. tst) then
        alpha = 0.d0
    else
        alpha = atan2(gx(2),gx(1))
    endif
    p = sqrt( gx(1)*gx(1) + gx(2)*gx(2) )
    if (abs(gx(3)) .le. tst .and. abs(p) .le. tst) then
        beta = 0.d0
    else
        beta = - atan2(gx(3),p)
    endif
end subroutine
