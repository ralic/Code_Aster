subroutine brseff(k0, mu0, e0s, e0d, sigeff)
!
!     ROUTINE ANCIENNEMENT NOMMEE SIGMA_EFF
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
    real(kind=8) :: e0d(6), sigeff(6)
    integer :: i
!
!       CLASSEMENT DES CONTRAINTES ET DES DEFORMATION:*
!      I=1,3 => SIGMA II
!      I=4   => SIGMA 12
!      I=5   => SIGMA 13
!      I=6   => SIGMA 23
!
!
!       TENSEUR DES CONTRAINTES EFFECTIVES DANS LA PATE (NIVEAU P1)
!
!       CONTRAINTE SPHÈRIQUE
!-----------------------------------------------------------------------
    real(kind=8) :: e0s, sigs, k0, mu0
!-----------------------------------------------------------------------
    sigs=k0*e0s
!     PARTIE DEVIATORIQUE , ATTENTION LES EOD SONT EN FAIT DES GAMMA ...
    do 10 i = 1, 3
        sigeff(i)=sigs+mu0*e0d(i)
10  end do
!     LES TROIS TERMES HORS DIAGONALES SONT ENCORE DES GAMMA...
    do 20 i = 4, 6
        sigeff(i)=mu0*e0d(i)
20  end do
!
end subroutine
