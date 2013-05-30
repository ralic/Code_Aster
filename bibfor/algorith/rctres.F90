subroutine rctres(sigm, tresca)
    implicit   none
    include 'asterfort/lcqeqv.h'
    include 'asterfort/rcjaco.h'
    real(kind=8) :: sigm(*), tresca
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
!     CALCUL DU TRESCA
! ----------------------------------------------------------------------
!     IN    T       TENSEUR CONTRAINTE
!     OUT   TRESCA  LE TRESCA
! ----------------------------------------------------------------------
    real(kind=8) :: tr(6), tu(6), nul(6)
    real(kind=8) :: equi(3)
    integer :: nt, nd
    common /tdim/  nt, nd
    data   nul     /6*0.d0/
! ----------------------------------------------------------------------
    nt = 6
    nd = 3
!
    tu(1) = 1.d0
    tu(2) = 0.d0
    tu(3) = 0.d0
    tu(4) = 1.d0
    tu(5) = 0.d0
    tu(6) = 1.d0
!
! --- MATRICE TR = (XX XY XZ YY YZ ZZ) (POUR JACOBI)
!
    tr(1) = sigm(1)
    tr(2) = sigm(4)
    tr(3) = sigm(5)
    tr(4) = sigm(2)
    tr(5) = sigm(6)
    tr(6) = sigm(3)
!
    if (lcqeqv(tr,nul) .eq. 'OUI') then
        tresca = 0.d0
    else
        call rcjaco(tr, tu, equi)
! ------ TRESCA = MAX DIFF VALEURS PRINCIPALES
        tresca = max ( abs(equi(1)-equi(2)), abs(equi(1)-equi(3)), abs(equi(2)-equi(3)) )
    endif
!
end subroutine
