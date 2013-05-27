subroutine epdcp(tc, td, sigi, epsi)
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
!
! FONCTION : CALCUL DE LA DEFORMATION DANS LA DIRECTION DE LA PLUS
!     GRANDE CONTRAINTE EQUIVALENTES
! ----------------------------------------------------------------------
!     IN     TC    TENSEUR DES CONTRAINTE  (XX YY ZZ XY XZ YZ)
!            TD   TENSEUR DES DEFORMATIONS (XX YY ZZ XY XZ YZ)
!            NDIM  DIMENSION ESPACE 3 OU 2
!     OUT    SIGI  LA PLUS GRANDE CONTRAINTE EQUIVALENTE
!            EPSI  DEFORMATION DANS LA DIRECTION DE LA PLUS
!                  GRANDE CONTRAINTE EQUIVALENTES
!
!
! ----------------------------------------------------------------------
    implicit none
    include 'asterfort/jacobi.h'
    real(kind=8) :: tc(6), td(6), tr(6), tu(6), vecp(3, 3), vecint(3)
    real(kind=8) :: equi(3), sigi, epsi, jacaux(3), t(3, 3)
!
    real(kind=8) :: tol, toldyn
    integer :: nbvec, nperm
    integer :: type, iordre
!-----------------------------------------------------------------------
    integer :: i, k, nitjac
!-----------------------------------------------------------------------
    data nperm,tol,toldyn/20,1.d-10,1.d-2/
! ----------------------------------------------------------------------
!
!
!       MATRICE TR = (XX XY XZ YY YZ ZZ) (POUR JACOBI)
!
    tr(1) = tc(1)
    tr(2) = tc(4)
    tr(3) = tc(5)
    tr(4) = tc(2)
    tr(5) = tc(6)
    tr(6) = tc(3)
!
!       MATRICE UNITE = (1 0 0 1 0 1) (POUR JACOBI)
!
    tu(1) = 1.d0
    tu(2) = 0.d0
    tu(3) = 0.d0
    tu(4) = 1.d0
    tu(5) = 0.d0
    tu(6) = 1.d0
!
!       MATRICE DES DEFORMATIONS PLASTIQUES
!
    t(1,1)=td(1)
    t(2,2)=td(2)
    t(3,3)=td(3)
    t(1,2)=td(4)
    t(2,1)=td(4)
    t(3,1)=td(5)
    t(1,3)=td(5)
    t(3,2)=td(6)
    t(2,3)=td(6)
!
    nbvec = 3
    type = 0
    iordre = 1
    call jacobi(nbvec, nperm, tol, toldyn, tr,&
                tu, vecp, equi(1), jacaux, nitjac,&
                type, iordre)
    if (equi(1) .gt. (0.d0)) then
        vecint(1) = 0.d0
        vecint(2) = 0.d0
        vecint(3) = 0.d0
        do 1 k = 1, 3
            vecint(1)=vecint(1)+t(1,k)*vecp(k,1)
            vecint(2)=vecint(2)+t(2,k)*vecp(k,1)
            vecint(3)=vecint(3)+t(3,k)*vecp(k,1)
 1      continue
        epsi = 0.d0
        do 3 i = 1, 3
            epsi=epsi+vecint(i)*vecp(i,1)
 3      continue
        sigi=equi(1)
    else
        epsi = 0.d0
        sigi=0.d0
    endif
!
end subroutine
