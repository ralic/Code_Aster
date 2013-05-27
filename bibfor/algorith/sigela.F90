subroutine sigela(typmod, ndim, e, nu, epse,&
                  sigel)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
    include 'asterfort/bptobg.h'
    include 'asterfort/jacobi.h'
    include 'asterfort/r8inir.h'
    character(len=8) :: typmod(1)
    integer :: ndim
    real(kind=8) :: epse(6), e, nu
    real(kind=8) :: sigel(6)
! ----------------------------------------------------------------------
!  CALCUL DES CONTRAINTES ELASTIQUES A PARTIR DES DEFORMATIONS
!   ELASTIQUES POUR UN MATERIAU ISOTROPE
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  EPSE    : DEFORMATIONS ELASTIQUES
! IN  E       : MODULE ELASTIQUE DE YOUNG
! IN  NU      : COEFFICIENT DE POISSON
! IN TYPMOD   : TYPE DE MODELISATION (C_PLAN...)
!
! OUT SIGEL   : CONTRAINTES ELASTIQUES
! ----------------------------------------------------------------------
!
    integer :: ndimsi, nperm, nitjac, trij, ordrej, k
    real(kind=8) :: epsep(3), vecpe(3, 3), sigelp(3)
    real(kind=8) :: tol, toldyn, tr(6), tu(6), jacaux(3)
    real(kind=8) :: rac2, coplan, lambda, deuxmu
!
! ======================================================================
!
!
!--------------------------------------------------------
!                            INITIALISATION
!--------------------------------------------------------
!
    ndimsi = 2*ndim
    rac2=sqrt(2.d0)
!
    lambda = e * nu / (1.d0+nu) / (1.d0 - 2.d0*nu)
    deuxmu = e/(1.d0+nu)
!
    if (typmod(1) .eq. 'C_PLAN  ') then
        coplan = - nu/(1.d0-nu)
        epse(3) = coplan * (epse(1)+epse(2))
    endif
!
    do 30 k = 4, ndimsi
        epse(k) = epse(k)/rac2
30  end do
!
!--------------------------------------------------------
!  -   ON PASSE DANS LE REPERE PROPRE DE EPS
!--------------------------------------------------------
!
    nperm = 12
    tol = 1.d-10
    toldyn = 1.d-2
!       MATRICE  TR = (XX XY XZ YY YZ ZZ) POUR JACOBI)
    tr(1) = epse(1)
    tr(2) = epse(4)
    tr(3) = epse(5)
    tr(4) = epse(2)
    tr(5) = epse(6)
    tr(6) = epse(3)
!     MATRICE UNITE = (1 0 0 1 0 1) (POUR JACOBI)
    tu(1) = 1.d0
    tu(2) = 0.d0
    tu(3) = 0.d0
    tu(4) = 1.d0
    tu(5) = 0.d0
    tu(6) = 1.d0
    trij = 2
    ordrej = 2
!
    call jacobi(3, nperm, tol, toldyn, tr,&
                tu, vecpe, epsep, jacaux, nitjac,&
                trij, ordrej)
!
!
!----------------------------------------------------------------
!     CALCUL DES CONTRAINTES ELASTIQUES (REPERE PRINCIPAL)
!----------------------------------------------------------------
    do 50 k = 1, 3
        sigelp(k) = lambda*(epsep(1)+epsep(2)+epsep(3))
50  continue
    do 60 k = 1, 3
        sigelp(k) = sigelp(k) + deuxmu*epsep(k)
60  continue
!
!------------------------------------------------------------------
!     ON PASSE DANS LE REPERE INITIAL LES CONTRAINTES ELASTIQUES
!------------------------------------------------------------------
    call r8inir(6, 0.d0, sigel, 1)
    tr(1) = sigelp(1)
    tr(2) = sigelp(2)
    tr(3) = sigelp(3)
    tr(4) = 0.d0
    tr(5) = 0.d0
    tr(6) = 0.d0
    call bptobg(tr, sigel, vecpe)
    do 90 k = 4, ndimsi
        sigel(k)=rac2*sigel(k)
90  continue
end subroutine
