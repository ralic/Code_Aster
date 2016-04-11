subroutine mctg3d(stress, strain, rprops, dsidep, ii, jj, mm, &
                  edge, right, apex, codret)
!***********************************************************************
!
!   OBJECT:
!   COMPUTE THE CONSISTENT TANGENT MATRIX FOR MOHR-COULOMB LAW
!   IN THE 3 DIMENSIONNAL CASE
!
! ----------------------------------------------------------------------
!
!     LOI DE COMPORTEMENT DE MOHR-COULOMB
!
! IN  STRESS  : VECTEUR CONTRAINTE REACTUALISEE
! IN  STRAIN  : VECTEUR DEFORMATION A LA PREDICTION ELASTIQUE
! IN  RPROPS  : PROPIETES MECANIQUES (3)
! IN  EDGE    : INDICATEUR D'ACTIVATION (1) DE 2 MECANISMES
! IN  RIGHT   : INDICATEUR DE PROJECTION A DROITE (1)
!               QUAND 2 MECANISMES SONT ACITFS
! IN  APEX    : INDICATEUR D'ACTIVATION (1) DE 3 MECANISMES
! IN  II      : COMPOSANTE DE LA CONTRAINTE PRINCIPALE MINEURE
! IN  JJ      : COMPOSANTE DE LA CONTRAINTE PRINCIPALE MAJEURE
! IN  MM      : COMPOSANTE DE LA CONTRAINTE PRINCIPALE INTERMEDIAIRE
!
! OUT DSIDEP  : MATRICE TANGENTE COHERENTE REACTUALISEE
! OUT CODRET  : CODE RETOUR
!
!***********************************************************************
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    real(kind=8) :: stress(6)
    real(kind=8) :: strain(6)
    real(kind=8) :: rprops(6)
    real(kind=8) :: dsidep(6, 6)
    real(kind=8) :: edge
    real(kind=8) :: right
    real(kind=8) :: apex
    integer :: codret
!
#include "asterf_types.h"
#include "asterfort/jacobi.h"
#include "asterfort/lceqvn.h"
#include "asterfort/mcordo.h"
#include "asterfort/mctanp.h"
#include "asterfort/mctgep.h"
#include "asterfort/vecini.h"
!
! Declaration of integer type variables
    integer :: itri, iorder, mmax, nmax, mxiter, itjac1
    integer :: ii, jj, mm
!
! Declaration of integer type variables
!     aster_logical :: epflag
!
    parameter (   mmax=3     ,nmax=6     )
!
    real(kind=8) :: dpstrs(mmax, mmax), pstra(mmax), pstrs(mmax)
    real(kind=8) :: eigprj(mmax, mmax), eigxpr(mmax, mmax)
    real(kind=8) :: small, vaux(mmax), tu(nmax), tr(nmax), t1(nmax)
    real(kind=8) :: r0, r1, r2, r3, r4, sqr, tol
!
! Declaration of constant variables
    data  r0   ,r1   ,r2   ,r3   ,r4   ,small ,tol   ,sqr   /&
     &    0.0d0,1.0d0,2.0d0,3.0d0,4.0d0,1.d-06,1.d-10,&
     &    1.4142135623730951d0                              /
    data  mxiter / 50 /
!
! Spectral decomposition of the trial stress
!
! ITRI =  0 : TRI EN VALEUR RELATIVE
!         1 : TRI EN VALEUR ABSOLUE
!         2 : PAS DE TRI
    itri  =2
! IORDER =  0 : TRI PAR ORDRE CROISSANT
!           1 : TRI PAR ORDRE DECROISSANT
!           2 : PAS DE TRI
    iorder=2
!
! Initialize unit matrix = (1 0 0 1 0 1) for Jacobi
    call vecini(nmax, r0, t1)
    t1(1)=r1
    t1(4)=r1
    t1(6)=r1
!
! Matrix  TR = (SIXX SIXY SIXZ SIYY SIYZ SIZZ) for Jacobi
! Produce EIGPRJ: Base Projection Matrix from initial base
!                 to principal directions base
!         PSTRS : principal stresses
    tr(1)=stress(1)
    tr(2)=stress(4)/sqr
    tr(3)=stress(5)/sqr
    tr(4)=stress(2)
    tr(5)=stress(6)/sqr
    tr(6)=stress(3)
! Unit matrix = (1 0 0 1 0 1) for Jacobi
    call lceqvn(nmax, t1, tu)
!
    call jacobi(mmax, mxiter, tol, small, tr,&
                tu, eigprj, pstrs, vaux, itjac1,&
                itri, iorder)
!
! Matrix  TR = (EPXX EPXY EPXZ EPYY EPYZ EPZZ) for Jacobi
! Produce EIGXPR: Base Projection Matrix from initial base
!                 to principal directions base
!         PSTRA : principal strains
    tr(1)=strain(1)
    tr(2)=strain(4)
    tr(3)=strain(5)
    tr(4)=strain(2)
    tr(5)=strain(6)
    tr(6)=strain(3)
! Unit matrix = (1 0 0 1 0 1) for Jacobi
    call lceqvn(nmax, t1, tu)
!
    call jacobi(mmax, mxiter, tol, small, tr, &
                tu, eigxpr, pstra, vaux, itjac1, &
                itri, iorder)
!
! Compute Consistent Plastic Jacobian Matrix in the eigenbasis
! Inputs: PSTRA ,RPROPS,EDGE  ,RIGHT ,APEX
! Output: DPSTRS
! ------------------------------------------------------------------
    call mctanp(dpstrs, rprops, ii, jj, mm, edge, right, apex)
!
! Print derivative tensor
!
! Check for repeated eigenvalues of strain and re-ordering
    call mcordo(dpstrs, pstrs, pstra, eigxpr, edge, apex, codret)
!
    if (codret .eq. 1) goto 999
!
! Input Elastic Stiffness Vector in the cartesian base
! -------------------------------------------------------
    tr(1)=strain(1)
    tr(4)=strain(4)
    tr(5)=strain(5)
    tr(2)=strain(2)
    tr(6)=strain(6)
    tr(3)=strain(3)
! Inputs:
!   DPSTRS : DSIGMA/DEPSI in the eigenbasis
!   PSTRA  : Strain eigenvalues vector
!   PSTRS  : Stress eigenvalues vector
!   TR     : Total Predictor Strain vector
!   EIGPRJ : Eigen directions Matrix (3x3)
!   EDGE   : Indicator of projection to an edge
!   APEX   : Indicator of projection to the apex
!
! Output:
!   DSIDEP : Consistent Plastic Jacobian Matrix in the cartesian basis
! --------------------------------------------------------------------
    call mctgep(dpstrs, dsidep, pstra, pstrs, tr, eigxpr, edge, apex)
!
999 continue
end subroutine
