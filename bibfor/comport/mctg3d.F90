subroutine mctg3d(stress, strain, rprops, dsidep, edge,&
                  right, apex, codret)
!
    implicit none
! Declaration of real type variables

! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
    real(kind=8) :: dsidep(6,6)
    real(kind=8) :: edge
    real(kind=8) :: right
    real(kind=8) :: apex
    integer :: codret
!
#include "asterfort/jacobi.h"
#include "asterfort/lceqvn.h"
#include "asterfort/mcordo.h"
#include "asterfort/mctanp.h"
#include "asterfort/mctgep.h"
#include "asterfort/vecini.h"
!
! Declaration of integer type variables
    integer :: itri, iorder, mmax, nmax, mxiter, i, j, itjac1
!
! Declaration of integer type variables
    logical :: epflag
!
    parameter&
     &(   mmax=3     ,nmax=6     )
!
    real(kind=8) :: dpstrs(mmax,mmax), pstra(mmax), pstrs(mmax), r0, r1, r2, r3, r4
    real(kind=8) :: eigprj(mmax,mmax), eigxpr(mmax,mmax)
    real(kind=8) :: small, vaux(mmax), sqr, tol, tu(nmax), tr(nmax), t1(nmax)
!
! Declaration of constant variables
    data&
     &    r0   ,r1   ,r2   ,r3   ,r4   ,small ,tol   ,sqr   /&
     &    0.0d0,1.0d0,2.0d0,3.0d0,4.0d0,1.d-06,1.d-10,&
     &    1.4142135623730951d0                              /
    data&
     &    mxiter / 50 /
!
! Declaration of Common space variables
    common / debug / epflag
!***********************************************************************
!
!   COMPUTE THE CONSISTENT TANGENT MATRIX FOR MOHR-COULOMB LAW
!   IN THE 3 DIMENSIONNAL CASE
!
! ----------------------------------------------------------------------
!
!     LOI DE COMPORTEMENT DE MOHR-COULOMB
! IN  STRESS  : VECTEUR CONTRAINTE REACTUALISEE
! IN  STRAIN  : VECTEUR DEFORMATION A LA PREDICTION ELASTIQUE
! IN  RPROPS  : PROPIETES MECANIQUES (3)
! IN  EDGE    : INDICATEUR D'ACTIVATION (1) DE 2 MECANISMES
! IN  RIGHT   : INDICATEUR DE PROJECTION A DROITE (1)
!               QUAND 2 MECANISMES SONT ACITFS
! IN  APEX    : INDICATEUR D'ACTIVATION (1) DE 3 MECANISMES
!
! OUT DSIDEP  : MATRICE TANGENTE COHERENTE REACTUALISEE
!
!***********************************************************************
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
    call jacobi(mmax, mxiter, tol, small, tr,&
                tu, eigxpr, pstra, vaux, itjac1,&
                itri, iorder)
!
! Compute Consistent Plastic Jacobian Matrix in the eigenbasis
! Inputs: PSTRA ,RPROPS,EDGE  ,RIGHT ,APEX
! Output: DPSTRS
! ------------------------------------------------------------------
    call mctanp(dpstrs, rprops, pstra, edge, right,&
                apex)
!
    if (epflag) then
        write(6,'(A,3(1X,E12.5))') '> AFTER MCTANP :: PSTRS =',(pstrs(&
        i),i=1,3)
        write(6,'(A,3(1X,E12.5))') 'PSTRA =',(pstra(i),i=1,3)
        write(6,'(A)')'> EIGPRJ='
        do i = 1, 3
            write(6,'(I3,3(1X,E12.5))')i,(eigprj(i,j),j=1,3)
        enddo
        write(6,'(A)')'> EIGXPR='
        do i = 1, 3
            write(6,'(I3,3(1X,E12.5))')i,(eigxpr(i,j),j=1,3)
        enddo
        write(6,'(A)')'> DPSTRS='
        do i = 1, 3
            write(6,'(I3,3(1X,E12.5))')i,(dpstrs(i,j),j=1,3)
        enddo
    endif
!
! Check for repeated eigenvalues of strain and re-ordering
    call mcordo(dpstrs, pstrs, pstra, eigxpr, edge,&
                apex, codret)
    if (codret .eq. 1) goto 999
!
    if (epflag) then
        write(6,'(A,3(1X,E12.5))') '> AFTER MCORDO :: PSTRA =',(pstra(&
        i) ,i=1,3)
        write(6,'(A,3(1X,E12.5))')'> PSTRS  =',(pstrs(i) ,i=1,3)
        write(6,'(A)')'> DPSTRS='
        do i = 1, 3
            write(6,'(I3,3(1X,E12.5))')i,(dpstrs(i,j),j=1,3)
        enddo
        write(6,'(A)')'> EIGXPR='
        do i = 1, 3
            write(6,'(I3,3(1X,E12.5))')i,(eigxpr(i,j),j=1,3)
        enddo
    endif
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
! Output:
!   DSIDEP : Consistent Plastic Jacobian Matrix in the cartesian basis
! --------------------------------------------------------------------
    call mctgep(dpstrs, dsidep, pstra, pstrs, tr,&
                eigxpr, edge, apex)
!
    if (epflag) then
        write(6,'(A)')'> AFTER MCTGEP :: DSDE ='
        do i = 1, 6
            write(6,'(6(1X,E12.5))')(dsidep(i,j),j=1,6)
        enddo
    endif
999 continue
end subroutine
