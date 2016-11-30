subroutine ratg2d(stress, strain, rprops, dsidep, ii, jj, mm, &
                  edge, apex, outofp)
!***********************************************************************
!
!   OBJECT:
!   COMPUTE THE CONSISTENT TANGENT MATRIX FOR MOHR-COULOMB LAW
!   IN THE 2 DIMENSIONNAL CASE
!
! ----------------------------------------------------------------------
!
!     LOI DE COMPORTEMENT DE RANKINE
!
! IN  STRESS  : VECTEUR CONTRAINTE REACTUALISEE
! IN  STRAIN  : VECTEUR DEFORMATION A LA PREDICTION ELASTIQUE
! IN  RPROPS  : PROPIETES MECANIQUES (3)
! IN  II      : COMPOSANTE DE LA CONTRAINTE PRINCIPALE MINEURE
! IN  JJ      : COMPOSANTE DE LA CONTRAINTE PRINCIPALE MAJEURE
! IN  MM      : COMPOSANTE DE LA CONTRAINTE PRINCIPALE INTERMEDIAIRE
! IN  EDGE    : INDICATEUR D'ACTIVATION (1) DE 2 MECANISMES
! IN  APEX    : INDICATEUR D'ACTIVATION (1) DE 3 MECANISMES
! IN  OUTOFP  : COMPOSANTE HORS PLAN = | TRUE :  D_PLAN
!                                      | FALSE:  C_PLAN
!
! OUT DSIDEP  : MATRICE TANGENTE COHERENTE REACTUALISEE
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
!
#include "asterf_types.h"
#include "asterfort/jacobi.h"
#include "asterfort/lceqvn.h"
#include "asterfort/matini.h"
#include "asterfort/ratanp.h"
#include "asterfort/mctge2.h"
#include "asterfort/mcpstr.h"
#include "asterfort/vecini.h"
!
    real(kind=8) :: stress(6), strain(6)
    real(kind=8) :: rprops(6), dsidep(6, 6)
    real(kind=8) :: edge, apex
    aster_logical :: outofp
!
! Declaration of integer type variables
    integer :: itri, iorder, mmax, nmax, mxiter
    integer :: ii, jj, mm, itjac1
!
! Declaration of integer type variables
    aster_logical :: epflag
!
    parameter (mmax=3, nmax=6)
!
! Declaration of vector and matrix type variables
    real(kind=8) :: dpstrs(mmax, mmax), pstra(mmax), pstrs(mmax)
    real(kind=8) :: eigprj(mmax, mmax), eigxpr(mmax, mmax)
    real(kind=8) :: small, vaux(mmax), tu(nmax), tr(nmax), t1(nmax)
    real(kind=8) :: r0, r1, r2, r3, r4, sqr, dmax1, tol, refe
!
! Declaration of constant variables
    data  r0   ,r1   ,r2   ,r3   ,r4   ,small ,tol   ,sqr   /&
     &    0.0d0,1.0d0,2.0d0,3.0d0,4.0d0,1.d-06,1.d-10,&
     &    1.4142135623730951d0                              /
    data  mxiter / 50 /
!
! Declaration of Common space variables
    common / debug / epflag
!
!     if (epflag) then
!        write(6,'(A)')'!'
!        write(6,'(A)')'!-----------------------------------------!'
!        write(6,'(A)')'!                                         !'
!        write(6,'(A)')'!              ENTER RATG2D               !'
!        write(6,'(A)')'!                                         !'
!        write(6,'(A)')'!-----------------------------------------!'
!        write(6,'(A)')'!'
!         write(6,'(2(A,E12.5))')'! * EDGE =',edge,'  APEX =',apex
!         write(6,'(A)')'!'
!     endif
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
    call matini(mmax, mmax, r0, eigprj)
! Matrix  TR = (SIXX SIXY SIYY) for Jacobi
! Produce EIGPRJ: Base Projection Matrix from initial base
!                 to principal directions base
!         PSTRS : principal stresses
    tr(1)=stress(1)
    tr(2)=stress(4)/sqr
    tr(3)=r0
    tr(4)=stress(2)
    tr(5)=r0
    tr(6)=r0
!
! Unit matrix = (1 0 0 1 0 1) for Jacobi
    call lceqvn(nmax, t1, tu)
!
    call jacobi(mmax, mxiter, tol, small, tr,&
                tu, eigprj, pstrs, vaux, itjac1,&
                itri, iorder)
!
    pstrs(3)         =stress(3)
    eigprj(mmax,mmax)=r1
!
!     if (epflag) then
!         write(6,'(A)')'! EIGEN VALUES AND VECTORS OF STRESS:'
!         write(6,'(A)')'!'
!         write(6,'(A,3(1X,E15.8))') '! * SIGM_PRINC (N) =',(pstrs(i),i=1,3)
! !
!         write(6,'(A)')'!'
!         write(6,'(A)')&
!         '!   DIRE_PRINC_1   |   DIRE_PRINC_2   |   DIRE_PRINC_3'
!         do i = 1, 3
!             write(6,'(3(1X,E15.8))')(eigprj(i,j),j=1,3)
!         enddo
! !
!         write(6,'(A)')'!'
!         write(6,'(A)')'! * EIGENBASIS : M_AB = N_A * N_B:'
!         write(6,'(A,A)')&
!         '!      M_11      |      M_22      |      M_33      ',&
!         '|      M_12      |      M_13      |      M_23'
!             do i = 1, 3
!                 write(6,'(6(1X,E15.8))')&
!                 (eigprj(i,j)*eigprj(i,j),j=1,3),&
!                 eigprj(i,1)*eigprj(i,2),&
!                 eigprj(i,1)*eigprj(i,3),&
!                 eigprj(i,2)*eigprj(i,3)
!             enddo
!             write(6,'(6(1X,E15.8))')&
!                 (eigprj(1,j)*eigprj(2,j),j=1,3),&
!                 eigprj(1,1)*eigprj(2,2),&
!                 eigprj(1,1)*eigprj(2,3),&
!                 eigprj(1,2)*eigprj(2,3),&
!                 (eigprj(1,j)*eigprj(3,j),j=1,3),&
!                 eigprj(1,1)*eigprj(3,2),&
!                 eigprj(1,1)*eigprj(3,3),&
!                 eigprj(1,2)*eigprj(3,3),&
!                 (eigprj(2,j)*eigprj(3,j),j=1,3),&
!                 eigprj(2,1)*eigprj(3,2),&
!                 eigprj(2,1)*eigprj(3,3),&
!                 eigprj(2,2)*eigprj(3,3)
!     endif
!
! Matrix  TR = (EPXX EPXY EPYY) for Jacobi
! Produce EIGXPR: Base Projection Matrix from initial base
!                 to principal directions base
!         PSTRA : principal strains
    tr(1)=strain(1)
    tr(2)=strain(4)
    tr(4)=strain(2)
    tr(3)=r0
    tr(5)=r0
    tr(6)=r0
!
! Unit matrix = (1 0 0 1 0 1) for Jacobi
    call lceqvn(nmax, t1, tu)
    call matini(mmax, mmax, r0, eigxpr)
!
    call jacobi(mmax, mxiter, tol, small, tr,&
                tu, eigxpr, pstra, vaux, itjac1,&
                itri, iorder)
!
    pstra(3)         =strain(3)
    eigxpr(mmax,mmax)=r1
!
!     if (epflag) then
!         write(6,'(A)')'!'
!         write(6,'(A)')'! EIGEN VALUES AND VECTORS OF STRAIN:'
!         write(6,'(A)')'!'
!         write(6,'(A,3(1X,E15.8))') '! * EPSI_PRINC =',(pstra(i),i=1,3)
! !
!         write(6,'(A)')'!'
!         write(6,'(A)')&
!         '!   DIRE_PRINC_1   |   DIRE_PRINC_2   |   DIRE_PRINC_3'
!         do i = 1, 3
!             write(6,'(3(1X,E15.8))')(eigxpr(i,j),j=1,3)
!         enddo
! !
!         write(6,'(A)')'!'
!         write(6,'(A)')'! * EIGENBASIS : M_AB = N_A * N_B:'
!         write(6,'(A,A)')&
!         '!      M_11      |      M_22      |      M_33      ',&
!         '|      M_12      |      M_13      |      M_23'
!             do i = 1, 3
!                 write(6,'(6(1X,E15.8))')&
!                 (eigxpr(i,j)*eigxpr(i,j),j=1,3),&
!                 eigxpr(i,1)*eigxpr(i,2),&
!                 eigxpr(i,1)*eigxpr(i,3),&
!                 eigxpr(i,2)*eigxpr(i,3)
!             enddo
!             write(6,'(6(1X,E15.8))')&
!                 (eigxpr(1,j)*eigxpr(2,j),j=1,3),&
!                 eigxpr(1,1)*eigxpr(2,2),&
!                 eigxpr(1,1)*eigxpr(2,3),&
!                 eigxpr(1,2)*eigxpr(2,3),&
!                 (eigxpr(1,j)*eigxpr(3,j),j=1,3),&
!                 eigxpr(1,1)*eigxpr(3,2),&
!                 eigxpr(1,1)*eigxpr(3,3),&
!                 eigxpr(1,2)*eigxpr(3,3),&
!                 (eigxpr(2,j)*eigxpr(3,j),j=1,3),&
!                 eigxpr(2,1)*eigxpr(3,2),&
!                 eigxpr(2,1)*eigxpr(3,3),&
!                 eigxpr(2,2)*eigxpr(3,3)
!     endif
!
! Compute Consistent Plastic Jacobian Matrix in the eigenbasis
! Inputs: PSTRA ,RPROPS,EDGE  ,RIGHT ,APEX
! Output: DPSTRS
! ------------------------------------------------------------------
    call ratanp(dpstrs, rprops, ii, jj, mm, edge, apex)
! Print derivative tensor
!     if (epflag) then
!         write(6,'(A)')'!'
!         write(6,'(A)')'!    TANGENT MATRIX IN EIGENBASIS    !'
!         write(6,'(A)')'!'
!         write(6,'(A)')'! * DPSTRS ='
!         do i = 1, 3
!             write(6,'(I3,3(1X,E15.8))')i,(dpstrs(i,j),j=1,3)
!         enddo
!     endif
!
! Check for repeated eigenvalues of strain
    refe=dmax1(abs(pstra(1)),abs(pstra(2)))*small
    if (abs(pstra(1)-pstra(2)) .lt. refe) then
        edge=r1
!
!         if (epflag) then
!             write(6,'(A)')'!(@_@)'
!             write(6,'(A)')'!(@_@)   REPEATED EIGENVALUES    (@_@)'
!             write(6,'(A)')'!(@_@)'
!         endif
    else
        edge=r0
    endif
!
! Inputs:
!
!   DPSTRS : DSIGMA/DEPSI in the eigenbasis
!   PSTRA  : Strain eigenvalues vector
!   PSTRS  : Stress eigenvalues vector
!   EIGPRJ : Eigen directions Matrix (3x3)
!   EDGE   : Indicator of projection to an edge
!
! Output:
!
!   DSIDEP : Consistent Plastic Jacobian Matrix in the cartesian basis
! --------------------------------------------------------------------
    call mctge2(dpstrs, dsidep, eigxpr, pstra, pstrs, edge, outofp)
!
!     if (epflag) then
!         write(6,'(A)')'!'
!         write(6,'(A)')'!    TANGENT MATRIX IN CARTESIAN BASIS    !'
!         write(6,'(A)')'!'
!         write(6,'(A)')'! * DSIDEP ='
!         do i = 1, 6
!             write(6,'(6(1X,E15.8))')(dsidep(i,j),j=1,6)
!         enddo
!     endif
!
end subroutine
