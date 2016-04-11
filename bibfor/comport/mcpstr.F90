subroutine mcpstr(stress, tridim, pstrs, eigprj, ii, &
                  jj, mm, codret)
! ----------------------------------------------------------------------
!
! OBJECT: COMPUTE EIGENVALUES OF STRESS AND RE-ORDER
!
! IN  STRESS  : CONTRAINTE
! IN  TRIDIM  : [LOGICAL] LA MODELISATION EST-ELLE 3D?
!
! OUT PSTRS   : CONTRAINTES PRINCIPALES (DIM=3)
! OUT EIGPRJ  : DIRECTIONS PRINCIPALES  (DIM=3xNDIM)
! OUT II      : INDICE DE LA CONTRAINTE PRINCIPALE MINEURE
! OUT JJ      : INDICE DE LA CONTRAINTE PRINCIPALE MAJEURE
! OUT MM      : INDICE DE LA CONTRAINTE PRINCIPALE INTERMEDIAIRE
! OUT CODRET  : CODE RETOUR
!               = | 0: OK
!                 | 1: NOOK
!
! ----------------------------------------------------------------------
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
    integer      :: codret
!
#include "asterf_types.h"
#include "asterfort/jacobi.h"
#include "asterfort/lceqvn.h"
#include "asterfort/vecini.h"
#include "asterfort/matini.h"
!
! Declaration of constant parameters
    integer      :: mmax, nmax, ndt, ndi
    parameter    (mmax=3, nmax=6)
!
! Declaration of real type variables
    real(kind=8) :: stress(nmax), vaux(mmax)
    real(kind=8) :: pstrs1, pstrs2, pstrs3
    real(kind=8) :: eigprj(mmax, mmax), pstrs(mmax)
    real(kind=8) :: r0, r1, r2, r3, r4, small, tol
    real(kind=8) :: tu(nmax), tr(nmax), t1(nmax)
!
! Declaration of integer type variables
    integer :: itri, iorder, ii, jj, mm
    integer :: mxiter, i, itjac1
!
! Declaration of integer type variables
    aster_logical :: tridim
!
! Declaration of constant variables
    data  r0   ,r1   ,r2   ,r3   ,r4   ,small ,tol   /&
     &    0.0d0,1.0d0,2.0d0,3.0d0,4.0d0,1.d-06,1.d-10/
    data  mxiter / 50 /
!
! Declaration of Common space variables
    common / tdim  / ndt, ndi
!
    codret=0
!
! Initialize unit matrix = (1 0 0 1 0 1) for Jacobi
    call vecini(nmax, r0, t1)
    t1(1) =r1
    t1(4) =r1
    t1(6) =r1
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
! Matrix  TR = (SIXX SIXY SIXZ SIYY SIYZ SIZZ) for Jacobi
! Produce EIGPRJ: Base Projection Matrix from initial base
!                 to principal directions base
!         PSTRS : principal stresses
    if (tridim) then
        tr(1)=stress(1)
        tr(2)=stress(4)
        tr(3)=stress(5)
        tr(4)=stress(2)
        tr(5)=stress(6)
        tr(6)=stress(3)
    else
        tr(1)=stress(1)
        tr(2)=stress(4)
        tr(3)=r0
        tr(4)=stress(2)
        tr(5)=r0
        tr(6)=r0
    endif
!
! Unit matrix = (1 0 0 1 0 1) for Jacobi
    call lceqvn(nmax, t1, tu)
!
    call matini(mmax, mmax, r0, eigprj)
!
    call jacobi(mmax, mxiter, tol, tol, tr,&
                tu, eigprj, pstrs, vaux, itjac1,&
                itri, iorder)
!
    if (.not.tridim) then
        eigprj(mmax,mmax)=r1
        pstrs(mmax)      =stress(3)
    endif
!
! Verification of the spectral components
! ----------------------------------------
!
! Identify minor (PSTRS1) and major (PSTRS3) principal stresses
! --ok!
    ii=1
    jj=1
    pstrs1=pstrs(ii)
    pstrs3=pstrs(jj)
    do i = 2, mmax
        if (pstrs(i) .ge. pstrs1) then
            ii=i
            pstrs1=pstrs(ii)
        endif
        if (pstrs(i) .lt. pstrs3) then
            jj=i
            pstrs3=pstrs(jj)
        endif
    end do
    if (ii .ne. 1 .and. jj .ne. 1) mm=1
    if (ii .ne. 2 .and. jj .ne. 2) mm=2
    if (ii .ne. 3 .and. jj .ne. 3) mm=3
    pstrs2=pstrs(mm)
!
end subroutine
