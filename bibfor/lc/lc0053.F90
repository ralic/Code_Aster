subroutine lc0053(fami, kpg, ksp, ndim, imate,&
                  compor, crit, instam, instap, epsm,&
                  deps, sigm, vim, option, angmas,&
                  sigp, vip, tampon, typmod, icomp,&
                  nvi, dsidep, codret)
!
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
!
! aslint: disable=W1504
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/lckimp.h'
    integer :: imate, ndim, codret, kpg, ksp
    integer :: icomp, nvi
    real(kind=8) :: tampon(*), crit(*)
    real(kind=8) :: epsm(*), deps(*), sigp(*), vim(*), vip(*), dsidep(*)
    real(kind=8) :: instam, instap, sigm(*), angmas(*)
    character(len=*) :: fami
    character(len=8) :: typmod(*),mod
    character(len=16) :: option, compor(*)
! ---------------------------------------------------------------------
!        BUT: LOI D'ENDOMMAGEMENT D'UN MATERIAU ELASTIQUE FRAGILE :
!                    RELATION : 'ENDO_CARRE'
! ---------------------------------------------------------------------
!
!     IN      NDIM    DIMENSION DE L ESPACE (3D=3,2D=2,1D=1)
!             TYPMOD  TYPE DE MODELISATION
!             OPTION  OPTION DE CALCUL A FAIRE
!                           'RIGI_MECA_TANG'> DSIDEP(T)
!                           'FULL_MECA'     > DSIDEP(T+DT) , SIG(T+DT)
!                           'RAPH_MECA'     > SIG(T+DT)
!             IMATE   ADRESSE DU MATERIAU CODE
!             EPSM    DEFORMATION TOTALE A T
!             DEPS    INCREMENT DE DEFORMATION TOTALE
!             VIM     VARIABLES INTERNES A T + INDICATEURS ETAT T
!  ATTENTION  VIM     VARIABLES INTERNES A T MODIFIEES SI REDECOUPAGE
!     OUT     SIGP    CONTRAINTE A T+DT
!             VIP     VARIABLES INTERNES A T+DT + INDICATEURS ETAT T+DT
!             DSIDEP  MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
!
! ---------------------------------------------------------------------
!
!     FORMULATION NON-LOCALE A GRADIENT D'ENDOMMAGEMENT AUX NOEUDS
    if (typmod(2) .eq. 'GDVARINO') then
        mod=typmod(1)
        call lckimp(ndim, mod, option, imate, epsm,&
                    deps, vim, tampon, sigp, vip,&
                    dsidep)
!
    else
        call assert(.false.)
    endif
!
end subroutine
