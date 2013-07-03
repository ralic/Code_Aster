subroutine lc0005(fami, kpg, ksp, ndim, imate,&
                  compor, crit, instam, instap, epsm,&
                  deps, sigm, vim, option, angmas,&
                  sigp, vip, tampon, typmod, icomp,&
                  nvi, dsidep, codret)
! aslint: disable=W1504
    implicit none
#include "asterfort/fragex.h"
#include "asterfort/lcfrge.h"
#include "asterfort/lcfrlo.h"
    integer :: imate, ndim, ksp, kpg
    integer :: icomp, nvi
    integer :: codret
    real(kind=8) :: angmas(*)
    real(kind=8) :: tampon(*)
    character(len=16) :: compor(*), option
    character(len=8) :: typmod(*)
    character(len=*) :: fami
    real(kind=8) :: epsm(*), deps(*), crit(*)
    real(kind=8) :: sigp(*), sigm(*), instam, instap
    real(kind=8) :: vim(*), vip(*)
    real(kind=8) :: dsidep(*)
!
!
! ======================================================================
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
! person_in_charge: jean-michel.proix at edf.fr
! ======================================================================
!.......................................................................
!
!     BUT: LOI D'ENDOMMAGEMENT D'UN MATERIAU ELASTIQUE FRAGILE
!
!          RELATION : 'ENDO_FRAGILE'
!
!       IN      NDIM    DIMENSION DE L ESPACE (3D=3,2D=2,1D=1)
!               TYPMOD  TYPE DE MODELISATION
!               OPTION     OPTION DE CALCUL A FAIRE
!                             'RIGI_MECA_TANG'> DSIDEP(T)
!                             'FULL_MECA'     > DSIDEP(T+DT) , SIG(T+DT)
!                             'RAPH_MECA'     > SIG(T+DT)
!               IMATE    ADRESSE DU MATERIAU CODE
!               EPSM   DEFORMATION TOTALE A T
!               DEPS   INCREMENT DE DEFORMATION TOTALE
!               VIM    VARIABLES INTERNES A T    + INDICATEUR ETAT T
!    ATTENTION  VIM    VARIABLES INTERNES A T MODIFIEES SI REDECOUPAGE
!       OUT     SIGP    CONTRAINTE A T+DT
!               VIP    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
!               DSIDEP    MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
!
!
!
!     NORMALEMENT, LES VERIF ONT ETE FAITES AVANT POUR INTERDIRE
!     SIMO_MIEHE
!     EXPLICITE
!
!     FORMULATION NON-LOCALE AVEC REGULARISATION DES DEFORMATIONS
    if (typmod(2) .eq. 'GRADEPSI') then
!
        call lcfrge(ndim, typmod, imate, epsm, deps,&
                    vim, option, sigp, vip, dsidep,&
                    tampon)
!
!     FORMULATION LOCALE
    else
        if (crit(2) .ne. 9) then
            call lcfrlo(ndim, typmod, imate, epsm, deps,&
                        vim, option, sigp, vip, dsidep)
        else
            call fragex(ndim, imate, instam, instap, epsm,&
                        deps, vim, option, sigp, vip,&
                        typmod, dsidep, codret)
        endif
!
    endif
!
end subroutine
