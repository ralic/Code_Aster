subroutine lc0046(fami, kpg, ksp, ndim, imate,&
                  compor, crit, instam, instap, neps,&
                  epsm, deps, nsig, sigm, vim,&
                  option, angmas, sigp, vip, nwkin,&
                  wkin, typmod, icomp, nvi, ndsde,&
                  dsidep, nwkout, wkout, codret)

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
! aslint: disable=W1504
    implicit none
#include "asterfort/lcesgv.h"
#include "asterfort/utmess.h"
#include "asterfort/lcquma.h"
#include "asterfort/lcquga.h"

    integer :: imate, ndim, codret, kpg, ksp, neps, nsig, ndsde, nwkin, nwkout
    integer :: icomp, nvi
    real(kind=8) :: wkin(nwkin), wkout(nwkout), crit(*)
    real(kind=8) :: epsm(neps), deps(neps), sigp(nsig), vim(nvi), vip(nvi)
    real(kind=8) :: dsidep(ndsde), instam, instap, sigm(nsig), angmas(3)
    character(len=*) :: fami
    character(len=8) :: typmod(*)
    character(len=16) :: option, compor(*)
! ----------------------------------------------------------------------
!        BUT: LOI D'ENDOMMAGEMENT D'UN MATERIAU ELASTIQUE FRAGILE :
!                    RELATION : 'ENDO_SCALAIRE'
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------

!   FORMULATION NON-LOCALE A GRADIENT D'ENDOMMAGEMENT
    if (typmod(2) .eq. 'GRADVARI') then
!
        call lcesgv(fami, kpg, ksp, neps, typmod, option, imate, lcquma, lcquga, &
                    epsm, deps, vim, .false._1, nint(crit(1)), crit(3), &
                    sigp, vip, dsidep,codret)
!
!   ENDO_SCALAIRE N'EST PAS DISOPNIBLE POUR D'AUTRE FORMULATIONS
    else
!
        call utmess('F', 'COMPOR1_66')
!
    endif
!
end subroutine
