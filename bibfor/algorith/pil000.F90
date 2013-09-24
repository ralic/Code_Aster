subroutine pil000(typilo, compor, neps, tau, mat,&
                  vim, sigm, epsm, epsp, epsd,&
                  typmod, etamin, etamax, copilo)
!
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
!
    implicit none
#include "asterc/r8gaem.h"
#include "asterfort/pidegv.h"
#include "asterfort/pieigv.h"
#include "asterfort/piesgv.h"
#include "asterfort/utmess.h"
    character(len=8) :: typmod(*)
    character(len=16) :: compor(*), typilo
    integer :: neps, mat
    real(kind=8) :: tau, epsm(neps), epsd(neps), epsp(neps), etamin, etamax
    real(kind=8) :: vim(*), sigm(neps), copilo(2, 2)
!
! ----------------------------------------------------------------------
!     PILOTAGE PRED_ELAS : BRANCHEMENT SELON COMPORTEMENT
! ----------------------------------------------------------------------
! IN  TYPILO  TYPE DE PILOTAGE : 'PRED_ELAS' OU 'DEFORMATION'
! IN  NEPS    DIMENSION DES DEFORMATIONS
! IN  TAU     INCREMENT DE PILOTAGE
! IN  MAT     NATURE DU MATERIAU                             (PRED_ELAS)
! IN  VIM     VARIABLES INTERNES EN T-                       (PRED_ELAS)
! IN  SIGM    CONTRAINTES EN T- (SI NECESSAIRE)              (PRED_ELAS)
! IN  EPSM    CHAMP DE DEFORMATION EN T-
! IN  EPSP    INCREMENT FIXE
! IN  EPSD    INCREMENT PILOTE
! IN  ETAMIN  BORNE INF DU PILOTAGE (SI UTILE)               (PRED_ELAS)
! IN  ETAMAX  BORNE SUP DU PILOTAGE (SI UTILE)               (PRED_ELAS)
! OUT COPILO  COEFFICIENT DE PILOTAGE : F := A0+A1*ETA = TAU
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
! MODELISATION A GRADIENT DE VARIABLES INTERNES
    if (typmod(2) .eq. 'GRADVARI') then
!
!      PILOTAGE 'DEFORMATION'
        if (typilo .eq. 'DEFORMATION') then
            call pidegv(neps, tau, epsm, epsp, epsd,&
                        copilo)
!
!      PILOTAGE 'PRED_ELAS'
        else
            if (etamin .eq. -r8gaem() .or. etamax .eq. r8gaem()) then
                call utmess('F', 'MECANONLINE_60', sk=compor(1))
            endif
!
            if (compor(1) .eq. 'ENDO_SCALAIRE') then
                call piesgv(neps, tau, mat, vim, epsm,&
                            epsp, epsd, typmod(1), etamin, etamax,&
                            copilo)
!
            else if (compor(1).eq.'ENDO_ISOT_BETON') then
                call pieigv(neps, tau, mat, vim, epsm,&
                            epsp, epsd, typmod(1), etamin, etamax,&
                            copilo)
!
            else
                call utmess('F', 'MECANONLINE_59')
            endif
!
        endif
    else
        call utmess('F', 'MECANONLINE_61', sk=typmod(2))
    endif
!
end subroutine
