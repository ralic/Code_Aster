subroutine mmstac(indcoi, lvites, jeu, jeuvit, lambdc,&
                  coefac, indcon)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: indcoi, indcon
    logical :: lvites
    real(kind=8) :: jeu, jeuvit, lambdc, coefac
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - CONTRAINTES ACTIVES)
!
! STATUT DE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  INDCOI : INDICATEUR DE CONTACT INITIAL
!              - INDCOI = 0: PAS DE CONTACT
!              - INDCOI = 1: CONTACT
! IN  LVITES : .TRUE. SI FORMULATION EN VITESSE
! IN  JEU    : VALEUR DU JEU
! IN  JEUVIT : VALEUR DU GAP DES VITESSES NORMALES
! IN  LAMBDC : MULTIPLICATEUR DE CONTACT DU POINT DE CONTACT
! IN  COEFAC : COEFFICIENT D'AUGMENTATION DU CONTACT RHO_N
! OUT INDCON : INDICATEUR DE CONTACT FINAL
!              - INDCON = 0: PAS DE CONTACT
!              - INDCON = 1: CONTACT
!
!
!
!
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    indcon = indcoi
!
! --- VERIFICATION DU SIGNE DU MULTIPLICATEUR AUGMENTE
!
    if ((lambdc-coefac*jeu) .le. r8prem()) then
        indcon = 1
    else
        indcon = 0
    endif
!
! --- FORMULATION EN VITESSE
!
    if (lvites) then
        if ((indcoi.eq.0) .and. (jeuvit.le.0.d0)) then
            indcon = 0
        endif
    endif
!
    call jedema()
end subroutine
