subroutine nmdiri(modele, mate, carele, lischa, sddyna,&
                  depl, vite, acce, vediri)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/ndynin.h"
#include "asterfort/ndynlo.h"
#include "asterfort/vebtla.h"
    character(len=19) :: lischa
    character(len=24) :: modele, mate, carele
    character(len=19) :: vediri, sddyna
    character(len=19) :: depl, vite, acce
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! CALCUL DES VECT_ELEM POUR LES REACTIONS D'APPUI BT.LAMBDA
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE
! IN  MATE   : NOM DU CHAMP DE MATERIAU
! IN  LISCHA : LISTE DES CHARGES
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  SDDYNA : SD DYNAMIQUE
! OUT VEDIRI : VECT_ELEM DES REACTIONS D'APPUI BT.LAMBDA
!
!
!
!
    aster_logical :: lstat, ldepl, lvite, lacce
    character(len=19) :: veclag
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- FONCTIONNALITES ACTIVEES
!
    lstat = ndynlo(sddyna,'STATIQUE')
    if (lstat) then
        ldepl = .true.
        lvite = .false.
        lacce = .false.
    else
        ldepl = ndynin(sddyna,'FORMUL_DYNAMIQUE').eq.1
        lvite = ndynin(sddyna,'FORMUL_DYNAMIQUE').eq.2
        lacce = ndynin(sddyna,'FORMUL_DYNAMIQUE').eq.3
    endif
!
! --- QUEL VECTEUR D'INCONNUES PORTE LES LAGRANGES ?
!
    if (ldepl) then
        veclag = depl
    else if (lvite) then
        veclag = vite
!       VILAINE GLUTE POUR L'INSTANT
        veclag = depl
    else if (lacce) then
        veclag = acce
    else
        ASSERT(.false.)
    endif
!
! --- CALCUL DES VECT_ELEM POUR LES REACTIONS D'APPUI BT.LAMBDA
!
    call vebtla('V', modele, mate, carele, veclag,&
                lischa, vediri)
!
    call jedema()
end subroutine
