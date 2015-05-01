subroutine nmimin(sdimpr, fonact, sddisc, sdsuiv, numins)
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
    implicit     none
#include "asterfort/nmimac.h"
#include "asterfort/nmimpa.h"
#include "asterfort/nmimpt.h"
#include "asterfort/nmimpx.h"
    character(len=24) :: sdimpr, sdsuiv
    integer :: fonact(*)
    character(len=19) :: sddisc
    integer :: numins
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE)
!
! INITIALISATION DES IMPRESSIONS
!
! ----------------------------------------------------------------------
!
!
! IN  SDIMPR : SD AFFICHAGE
! IN  SDSUIV : SD SUIVI_DDL
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  NUMINS : NUMERO INSTANT COURANT
!
! ----------------------------------------------------------------------
!
!
! --- ACTIVATION DES COLONNES
!
    call nmimac(sdimpr, sdsuiv, fonact)
!
! --- DOIT-ON ACTIVER L'AFFICHAGE POUR CE PAS DE TEMPS ?
!
    call nmimpa(numins, sdimpr)
!
! --- IMPRESSION LIGNE DE SEPARATION
!
    call nmimpx(sdimpr)
!
! --- IMPRESSION ENTETE
!
    call nmimpt(numins, sddisc, sdimpr)
!
end subroutine
