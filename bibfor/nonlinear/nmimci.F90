subroutine nmimci(sdimpr, typcoz, vali, laffe)
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
    implicit      none
#include "asterfort/impsdr.h"
#include "asterfort/obgeto.h"
#include "asterfort/oblgop.h"
#include "asterfort/obsetb.h"
    character(len=24) :: sdimpr
    character(len=*) :: typcoz
    integer :: vali
    logical(kind=1) :: laffe
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - IMPRESSION)
!
! ENREGISTRE LES DONNEES REELLES DANS LE TABLEAU DE CONVERGENCE
!
! ----------------------------------------------------------------------
!
!
! IN  SDIMPR : SD AFFICHAGE
! IN  TYPCOL : CODE TYPE DE LA COLONNE
! IN  VALI   : VALEUR
! IN  LAFFE  : .TRUE. SI LA VALEUR EST AFFECTEE
!
! ----------------------------------------------------------------------
!
    character(len=24) :: sdtabc, slcolo, sdcolo
    character(len=16) :: k16bid
    real(kind=8) :: r8bid
!
! ----------------------------------------------------------------------
!
!
!
! --- RECUPERATION DU TABLEAU DE CONVERGENCE
!
    call obgeto(sdimpr, 'TABLEAU_CONV', sdtabc)
!
! --- LISTE DES COLONNES
!
    call obgeto(sdtabc, 'COLONNES_DISPOS', slcolo)
!
! --- COLONNE CORRESPONDANTE
!
    call oblgop(slcolo, typcoz, sdcolo)
!
! --- AFFECTATION DANS LE TABLEAU
!
    call impsdr(sdcolo, k16bid, r8bid, vali)
!
! --- VALIDATION DE LA VALEUR
!
    call obsetb(sdcolo, 'VALE_AFFE', laffe)
!
end subroutine
