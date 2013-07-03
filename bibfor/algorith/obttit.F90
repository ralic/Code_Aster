subroutine obttit(sdtabl, sepcol, entet1, entet2, entet3)
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
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/obgeti.h"
#include "asterfort/obgetk.h"
#include "asterfort/obgeto.h"
#include "asterfort/oblgai.h"
#include "asterfort/oblgoi.h"
#include "asterfort/obtlig.h"
    character(len=24) :: sdtabl
    character(len=1) :: sepcol
    character(len=255) :: entet1, entet2, entet3
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (GESTION STRUCTS - TABLEAU POUR IMPRESSION)
!
! CREATION DE L'ENTETE DU TABLEAU
!
! ----------------------------------------------------------------------
!
!
! IN  SDTABL : STRUCT TABLEAU POUR IMPRESSION
! IN  SEPCOL : SEPARATEUR COLONNE
! OUT ENTET1 : LIGNE 1 DE L'ENTETE
! OUT ENTET2 : LIGNE 2 DE L'ENTETE
! OUT ENTET3 : LIGNE 3 DE L'ENTETE
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=24) :: slcolo, sdcolo
    integer :: icol, ncol, pos
    integer :: larcol, larlig, tithau
    logical :: lacti
    character(len=16) :: titli1, titli2, titli3
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
! --- INITIALISATIONS
!
    entet1 = ' '
    entet2 = ' '
    entet3 = ' '
!
! --- HAUTEUR TITRE
!
    call obgeti(sdtabl, 'HAUTEUR_TITRE', tithau)
    call assert((tithau.gt.0).and.(tithau.le.3))
!
! --- LISTE DES COLONNES DISPONIBLES
!
    call obgeto(sdtabl, 'COLONNES_DISPOS', slcolo)
!
! --- NOMBRE DE COLONNES
!
    call obgeti(slcolo, 'NBRE_STRUCTS', ncol)
!
! --- LARGEUR D'UNE LIGNE
!
    call obgeti(sdtabl, 'LARGEUR_LIGNE', larlig)
    call assert(larlig.le.255)
!
! --- LIGNES VIDES AVEC LES SEPARATEURS DE COLONNES
!
    call obtlig(sdtabl, sepcol, entet1)
    if (tithau .ge. 2) call obtlig(sdtabl, sepcol, entet2)
    if (tithau .eq. 3) call obtlig(sdtabl, sepcol, entet3)
!
! --- TITRE DES COLONNES
!
    pos = 2
    do 10 icol = 1, ncol
        call oblgoi(slcolo, icol, sdcolo)
        call oblgai(slcolo, icol, lacti)
        if (lacti) then
            call obgeti(sdcolo, 'LARGEUR', larcol)
            call obgetk(sdcolo, 'TITRE_LIGN_1', titli1)
            if (tithau .ge. 2) call obgetk(sdcolo, 'TITRE_LIGN_2', titli2)
            if (tithau .eq. 3) call obgetk(sdcolo, 'TITRE_LIGN_3', titli3)
            entet1(pos:pos+larcol-1) = titli1
            if (tithau .ge. 2) entet2(pos:pos+larcol-1) = titli2
            if (tithau .eq. 3) entet3(pos:pos+larcol-1) = titli3
            pos = pos+larcol+1
        endif
10  end do
!
    call jedema()
!
end subroutine
