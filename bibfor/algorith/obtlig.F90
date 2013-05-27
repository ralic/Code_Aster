subroutine obtlig(sdtabl, sepcol, ligne)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    include       'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/obgeti.h'
    include 'asterfort/obgeto.h'
    include 'asterfort/oblgai.h'
    include 'asterfort/oblgoi.h'
    character(len=24) :: sdtabl
    character(len=1) :: sepcol
    character(len=255) :: ligne
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (GESTION STRUCTS - TABLEAU POUR IMPRESSION)
!
! CREATION D'UNE LIGNE VIDE AVEC LES SEPARATEURS DE COLONNES
!
! ----------------------------------------------------------------------
!
!
! IN  SDTABL : STRUCT TABLEAU POUR IMPRESSION
! IN  SEPCOL : SEPARATEUR COLONNE
! OUT LIGNE  : LIGNE
!
! ----------------------------------------------------------------------
!
    character(len=24) :: slcolo, sdcolo
    integer :: icolo, nbcolo
    integer :: larcol, larlig, larcum
    logical :: lacti
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    ligne = ' '
!
! --- LISTE DES COLONNES DISPONIBLES
!
    call obgeto(sdtabl, 'COLONNES_DISPOS', slcolo)
!
! --- NOMBRE DE COLONNES TOTAL
!
    call obgeti(slcolo, 'NBRE_STRUCTS', nbcolo)
!
! --- LARGEUR D'UNE LIGNE
!
    call obgeti(sdtabl, 'LARGEUR_LIGNE', larlig)
!
! --- MESURE LARGEUR
!
    ligne(1:1) = sepcol
    larcum = 1
    do 10 icolo = 1, nbcolo
        call oblgoi(slcolo, icolo, sdcolo)
        call oblgai(slcolo, icolo, lacti)
        if (lacti) then
            call obgeti(sdcolo, 'LARGEUR', larcol)
            larcum = larcum + larcol + 1
            ligne(larcum:larcum) = sepcol
        endif
10  end do
!
! --- VERIF
!
    call assert(larcum.eq.larlig)
!
    call jedema()
!
end subroutine
