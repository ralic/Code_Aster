subroutine obtcla(sdtabl)
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
    include       'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/obgeti.h'
    include 'asterfort/obgeto.h'
    include 'asterfort/oblgai.h'
    include 'asterfort/oblgoi.h'
    include 'asterfort/obseti.h'
    include 'asterfort/u2mesi.h'
    character(len=24) :: sdtabl
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (GESTION STRUCTS - TABLEAU POUR IMPRESSION)
!
! CALCUL DE LA LARGEUR D'UNE LIGNE
!
! ----------------------------------------------------------------------
!
!
! IN  SDTABL : STRUCT TABLEAU POUR IMPRESSION
!
! ----------------------------------------------------------------------
!
    character(len=24) :: slcolo, sdcolo
    integer :: icolo, nbcolo
    integer :: nbacti, larcol, larlig
    logical :: lacti
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    larlig = 1
!
! --- LISTE DES COLONNES DISPONIBLES
!
    call obgeto(sdtabl, 'COLONNES_DISPOS', slcolo)
!
! --- NOMBRE DE COLONNES ACTIVES
!
    call obgeti(slcolo, 'NBRE_ACTI', nbacti)
    if (nbacti .ge. 15) call u2mesi('F', 'IMPRESSION_1', 1, nbacti)
!
! --- NOMBRE DE COLONNES TOTAL
!
    call obgeti(slcolo, 'NBRE_STRUCTS', nbcolo)
!
! --- MESURE LARGEUR
!
    larlig = 1
    do 10 icolo = 1, nbcolo
        call oblgoi(slcolo, icolo, sdcolo)
        call oblgai(slcolo, icolo, lacti)
        if (lacti) then
            call obgeti(sdcolo, 'LARGEUR', larcol)
            larlig = larlig + (larcol+1)
        endif
10  end do
!
! --- SAUVEGARDE LARGEUR LIGNE
!
    call obseti(sdtabl, 'LARGEUR_LIGNE', larlig)
    if (larlig .gt. 255) call u2mesi('F', 'IMPRESSION_2', 1, larlig)
!
    call jedema()
end subroutine
