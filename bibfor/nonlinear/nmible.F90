subroutine nmible(modele, noma, defico, resoco, fonact,&
                  numins, niveau, numedd, sdstat, sdtime,&
                  sdimpr)
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
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mmbouc.h"
#include "asterfort/nmctcg.h"
#include "asterfort/nmimci.h"
    integer :: numins, niveau
    character(len=8) :: noma
    character(len=24) :: defico, resoco
    character(len=24) :: sdstat, sdtime, sdimpr
    character(len=24) :: modele, numedd
    integer :: fonact(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! GESTION DEBUT DE BOUCLE POINTS FIXES
!
! ----------------------------------------------------------------------
!
!
! LES ITERATIONS ONT LIEU ENTRE CETTE ROUTINE ET SA COUSINE
! (NMTBLE) QUI COMMUNIQUENT PAR LA VARIABLE NIVEAU
!
! I/O NIVEAU : INDICATEUR D'UTILISATION DE LA BOUCLE DE POINT FIXE
!                  0     ON N'UTILISE PAS CETTE BOUCLE
!                  3     BOUCLE GEOMETRIE
!                  2     BOUCLE SEUILS DE FROTTEMENT
!                  1     BOUCLE CONTRAINTES ACTIVES
! IN  NUMINS : NUMERO D'INSTANT
! IN  MODELE : NOM DU MODELE
! IN  NOMA   : NOM DU MAILLAGE
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  SDIMPR : SD AFFICHAGE
! IN  NUMEDD : NOM DU NUME_DDL
!
! ----------------------------------------------------------------------
!
    integer :: mmitgo, mmitca, mmitfr
    logical :: lboucf, lboucg, lboucc
    logical :: lappa, loptin
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    if (niveau .eq. 0) then
        goto 999
    endif
!
! --- PAS D'ACTIVATION DES OPTIONS *_INIT (FAIT DANS MMAPIN)
!
    loptin = .false.
!
! --- PAS D'APPARIEMENT AU PREMIER COUP (FAIT DANS MMAPIN)
!
    lappa = .true.
    call mmbouc(resoco, 'GEOM', 'READ', mmitgo)
    if (mmitgo .eq. 1) lappa = .false.
!
! --- INFOS SUR LES BOUCLES
!
    lboucf = isfonc(fonact,'BOUCLE_EXT_FROT')
    lboucg = isfonc(fonact,'BOUCLE_EXT_GEOM')
    lboucc = isfonc(fonact,'BOUCLE_EXT_CONT')
!
! --- NIVEAU: 3   BOUCLE GEOMETRIE
!
    if (niveau .ge. 3) then
!
! --- ECRITURE NUMERO ITERATION
!
        call nmimci(sdimpr, 'BOUC_GEOM', mmitgo, .true.)
!
! --- NOUVELLE ITERATION DE GEOMETRIE
!
        if (lboucg) then
            niveau = 3
            if (lappa) then
                call nmctcg(modele, noma, defico, resoco, loptin,&
                            sdstat, sdtime, numedd)
            endif
        endif
!
! --- PREMIERE ITERATION DE FROTTEMENT
!
        call mmbouc(resoco, 'FROT', 'INIT', mmitfr)
        call mmbouc(resoco, 'FROT', 'INCR', mmitfr)
        call nmimci(sdimpr, 'BOUC_FROT', mmitfr, .true.)
    endif
!
! --- NIVEAU: 2   BOUCLE SEUILS DE FROTTEMENT
!
    if (niveau .ge. 2) then
!
! --- NOUVELLE ITERATION DE FROTTEMENT
!
        if (lboucf) then
            niveau = 2
        endif
!
! --- PREMIERE ITERATION DE CONTACT
!
        call mmbouc(resoco, 'CONT', 'INIT', mmitca)
        call mmbouc(resoco, 'CONT', 'INCR', mmitca)
        call nmimci(sdimpr, 'BOUC_CONT', mmitca, .true.)
    endif
!
! --- NIVEAU: 1   BOUCLE CONTACT
!
    if (niveau .ge. 1) then
!
! --- NOUVELLE ITERATION DE CONTACT
!
        if (lboucc) then
            niveau = 1
        endif
    endif
!
999 continue
!
    call jedema()
end subroutine
