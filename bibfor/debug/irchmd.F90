subroutine irchmd(ifichi, chanom, partie, nochmd, codret)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/irchme.h"
    character(len=19) :: chanom
    character(len=*) :: partie, nochmd
!
    integer :: ifichi, codret
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
! person_in_charge: nicolas.sellenet at edf.fr
!        UTILITAIRE D'IMPRESSION DU CHAMP CHANOM NOEUD/ELEMENT
!        ENTIER/REEL AU FORMAT MED
!     ENTREES:
!        IFICHI : UNITE LOGIQUE D'IMPRESSION DU CHAMP
!        CHANOM : NOM ASTER DU CHAM A ECRIRE
!        PARTIE : IMPRESSION DE LA PARTIE IMAGINAIRE OU REELLE POUR
!                  UN CHAMP COMPLEXE AU FORMAT CASTEM OU GMSH OU MED
!        NOCHMD : NOM DU CHAMP DANS LE FICHIER MED (CHOIX DE
!                  L'UTILISATEUR)
!     SORTIES:
!        CODRET : CODE DE RETOUR (0 : PAS DE PB, NON NUL SI PB)
!
    integer :: numord
!
    character(len=8) :: typech, noresu, sdcarm
    character(len=16) :: nomsym
    character(len=19) :: chan19
    character(len=64) :: noch64
    parameter (sdcarm = ' ')
!
    chan19=chanom
    noch64=nochmd
    call dismoi('TYPE_CHAMP', chan19, 'CHAMP', repk=typech)
!
    noresu=' '
    nomsym=' '
    numord=0
    call irchme(ifichi, chan19, partie, noch64, noresu,&
                nomsym, typech, numord, 0, [' '],&
                0, [0], 0, [0], .false._1,&
                sdcarm, codret)
!
end subroutine
