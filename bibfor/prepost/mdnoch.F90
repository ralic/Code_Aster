subroutine mdnoch(nochmd, lnochm, lresu, noresu, nomsym,&
                  codret)
!_____________________________________________________________________
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: nicolas.sellenet at edf.fr
!_____________________________________________________________________
!        FORMAT MED : ELABORATION D'UN NOM DE CHAMP DANS LE FICHIER
!               - -                    --     --
!_____________________________________________________________________
!
!   LA REGLE EST LA SUIVANTE :
!
!     LE NOM EST LIMITE A 64 CARACTERES DANS MED. ON UTILISE ICI
!     EXACTEMENT 64 CARACTERES.
!       . POUR UN CHAMP ISSU D'UNE STRUCTURE DE RESULTAT :
!                 12345678901234567890123456789012
!                 AAAAAAAABBBBBBBBBBBBBBBBCCCCCCCC
!       AAAAAAAA : NOM DU RESULTAT D'OU PROVIENT LE CHAMP A IMPRIMER
!       BBBBBBBBBBBBBBBB : NOM SYMBOLIQUE DU CHAMP
!       CCCCCCCC : NOM D'UN PARAMETRE EVENTUEL
!       . POUR UN CHAMP DE GRANDEUR :
!                 12345678901234567890123456789012
!                 AAAAAAAA
!       AAAAAAAA : NOM DE LA GRANDEUR A IMPRIMER
!
!   REMARQUE :
!       LES EVENTUELS BLANCS SONT REMPLACES PAR DES '_'
!
!   EXEMPLE :
!     . CHAMP DE GRANDEUR 'EXTRRESU' :
!                 12345678901234567890123456789012
!       NOCHMD = 'EXTRRESU________________________'
!
!     SORTIES :
!       NOCHMD : NOM DU CHAMP DANS LE FICHIER MED
!       LNOCHM : LONGUEUR UTILE DU NOM DU CHAMP DANS LE FICHIER MED
!       CODRET : CODE DE RETOUR DE L'OPERATION
!                0 --> TOUT VA BIEN
!                1 --> LA DECLARATION DU NOM DE L'OBJET NE CONVIENT PAS
!               10 --> AUTRE PROBLEME
!    ENTREES :
!       LRESU  : .TRUE. : INDIQUE IMPRESSION D'UN CONCEPT RESULTAT
!                .FALSE. : IMPRESSION D'UN CHAMP GRANDEUR
!       NORESU : NOM DU RESULTAT D'OU PROVIENT LE CHAMP A IMPRIMER
!       NOMSYM : NOM SYMBOLIQUE DU CHAMP, SI RESULTAT
!                CHAINE BLANCHE SI GRANDEUR
!_____________________________________________________________________
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "asterfort/assert.h"
#include "asterfort/lxlgut.h"
#include "asterfort/utmess.h"
    character(len=64) :: nochmd
    character(len=16) :: nomsym
    character(len=8) :: noresu
!
    logical :: lresu
!
    integer :: lnochm
    integer :: codret
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
!
    integer :: iaux, jaux
!
!====
! 1. PREALABLES
!====
!
    codret = 0
!
!====
! 2. CREATION DU NOM
!====
!
    if (codret .eq. 0) then
!
! 2.1. ==> BLANCHIMENT INITIAL
!
        do 21 , iaux = 1 , 64
        nochmd(iaux:iaux) = ' '
        21     end do
!
! 2.2. ==> NOM DU RESULTAT
!
        jaux = lxlgut(noresu)
        ASSERT(jaux.ge.1 .and. jaux.le.8)
!
        lnochm = jaux
        nochmd(1:lnochm) = noresu(1:lnochm)
!
! 2.3. ==> NOM SYMBOLIQUE DU CHAMP
!
        if (lresu) then
!
            jaux = lxlgut(nomsym)
            ASSERT(jaux.ge.1 .and. jaux.le.16)
!
            do 23 , iaux = lnochm+1 , 8
            nochmd(iaux:iaux) = '_'
23          continue
            lnochm = 8+jaux
            nochmd(9:8+jaux) = nomsym(1:jaux)
!
        endif
!
    endif
!
!====
! 3. BILAN
!====
!
    if (codret .ne. 0) then
        call utmess('F', 'MED_91')
    endif
!
end subroutine
