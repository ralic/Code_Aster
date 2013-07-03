subroutine irceme(ifi, nochmd, chanom, typech, modele,&
                  nbcmp, nomcmp, etiqcp, partie, numpt,&
                  instan, numord, nbmaec, limaec, sdcarm,&
                  codret)
!_______________________________________________________________________
! person_in_charge: nicolas.sellenet at edf.fr
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
!        IMPRESSION DU CHAMP CHANOM ELEMENT ENTIER/REEL
!        AU FORMAT MED
!     ENTREES:
!       IFI    : UNITE LOGIQUE D'IMPRESSION DU CHAMP
!       NOCHMD : NOM MED DU CHAM A ECRIRE
!       PARTIE: IMPRESSION DE LA PARTIE IMAGINAIRE OU REELLE POUR
!               UN CHAMP COMPLEXE
!       CHANOM : NOM ASTER DU CHAM A ECRIRE
!       TYPECH : TYPE DU CHAMP
!       MODELE : MODELE ASSOCIE AU CHAMP
!       NBCMP  : NOMBRE DE COMPOSANTES A ECRIRE
!       NOMCMP : NOMS DES COMPOSANTES A ECRIRE
!       ETIQCP : NOMS DES COMPOSANTES A DONNER A MED (LABEL)
!       NUMPT  : NUMERO DE PAS DE TEMPS
!       INSTAN : VALEUR DE L'INSTANT A ARCHIVER
!       NUMORD : NUMERO D'ORDRE DU CHAMP
!       NBMAEC : NOMBRE DE MAILLES A ECRIRE (0, SI TOUTES LES MAILLES)
!       LIMAEC : LISTE DES MAILLES A ECRIRE SI EXTRAIT
!       SDCARM : CARA_ELEM (UTILE POUR LES SOUS-POINTS)
!    SORTIES:
!       CODRET : CODE DE RETOUR (0 : PAS DE PB, NON NUL SI PB)
! -------------------------------------------------------------------
!     ASTER INFORMATIONS:
!_______________________________________________________________________
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "asterfort/carces.h"
#include "asterfort/celces.h"
#include "asterfort/detrsd.h"
#include "asterfort/ircame.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mesk.h"
    character(len=8) :: typech, modele, sdcarm
    character(len=19) :: chanom
    character(len=64) :: nochmd
    character(len=*) :: nomcmp(*), partie, etiqcp
!
    integer :: nbcmp, numpt, numord
    integer :: nbmaec, cret
    integer :: ifi, limaec(*)
!
    real(kind=8) :: instan
!
    integer :: codret
!
! 0.2. ==> COMMUNS
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'IRCEME' )
!
    character(len=19) :: chamns
!
    integer :: jcesk, jcesd, jcesc, jcesv, jcesl
!     ------------------------------------------------------------------
!
    call jemarq()
!
!====
! 1. PREALABLE
!====
!
!    --- CONVERSION CHAM_ELEM -> CHAM_ELEM_S
!               1234567890123456789
    chamns = '&&      .CES.MED'
    chamns(3:8) = nompro
    if (typech .eq. 'CART') then
        call carces(chanom, 'ELEM', ' ', 'V', chamns,&
                    ' ', cret)
        typech = 'ELEM'
    else
        call celces(chanom, 'V', chamns)
    endif
!
!    --- ON RECUPERE LES OBJETS
!
    call jeveuo(chamns//'.CESK', 'L', jcesk)
    call jeveuo(chamns//'.CESD', 'L', jcesd)
    call jeveuo(chamns//'.CESC', 'L', jcesc)
    call jeveuo(chamns//'.CESV', 'L', jcesv)
    call jeveuo(chamns//'.CESL', 'L', jcesl)
!
!====
! 2. ECRITURE DES CHAMPS AU FORMAT MED
!====
!
    call ircame(ifi, nochmd, chanom, typech, modele,&
                nbcmp, nomcmp, etiqcp, partie, numpt,&
                instan, numord, jcesk, jcesd, jcesc,&
                jcesv, jcesl, nbmaec, limaec, sdcarm,&
                codret)
!
!====
! 3. ON NETTOIE
!====
!
    call detrsd('CHAM_ELEM_S', chamns)
!
!====
! 4. BILAN
!====
!
    if (codret .ne. 0 .and. codret .ne. 100) then
        call u2mesk('A', 'MED_89', 1, chanom)
    endif
!
    call jedema()
!
end subroutine
