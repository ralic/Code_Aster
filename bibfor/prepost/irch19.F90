subroutine irch19(cham19, partie, form, ifi, titre,&
                  nomsd, nomsym, numord, lcor, nbnot,&
                  numnoe, nbmat, nummai, nbcmp, nomcmp,&
                  lsup, borsup, linf, borinf, lmax,&
                  lmin, lresu, formr, nive)
!     ------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!     IMPRIMER UN CHAMP (CHAM_NO OU CHAM_ELEM)
!
! IN  CHAM19: NOM DU CHAM_XX
! IN  PARTIE: IMPRESSION DE LA PARTIE IMAGINAIRE OU REELLE POUR
!             UN CHAMP COMPLEXE AU FORMAT CASTEM OU GMSH OU MED
! IN  FORM  : FORMAT :'RESULTAT' OU 'SUPERTAB'
! IN  IFI   : UNITE LOGIQUE D'IMPRESSION DU CHAMP.
! IN  TITRE : TITRE.
! IN  NOMO  : NOM DU MODELE SUPPORT.
! IN  NOMSD : NOM DU RESULTAT D'OU PROVIENT LE CHAMP A IMPRIMER.
! IN  NOMSYM: NOM SYMBOLIQUE DU CHAMP A IMPRIMER
! IN  NUMORD: NUMERO D'ORDRE DU CHAMP DANS LE RESULTAT_COMPOSE.
!             (1 SI LE RESULTAT EST UN CHAM_GD)
! IN  LCOR  : IMPRESSION DES COORDONNEES DE NOEUDS .TRUE. IMPRESSION
! IN  NBNOT : NOMBRE DE NOEUDS A IMPRIMER
! IN  NUMNOE: NUMEROS DES NOEUDS A IMPRIMER
! IN  NBMAT : NOMBRE DE MAILLES A IMPRIMER
! IN  NUMMAI: NUMEROS DES MAILLES A IMPRIMER
! IN  NBCMP : NOMBRE DE COMPOSANTES A IMPRIMER
! IN  NOMCMP: NOMS DES COMPOSANTES A IMPRIMER
! IN  LSUP  : =.TRUE. INDIQUE PRESENCE D'UNE BORNE SUPERIEURE
! IN  BORSUP: VALEUR DE LA BORNE SUPERIEURE
! IN  LINF  : =.TRUE. INDIQUE PRESENCE D'UNE BORNE INFERIEURE
! IN  BORINF: VALEUR DE LA BORNE INFERIEURE
! IN  LMAX  : =.TRUE. INDIQUE IMPRESSION VALEUR MAXIMALE
! IN  LMIN  : =.TRUE. INDIQUE IMPRESSION VALEUR MINIMALE
! IN  LRESU : =.TRUE. INDIQUE IMPRESSION D'UN CONCEPT RESULTAT
! IN  FORMR : FORMAT D'ECRITURE DES REELS SUR "RESULTAT"
! IN  NIVE  : NIVEAU IMPRESSION CASTEM 3 OU 10
! ----------------------------------------------------------------------
!
! aslint: disable=W1306,W1504
    implicit none
!
! 0.1. ==> ARGUMENTS
!
!
#include "asterfort/dismoi.h"
#include "asterfort/irchml.h"
#include "asterfort/irdepl.h"
#include "asterfort/utcmp3.h"
#include "asterfort/utmess.h"
    character(len=*) :: cham19, nomsd, nomsym
    character(len=*) :: form, formr, titre, nomcmp(*), partie
    real(kind=8) :: borsup, borinf
    integer :: numord, nbmat
    integer :: nbnot, numnoe(*), nummai(*), nbcmp, ncmp
    integer :: nive
    logical :: lcor, lsup, linf, lmax, lmin, lresu
!
! 0.3. ==> VARIABLES LOCALES
!
!
    character(len=8) :: tych, nomgd, nomsd8
    character(len=19) :: ch19
    character(len=24) :: valk(2), tyres
    integer :: ibid, ierd, ifi, numcmp(nbcmp)
!
!     PASSAGE DANS DES VARIABLES FIXES
!
    ch19 = cham19
    nomsd8 = nomsd
!
!     --- TYPE DU CHAMP A IMPRIMER (CHAM_NO OU CHAM_ELEM)
    call dismoi('F', 'TYPE_CHAMP', ch19, 'CHAMP', ibid,&
                tych, ierd)
    call dismoi('F', 'TYPE_RESU', nomsd8, 'RESULTAT', ibid,&
                tyres, ierd)
!
    if ((tych(1:4).eq.'NOEU') .or. (tych(1:2).eq.'EL')) then
    else if (tych(1:4).eq. 'CART') then
        goto 9999
    else
        valk(1) = tych
        valk(2) = ch19
        if (tyres(1:9) .eq. 'MODE_GENE' .or. tyres(1:9) .eq. 'HARM_GENE') then
            call utmess('A+', 'PREPOST_87', nk=2, valk=valk)
            call utmess('A', 'PREPOST6_36')
        else
            call utmess('A', 'PREPOST_87', nk=2, valk=valk)
        endif
    endif
!
!     --- NOM DE LA GRANDEUR ASSOCIEE AU CHAMP CH19
    call dismoi('F', 'NOM_GD', ch19, 'CHAMP', ibid,&
                nomgd, ierd)
!
    ncmp = 0
    if (nbcmp .ne. 0) then
        if ((nomgd.eq.'VARI_R') .and. (tych(1:2).eq.'EL')) then
! --------- TRAITEMENT SUR LES "NOMCMP"
            ncmp = nbcmp
            call utcmp3(nbcmp, nomcmp, numcmp)
        endif
    endif
!
!     -- ON LANCE L'IMPRESSION:
!     -------------------------
!
    if (tych(1:4) .eq. 'NOEU' .and. nbnot .ge. 0) then
        call irdepl(ch19, partie, ifi, form, titre,&
                    nomsd, nomsym, numord, lcor, nbnot,&
                    numnoe, nbcmp, nomcmp, lsup, borsup,&
                    linf, borinf, lmax, lmin, lresu,&
                    formr, nive)
    else if (tych(1:2).eq.'EL'.and.nbmat.ge.0) then
        call irchml(ch19, partie, ifi, form, titre,&
                    tych(1:4), nomsd, nomsym, numord, lcor,&
                    nbnot, numnoe, nbmat, nummai, nbcmp,&
                    nomcmp, lsup, borsup, linf, borinf,&
                    lmax, lmin, lresu, formr, ncmp,&
                    numcmp, nive)
    endif
!
9999  continue
!
    if (ierd .ne. 0) then
        valk(1) = ch19
        valk(2) = form(1:7)
        call utmess('A', 'PREPOST_90', nk=2, valk=valk)
    endif
!
end subroutine
