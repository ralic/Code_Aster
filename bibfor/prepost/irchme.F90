subroutine irchme(ifichi, chanom, partie, nochmd, noresu,&
                  nomsym, typech, numord, nbrcmp, nomcmp,&
                  nbnoec, linoec, nbmaec, limaec, lvarie,&
                  sdcarm, codret)
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
!        IMPRESSION DU CHAMP CHANOM NOEUD/ELEMENT ENTIER/REEL
!        AU FORMAT MED
!     ENTREES:
!        IFICHI : UNITE LOGIQUE D'IMPRESSION DU CHAMP
!        CHANOM : NOM ASTER DU CHAM A ECRIRE
!        PARTIE : IMPRESSION DE LA PARTIE IMAGINAIRE OU REELLE POUR
!                  UN CHAMP COMPLEXE AU FORMAT CASTEM OU GMSH OU MED
!        NORESU : NOM DU RESULTAT D'OU PROVIENT LE CHAMP A IMPRIMER.
!        NOMSYM : NOM SYMBOLIQUE DU CHAMP
!        TYPECH : TYPE DU CHAMP
!        NUMORD : NUMERO D'ORDRE DU CHAMP DANS LE RESULTAT_COMPOSE.
!        NBRCMP : NOMBRE DE COMPOSANTES A ECRIRE
!        NOMCMP : NOMS DES COMPOSANTES A ECRIRE
!        NBNOEC : NOMBRE DE NOEUDS A ECRIRE (O, SI TOUS LES NOEUDS)
!        LINOEC : LISTE DES NOEUDS A ECRIRE SI EXTRAIT
!        NBMAEC : NOMBRE DE MAILLES A ECRIRE (0, SI TOUTES LES MAILLES)
!        LIMAEC : LISTE DES MAILLES A ECRIRE SI EXTRAIT
!        SDCARM : CARA_ELEM (UTILE POUR LES SOUS-POINTS)
!     SORTIES:
!        CODRET : CODE DE RETOUR (0 : PAS DE PB, NON NUL SI PB)
!_______________________________________________________________________
!
!     ARBORESCENCE DE L'ECRITURE DES CHAMPS AU FORMAT MED :
!  IRCH19
!  IRCHME
!  MDNOCH RSADPA  IRCNME IRCEME
!                   .    .
!                    .  .
!                   IRCAME
!                    .  .
!                   .    .
!  MDNOMA MDEXMA IRMAIL UTLICM LRMTYP IRCMPR MDEXCH EFOUVR ...
!                   ... IRCMCC IRCMPG IRCMVA IRCMEC EFFERM
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "jeveux.h"
!
#include "asterc/utflsh.h"
#include "asterfort/infniv.h"
#include "asterfort/irceme.h"
#include "asterfort/ircnme.h"
#include "asterfort/irvari.h"
#include "asterfort/jeexin.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/rsadpa.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    character(len=8) :: noresu, typech, sdcarm
    character(len=16) :: nomsym
    character(len=19) :: chanom
    character(len=24) :: nocelk
    character(len=*) :: nomcmp(*), partie
!
    integer :: numord, nbrcmp, ifichi, iret
    integer :: nbnoec, nbmaec, icelk
    integer :: linoec(*), limaec(*)
!
    logical :: lvarie
!
    integer :: codret
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
    integer :: ednono
    parameter (ednono=-1)
    integer :: ednopt
    parameter (ednopt=-1)
!
    integer :: ifm, nivinf, numpt, iaux
!
    character(len=8) :: saux08, modele
    character(len=24) :: valk(1)
    character(len=64) :: nochmd
!
    real(kind=8) :: instan
!
!====
! 1. PREPARATIFS
!====
!
    call infniv(ifm, nivinf)
!
    10000 format(/,81('='),/,81('='),/)
    10001 format(81('-'),/)
    if (nivinf .gt. 1) then
        call utflsh(codret)
        write (ifm,10000)
        call u2mesk('I', 'MED_90', 1, chanom)
    endif
!
! 1.1. ==> NOM DU CHAMP DANS LE FICHIER MED
!
    saux08 = noresu
!
    if (codret .eq. 0) then
        if (nivinf .gt. 1) then
            write (ifm,11000) saux08
            write (ifm,11001) nomsym
        endif
        if (nivinf .gt. 1) then
            write (ifm,11003) typech
            write (ifm,11004) nochmd
        endif
    else
        call u2mess('A', 'MED_91')
        call u2mesk('A', 'MED_44', 1, chanom)
        call u2mesk('A', 'MED_45', 1, noresu)
    endif
!
    11000 format(1x,'RESULTAT           : ',a8)
    11001 format(1x,'CHAMP              : ',a16)
    11003 format(1x,'TYPE DE CHAMP      : ',a)
    11004 format(3x,'==> NOM MED DU CHAMP : ',a64,/)
!
! 1.2. ==> INSTANT CORRESPONDANT AU NUMERO D'ORDRE
!
    if (codret .eq. 0) then
!
        if (noresu .ne. ' ') then
            instan=999.999d0
!         -- DANS UN EVOL_NOLI, IL PEUT EXISTER INST ET FREQ.
!            ON PREFERE INST :
            call jenonu(jexnom(noresu//'           .NOVA', 'INST'), iret)
            if (iret .ne. 0) then
                call rsadpa(noresu, 'L', 1, 'INST', numord,&
                            0, iaux, saux08)
                instan = zr(iaux)
            else
                call jenonu(jexnom(noresu//'           .NOVA', 'FREQ'), iret)
                if (iret .ne. 0) then
                    call rsadpa(noresu, 'L', 1, 'FREQ', numord,&
                                0, iaux, saux08)
                    instan = zr(iaux)
                else
                    call jenonu(jexnom(noresu//'           .NOVA', 'CHAR_CRIT'), iret)
                    if (iret .ne. 0) then
                        call rsadpa(noresu, 'L', 1, 'CHAR_CRIT', numord,&
                                    0, iaux, saux08)
                        instan = zr(iaux)
                    endif
                endif
            endif
            numpt = numord
!
        else
!
            numord = ednono
            numpt = ednopt
!
        endif
!
    endif
!
! 1.3. ==> NOM DU MODELE ASSOCIE, DANS LE CAS D'UNE STRUCTURE RESULTAT
!
    if (codret .eq. 0) then
!
        if (typech(1:4) .ne. 'ELGA') then
            modele = ' '
        else
            nocelk = chanom//'.CELK'
            call jeveuo(nocelk, 'L', icelk)
            modele = zk24(icelk)(1:8)
            call jeexin(modele//'.MAILLE', iret)
            if (iret .eq. 0) then
                if (noresu .ne. ' ') then
                    call rsadpa(noresu, 'L', 1, 'MODELE', numord,&
                                0, iaux, saux08)
                    modele = zk8(iaux)
                    call jeexin(modele//'.MAILLE', iret)
                endif
            endif
            if (iret .eq. 0) then
                valk (1) = chanom
                call u2mesg('F', 'MED_82', 1, valk, 0,&
                            0, 0, 0.d0)
            endif
        endif
!
        if (nivinf .gt. 1) then
            write (ifm,13001) modele
            13001 format(2x,'MODELE ASSOCIE AU CHAMP : ',a)
        endif
    endif
!
!====
! 2. ECRITURE DANS LE FICHIER MED
!====
!
    if (codret .eq. 0) then
!
        if (typech(1:4) .eq. 'NOEU') then
            call ircnme(ifichi, nochmd, chanom, typech, modele,&
                        nbrcmp, nomcmp, partie, numpt, instan,&
                        numord, nbnoec, linoec, sdcarm, codret)
        else if (typech(1:2).eq.'EL') then
!
!         SI ON EST DANS LE CAS VARI ET QU'ON A DEMANDE L'EXPLOSION
!         DU CHAMP SUIVANT LE COMPORTEMENT, ON DOIT RAJOUTER
!         CERTAINS TRAITEMENT
            if ((nomsym(1:5).eq.'VARI_') .and. lvarie) then
                call irvari(ifichi, nochmd, chanom, typech, modele,&
                            nbrcmp, nomcmp, partie, numpt, instan,&
                            numord, nbmaec, limaec, noresu, sdcarm,&
                            codret)
            else
                call irceme(ifichi, nochmd, chanom, typech, modele,&
                            nbrcmp, nomcmp, ' ', partie, numpt,&
                            instan, numord, nbmaec, limaec, sdcarm,&
                            codret)
            endif
        else if (typech(1:4).eq.'CART') then
!
            call irceme(ifichi, nochmd, chanom, typech, modele,&
                        nbrcmp, nomcmp, ' ', partie, numpt,&
                        instan, numord, nbmaec, limaec, sdcarm,&
                        codret)
        else
            codret = 1
            call u2mesk('A', 'MED_92', 1, typech(1:4))
        endif
!
    endif
!
!====
! 3. BILAN
!====
!
    if (codret .ne. 0 .and. codret .ne. 100) then
        call u2mesk('A', 'MED_89', 1, chanom)
    endif
!
    if (nivinf .gt. 1) then
        call u2mesk('I', 'MED_93', 1, chanom)
        write (ifm,10000)
        call utflsh(codret)
        write (ifm,10001)
    endif
!
end subroutine
