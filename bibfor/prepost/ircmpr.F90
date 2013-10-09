subroutine ircmpr(nofimd, typech, nbimpr, ncaimi, ncaimk,&
                  ncmprf, ncmpve, ntlcmp, nbvato, nbenec,&
                  lienec, adsd, adsl, nomaas, modele,&
                  typgeo, nomtyp, ntproa, chanom, sdcarm)
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
!_______________________________________________________________________
!     ECRITURE D'UN CHAMP -  FORMAT MED - CREATION DU PROFIL
!        -  -       -               -                 --
!_______________________________________________________________________
!     ENTREES :
!       NOFIMD : NOM DU FICHIER MED
!       TYPECH : TYPE DU CHAMP ('NOEU', 'ELNO', 'ELGA')
!       NCMPRF : NOMBRE DE COMPOSANTES DU CHAMP DE REFERENCE
!       NCMPVE : NOMBRE DE COMPOSANTES VALIDES EN ECRITURE
!       NTLCMP : SD DES NUMEROS DES COMPOSANTES VALIDES
!       NBVATO : NOMBRE DE VALEURS TOTALES
!       NBENEC : NOMBRE D'ENTITES A ECRIRE (O, SI TOUTES)
!       LIENEC : LISTE DES ENTITES A ECRIRE SI EXTRAIT
!       ADSK, D, ... : ADRESSES DES TABLEAUX DES CHAMPS SIMPLIFIES
!       NOMAAS : SD MAILLAGE ASTER
!       MODELE : SD MODELE
!       TYPGEO : TYPE GEOMETRIQUE DE MAILLE ASSOCIEE AU TYPE ASTER
!       NOMTYP : NOM DES TYPES DE MAILLES ASTER
!     SORTIES :
!       NBIMPR : NOMBRE D'IMPRESSIONS A REALISER
!       NCAIMI : STRUCTURE ASSOCIEE AU TABLEAU CAIMPI
!         CAIMPI : ENTIERS POUR CHAQUE IMPRESSION
!                  CAIMPI(1,I) = TYPE D'EF / MAILLE ASTER (0, SI NOEUD)
!                  CAIMPI(2,I) = NOMBRE DE POINTS (GAUSS OU NOEUDS)
!                  CAIMPI(3,I) = NOMBRE DE SOUS-POINTS
!                  CAIMPI(4,I) = NOMBRE DE COUCHES
!                  CAIMPI(5,I) = NOMBRE DE SECTEURS
!                  CAIMPI(6,I) = NOMBRE DE FIBTRES
!                  CAIMPI(7,I) = NOMBRE DE MAILLES A ECRIRE
!                  CAIMPI(8,I) = TYPE DE MAILLES ASTER (0, SI NOEUD)
!                  CAIMPI(9,I) = TYPE GEOMETRIQUE AU SENS MED
!                  CAIMPI(10,I) = NOMBRE TOTAL DE MAILLES IDENTIQUES
!       NCAIMK : STRUCTURE ASSOCIEE AU TABLEAU CAIMPK
!         CAIMPK : CARACTERES POUR CHAQUE IMPRESSION
!                  CAIMPK(1,I) = NOM DE LA LOCALISATION ASSOCIEE
!                  CAIMPK(2,I) = NOM DU PROFIL AU SENS MED
!       NTPROA : SD DU PROFIL ASTER. C'EST LA LISTE DES NUMEROS ASTER
!                DES NOEUDS OU DES ELEMENTS POUR LESQUELS LE CHAMP
!                EST DEFINI
!_______________________________________________________________________
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "jeveux.h"
#include "asterc/utflsh.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/ircmpe.h"
#include "asterfort/ircmpn.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nbvato, ncmprf, ncmpve
    integer :: nbenec, adtyp2
    integer :: lienec(*)
    integer :: adsd, adsl
    integer :: nbimpr
    integer :: typgeo(*)
!
    character(len=*) :: nofimd
    character(len=*) :: ntlcmp, ntproa
    character(len=8) :: nomaas, modele, typech, sdcarm
    character(len=8) :: nomtyp(*)
    character(len=19) :: chanom
    character(len=24) :: ncaimi, ncaimk
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'IRCMPR' )
!
    integer :: ifm, nivinf, j, i
    integer :: iaux, ima, nbno, nbma, ipe18, ipe15
    integer :: nbmail, iadcnx, ilcnx, iadtyp
    integer :: codret, jnoce, jco
    integer :: adtypm, adefma
    integer :: adcaii, adcaik
    integer :: adproa, adprom, adexic, adpror
    integer :: adnucm
    integer :: adauxi
!
    character(len=24) :: ntprom, exicmp, ntpror
    character(len=24) :: ntauxi
    character(len=80) :: caimpk(3)
!
!====
! 1. PREALABLES
!====
!
! 1.1. ==> RECUPERATION DU NIVEAU D'IMPRESSION
!          -----------------------------------
!
    call infniv(ifm, nivinf)
!
    if (nivinf .gt. 1) then
        write (ifm,1001) 'DEBUT DE '//nompro
        call utflsh(codret)
    endif
    1001 format(/,4x,10('='),a,10('='),/)
!
!               12   345678   9012345678901234
    ntprom = '&&'//nompro//'.PROFIL_MED     '
    exicmp = '&&'//nompro//'.EXICMP         '
    ntauxi = '&&'//nompro//'.NUME_RECIPROQUE'
    ntpror = '&&'//nompro//'.PROFIL_RECIPROQ'
!
! 1.2. ==> ALLOCATIONS DES TABLEAUX DE RENUMEROTATIONS
!
    call wkvect(ntproa, 'V V I', nbvato, adproa)
    call wkvect(ntprom, 'V V I', nbvato, adprom)
    call wkvect(exicmp, 'V V L', nbvato, adexic)
!
    call jeveuo(ntlcmp, 'L', adnucm)
!
! 1.3. ==> COMPLEMENTS
!
! 1.3.1. ==> COMPLEMENTS POUR UN CHAMP AUX NOEUDS
!
    call jenonu(jexnom('&CATA.TM.NOMTM', 'PENTA15'), ipe15)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'PENTA18'), ipe18)
!
    if (typech(1:4) .eq. 'NOEU') then
!
        nbimpr = 1
        iaux = 10*nbimpr
        call wkvect(ncaimi, 'V V I', iaux, adcaii)
        iaux = 3*nbimpr
        call wkvect(ncaimk, 'V V K80', iaux, adcaik)
!
!       ON CREE UN TABLEAU QUI PERMET DE DETECTER L'EXISTENCE DE NOEUDS
!       CENTRE (APPARTENANT AUX MAILLES DE TYPE TRIA7,QUAD9,PENTA18 OU
!       HEXA27)
!
        call jeveuo(nomaas//'.TYPMAIL', 'L', iadtyp)
        call jeveuo(nomaas//'.CONNEX', 'L', iadcnx)
        call jeveuo(jexatr(nomaas//'.CONNEX', 'LONCUM'), 'L', ilcnx)
        call dismoi('NB_NO_MAILLA', nomaas, 'MAILLAGE', repi=nbno)
        call dismoi('NB_MA_MAILLA', nomaas, 'MAILLAGE', repi=nbma)
        call wkvect('&&IRCMPR.NOEU_CENTR', 'V V I', nbno, jnoce)
        do i = 1, nbno
            zi(jnoce+i-1)=0
        end do
!
        do i = 1, nbma
            if (zi(iadtyp+i-1) .eq. ipe18) then
                jco=iadcnx+zi(ilcnx+i-1)-1
                do j = 1, 3
                    zi(jnoce+zi(jco+15+j-1)-1)=1
                end do
            endif
        end do
!
! 1.3.2. ==> COMPLEMENTS POUR DES CHAMPS AUX ELEMENTS
!
    else if (typech(1:2).eq.'EL') then
!
        call jeveuo(nomaas//'.TYPMAIL', 'L', adtypm)
        call jelira(nomaas//'.TYPMAIL', 'LONUTI', nbmail)
        call wkvect('&&IRCMPR.TYPMA', 'V V I', nbmail, adtyp2)
        do ima = 1, nbmail
            if (zi(adtypm+ima-1) .eq. ipe18) then
                zi(adtyp2+ima-1)=ipe15
            else
                zi(adtyp2+ima-1)=zi(adtypm+ima-1)
            endif
        end do
        if (typech(1:4) .eq. 'ELGA') then
            call jeveuo(modele//'.MAILLE', 'L', adefma)
        endif
        call wkvect(ntpror, 'V V I', nbvato, adpror)
        call wkvect(ntauxi, 'V V I', nbvato, adauxi)
!
! 1.3.3. ==> ERREUR
!
    else
!
        call utmess('F', 'MED_46', sk=typech)
!
    endif
!
!====
! 2. APPELS DES PROGRAMMES SPECIFIQUES
!====
!
    if (typech(1:4) .eq. 'NOEU') then
!
! 2.1. ==> LES NOEUDS
!
        call ircmpn(nofimd, ncmprf, ncmpve, zi(adnucm), zl(adexic),&
                    nbvato, nbenec, lienec, adsl, zi(adcaii),&
                    caimpk, zi(adproa), zi(jnoce))
        zk80(adcaik:adcaik+2)=caimpk(1:3)
!
    else
!
! 2.2. ==> LES ELEMENTS
!
        if (typech(1:4) .eq. 'ELGA') then
            iaux = adefma
        else
            iaux = adtyp2
        endif
!
        call ircmpe(nofimd, ncmpve, zi(adnucm), zl(adexic), nbvato,&
                    nbenec, lienec, adsd, adsl, nbimpr,&
                    ncaimi, ncaimk, zi(iaux), zi(adtyp2), typgeo,&
                    nomtyp, typech, zi(adproa), zi(adprom), zi(adpror),&
                    zi(adauxi), chanom, sdcarm)
!
    endif
!
!====
! 3. LA FIN
!====
!
! --- MENAGE
    call jedetr(ntpror)
    call jedetr(ntprom)
    call jedetr(exicmp)
    call jedetr(ntauxi)
    call jedetr('&&IRCMPR.NOEU_CENTR')
    call jedetr('&&IRCMPR.TYPMA')
!
    if (nivinf .gt. 1) then
        write (ifm,1001) 'FIN DE '//nompro
    endif
!
end subroutine
