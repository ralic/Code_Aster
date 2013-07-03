subroutine ircame(ifi, nochmd, chanom, typech, modele,&
                  nbcmp, nomcmp, etiqcp, partie, numpt,&
                  instan, numord, adsk, adsd, adsc,&
                  adsv, adsl, nbenec, lienec, sdcarm,&
                  codret)
!_______________________________________________________________________
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
!     ECRITURE D'UN CHAMP - FORMAT MED
!        -  -       - -            --
!-----------------------------------------------------------------------
!     ENTREES :
!       IFI    : UNITE LOGIQUE D'IMPRESSION DU CHAMP
!       NOCHMD : NOM MED DU CHAMP A ECRIRE
!       PARTIE: IMPRESSION DE LA PARTIE IMAGINAIRE OU REELLE POUR
!               UN CHAMP COMPLEXE
!       CHANOM : NOM ASTER DU CHAM A ECRIRE
!       TYPECH : TYPE DU CHAMP ('NOEU', 'ELNO', 'ELGA')
!       MODELE : MODELE ASSOCIE AU CHAMP
!       NBCMP  : NOMBRE DE COMPOSANTES A ECRIRE. S'IL EST NUL, ON
!                PREND TOUT
!       NOMCMP : NOMS DES COMPOSANTES A ECRIRE
!       NUMPT  : NUMERO DE PAS DE TEMPS
!       INSTAN : VALEUR DE L'INSTANT A ARCHIVER
!       NUMORD : NUMERO D'ORDRE DU CHAMP
!       ADSK, D, ... : ADRESSES DES TABLEAUX DES CHAMPS SIMPLIFIES
!       NBVATO : NOMBRE DE VALEURS TOTALES
!       NBENEC : NOMBRE D'ENTITES A ECRIRE (O, SI TOUTES)
!       LIENEC : LISTE DES ENTITES A ECRIRE SI EXTRAIT
!       SDCARM : CARA_ELEM (UTILE POUR LES SOUS-POINTS)
!     SORTIES:
!       CODRET : CODE DE RETOUR (0 : PAS DE PB, NON NUL SI PB)
!_______________________________________________________________________
!
! aslint: disable=W1504
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/ircam1.h"
#include "asterfort/ircmpr.h"
#include "asterfort/irelst.h"
#include "asterfort/irmail.h"
#include "asterfort/irmpga.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lrmtyp.h"
#include "asterfort/mdexch.h"
#include "asterfort/mdexma.h"
#include "asterfort/mdnoma.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/ulisog.h"
#include "asterfort/utlicm.h"
    character(len=8) :: typech, modele, sdcarm
    character(len=19) :: chanom
    character(len=64) :: nochmd
    character(len=*) :: nomcmp(*), partie, etiqcp
!
    integer :: nbcmp, numpt, numord, ifi
    integer :: adsk, adsd, adsc, adsv, adsl
    integer :: nbenec
    integer :: lienec(*)
    integer :: typent, tygeom
!
    real(kind=8) :: instan
!
    integer :: codret
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'IRCAME' )
!
    integer :: ntymax
    parameter (ntymax=69)
    integer :: nnomax
    parameter (nnomax=27)
    integer :: ednoeu
    parameter (ednoeu=3)
    integer :: edmail
    parameter (edmail=0)
    integer :: ednoma
    parameter (ednoma=4)
    integer :: typnoe
    parameter (typnoe=0)
!
    character(len=1) :: saux01, k1bid
    character(len=8) :: saux08, k8bid
    character(len=8) :: nomaas
    character(len=8) :: nomtyp(ntymax)
    character(len=16) :: formar
    character(len=24) :: ntlcmp, ntncmp, ntucmp, ntproa, nmcmfi
    character(len=24) :: ncaimi, ncaimk
    character(len=64) :: nomamd, saux32
    character(len=200) :: nofimd
    character(len=255) :: kfic
!
    integer :: ndim, typgeo(ntymax)
    integer :: nbtyp, nnotyp(ntymax)
    integer :: modnum(ntymax)
    integer :: numnoa(ntymax, nnomax), nuanom(ntymax, nnomax)
    integer :: renumd(ntymax)
    integer :: ifm, nivinf, ifimed
    integer :: lnomam
    integer :: ncmpve, nvalec
    integer :: nbvato, ncmprf
    integer :: nbimpr, jnocm1, jnocm2, nbcmp2, icmp1, icmp2
    integer :: adcaii, adcaik
!
    integer :: iaux, jaux
    integer :: nrimpr
    integer :: existc, nbcmfi, nbval
!
    logical :: lgaux
    logical :: existm
!
    call jemarq()
!
!====
! 1. PREALABLES
!====
!
! 1.1. ==> RECUPERATION DU NIVEAU D'IMPRESSION
!
    call infniv(ifm, nivinf)
!
! 1.2. ==> NOMS DES TABLEAUX DE TRAVAIL
!               12   345678   9012345678901234
    ntlcmp = '&&'//nompro//'.LISTE_N0MCMP   '
    ntncmp = '&&'//nompro//'.NOMCMP         '
    ntucmp = '&&'//nompro//'.UNITECMP       '
    ntproa = '&&'//nompro//'.PROFIL_ASTER   '
    nmcmfi = '&&'//nompro//'.NOMCMP_FICHIER '
    ncaimi = '&&'//nompro//'.CARAC_NOMBRES__'
    ncaimk = '&&'//nompro//'.CARAC_CHAINES__'
!
! 1.3. ==> NOM DU FICHIER MED
!
    call ulisog(ifi, kfic, saux01)
    if (kfic(1:1) .eq. ' ') then
        call codent(ifi, 'G', saux08)
        nofimd = 'fort.'//saux08
    else
        nofimd = kfic(1:200)
    endif
!
    if (nivinf .gt. 1) then
        write (ifm,*) '<',nompro,'> NOM DU FICHIER MED : ',nofimd
    endif
!
! 1.4. ==> LES NOMBRES CARACTERISTIQUES
!
    nbvato = zi(adsd)
    ncmprf = zi(adsd+1)
!
!====
! 2. LE MAILLAGE
!====
!
! 2.1. ==> NOM ET DIMENSION DU MAILLAGE ASTER
!
    nomaas = zk8(adsk-1+1)
    call dismoi('F', 'DIM_GEOM_B', nomaas, 'MAILLAGE', ndim,&
                saux32, codret)
    if (codret .ne. 0) then
        call u2mess('F', 'MED_43')
    endif
!
! 2.2. ==> CREATION DU NOM DU MAILLAGE POUR MED
!
    call mdnoma(nomamd, lnomam, nomaas, codret)
    if (codret .ne. 0) then
        saux08='MDNOMA  '
        call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                    codret, 0, 0.d0)
    endif
!
! 2.3. ==> CE MAILLAGE EST-IL DEJA PRESENT DANS LE FICHIER ?
!
    iaux = 0
    ifimed = 0
    call mdexma(nofimd, ifimed, nomamd, iaux, existm,&
                jaux, codret)
!
! 2.4. ==> SI LE MAILLAGE EST ABSENT, ON L'ECRIT
!
    if (.not.existm) then
        saux08 = 'MED     '
        lgaux = .false.
        k8bid = '        '
        formar=' '
        call irmail(saux08, ifi, iaux, nomaas, lgaux,&
                    k8bid, jaux, nivinf, formar)
    endif
!
!====
! 3. PREPARATION DU CHAMP A ECRIRE
!====
!
! 3.1. ==> NUMEROS, NOMS ET UNITES DES COMPOSANTES A ECRIRE
!
    call utlicm(nbcmp, nomcmp, zk8(adsk+1), ncmprf, zk8(adsc),&
                ncmpve, ntlcmp, ntncmp, ntucmp)
!
    if (ncmpve .gt. 80) then
        call u2mesk('A', 'MED_99', 1, nochmd)
        goto 9999
    endif
!
!     ON REMPLACE LES NOMS DES COMPOSANTES
    if (etiqcp .ne. ' ') then
        call jeveuo(ntncmp, 'L', jnocm1)
        call jeveuo(etiqcp, 'L', jnocm2)
        call jelira(etiqcp, 'LONMAX', nbcmp2, k1bid)
        nbcmp2=nbcmp2/2
        do 10,icmp1 = 1,ncmpve
        do 20,icmp2 = 1,nbcmp2
        if (zk16(jnocm1+icmp1-1) .eq. zk16(jnocm2+2*(icmp2-1))) then
            zk16(jnocm1+icmp1-1) = zk16(jnocm2+2*icmp2-1)
            goto 10
        endif
20      continue
10      continue
    endif
!
! 3.2. ==> . RECUPERATION DES NB/NOMS/NBNO/NBITEM DES TYPES DE MAILLES
!            DANS CATALOGUE
!          . RECUPERATION DES TYPES GEOMETRIE CORRESPONDANT POUR MED
!          . VERIF COHERENCE AVEC LE CATALOGUE
!
    call lrmtyp(nbtyp, nomtyp, nnotyp, typgeo, renumd,&
                modnum, nuanom, numnoa)
!
! 3.3. ==> DEFINITIONS DES IMPRESSIONS ET CREATION DES PROFILS EVENTUELS
!
    call ircmpr(nofimd, typech, nbimpr, ncaimi, ncaimk,&
                ncmprf, ncmpve, ntlcmp, nbvato, nbenec,&
                lienec, adsd, adsl, nomaas, modele,&
                typgeo, nomtyp, ntproa, chanom, sdcarm)
!
    call jeveuo(ncaimi, 'L', adcaii)
    call jeveuo(ncaimk, 'L', adcaik)
!
! 3.4. ==> CARACTERISATION DES SUPPORTS QUAND CE NE SONT PAS DES NOEUDS
!
    if (typech(1:4) .eq. 'ELGA' .or. typech(1:4) .eq. 'ELEM') then
!
        if (sdcarm .ne. ' ' .and. typech(1:4) .eq. 'ELGA') then
            call irelst(nofimd, chanom, typech, nomaas, nomamd,&
                        nbimpr, zi( adcaii), zk80(adcaik), sdcarm)
        endif
!
        call irmpga(nofimd, chanom, typech, nomtyp, nbimpr,&
                    zi( adcaii), zk80(adcaik), modnum, nuanom, sdcarm,&
                    codret)
!
    endif
!
!====
! 4. REPERAGE DU CHAMP : EXISTE-T-IL DEJA ?
!    ON DOIT PARCOURIR TOUTES LES IMPRESSIONS POSSIBLES POUR CE CHAMP
!====
!
    existc = 0
!
    do 41 , nrimpr = 1 , nbimpr
!
    if (codret .eq. 0) then
!
        tygeom = zi(adcaii+10*nrimpr-2)
        if (tygeom .eq. typnoe) then
            typent = ednoeu
        else
            if (typech .eq. 'ELNO') then
                typent = ednoma
            else
                typent = edmail
            endif
        endif
        nvalec = zi(adcaii+10*nrimpr-4)
!
        call jedetr(nmcmfi)
!
        ifimed = 0
        call mdexch(nofimd, ifimed, nochmd, numpt, numord,&
                    ncmpve, ntncmp, nvalec, typent, tygeom,&
                    jaux, nbcmfi, nmcmfi, nbval, codret)
!
        existc = max ( existc, jaux )
!
    endif
!
    41 end do
!
!====
! 5. ECRITURE SI C'EST POSSIBLE
!====
!
    if (existc .le. 2) then
!
        call ircam1(nofimd, nochmd, existc, ncmprf, numpt,&
                    instan, numord, adsd, adsv, adsl,&
                    adsk, partie, ncmpve, ntlcmp, ntncmp,&
                    ntucmp, ntproa, nbimpr, zi(adcaii), zk80(adcaik),&
                    typech, nomamd, nomtyp, modnum, nuanom,&
                    codret)
!
    else
!
        call u2mesg('F', 'MED2_4', 1, nochmd, 0,&
                    0, 1, instan)
!
    endif
!
!====
! 6. LA FIN
!====
!
9999  continue
    if (nivinf .gt. 1) then
        write (ifm,*) ' '
    endif
!
! --- MENAGE
    call jedetr('&&'//nompro//'.LISTE_N0MCMP   ')
    call jedetr('&&'//nompro//'.NOMCMP         ')
    call jedetr('&&'//nompro//'.UNITECMP       ')
    call jedetr('&&'//nompro//'.PROFIL_ASTER   ')
    call jedetr('&&'//nompro//'.NOMCMP_FICHIER ')
    call jedetr('&&'//nompro//'.CARAC_NOMBRES__')
    call jedetr('&&'//nompro//'.CARAC_CHAINES__')
!
    call jedema()
!
end subroutine
