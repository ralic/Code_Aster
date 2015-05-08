subroutine irvari(ifi, nochmd, chanom, typech, modele,&
                  nbcmp, nomcmp, partie, numpt, instan,&
                  numord, nbmaec, limaec, noresu, carael,&
                  codret)
    implicit none
!
#include "jeveux.h"
!
#include "asterc/lccree.h"
#include "asterc/lcinfo.h"
#include "asterc/lcvari.h"
#include "asterc/lcdiscard.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/cescrm.h"
#include "asterfort/cesexi.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/irceme.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsexch.h"
#include "asterfort/wkvect.h"
    integer :: nbcmp, numpt, numord, nbmaec, ifi, limaec(*), codret
!
    character(len=8) :: typech, modele, noresu, carael
    character(len=19) :: chanom
    character(len=64) :: nochmd
    character(len=*) :: nomcmp(*), partie
!
    real(kind=8) :: instan
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! -------------------------------------------------------------------
!        IMPRESSION DU CHAMP CHANOM ELEMENT ENTIER/REEL
!        AU FORMAT MED CAS D'UN CHAMP DE VARIABLES INTERNES
!     ENTREES:
!       IFI    : UNITE LOGIQUE D'IMPRESSION DU CHAMP
!       NOCHMD : NOM MED DU CHAM A ECRIRE
!       PARTIE : IMPRESSION DE LA PARTIE IMAGINAIRE OU REELLE POUR
!                UN CHAMP COMPLEXE
!       CHANOM : NOM ASTER DU CHAM A ECRIRE
!       TYPECH : TYPE DU CHAMP
!       MODELE : MODELE ASSOCIE AU CHAMP
!       NBCMP  : NOMBRE DE COMPOSANTES A ECRIRE
!       NOMCMP : NOMS DES COMPOSANTES A ECRIRE
!       NUMPT  : NUMERO DE PAS DE TEMPS
!       INSTAN : VALEUR DE L'INSTANT A ARCHIVER
!       NUMORD : NUMERO D'ORDRE DU CHAMP
!       NBMAEC : NOMBRE DE MAILLES A ECRIRE (0, SI TOUTES LES MAILLES)
!       LIMAEC : LISTE DES MAILLES A ECRIRE SI EXTRAIT
!       NORESU : NOM DU RESULTAT D'OU PROVIENT LE CHAMP A IMPRIMER.
!    SORTIES:
!       CODRET : CODE DE RETOUR (0 : PAS DE PB, NON NUL SI PB)
! -------------------------------------------------------------------
!
    integer :: nbmax,  inum, nbre, iret,  nbcomp
    integer :: lon3, numlc, nbvari, ntvari, mxnbva, jnovar, jmnova
    integer :: nredva, inum2, inum3, jcorva, jcesd, posit, nbvar2
    integer :: jconi1, jconi2, typaff, nbzone, nbmail, ima2, jnocmp
    integer :: jcesl,  jcesdb, jceslb,  ima, ipt, icmp
    integer :: nbcmpc, isp, nbpt, nbsp, iad, iad2, icmp2, nbma2, jnocm2
!
    character(len=7) :: saux07
    character(len=8) :: base, saux08
    parameter ( base = '&&IRVARI' )
    character(len=16) :: compor, nomtmp, lcompo(2), comco2
    character(len=19) :: noch19, ligrel, chamns, chnova, chmano, chcorr, chabis
    character(len=19) :: chater, noetcm
    parameter ( chamns = '&&IRVARI.CH_EL_S_TM' )
    parameter ( chnova = '&&IRVARI.CH_NOM_VAR' )
    parameter ( chmano = '&&IRVARI.CH_TOT_NOM' )
    parameter ( chabis = '&&IRVARI.CH_EL_S_BI' )
    parameter ( chater = '&&IRVARI.CH_EL_S_TE' )
    character(len=64) :: nomres
    real(kind=8), pointer :: cesvb(:) => null()
    real(kind=8), pointer :: cesv(:) => null()
    character(len=16), pointer :: vale(:) => null()
    integer, pointer :: desc(:) => null()
!
    call jemarq()
!
!   RECHERCHE DE LA CARTE DE COMPORTEMENT
    call rsexch('F', noresu, 'COMPORTEMENT', numord, noch19,&
                iret)
    call jeveuo(noch19//'.DESC', 'L', vi=desc)
    call jeveuo(noch19//'.VALE', 'L', vk16=vale)
    call jelira(noch19//'.VALE', 'LONMAX', lon3)
    ligrel=modele//'.MODELE'
!
!   NOMBRE DE COMPORTEMENT
    nbre=desc(3)
    nbmax=desc(2)
    nbcomp=lon3/nbmax
!
    call jeveuo(jexnum(noch19//'.LIMA', 1), 'L', jconi1)
    call jeveuo(jexatr(noch19//'.LIMA', 'LONCUM'), 'L', jconi2)
!
    ntvari=0
    mxnbva=0
    nredva=0
!
!   PARCOUR DE LA CARTE POUR CALCULER LE NOMBRE TOTAL DE VARI_INTERNE
    do inum = 2,nbre
!
!       NOM DU COMPORTEMENT
        compor=vale(1+nbcomp*(inum-1))
        if (compor .eq. 'ELAS') goto 10
!
        nomtmp=vale(1+nbcomp*(inum-1)+1)
        read(nomtmp,'(I16)') nbvari
        ntvari=ntvari+nbvari
        mxnbva=max(mxnbva,nbvari)
10  end do
!
    call wkvect(chnova, 'V V K16', ntvari, jnovar)
    call wkvect(chmano, 'V V K16', mxnbva, jmnova)
!
!   ON TRI LES COMPOSANTES POUR LES REUNIR
    do inum = 2,nbre
!
        compor=vale(1+nbcomp*(inum-1))
        if (compor .eq. 'ELAS') goto 20
!
        call lcinfo(compor, numlc, nbvari)
        nomtmp=vale(1+nbcomp*(inum-1)+1)
        read(nomtmp,'(I16)') nbvar2
        if (nbvari .ne. nbvar2) then
            lcompo(1)=compor
            lcompo(2)=vale(1+nbcomp*(inum-1)+2)
            call lccree(2, lcompo, comco2)
            call lcinfo(comco2, numlc, nbvari)
            call lcdiscard(comco2)
            if (nbvari .ne. nbvar2) then
                codret = 200
                exit
            endif
!
        else
            comco2=compor
        endif
        call lcvari(comco2, nbvari, zk16(jmnova))
        call codent(inum, 'G', saux08)
        chcorr=base//saux08
        call wkvect(chcorr, 'V V I', nbvari, jcorva)
!
!       TRI A PROPREMENT PARLER
        do inum3 = 1,nbvari
            do inum2 = 1,nredva
                if (zk16(jmnova+inum3-1) .eq. zk16(jnovar+inum2-1)) goto 50
            end do
            zk16(jnovar+nredva)=zk16(jmnova+inum3-1)
            nredva=nredva+1
50          continue
            zi(jcorva+inum3-1)=inum2
        end do
        call lcdiscard(comco2)
20  end do
    if ( codret.eq.200 ) goto 9999
!
    call celces(chanom, 'V', chamns)
    call jeveuo(chamns//'.CESD', 'L', jcesd)
    call jeveuo(chamns//'.CESL', 'L', jcesl)
    call jeveuo(chamns//'.CESV', 'L', vr=cesv)
    nbma2 = zi(jcesd)
!
    noetcm=base//'.NOCMP'
    call wkvect(base//'.NOCMPTMP', 'V V K8', nredva, jnocmp)
    call wkvect(noetcm, 'V V K16', 2*nredva, jnocm2)
    do inum = 1,nredva
        call codent(inum, 'G', saux07)
        zk8(jnocmp-1+inum) = 'V'//saux07
        zk16(jnocm2+2*(inum-1)) = 'V'//saux07
        zk16(jnocm2+2*(inum-1)+1) = zk16(jnovar+inum-1)
    end do
    call cescrm('V', chabis, typech, 'VARI_R', nredva,&
                zk8(jnocmp), chamns)
    call jeveuo(chabis//'.CESD', 'L', jcesdb)
    call jeveuo(chabis//'.CESL', 'L', jceslb)
    call jeveuo(chabis//'.CESV', 'L', vr=cesvb)
!
!   CREATION DU CHAMP A IMPRIMER
    do 60, inum = 2,nbre
        typaff=desc(1+3+(inum-1)*2)
        nbzone=desc(1+4+(inum-1)*2)
!
        compor=vale(1+nbcomp*(inum-1))
        if (compor .eq. 'ELAS') goto 60
!
        call lcinfo(compor, numlc, nbvari)
        nomtmp=vale(1+nbcomp*(inum-1)+1)
        read(nomtmp,'(I16)') nbvar2
        if (nbvari .ne. nbvar2) then
            lcompo(1)=compor
            lcompo(2)=vale(1+nbcomp*(inum-1)+2)
            call lccree(2, lcompo, comco2)
            call lcinfo(comco2, numlc, nbvari)
!
        else
            comco2=compor
        endif
!
        call lcvari(comco2, nbvari, zk16(jmnova))
        call codent(inum, 'G', saux08)
        chcorr=base//saux08
        call jeveuo(chcorr, 'L', jcorva)
!
        if (typaff .ne. 1) then
!
!           NOMBRE DE MAILLES POUR LE COMPORTEMENT CONSIDERE
            nbmail=zi(jconi2+nbzone)-zi(jconi2+nbzone-1)
            posit = zi(jconi2+nbzone-1)
        else
            nbmail=nbma2
            posit=0
        endif
!
        do ima = 1,nbmail
            if (typaff .ne. 1) then
                ima2=zi(jconi1+posit+ima-2)
            else
                ima2=ima
            endif
            nbpt = zi(jcesd-1+5+4* (ima2-1)+1)
            nbsp = zi(jcesd-1+5+4* (ima2-1)+2)
            nbcmpc = zi(jcesd-1+5+4* (ima2-1)+3)
            do ipt = 1,nbpt
                do isp = 1,nbsp
                    do icmp = 1,nbcmpc
                        call cesexi('C', jcesd, jcesl, ima2, ipt,&
                                    isp, icmp, iad)
                        if (iad .gt. 0) then
                            icmp2=zi(jcorva+icmp-1)
                            call cesexi('C', jcesdb, jceslb, ima2, ipt,&
                                        isp, icmp2, iad2)
                            ASSERT(iad2.lt.0)
                            cesvb(1-1-iad2)=cesv(iad)
                            zl(jceslb-1-iad2)=.true.
                        endif
                    end do
                end do
            end do
        end do
        call lcdiscard(comco2)
60  continue
!
    nomres=nochmd(1:8)//'VARI_ELGA_NOMME'
    call cescel(chabis, ligrel, ' ', ' ', 'OUI',&
                ima2, 'V', chater, 'F', codret)
    call irceme(ifi, nomres, chater, typech, modele,&
                nbcmp, nomcmp, noetcm, partie, numpt,&
                instan, numord, nbmaec, limaec, carael,&
                codret)
!
9999  continue
!
! --- MENAGE
    call detrsd('CHAM_ELEM_S', chamns)
    call detrsd('CHAM_ELEM_S', chabis)
    call detrsd('CHAM_ELEM_S', chater)
    call jedetr(chnova)
    call jedetr(chmano)
    call jedetr(base//'.NOCMPTMP')
    call jedetr(base//'.NOCMP')
    do inum = 2,nbre
        call codent(inum, 'G', saux08)
        call jedetr(base//saux08)
    end do
!
    call jedema()
!
end subroutine
