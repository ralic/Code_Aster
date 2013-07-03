subroutine fetfac(lmat, matas, idd, nprec, nbsd,&
                  matass, sdfeti, nbsdf, base, infofe)
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  RECHERCHE DES MODES DE CORPS RIGIDES ET REM
!      PLISSAGE DES OBJETS JEVEUX AD HOC POUR FETI.
!
!      IN   LMAT: IN  : DESCRIPTEUR DE LA MATRICE DE RIGIDITE LOCALE
!      IN  MATAS: K19 : NOM DE LA MATRICE DE RIGIDITE LOCALE
!      IN    IDD: IN  : NUMERO DU SOUS-DOMAINE
!      IN  NPREC: IN  : PARAMETRE STIPULANT LA QUASI-NULLITE DU PIVOT
!      IN   NBSD: IN  : NBRE DE SOUS-DOMAINES
!      IN MATASS: K19 : MATRICE DE RIGIDITE GLOBALE
!      IN SDFETI: K24 : STRUCTURE DE DONNEES SD_FETI
!   IN/OUT NBSDF:  IN : NBRE DE SOUS-DOMAINES FLOTTANTS
!      IN   BASE:  K1 : BASE SUR LAQUELLE EST CREE LA MATR_ASSE
!      IN INFOFE: K24 : VECTEUR DE MONITORING POUR FETI
!----------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
!
#include "asterfort/fettsd.h"
#include "asterfort/infniv.h"
#include "asterfort/iunifi.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/tldlg2.h"
#include "asterfort/utimsd.h"
#include "asterfort/wkvect.h"
    integer :: lmat, idd, nbsd, nprec, nbsdf
    character(len=1) :: base
    character(len=19) :: matas, matass
    character(len=24) :: sdfeti, infofe
!
!
! DECLARATION VARIABLES LOCALES
    integer :: ifm, niv, nbmocr, i, jadr, kadr, neq, naux, ifr, ifetf, iexist
    integer :: irefa, ibid
    character(len=8) :: nomsd, k8bid
    character(len=19) :: vemocr, veinpn, k19bid
    character(len=24) :: nomsdf, nomsdp, nomsda, nomsdr, k24num
    logical :: lbid
!
! CORPS DU PROGRAMME
    call jemarq()
!
! RECUPERATION DU NIVEAU D'IMPRESSION
    call infniv(ifm, niv)
!
! INIT. NOMS OBJETS JEVEUX
    nomsdp=matass//'.FETP'
    nomsdr=matass//'.FETR'
    nomsda=sdfeti(1:19)//'.FETA'
!
! NOM DU SOUS-DOMAINE
    call jenuno(jexnum(nomsda, idd), nomsd)
! NBRE DE DDLS
    neq=zi(lmat+2)
    nomsdf=matass//'.FETF'
!
    call jeexin(nomsdf, iexist)
    if (iexist .eq. 0) then
! MATASS.FETF
        call wkvect(nomsdf, base(1:1)//' V I', nbsd, ifetf)
        do 10 i = 1, nbsd
            zi(ifetf+i-1)=0
10      continue
!
! MATASS.FETP/.FETR
        call jecrec(nomsdp, base(1:1)//' V I', 'NO', 'DISPERSE', 'VARIABLE',&
                    nbsd)
        call jecrec(nomsdr, base(1:1)//' V R', 'NO', 'DISPERSE', 'VARIABLE',&
                    nbsd)
    else
        call jeveuo(nomsdf, 'E', ifetf)
    endif
!
! VEMOCR: MATRICE DES MODES DE CORPS RIGIDES
    vemocr = '&&FETFAC.FETI.MOCR'
    veinpn = '&&FETFAC.FETI.INPN'
! RECHERCHE DES MODES DE CORPS RIGIDES DANS LA MATRICE LMAT
    call tldlg2(lmat, nprec, nbmocr, vemocr, 'FETI',&
                veinpn)
!
! RANGEMENT DANS LES STRUCTURES DE DONNEES
    if (nbmocr .ne. 0) then
! PRESENCE DE MODES DE CORPS RIGIDES
        nbsdf=nbsdf+1
! MATASS.FETF
        zi(ifetf+idd-1)=nbmocr
! MATASS.FETP
        call jecroc(jexnom(nomsdp, nomsd))
        call jeecra(jexnom(nomsdp, nomsd), 'LONMAX', nbmocr, k8bid)
        call jeveuo(jexnom(nomsdp, nomsd), 'E', jadr)
! VEINPN: VECTEUR DES INDICES DE PIVOTS QUASI NULS
        call jeveuo(veinpn, 'L', kadr)
        do 30 i = 1, nbmocr
            zi(jadr+i-1)=zi(kadr+i-1)
30      continue
! MATASS.FETR
        naux=neq*nbmocr
        call jeveuo(vemocr, 'L', kadr)
        call jecroc(jexnom(nomsdr, nomsd))
        call jeecra(jexnom(nomsdr, nomsd), 'LONMAX', naux, k8bid)
        call jeveuo(jexnom(nomsdr, nomsd), 'E', jadr)
        do 40 i = 1, naux
            zr(jadr+i-1)=zr(kadr+i-1)
40      continue
    endif
!
! NOM DU NUME_DDL ASSOCIE A LA MATRICE LOCALE POUR IMPRESSIONS
! FICHIER SI INFO_FETI(15:15)='T'
    call jeveuo(matas//'.REFA', 'L', irefa)
    k24num=zk24(irefa+1)
    call fettsd(infofe, idd, naux, ibid, sdfeti(1:19),&
                k24num, nbmocr, kadr, ibid, ifm,&
                lbid, ibid, ibid, ibid, k19bid,&
                10, lbid)
! MONITORING
    ifr=iunifi('MESSAGE')
    if (infofe(1:1) .eq. 'T') write(ifm, *)'<FETI/FETFAC> SD: ', idd, ' ', matas(1:19)
    if (infofe(3:3) .eq. 'T') call utimsd(ifr, 2, .false., .true., matas(1:19),&
                                          1, ' ')
    if ((infofe(3:3).eq.'T') .and. (idd.eq.nbsd)) call utimsd(ifr, 2, .false., .true.,&
                                                              matass(1:19), 1, ' ')
!
    call jedetr('&&FETFAC.FETI.MOCR')
    call jedetr('&&FETFAC.FETI.INPN')
    call jedema()
end subroutine
