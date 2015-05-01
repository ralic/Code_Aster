subroutine pjrisp(moa2, masp, corres, noca)
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
!
! --------------------------------------------------------------------------------------------------
!
!           COMMANDE PROJ_CHAMP / METHODE='SOUS_POINT_RIGI'
!
!   créer un maillage dont les noeuds sont positionnés sur les sous-points de gauss
!   d'un modele (moa2) pour la famille de points RIGI
!
! --------------------------------------------------------------------------------------------------
!
!   in :
!       moa2    : modele "2".
!       masp    : nom du maillage des points de Gauss. La SD correspondante est vide.
!       corres  : nom de l'objet qui contient les données de correspondance des mailles.
!       noca    : nom du CARA_ELEM.
!
!   out :
!       masp    : SD contenant le maillage de POI1 correspondant aux points de Gauss
!       corres  : création de l'objet corres.PJEF_SP
!
! --------------------------------------------------------------------------------------------------
!
    implicit   none
    character(len=8) :: masp, moa2, noca
    character(len=16) :: corres
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/cesvar.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ntgeo, ipo, ipg, nuno2
    integer :: ibid, nbnosp, nno2, ino2p
    integer :: k, j1, j4, ipoi1
    integer :: nbma, nbpt, nbsp, nbcmp
    integer :: ima, ipt, isp, icmp, iad, iadime
    integer :: jtypma, jpo2
    integer :: jcesd, jcesl, jcesv, iatypm
    integer :: nchi, nbpgmx, nbspmx
    character(len=8) :: nom, mail2, lpain(6)
    character(len=19) :: chamg, ces, chgeom, ligrel
    character(len=24) :: coodsc
    character(len=24) :: lchin(6)
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!   récuperation du nom du maillage 2
    call dismoi('NOM_MAILLA', moa2, 'MODELE', repk=mail2)
    call jeveuo(mail2//'.TYPMAIL', 'L', jtypma)
!
!   Récuperation du champ de coordonnées du maillage 2
    chgeom=mail2//'.COORDO'
!
    ligrel = moa2//'.MODELE'
!
! --------------------------------------------------------------------------------------------------
!   Calcul du champ de coordonnées des ELGA (chamg):
    nchi=6
    lchin(1)=chgeom(1:19)
    lpain(1)='PGEOMER'
    lchin(2)=noca//'.CARORIEN'
    lpain(2)='PCAORIE'
    lchin(3)=noca//'.CAFIBR'
    lpain(3)='PFIBRES'
    lchin(4)=noca//'.CANBSP'
    lpain(4)='PNBSP_I'
    lchin(5)=noca//'.CARCOQUE'
    lpain(5)='PCACOQU'
    lchin(6)=noca//'.CARGEOPO'
    lpain(6)='PCAGEPO'
    chamg='&&PJRISP.PGCOOR'
    call cesvar(noca, ' ', ligrel, chamg)
    call calcul('S', 'COOR_ELGA', ligrel, nchi, lchin,&
                lpain, 1, chamg, 'PCOORPG', 'V',&
                'OUI')
!   chamg : 4 composantes X,Y,Z,W
!   Transformation en CHAM_ELEM_S
    ces='&&PJRISP.PGCORS'
    call celces(chamg, 'V', ces)
!
    call jeveuo(ces//'.CESD', 'L', jcesd)
    call jeveuo(ces//'.CESL', 'L', jcesl)
    call jeveuo(ces//'.CESV', 'E', jcesv)
    nbma=zi(jcesd-1+1)
!
! --------------------------------------------------------------------------------------------------
!   Calcul de nbnosp : nombre de noeuds (et de mailles) de masp
    nbnosp=0
    nbpgmx = zi(jcesd-1+3)
    nbspmx = zi(jcesd-1+4)
!
!   On crée un tableau :
!      * la premiere valeur est le numero de la maille
!      * la deuxieme valeur est le numero du pg dans cette maille
!      * la troisieme valeur est le numero du sous-point
!   Dimension : (NBMA*NBPGMX*NBSPMX)*3 = (NB DE MAILLES * NB DE PG MAX  * NB DE SP MAX) * 3
!
    call wkvect(corres//'.PJEF_SP', 'V V I', nbma*nbpgmx*nbspmx*3, jpo2)
!
    ipo=1
    do ima=1,nbma
        nbpt=zi(jcesd-1+5+4*(ima-1)+1)
        nbsp=zi(jcesd-1+5+4*(ima-1)+2)
        if (nbsp .lt. 1) goto 100
        do ipg=1,nbpt
            do isp = 1, nbsp
                zi(jpo2-1+ipo)   = ima
                zi(jpo2-1+ipo+1) = ipg
                zi(jpo2-1+ipo+2) = isp
                ipo=ipo+3
            enddo
        enddo
        nbnosp = nbnosp + nbpt*nbsp
100     continue
    enddo
!
! --------------------------------------------------------------------------------------------------
!   Création du .DIME du nouveau maillage
!   Il y a autant de mailles que de noeuds car toutes les mailles sont des poi1
!
    call wkvect(masp//'.DIME', 'V V I', 6, iadime)
    zi(iadime-1+1)=nbnosp
    zi(iadime-1+3)=nbnosp
    zi(iadime-1+6)=3
!
!   Création du .nomnoe et du .nommai du nouveau maillage
    call jecreo(masp//'.NOMNOE', 'V N K8')
    call jeecra(masp//'.NOMNOE', 'NOMMAX', nbnosp)
    call jecreo(masp//'.NOMMAI', 'V N K8')
    call jeecra(masp//'.NOMMAI', 'NOMMAX', nbnosp)
!
    nom(1:1)='N'
    do k=1,nbnosp
        call codent(k, 'G', nom(2:8))
        call jecroc(jexnom(masp//'.NOMNOE', nom))
    enddo
    nom(1:1)='M'
    do k=1,nbnosp
        call codent(k, 'G', nom(2:8))
        call jecroc(jexnom(masp//'.NOMMAI', nom))
    enddo
!
! --------------------------------------------------------------------------------------------------
!   Création du .CONNEX et du .TYPMAIL du nouveau maillage
!
    call jecrec(masp//'.CONNEX', 'V V I', 'NU', 'CONTIG', 'VARIABLE', nbnosp)
    call jeecra(masp//'.CONNEX', 'LONT', nbnosp, ' ')
    call jeveuo(masp//'.CONNEX', 'E', ibid)
!
    call wkvect(masp//'.TYPMAIL', 'V V I', nbnosp, iatypm)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'POI1'), ipoi1)
!
    nuno2=0
    do ima=1,nbnosp
        zi(iatypm-1+ima)=ipoi1
        nno2=1
        call jecroc(jexnum(masp//'.CONNEX', ima))
        call jeecra(jexnum(masp//'.CONNEX', ima), 'LONMAX', nno2)
        nuno2=nuno2+1
        zi(ibid-1+nuno2)=nuno2
    enddo
!
! --------------------------------------------------------------------------------------------------
!   Création du .REFE du nouveau maillage
!
    call wkvect(masp//'.COORDO    .REFE', 'V V K24', 4, j4)
    zk24(j4)='MASP'
!
!   COORDO.VALE du nouveau maillage
    call wkvect(masp//'.COORDO    .VALE', 'V V R', 3*nbnosp, j1)
!
    ino2p=0
    do ima=1,nbma
        nbpt=zi(jcesd-1+5+4*(ima-1)+1)
        nbsp=zi(jcesd-1+5+4*(ima-1)+2)
        nbcmp=zi(jcesd-1+5+4*(ima-1)+3)
        if (nbpt .eq. 0) goto 160
        ASSERT(nbcmp.ge.3)
        do ipt = 1, nbpt
            do isp = 1, nbsp
                ino2p = ino2p+1
                do icmp = 1, 3
                    call cesexi('C', jcesd, jcesl, ima, ipt,&
                        isp, icmp, iad)
                    if (iad .gt. 0) then
                        zr(j1-1+3*(ino2p-1)+icmp)=zr(jcesv-1+iad)
                    endif
                enddo
            enddo
        enddo
160     continue
    enddo
    ASSERT(ino2p.eq.nbnosp)
!
! --------------------------------------------------------------------------------------------------
!   Création du .DESC du nouveau maillage
!
    coodsc=masp//'.COORDO    .DESC'
    call jenonu(jexnom('&CATA.GD.NOMGD', 'GEOM_R'), ntgeo)
    call jecreo(coodsc, 'V V I')
    call jeecra(coodsc, 'LONMAX', 3)
    call jeecra(coodsc, 'DOCU', cval='CHNO')
    call jeveuo(coodsc, 'E', iad)
    zi(iad)=ntgeo
    zi(iad+1)=-3
    zi(iad+2)=14
!
    call detrsd('CHAM_ELEM', chamg)
    call detrsd('CHAM_ELEM_S', ces)
    call jedema()
end subroutine
