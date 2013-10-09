subroutine pjma2p(ndim, moa2, ma2p, corres)
! person_in_charge: jacques.pellet at edf.fr
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
    implicit none
! ----------------------------------------------------------------------
! COMMANDE PROJ_CHAMP / METHODE='ECLA_PG'
!
! BUT :  CREER UN MAILLAGE (MA2P) DONT LES NOEUDS SONT POSITIONNES SUR
!        LES POINTS DE GAUSS D'UN MODELE (MOA2).
! REMARQUE : ON UTILISE L'OPTION COOR_ELGA CE QUI CORRESPOND EN GENERAL
!            A LA FAMILLE DE POINTS DE GAUS "RIGI"
! ----------------------------------------------------------------------
! IN NDIM : 2/3 : DIMENSION DES MAILLES A PROJETER
! IN MOA2 : MODELE "2"
! IN/JXOUT MA2P : MAILLAGE 2 PRIME (OBTENU A PARTIR DES PG DU MODELE 2)
! IN/JXVAR : ON COMPLETE LA SD_CORRESP_2_MAILLA AVEC L'OBJET .PJEL
! ----------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exlim1.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utflmd.h"
#include "asterfort/utmamo.h"
#include "asterfort/wkvect.h"
!
    character(len=16) :: corres
    character(len=8) :: ma2p, moa2
    integer :: ndim
! ----------------------------------------------------------------------
    integer :: ntgeo, ipo, ipg, nuno2
    integer :: ibid, nbno2p, nno2, ino2p
    integer :: k, j1, j4, ipoi1, ipy5, ipy13
    integer :: nbma, nbpt, nbcmp, nbmamo
    integer :: ima, ipt, icmp, iad, iadime
    integer :: jtypma, jdimt, jpo2, nbtrou, jlitr
    integer :: jcesd, jcesl, jcesv, iatypm
    character(len=8) :: nom, mail2, noma
    character(len=19) :: chamg, ces, chgeom, ligrel
    character(len=24) :: coodsc, limato, litrou
    real(kind=8) :: xmoy(3), rayo
! ----------------------------------------------------------------------
    call jemarq()
!
!     -- RECUPERATION DU NOM DU MAILLAGE 2
    call dismoi('NOM_MAILLA', moa2, 'MODELE', repk=mail2)
    call jeveuo(mail2//'.TYPMAIL', 'L', jtypma)
!
!     -- RECUPERATION DU CHAMP DE COORDONNEES DU MAILLAGE 2
    chgeom=mail2//'.COORDO'
!
!
!     -- ON REDUIT LE LIGREL DE MOA2 SUR MAILLES DE DIMENSION NDIM :
    ligrel='&&PJMA2P.LIGREL'
    limato='&&PJMA2P.LIMATOT'
    litrou='&&PJMA2P.LITROU'
!     -- ON NE CONSERVE QUE LES MAILLES AFFECTEES :
    call utmamo(moa2, nbmamo, limato)
!     -- ON NE CONSERVE QUE LES MAILLES DE DIMENSION NDIM :
    call utflmd(mail2, limato, nbmamo, ndim, ' ',&
                nbtrou, litrou)
    ASSERT(nbtrou.gt.0)
    call jeveuo(litrou, 'L', jlitr)
    call exlim1(zi(jlitr), nbtrou, moa2, 'V', ligrel)
    call jedetr(limato)
    call jedetr(litrou)
!
!
!
!     1.  CALCUL DU CHAMP DE COORDONNEES DES ELGA (CHAMG):
!     -------------------------------------------------------
    chamg='&&PJMA2P.PGCOOR'
    call calcul('S', 'COOR_ELGA', ligrel, 1, chgeom,&
                'PGEOMER', 1, chamg, 'PCOORPG ', 'V',&
                'OUI')
!
!     -- TRANSFORMATION DE CE CHAMP EN CHAM_ELEM_S
    ces='&&PJMA2P.PGCORS'
    call celces(chamg, 'V', ces)
!
    call jeveuo(ces//'.CESD', 'L', jcesd)
    call jeveuo(ces//'.CESL', 'L', jcesl)
    call jeveuo(ces//'.CESV', 'E', jcesv)
    nbma=zi(jcesd-1+1)
!
!     2.1 MODIFICATION DES COORDONNEES DE CERTAINS PG (PYRAM/FPG27)
!         CAR CES POINTS DE GAUSS SONT EN "DEHORS" DES PYRAMIDES
!     ----------------------------------------------------------------
    call jenonu(jexnom('&CATA.TM.NOMTM', 'PYRAM5'), ipy5)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'PYRAM13'), ipy13)
    do 80 ima = 1, nbma
        if (zi(jtypma-1+ima) .eq. ipy5 .or. zi(jtypma-1+ima) .eq. ipy13) then
            nbpt=zi(jcesd-1+5+4*(ima-1)+1)
            if (nbpt .eq. 27) then
                do 20 icmp = 1, 3
                    xmoy(icmp)=0.d0
 20             continue
!           -- XMOY : CENTRE DE LA PYRAMIDE :
                do 40 ipt = 1, 15
                    do 30 icmp = 1, 3
                        call cesexi('C', jcesd, jcesl, ima, ipt,&
                                    1, icmp, iad)
                        ASSERT(iad.gt.0)
                        xmoy(icmp)=xmoy(icmp)+zr(jcesv-1+iad)
 30                 continue
 40             continue
                do 50 icmp = 1, 3
                    xmoy(icmp)=xmoy(icmp)/15
 50             continue
!
!           -- ON "RAMENE" LES 12 DERNIERS PG VERS LE CENTRE (10%) :
                do 70 ipt = 16, 27
                    do 60 icmp = 1, 3
                        call cesexi('C', jcesd, jcesl, ima, ipt,&
                                    1, icmp, iad)
                        ASSERT(iad.gt.0)
                        rayo=zr(jcesv-1+iad)-xmoy(icmp)
                        zr(jcesv-1+iad)=zr(jcesv-1+iad)-0.6d0*rayo
 60                 continue
 70             continue
            endif
        endif
 80 end do
!
!
!     2. CALCUL DE NBNO2P : NOMBRE DE NOEUDS (ET DE MAILLES) DE MA2P
!        CALCUL DE '.PJEF_EL'
!     ----------------------------------------------------------------
    nbno2p=0
!
!     NBMA*27*2 = NB MAX DE MAILLES * NB DE PG MAX PAR MAILLE * 2
!     ON CREE UN TABLEAU, POUR CHAQUE JPO2, ON STOCKE DEUX VALEURS :
!      * LA PREMIERE VALEUR EST LE NUMERO DE LA MAILLE
!      * LA DEUXIEME VALEUR EST LE NUMERO DU PG DANS CETTE MAILLE
    call wkvect(corres//'.PJEF_EL', 'V V I', nbma*27*2, jpo2)
!
    ipo=1
    do 100 ima = 1, nbma
        call jeveuo(jexnum('&CATA.TM.TMDIM', zi(jtypma-1+ima)), 'L', jdimt)
        if (zi(jdimt) .eq. ndim) then
            nbpt=zi(jcesd-1+5+4*(ima-1)+1)
            if (nbpt .eq. 0) goto 100
            call jenuno(jexnum(mail2//'.NOMMAI', ima), noma)
            do 90 ipg = 1, nbpt
                zi(jpo2-1+ipo)=ima
                zi(jpo2-1+ipo+1)=ipg
                ipo=ipo+2
 90         continue
            nbno2p=nbno2p+nbpt
        endif
100 end do
!
!
!     3. CREATION DU .DIME DU NOUVEAU MAILLAGE
!        IL Y A AUTANT DE MAILLES QUE DE NOEUDS
!        TOUTES LES MAILLES SONT DES POI1
!     --------------------------------------------------
    call wkvect(ma2p//'.DIME', 'V V I', 6, iadime)
    zi(iadime-1+1)=nbno2p
    zi(iadime-1+3)=nbno2p
    zi(iadime-1+6)=3
!
!
!     4. CREATION DU .NOMNOE ET DU .NOMMAI DU NOUVEAU MAILLAGE
!     ---------------------------------------------------------
    call jecreo(ma2p//'.NOMNOE', 'V N K8')
    call jeecra(ma2p//'.NOMNOE', 'NOMMAX', nbno2p)
    call jecreo(ma2p//'.NOMMAI', 'V N K8')
    call jeecra(ma2p//'.NOMMAI', 'NOMMAX', nbno2p)
!
!
    nom(1:1)='N'
    do 110 k = 1, nbno2p
        call codent(k, 'G', nom(2:8))
        call jecroc(jexnom(ma2p//'.NOMNOE', nom))
110 end do
    nom(1:1)='M'
    do 120 k = 1, nbno2p
        call codent(k, 'G', nom(2:8))
        call jecroc(jexnom(ma2p//'.NOMMAI', nom))
120 end do
!
!
!
!     5. CREATION DU .CONNEX ET DU .TYPMAIL DU NOUVEAU MAILLAGE
!     ----------------------------------------------------------
    call jecrec(ma2p//'.CONNEX', 'V V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbno2p)
    call jeecra(ma2p//'.CONNEX', 'LONT', nbno2p, ' ')
    call jeveuo(ma2p//'.CONNEX', 'E', ibid)
!
    call wkvect(ma2p//'.TYPMAIL', 'V V I', nbno2p, iatypm)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'POI1'), ipoi1)
!
    nuno2=0
    do 130 ima = 1, nbno2p
        zi(iatypm-1+ima)=ipoi1
        nno2=1
        call jecroc(jexnum(ma2p//'.CONNEX', ima))
        call jeecra(jexnum(ma2p//'.CONNEX', ima), 'LONMAX', nno2)
        nuno2=nuno2+1
        zi(ibid-1+nuno2)=nuno2
130 end do
!
!
!
!     -- CREATION DU .REFE DU NOUVEAU MAILLAGE
!     --------------------------------------------------
    call wkvect(ma2p//'.COORDO    .REFE', 'V V K24', 4, j4)
    zk24(j4)='MA2P'
!
!
!     -- CREATION DE COORDO.VALE DU NOUVEAU MAILLAGE
!     --------------------------------------------------
    call wkvect(ma2p//'.COORDO    .VALE', 'V V R', 3*nbno2p, j1)
!
    ino2p=0
    do 160 ima = 1, nbma
        nbpt=zi(jcesd-1+5+4*(ima-1)+1)
        nbcmp=zi(jcesd-1+5+4*(ima-1)+3)
        if (nbpt .eq. 0) goto 160
        call jeveuo(jexnum('&CATA.TM.TMDIM', zi(jtypma-1+ima)), 'L', jdimt)
!
        if (zi(jdimt) .eq. ndim) then
            ASSERT(nbcmp.ge.3)
            do 150 ipt = 1, nbpt
                ino2p=ino2p+1
                do 140 icmp = 1, 3
                    call cesexi('C', jcesd, jcesl, ima, ipt,&
                                1, icmp, iad)
                    if (iad .gt. 0) then
                        zr(j1-1+3*(ino2p-1)+icmp)=zr(jcesv-1+iad)
                    endif
140             continue
150         continue
        endif
160 end do
    ASSERT(ino2p.eq.nbno2p)
!
!
!     -- CREATION DU .DESC DU NOUVEAU MAILLAGE
!     --------------------------------------------------
    coodsc=ma2p//'.COORDO    .DESC'
!
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
!
    call jedema()
end subroutine
