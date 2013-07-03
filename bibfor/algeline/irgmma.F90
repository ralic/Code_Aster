subroutine irgmma(nomain, nomaou, nbmat, nummai, basz,&
                  nobj, nbel, versio)
    implicit none
#include "jeveux.h"
!
#include "asterfort/codent.h"
#include "asterfort/infniv.h"
#include "asterfort/irgmtb.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: nomain, nomaou
    integer :: nbmat, nummai(*), versio
    character(len=*) :: basz
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
!     TRANSFORME LE MAILLAGE "NOMAIN" EN UN MAILLAGE "NOMAOU"
!     LE MAILLAGE "NOMAOU" NE POSSEDE QUE DES MAILLES DE TYPE
!     POI1, SEG2, TRIA3, TETRA4 EN VERSION 1.0
!     + QUAD4, PENTA6, PYRAM5, HEXA8 EN VERSIO 1.2 (VOIR IRGMTB)
!     ------------------------------------------------------------------
    integer :: i, ima, nbma, nbmail, ifm, niv, ino, ima2, imav, iatyma, jrefe
    integer :: jtitr
    integer :: jtypm, jdime, jopt, jnpt, nbmac, jmail, im, jnumol
    logical :: logic
    character(len=1) :: base
    character(len=8) :: k8b, nomg, typm, typm2
    character(len=24) :: nommai, typmai, connex, nodime, nomnoe, cooval, coodsc
    character(len=24) :: cooref, titre, numold
    character(len=24) :: typmav, connev, nodimv, nomnov, coovav, coodsv, coorev
    character(len=24) :: valk(2)
    integer :: ind, numel, nbcr, nbp
    integer :: nbmmax
    parameter   (nbmmax = 9999999)
!     --- TABLEAU DE DECOUPAGE
    integer :: ntyele, maxel, maxno
    parameter (ntyele = 28)
    parameter (maxel  = 48)
    parameter (maxno  =  8)
    integer :: tdec(ntyele, maxel, maxno)
    integer :: typd(ntyele, 3)
    integer :: vali(3)
!     NBRE, POINTEURS, NOM D'OBJET POUR CHAQUE TYPE D'ELEMENT
    integer :: nbel(ntyele), jel(ntyele), impr
    character(len=24) :: nobj(ntyele)
!     ------------------------------------------------------------------
!
    call infniv(ifm, niv)
    call jemarq()
!
! --- INIT
    do 101 i = 1, ntyele
        nbel(i) = 0
        jel(i) = 0
101  end do
!
! --- TABLEAU DES INFOS DE DECOUPAGE
    call irgmtb(tdec, typd, versio)
!
    base = basz
!
    nomnov = nomain//'.NOMNOE         '
    typmav = nomain//'.TYPMAIL        '
    connev = nomain//'.CONNEX         '
    nodimv = nomain//'.DIME           '
    coovav = nomain//'.COORDO    .VALE'
    coodsv = nomain//'.COORDO    .DESC'
    coorev = nomain//'.COORDO    .REFE'
!
    nommai = nomaou//'.NOMMAI         '
    nomnoe = nomaou//'.NOMNOE         '
    typmai = nomaou//'.TYPMAIL        '
    connex = nomaou//'.CONNEX         '
    nodime = nomaou//'.DIME           '
    cooval = nomaou//'.COORDO    .VALE'
    coodsc = nomaou//'.COORDO    .DESC'
    cooref = nomaou//'.COORDO    .REFE'
    titre = nomaou//'           .TITR'
    numold = nomaou//'.NUMOLD         '
!
    call wkvect(titre, base//' V K80', 1, jtitr)
    zk80(jtitr) = 'MAILLAGE CREE PAR IRGMMA POUR GMSH'
!
    call jeveuo(typmav, 'L', jtypm)
    call jeveuo(nodimv, 'L', jdime)
    nbma = zi(jdime+3-1)
!
    logic = .false.
!
    if (nbmat .ne. 0) then
        nbmac = nbmat
        call wkvect('&&IRGMMA.NUME_MAILLE', 'V V I', nbmac, jmail)
        do 20 ima = 1, nbmac
            zi(jmail+ima-1) = nummai(ima)
20      continue
    else
        nbmac = nbma
        call wkvect('&&IRGMMA.NUME_MAILLE', 'V V I', nbmac, jmail)
        do 22 ima = 1, nbmac
            zi(jmail+ima-1) = ima
22      continue
    endif
!
! --- COMBIEN D'ELEMENTS DE CHAQUE TYPE VA-T-ON CREER ?
    do 10 im = 1, nbmac
        ima = zi(jmail+im-1)
!
        ind=zi(jtypm+ima-1)
        call jenuno(jexnum('&CATA.TM.NOMTM', ind), typm)
!
! ---    NUMEL = EN QUOI ON DECOUPE, NBCR = COMBIEN ON EN CREER
        numel = typd(ind,1)
        nbcr = typd(ind,2)
        if (numel .ne. 0) then
            nbel(numel)=nbel(numel)+nbcr
        else
            call u2mesk('A', 'ALGELINE_64', 1, typm)
        endif
10  end do
!
    nbmail = 0
    impr = 0
    do 102 i = 1, ntyele
        nbmail = nbmail + nbel(i)
!
        if (nobj(i) .ne. ' ') then
            call wkvect(nobj(i), 'V V I', max(1, nbel(i)), jel(i))
            if (niv .ge. 1) then
                call jenuno(jexnum('&CATA.TM.NOMTM', i), typm)
                call jenuno(jexnum('&CATA.TM.NOMTM', typd(i, 1)), typm2)
                nbcr=typd(i,2)
                nbp =typd(i,3)
                if (nbel(i) .gt. 0) then
                    if (impr .eq. 0) then
                        call u2mess('I', 'ALGELINE5_54')
                        impr=1
                    endif
                    valk(1) = typm
                    valk(2) = typm2
                    vali(1) = nbel(i)
                    vali(2) = nbcr
                    vali(3) = nbp
                    call u2mesg('I', 'ALGELINE5_55', 2, valk, 3,&
                                vali, 0, 0.d0)
                endif
            endif
        endif
102  end do
!
    call wkvect(numold, 'V V I', max(1, nbmail), jnumol)
!
    call jedupo(nodimv, base, nodime, logic)
    call jedupo(nomnov, base, nomnoe, logic)
    call jedupo(coovav, base, cooval, logic)
    call jedupo(coodsv, base, coodsc, logic)
    call jedupo(coorev, base, cooref, logic)
!
    call jeveuo(cooref, 'E', jrefe)
    zk24(jrefe) = nomaou
!
    call jeveuo(nodime, 'E', jdime)
    zi(jdime+3-1) = nbmail
!
! ----------------------------------------------------------------------
!     LE '.NOMMAI' ET LE '.CONNEX'
! ----------------------------------------------------------------------
    call jecreo(nommai, base//' N K8')
    call jeecra(nommai, 'NOMMAX', nbmail, ' ')
!
    call wkvect(typmai, base//' V I', nbmail, iatyma)
!
    call jecrec(connex, base//' V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbmail)
!#MC  1*NBMAIL NE SUFFIT PAS ?
    call jeecra(connex, 'LONT', ntyele*nbmail, ' ')
!
    do 103 i = 1, ntyele
        nbel(i) = 0
103  end do
    imav = 0
!
    do 100 im = 1, nbmac
        ima = zi(jmail+im-1)
!
        ind=zi(jtypm+ima-1)
        call jenuno(jexnum('&CATA.TM.NOMTM', ind), typm)
        call jeveuo(jexnum(connev, ima), 'L', jopt)
!
! ---    NUMEL = EN QUOI ON DECOUPE, NBCR = COMBIEN ON EN CREER
!        NBP = NBRE DE POINTS PAR ELEMENTS CREES
        numel = typd(ind,1)
        nbcr = typd(ind,2)
        nbp = typd(ind,3)
        do 110 i = 1, nbcr
            imav = imav + 1
            if (imav .gt. nbmmax) then
                call codent(nbmmax, 'G', k8b)
                call u2mesk('F', 'ALGELINE_65', 1, k8b)
            endif
            nomg = 'M       '
            call codent(imav, 'G', nomg(2:8))
            call jecroc(jexnom( nommai, nomg ))
!
            call jenonu(jexnom(nommai, nomg), ima2)
            zi(iatyma-1+ima2) = numel
!    STOCKAGE DU NUMERO DE LA MAILLE INITIALE DANS NUMOLD POUR IRGMCE
            zi(jnumol-1+ima2)=ima
!
            call jeecra(jexnum(connex, ima2), 'LONMAX', nbp, k8b)
            call jeveuo(jexnum(connex, ima2), 'E', jnpt)
            do 115 ino = 1, nbp
                zi(jnpt-1+ino) = zi(jopt-1+tdec(ind,i,ino))
115          continue
            nbel(numel) = nbel(numel) + 1
            zi(jel(numel)-1+nbel(numel)) = imav
110      continue
!
100  end do
!
    call jedetr('&&IRGMMA.NUME_MAILLE')
!
    call jedema()
!
end subroutine
