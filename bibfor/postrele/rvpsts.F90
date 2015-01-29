subroutine rvpsts(iocc, sdlieu, sdeval, sdmoye)
    implicit none
!
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
!
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/rvchve.h"
!
    character(len=19) :: sdeval
    character(len=24) :: sdmoye, sdlieu
    integer :: iocc
!
!**********************************************************************
!
!  OPERATION REALISEE
!  ------------------
!
!     OPERATION MOYENNE DU POST-TRAITEMENT POUR UN LIEU
!
!  ARGUMENTS EN ENTREE
!  -------------------
!
!     SDLIEU : SD DU LIEU TRAITEE
!     SDEVAL : SD DE L' EVALUATION DE LA QUANTITE SUR CE LIEU
!
!  ARGUMENTS EN SORTIE
!  -------------------
!
!     SDMOYE : NOM DE LA SD CONSERVANT LA SOMME ::= RECORD
!
!      .VALE : XD V R, UN OC PAR OC DE .ABSC DU LIEU
!                      DIM(V) = NB_CMP_SOMME*NB_COUCHE*NB_SS_PT
!      .NOCP : S V K8  NOM DES CMP
!
!**********************************************************************
!
!  FONCTIONS EXTERNES
!  ------------------
!
!
!  -----------------------------------------
!
!
!  ---------------------------------
!
!  VARIABLES LOCALES
!  -----------------
!
    character(len=24) :: nabsc, ntab
    character(len=24) :: nsocp, nsova
    character(len=4) :: docul, docu
!
    integer :: avale,   amoye, aabsc, atab, adr1, adr2
    integer :: deb, fin, lmoye, nbcp, nbco, nbsp, nboc, nbsgt, nres, nmom
    integer :: l1, l2, l3, l5, ioc, i, j, k, l, n, nbpt
!
    real(kind=8) :: t1, t(3), x, y, z, xpi=0.d0, ypi=0.d0, zpi=0.d0, rx, ry, rz, mx, my, mz
    real(kind=8) :: zero
!
!==================== CORPS DE LA ROUTINE =============================
!
!-----------------------------------------------------------------------
    integer :: lll
    integer, pointer :: pnbn(:) => null()
    integer, pointer :: padr(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    zero = 0.0d0
!
    ntab = '&&RVPSTM.VECT.INTER'
    nabsc = sdlieu(1:19)//'.ABSC'
    nsova = sdmoye(1:19)//'.VALE'
    nsocp = sdmoye(1:19)//'.NOCP'
!
    call getvtx('ACTION', 'RESULTANTE', iocc=iocc, nbval=0, nbret=nres)
    call getvtx('ACTION', 'MOMENT', iocc=iocc, nbval=0, nbret=nmom)
    nres = -nres
    nmom = -nmom

!   -- si on utilise RESULTANTE ou MOMENT, on verifie que REPERE est correct :
    if (nres.gt.0 .or. nmom.gt.0) then
        call rvchve(iocc,xpi,ypi,zpi)
    endif
!
    call jelira(sdlieu(1:19)//'.REFE', 'DOCU', cval=docul)
    call jelira(sdeval//'.VALE', 'DOCU', cval=docu)
    call jelira(nabsc, 'NMAXOC', nboc)
    call jelira(sdeval//'.NOCP', 'LONMAX', nbcp)
    call jecrec(nsova, 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                nboc)
!
    call jeveuo(sdeval//'.PNCO', 'L', i)
    nbco = zi(i)
    call jeveuo(sdeval//'.PNSP', 'L', i)
    nbsp = zi(i)
!
    call jeveuo(sdeval//'.VALE', 'L', avale)
    call jeveuo(sdeval//'.PADR', 'L', vi=padr)
    call jeveuo(sdeval//'.PNBN', 'L', vi=pnbn)
!
    l2 = nbsp*nbcp
    l1 = nbco*l2
    l3 = 2*l1
!
    if (nmom .eq. 0) then
        call wkvect(nsocp, 'V V K8', nbcp, deb)
        call jeveuo(sdeval//'.NOCP', 'L', fin)
        do 15, ioc = 1, nbcp, 1
        zk8(deb + ioc-1) = zk8(fin + ioc-1)
15      continue
        lmoye = nbcp*nbco*nbsp
    else
        call wkvect(nsocp, 'V V K8', 6, deb)
        zk8(deb-1 + 1) = 'RESULT_X'
        zk8(deb-1 + 2) = 'RESULT_Y'
        zk8(deb-1 + 3) = 'RESULT_Z'
        zk8(deb-1 + 4) = 'MOMENT_X'
        zk8(deb-1 + 5) = 'MOMENT_Y'
        zk8(deb-1 + 6) = 'MOMENT_Z'
        lmoye = 6*nbco*nbsp
    endif
!
    nbsgt = 0
    do 99, ioc = 1, nboc, 1
    call jelira(jexnum(nabsc , ioc), 'LONMAX', fin)
    nbsgt = max(nbsgt,fin)
    99 end do
    call wkvect(ntab, 'V V R', l3*(nbsgt+1), atab)
!
    fin = 0
!
    do 100, ioc = 1, nboc, 1
!
    call jecroc(jexnum(nsova, ioc))
    call jeecra(jexnum(nsova, ioc), 'LONMAX', lmoye)
    call jeveuo(jexnum(nsova, ioc), 'E', amoye)
    call jelira(jexnum(nabsc, ioc), 'LONMAX', nbpt)
    call jeveuo(jexnum(nabsc, ioc), 'L', aabsc)
!
    nbsgt = nbpt - 1
    deb = fin + 1
    fin = deb + nbsgt
!
    if ((docu .eq. 'CHLM') .or. (docul .ne. 'LSTN')) then
!
        fin = fin - 1
!
    endif
!
!     /* VECTEUR INTER IE : PASSAGE A UN SS_CHAM_NO */
!
    if ((docul .eq. 'LSTN') .or. (docu .eq. 'CHNO')) then
!
        do 200, i = 1, nbpt, 1
!
        adr1 = padr(1+ deb + i-2)
        n = pnbn(1+ deb + i-2)
!
        do 210, j = 1, nbco, 1
!
        l5 = (j-1)*n*l2
!
        do 220, k= 1, l2, 1
!
        t1 = 0.0d0
!
        lll = 0
        do 230, l = 1, n, 1
!
        if (zr(avale-1+adr1+l5+(l-1)*l2+k-1) .eq. r8vide()) goto 230
        lll = lll + 1
        t1 = t1 + zr(avale-1+adr1+l5+(l-1)*l2+k-1)
!
230      continue
!
        if (lll .eq. 0) then
            t1 = 0.d0
        else
            t1 = t1/lll
        endif
!
        adr2 = (i-1)*l1 + (j-1)*l2 + k - 1
!
        zr(atab + adr2) = t1
!
220      continue
!
210      continue
!
200      continue
!
    else
!
        adr1 = padr(deb)
        do 241, i = 1, nbco, 1
        l5 = (i-1)*l2
        do 242, j = 1, l2, 1
        if (zr(avale+adr1+2*l5+j-2) .eq. r8vide()) then
            zr(atab + l5 + j-1) = 0.d0
        else
            zr(atab + l5 + j-1) = zr(avale+adr1+2*l5+j-2)
        endif
242      continue
241      continue
!
        do 240, i = 1, nbsgt-1, 1
!
        adr1 = padr(1+ deb + i-2)
!
        do 250, j = 1, nbco, 1
!
        l5 = (j-1)*l2 + i*l1
        adr2 = avale + adr1-1 + (j-1)*l3 + l2
!
        do 260, k= 1, l2, 1
!
        if (zr(adr2+k-1) .eq. r8vide() .and. zr(adr2+l2* (nbco*2-1)+k-1) .eq. r8vide()) then
            zr(atab+l5+k-1) = 0.d0
        else if (zr(adr2+k-1).eq.r8vide()) then
            zr(atab+l5+k-1) = zr(adr2+l2*(nbco*2-1)+k- 1)
            elseif (zr(adr2+l2*(nbco*2-1)+k-1).eq.r8vide()&
                        ) then
            zr(atab+l5+k-1) = zr(adr2 + k-1)
        else
            zr(atab+l5+k-1) = 0.5d0*( zr(adr2 + k-1) + zr(adr2+l2*(nbco*2-1)+k-1))
        endif
!
260      continue
!
250      continue
!
240      continue
!
        adr1 = avale + padr(1+ deb + nbsgt-2)-1
        adr2 = atab + nbsgt*l1
        do 243, j = 1, nbco, 1
        l5 = (j-1)*l2
        do 244, k = 1, l2, 1
        if (zr(adr1+(i-1)*l3+k-1) .eq. r8vide()) then
            zr(adr2 + l5 + k-1) = 0.d0
        else
            zr(adr2 + l5 + k-1) = zr(adr1+(i-1)*l3+k-1)
        endif
244      continue
243      continue
!
    endif
!
!
!     /* CALCUL DES SOMMES SUR LE SS_CHAM_NO */
!
    if (nmom .eq. 0) then
        do 110, i = 1, l1, 1
        t1 = zero
        do 120, j = 1, nbpt, 1
        t1 = t1 + zr(atab + (j-1)*l1 + i-1)
120      continue
        zr(amoye + i-1) = t1
110      continue
    else
        do 140, i = 1, l1, 1
        zr(amoye + i-1) = zero
140      continue
        call getvr8('ACTION', 'POINT', iocc=iocc, nbval=0, nbret=n)
        n = -n
        call getvr8('ACTION', 'POINT', iocc=iocc, nbval=n, vect=t,&
                    nbret=i)

!       -- point P : (x,y,z)
        x = t(1)
        y = t(2)
        if (n .eq. 2) then
            z = zero
        else
            z = t(3)
        endif
        call jeveuo(jexnum(sdlieu(1:19)//'.COOR', ioc), 'L', k)
        do 150, i = 1, nbco*nbsp, 1
        adr1 = (i-1)*nbcp
        adr2 = (i-1)*6
        do 151, j = 1, nbpt, 1
            l = (j-1)*3 + k-1
            l5 = (j-1)*l1

!           -- calcul du vecteur PM :
            xpi = zr(l+1) - x
            ypi = zr(l+2) - y
            zpi = zr(l+3) - z

!           -- changement de repere pour le vecteur PM :
            call rvchve(iocc,xpi,ypi,zpi)

!
            if (nmom .eq. 3) then
                rx = zr(atab + l5 + adr1 + 1-1)
                ry = zr(atab + l5 + adr1 + 2-1)
                rz = zr(atab + l5 + adr1 + 3-1)
                mx = zr(atab + l5 + adr1 + 4-1)
                my = zr(atab + l5 + adr1 + 5-1)
                mz = zr(atab + l5 + adr1 + 6-1)
            else
                rx = zr(atab + l5 + adr1 + 1-1)
                ry = zr(atab + l5 + adr1 + 2-1)
                rz = zero
                mx = zr(atab + l5 + adr1 + 3-1)
                my = zr(atab + l5 + adr1 + 4-1)
                mz = zero
            endif
!
            mx = ypi*rz - zpi*ry + mx
            my = zpi*rx - xpi*rz + my
            mz = xpi*ry - ypi*rx + mz
!
            zr(amoye+adr2+1-1) = zr(amoye+adr2+1-1) + rx
            zr(amoye+adr2+2-1) = zr(amoye+adr2+2-1) + ry
            zr(amoye+adr2+3-1) = zr(amoye+adr2+3-1) + rz
            zr(amoye+adr2+4-1) = zr(amoye+adr2+4-1) + mx
            zr(amoye+adr2+5-1) = zr(amoye+adr2+5-1) + my
            zr(amoye+adr2+6-1) = zr(amoye+adr2+6-1) + mz
151      continue
150      continue
    endif
!
    100 end do
!
    call jedetr(ntab)
!
    call jedema()
end subroutine
