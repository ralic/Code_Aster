subroutine rvechn(ssch19, sdlieu, sdeval)
    implicit none
!
#include "jeveux.h"
!
#include "asterc/r8vide.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/tremno.h"
#include "asterfort/u2mesg.h"
#include "asterfort/wkvect.h"
    character(len=19) :: ssch19, sdlieu, sdeval
!
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
!     ------------------------------------------------------------------
!     OPERATION D' EXTRACTION DU POST-TRAITEMENT SUR UNE LISTE DE NOEUDS
!     ------------------------------------------------------------------
! IN  SSCH19 : K : NOM DU SOUS CHAMP DE GRANDEUR
! IN  SDLIEU : K : NOM DE LA SD REPRESENTANT LE LIEU
! OUT SDEVAL : K : NOM DE LA SD SOUS_CHAMP_GD PRODUITES
!            :   :(DESCRIPTION : CF RVPSTE)
!     ------------------------------------------------------------------
!
!
!
    character(len=24) :: invale, inpadr, inpcmp, innoma, innugd, inpnco, inpnsp
    character(len=24) :: ouvale, oupadr, oupcmp, ouerre, ounoma, oupnbn, ounugd
    character(len=24) :: oupnb2
    character(len=24) :: nrefe, nabsc, ndesc, nnumnd, nindir, nnmail, oupnco
    character(len=24) :: oupnsp
    character(len=24) :: valk
    character(len=19) :: sdemno
    character(len=15) :: nrepma
    character(len=8) :: mailla
    character(len=4) :: docu
!
    integer :: aipadr, aipcmp, iocer, j, anmail, anuma, aipnco, aipnsp, aopnco
    integer :: aopnbn, aovale, aopadr, aopcmp, aoerre, ainugd, aivale, aopnsp
    integer :: arefe, adesc, nbcmp, i, ibid, anumnd, acmpgd, lpt, aopnb2
    integer :: nbmpst, nbnpst, nbocer, n, m, adrin, adrou, nbm, numm
    integer :: nbtcmp, sdnund, sdvacp, aindir, pt, nsp, nco, lmc, lcc, lsc, lms
    integer :: indi1, indi2
    integer :: vali, ilong, k, l, lnc, ncom, nspm
!
    logical :: trouve
    character(len=1) :: k1bid
!
    character(len=1) :: cbid
    data cbid/' '/
!
!==================== CORPS DE LA ROUTINE =============================
!
    call jemarq()
    nnumnd = '&&RVECHN.NUM.NOEUD.LISTE'
    sdemno = '&&RVECHN.SDEMNO   '
    invale = ssch19//'.VALE'
    inpadr = ssch19//'.PADR'
    inpcmp = ssch19//'.PCMP'
    inpnco = ssch19//'.PNCO'
    inpnsp = ssch19//'.PNSP'
    innoma = ssch19//'.NOMA'
    innugd = ssch19//'.NUGD'
    ouvale = sdeval//'.VALE'
    oupnbn = sdeval//'.PNBN'
    oupnb2 = sdeval//'.PNB2'
    oupnco = sdeval//'.PNCO'
    oupnsp = sdeval//'.PNSP'
    oupadr = sdeval//'.PADR'
    oupcmp = sdeval//'.PCMP'
    ounoma = sdeval//'.NOMA'
    ounugd = sdeval//'.NUGD'
    ouerre = sdeval//'.ERRE'
    nabsc = sdlieu//'.ABSC'
    nrefe = sdlieu//'.REFE'
    ndesc = sdlieu//'.DESC'
    call jelira(invale, 'DOCU', ibid, docu)
    call jeveuo(nrefe, 'L', arefe)
    call jeveuo(ndesc, 'L', adesc)
    call jelira(jexnum(nabsc, 1), 'LONMAX', nbnpst, k1bid)
    nbmpst = nbnpst
    call jelira(inpcmp, 'LONMAX', nbtcmp, k1bid)
    call jeveuo(inpcmp, 'L', aipcmp)
    call wkvect(oupcmp, 'V V I', nbtcmp, aopcmp)
!
    nbcmp = 0
    do 20, i = 1, nbtcmp, 1
    nbcmp = nbcmp + min(zi(aipcmp + i-1),1)
    zi(aopcmp + i-1) = zi(aipcmp + i-1)
    20 end do
    call wkvect(ounoma, 'V V K8', 1, adrou)
    call jeveuo(innoma, 'L', adrin)
    mailla = zk8(adrin)
    zk8(adrou) = mailla
    call wkvect(ounugd, 'V V I', 1, adrou)
    call jeveuo(innugd, 'L', ainugd)
    zi(adrou) = zi(ainugd)
    nbocer = nbmpst
!
    call jecrec(ouerre, 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                nbocer)
    do 30, iocer = 1, nbocer, 1
    call jecroc(jexnum(ouerre, iocer))
    call jeecra(jexnum(ouerre, iocer), 'LONMAX', nbcmp, ' ')
    call jeveuo(jexnum(ouerre, iocer), 'E', aoerre)
    do 31, i = 1, nbcmp, 1
    zi(aoerre + i-1) = 0
31  continue
    30 end do
!
    call wkvect(nnumnd, 'V V I', nbnpst, anumnd)
    do 50, i = 1, nbnpst, 1
    call jenonu(jexnom(mailla//'.NOMNOE', zk8(adesc + i-1)), zi(anumnd + i-1))
    50 end do
    call wkvect(oupadr, 'V V I', nbnpst, aopadr)
    call jeveuo(invale, 'L', aivale)
    call jeveuo(inpadr, 'L', aipadr)
!
    if (docu .eq. 'CHNO') then
        call wkvect(ouvale, 'V V R', nbcmp*nbnpst, aovale)
        zi(aopadr + 1-1) = 1
        do 100, i = 1, nbnpst-1, 1
        zi(aopadr + i+1-1) = zi(aopadr + i-1) + nbcmp
100      continue
        do 110, i = 1, nbnpst, 1
        adrin = zi(aipadr + zi(anumnd + i-1)-1)
        adrou = zi(aopadr + i-1)
        do 120, j = 1, nbcmp, 1
        zr(aovale + adrou + j-2) = zr(aivale + adrin + j-2)
120      continue
110      continue
!
    else if (docu .eq. 'CHLM') then
        nindir = '&&RVECHN.TABLE.INDIR'
        call tremno(zk8(adesc + 1-1), ssch19, sdemno)
        call wkvect(oupnbn, 'V V I', nbnpst, aopnbn)
        call wkvect(oupnb2, 'V V I', nbnpst, aopnb2)
        call wkvect(oupnco, 'V V I', nbnpst, aopnco)
        call wkvect(oupnsp, 'V V I', nbnpst, aopnsp)
        call wkvect(nindir, 'V V I', nbnpst, aindir)
        call jeveuo(sdemno//'.NUND', 'L', sdnund)
        call jelira(sdemno//'.NUND', 'LONMAX', lpt, cbid)
        call jeveuo(inpnco, 'L', aipnco)
        call jeveuo(inpnsp, 'L', aipnsp)
        nnmail = sdeval//'.MAIL'
        nrepma = mailla//'.NOMMAI'
        do 200, i = 1, nbnpst, 1
        pt = 1
        trouve = .false.
        n = zi(anumnd + i-1)
210      continue
        if ((.not. trouve) .and. (pt .le. lpt)) then
            if (zi(sdnund + pt-1) .eq. n) then
                trouve = .true.
                call jelira(jexnum(sdemno//'.NUMA', pt), 'LONMAX', nbm, cbid)
                zi(aindir + i-1) = pt
                zi(aopnb2 + i-1) = nbm
                zi(aopnbn + i-1) = 1
            endif
            pt = pt + 1
            goto 210
        endif
        if (.not. trouve) then
            vali = n
            valk = zk8(adesc+i-1)
            call u2mesg('F', 'POSTRELE_40', 1, valk, 1,&
                        vali, 0, 0.d0)
        endif
        call jeveuo(jexnum(sdemno//'.NUMA', pt-1), 'L', anuma)
        nsp = zi(aipnsp + zi(anuma)-1)
        nco = zi(aipnco + zi(anuma)-1)
        do 205, j = 2, nbm, 1
        nsp = min(nsp,zi(aipnsp + zi(anuma + j-1)-1))
        nco = min(nco,zi(aipnco + zi(anuma + j-1)-1))
205      continue
        zi(aopnsp + i-1) = nsp
        zi(aopnco + i-1) = nco
200      continue
!
        zi(aopadr + 1-1) = 1
        do 240, i = 1, nbnpst-1, 1
        zi(aopadr + i+1-1) = zi(aopadr+i-1) + nbcmp*zi(aopnbn+i-1) * zi(aopnco+i-1)*zi(aopnsp+i-1&
                             &)
240      continue
        ilong = zi(aopadr+nbnpst-1) + nbcmp*zi(aopnbn+nbnpst-1)* zi(aopnco+nbnpst-1)*zi(aopnsp+nb&
                &npst-1) - 1
        call wkvect(ouvale, 'V V R', ilong, aovale)
        call jecrec(nnmail, 'V V K8', 'NU', 'DISPERSE', 'VARIABLE',&
                    nbnpst)
        do 427, i = 1, nbnpst, 1
        l = zi(aopnb2 + i-1)
        call jecroc(jexnum(nnmail, i))
        call jeecra(jexnum(nnmail, i), 'LONMAX', l, ' ')
        call jeveuo(jexnum(nnmail, i), 'E', anmail)
        call jeveuo(jexnum(sdemno//'.NUMA', zi(aindir + i-1)), 'L', anuma)
        do 428, j = 1, l, 1
        call jenuno(jexnum(nrepma, zi(anuma+j-1)), zk8(anmail+j- 1))
428      continue
427      continue
        call jedetr(sdemno//'.VACP')
        call jedetr(sdemno//'.NUMA')
        call jedetr(sdemno//'.NOCP')
        call jedetr(sdemno//'.NUCP')
        call jedetr(sdemno//'.NUND')
        call jeveuo(jexnum('&CATA.GD.NOMCMP', zi(ainugd)), 'L', acmpgd)
        do 220, i =1, nbtcmp, 1
!
        pt = zi(aopcmp + i-1)
        if (pt .gt. 0) then
            call tremno(zk8(acmpgd + i-1), ssch19, sdemno)
            do 221, j = 1, nbnpst, 1
            call jeveuo(jexnum(sdemno//'.VACP', zi(aindir + j- 1)), 'L', sdvacp)
            call jeveuo(jexnum(sdemno//'.NUMA', zi(aindir + j- 1)), 'L', anuma)
            nsp = zi(aopnsp + j-1)
            nco = zi(aopnco + j-1)
            nbm = zi(aopnb2 + j-1)
            lnc = zi(aopadr + j-1)
            lmc = nbcmp*nsp
!*+*                  LCC = LMC*NBM
            lcc = lmc
            lms = 0
            do 222, m = 1, nbm, 1
            numm = zi(anuma + m-1)
            nspm = zi(aipnsp + numm-1)
            ncom = zi(aipnco + numm-1)
            do 223, k = 1, nco, 1
            lsc = (k-1)*lcc
            do 224, l = 1, nsp, 1
            indi1 = lnc-1 + lsc + (l-1)*nbcmp + pt-1
            indi2 = lms + (k-1)*nspm + l-1
            if (zr(sdvacp+indi2) .eq. r8vide()) goto 224
            zr(aovale+indi1) = zr(aovale+indi1) + zr(sdvacp+indi2)
224          continue
!
223          continue
            lms = lms + nspm*ncom
222          continue
!
            if (nbm .gt. 1) then
                do 233, k = 1, nco, 1
                lsc = (k-1)*lcc
                do 234, l = 1, nsp, 1
                indi1 = lnc-1 + lsc + (l-1)*nbcmp + pt-1
                zr(aovale+indi1) = zr(aovale+indi1) / nbm
234              continue
233              continue
            endif
!
221          continue
!
            call jedetr(sdemno//'.VACP')
            call jedetr(sdemno//'.NUMA')
            call jedetr(sdemno//'.NOCP')
            call jedetr(sdemno//'.NUCP')
            call jedetr(sdemno//'.NUND')
        endif
220      continue
        call jedetr(nindir)
    else
    endif
    call jeecra(ouvale, 'DOCU', ibid, docu)
    call jedetr(nnumnd)
    call jedetr(oupnb2)
    call jedema()
end subroutine
