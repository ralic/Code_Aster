subroutine rvechc(ssch19, sdlieu, sdeval)
    implicit none
!
#include "jeveux.h"
!
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/rvecha.h"
#include "asterfort/rvechd.h"
#include "asterfort/rvechs.h"
#include "asterfort/wkvect.h"
    character(len=19) :: ssch19, sdeval, sdlieu
    character(len=8) :: courbe
!
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     EXTRACTION LE LONG DE SEGMENTS 2D OU 3D ET D' ARC DE CERCLE
!     ------------------------------------------------------------------
! IN  SSCH19 : K : NOM DU SOUS CHAMP DES CMP NECESAIRES
! IN  SDLIEU : K : NOM DE LA SD DU LIEU (MAILLAGE DU LIEU DE POST)
! OUT SDEVAL : K : NOM DU SOUS_CHAM_GD PRODUIT
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: nbadr
!-----------------------------------------------------------------------
!
!
!
    character(len=24) :: inpnbn, inpcmp, innoma, innugd
    character(len=24) :: oupnbn, ouvale, oupadr, oupcmp, ounoma, ounugd, ouerre
    character(len=24) :: nrefe, nabsc, ndesc, nnume, oupnco, oupnsp, inpnco
    character(len=24) :: inpnsp
    character(len=4) :: docu, doco
    integer :: aipcmp, aipnco
    integer :: aopnbn, aovale, aopadr, aopcmp, aoerre, aopnco
    integer :: arefe, adesc, nbcmp, nbtcmp, aipnsp, aopnsp
    integer :: adrin, adrou, isd, anume, nbco, nbsp
    integer :: nbpart, nbocer, iocer, nbmpst, nbnpst
    integer :: n1, ipart, i, long
!
    character(len=1) :: cbid
    data cbid/' '/
!
!======================================================================
!
    call jemarq()
    inpnbn = ssch19//'.PNBN'
    inpcmp = ssch19//'.PCMP'
    inpnco = ssch19//'.PNCO'
    inpnsp = ssch19//'.PNSP'
    innoma = ssch19//'.NOMA'
    innugd = ssch19//'.NUGD'
    ouvale = sdeval//'.VALE'
    oupnbn = sdeval//'.PNBN'
    oupadr = sdeval//'.PADR'
    oupcmp = sdeval//'.PCMP'
    oupnco = sdeval//'.PNCO'
    oupnsp = sdeval//'.PNSP'
    ounoma = sdeval//'.NOMA'
    ounugd = sdeval//'.NUGD'
    ouerre = sdeval//'.ERRE'
    nabsc = sdlieu//'.ABSC'
    nrefe = sdlieu//'.REFE'
    ndesc = sdlieu//'.DESC'
    nnume = sdlieu//'.NUME'
    call jelira(nrefe, 'DOCU', cval=docu)
    call jeveuo(nrefe, 'L', arefe)
    call jeveuo(ndesc, 'L', adesc)
    call jeveuo(nnume, 'L', anume)
    call jelira(nabsc, 'NMAXOC', nbpart)
    isd = zi(anume)
    nbnpst = 0
    nbmpst = 0
    nbcmp = 0
    do 10, ipart = 1, nbpart, 1
    call jelira(jexnum(nabsc, ipart), 'LONMAX', n1)
    nbnpst = nbnpst + n1
    10 end do
    nbmpst = nbnpst - nbpart
    call jeveuo(inpcmp, 'L', aipcmp)
    call jelira(inpcmp, 'LONMAX', nbtcmp)
    call wkvect(oupcmp, 'V V I', nbtcmp, aopcmp)
    do 20, i = 1, nbtcmp, 1
    nbcmp = nbcmp + min(zi(aipcmp + i-1),1)
    zi(aopcmp + i-1) = zi(aipcmp + i-1)
    20 end do
    call wkvect(ounoma, 'V V K8', 1, adrou)
    call jeveuo(innoma, 'L', adrin)
    zk8(adrou) = zk8(adrin)
    call wkvect(ounugd, 'V V I', 1, adrou)
    call jeveuo(innugd, 'L', adrin)
    zi(adrou) = zi(adrin)
    courbe = zk8(arefe)
    call jeexin(inpnbn, n1)
    if (n1 .ne. 0) then
        call jeveuo(inpnco, 'L', aipnco)
        call jeveuo(inpnsp, 'L', aipnsp)
        call wkvect(oupnbn, 'V V I', nbmpst, aopnbn)
        call wkvect(oupnco, 'V V I', nbmpst, aopnco)
        call wkvect(oupnsp, 'V V I', nbmpst, aopnsp)
        nbco = 1000
        nbsp = 1000
        long = 2*nbcmp*nbsp*nbco
        nbadr = nbmpst
        nbocer = nbmpst
        doco = 'CHLM'
    else
        long = nbcmp
        nbadr = nbnpst
        nbocer = nbnpst
        doco = 'CHNO'
        nbco = 1
        nbsp = 1
    endif
    call jecrec(ouerre, 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                nbocer)
    do 30, iocer = 1, nbocer, 1
    call jecroc(jexnum(ouerre, iocer))
    call jeecra(jexnum(ouerre, iocer), 'LONMAX', nbcmp)
    call jeveuo(jexnum(ouerre, iocer), 'E', aoerre)
    do 31, i = 1, nbcmp, 1
    zi(aoerre + i-1) = 0
31  continue
    30 end do
    call wkvect(ouvale, 'V V R', long*nbadr, aovale)
    call wkvect(oupadr, 'V V I', nbadr, aopadr)
    call jeecra(ouvale, 'DOCU', cval=doco)
    zi(aopadr + 1-1) = 1
    do i = 1, nbadr-1, 1
        zi(aopadr + i+1-1) = zi(aopadr + i-1) + long
    end do
    call jedema()
end subroutine
