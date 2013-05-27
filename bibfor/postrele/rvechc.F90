subroutine rvechc(dim, ssch19, sdlieu, sdeval, nbndf,&
                  clocf)
    implicit none
!
    include 'jeveux.h'
!
    include 'asterfort/jecrec.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/rvecha.h'
    include 'asterfort/rvechd.h'
    include 'asterfort/rvechs.h'
    include 'asterfort/wkvect.h'
    character(len=19) :: ssch19, sdeval, sdlieu
    character(len=8) :: courbe
    character(len=2) :: dim
    integer :: nbndf(6, *), clocf(6, 4, *)
!
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     EXTRACTION LE LONG DE SEGMENTS 2D OU 3D ET D' ARC DE CERCLE
!     ------------------------------------------------------------------
! IN  DIM    : K : '1D' OU '2D' OU '3D' (RELIQUAT --> MESSAGE )
! IN  SSCH19 : K : NOM DU SOUS CHAMP DES CMP NECESAIRES
! IN  SDLIEU : K : NOM DE LA SD DU LIEU (MAILLAGE DU LIEU DE POST)
! IN  NBNDF  : I : TABLE(1..6,1..3) NB NOEUD_FACE PAR TYPE_MAILLE
! IN  CLOCF  : I : TABLE(1..6,1..4,1..3) CONEC_LOC_FACE PAR TYPE_MAILLE
! OUT SDEVAL : K : NOM DU SOUS_CHAM_GD PRODUIT
!     ------------------------------------------------------------------
!
    real(kind=8) :: epsi
!-----------------------------------------------------------------------
    integer :: nbadr
!-----------------------------------------------------------------------
    parameter (epsi = 1.0d-6)
!
!
!
    character(len=24) :: inpnbn, inpcmp, innoma, innugd
    character(len=24) :: oupnbn, ouvale, oupadr, oupcmp, ounoma, ounugd, ouerre
    character(len=24) :: nrefe, nabsc, ndesc, nnume, oupnco, oupnsp, inpnco
    character(len=24) :: inpnsp
    character(len=24) :: nmail1, nmail2, nparor, nparex
    character(len=24) :: nfacor, nfacex, ncnxor, ncnxex, nartor, nartex
    character(len=4) :: docu, doco
    integer :: aipcmp, aipnco
    integer :: aopnbn, aovale, aopadr, aopcmp, aoerre, aopnco
    integer :: arefe, adesc, nbcmp, nbtcmp, aipnsp, aopnsp, aartor, aartex
    integer :: amail1, amail2, afacor, afacex, aparor, aparex, acnxor
    integer :: adrin, adrou, isd, anume, nbco, nbsp, nbma3, acnxex
    integer :: nbpart, nbsgpc, nbocer, iocer, ibid, vlcma3, nbmpst, nbnpst
    integer :: n1, ipart, ptadr, icnxor, icnxex, j, i, k, long
!
    character(len=1) :: cbid
    character(len=1) :: k1bid
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
    call jelira(nrefe, 'DOCU', ibid, docu)
    call jeveuo(nrefe, 'L', arefe)
    call jeveuo(ndesc, 'L', adesc)
    call jeveuo(nnume, 'L', anume)
    call jelira(nabsc, 'NMAXOC', nbpart, cbid)
    isd = zi(anume)
    nbnpst = 0
    nbmpst = 0
    nbcmp = 0
    do 10, ipart = 1, nbpart, 1
    call jelira(jexnum(nabsc, ipart), 'LONMAX', n1, k1bid)
    nbnpst = nbnpst + n1
    10 end do
    nbmpst = nbnpst - nbpart
    call jeveuo(inpcmp, 'L', aipcmp)
    call jelira(inpcmp, 'LONMAX', nbtcmp, k1bid)
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
    if ((docu .eq. 'SGTD') .or. (docu .eq. 'ARCC')) then
        nmail1 = courbe//'.MAIL1'
        nmail2 = courbe//'.MAIL2'
        nparor = courbe//'.PAROR'
        nparex = courbe//'.PAREX'
        nfacor = courbe//'.FACOR'
        nfacex = courbe//'.FACEX'
        ncnxor = courbe//'.CNXOR'
        ncnxex = courbe//'.CNXEX'
        call jeveuo(jexnum(nmail1, isd), 'L', amail1)
        call jeveuo(jexnum(nmail2, isd), 'L', amail2)
        call jeveuo(jexnum(nparor, isd), 'L', aparor)
        call jeveuo(jexnum(nparex, isd), 'L', aparex)
        call jeveuo(jexnum(nfacor, isd), 'L', afacor)
        call jeveuo(jexnum(nfacex, isd), 'L', afacex)
        call jeveuo(jexnum(ncnxor, isd), 'L', acnxor)
        call jeveuo(jexnum(ncnxex, isd), 'L', acnxex)
    else if (docu .eq. 'SGT3') then
        call jeveuo(courbe//'.NSDS', 'L', adrin)
        nmail2 = zk24(adrin + isd-1)
        nmail1 = nmail2(1:13)//'.MAIL      '
        nparor = nmail2(1:13)//'.CREFF.ORIG'
        nparex = nmail2(1:13)//'.CREFF.EXTR'
        nfacor = nmail2(1:13)//'.FACE .ORIG'
        nfacex = nmail2(1:13)//'.FACE .EXTR'
        ncnxor = nmail2(1:13)//'.CONEX.ORIG'
        ncnxex = nmail2(1:13)//'.CONEX.EXTR'
        nartor = nmail2(1:13)//'.ARETE.ORIG'
        nartex = nmail2(1:13)//'.ARETE.EXTR'
        call jeveuo(nparor, 'L', aparor)
        call jeveuo(nparex, 'L', aparex)
        call jeveuo(nfacor, 'L', afacor)
        call jeveuo(nfacex, 'L', afacex)
        call jeveuo(ncnxor, 'L', acnxor)
        call jeveuo(ncnxex, 'L', acnxex)
        call jeveuo(nartor, 'L', aartor)
        call jeveuo(nartex, 'L', aartex)
        call jeveuo(jexatr(nmail1, 'LONCUM'), 'L', vlcma3)
        call jeveuo(jexnum(nmail1, 1), 'L', amail1)
    else
    endif
    call jeexin(inpnbn, n1)
    if (n1 .ne. 0) then
        call jeveuo(inpnco, 'L', aipnco)
        call jeveuo(inpnsp, 'L', aipnsp)
        call wkvect(oupnbn, 'V V I', nbmpst, aopnbn)
        call wkvect(oupnco, 'V V I', nbmpst, aopnco)
        call wkvect(oupnsp, 'V V I', nbmpst, aopnsp)
        nbco = 1000
        nbsp = 1000
        if ((docu .eq. 'SGTD') .or. (docu .eq. 'ARCC')) then
            do 40, i = 1, nbmpst, 1
            nbsp = min(nbsp,zi(aipnsp + zi(amail1 + i-1)-1))
            nbco = min(nbco,zi(aipnco + zi(amail1 + i-1)-1))
            if (zi(amail2 + i-1) .gt. 0) then
                nbsp = min(nbsp,zi(aipnsp + zi(amail2 + i-1)-1))
                nbco = min(nbco,zi(aipnco + zi(amail2 + i-1)-1))
            endif
40          continue
            do 41, i = 1, nbmpst, 1
            zi(aopnbn + i-1) = 2
            zi(aopnsp + i-1) = nbsp
            zi(aopnco + i-1) = nbco
41          continue
        else if (docu .eq. 'SGT3') then
            do 45, i = 1, nbmpst, 1
            j = zi(vlcma3 + i-1)
            nbma3 = zi(vlcma3 + i) - j
            do 46, k = 1, nbma3, 1
            nbsp = min(nbsp,zi(aipnsp + zi(amail1+j+k-2)-1))
            nbco = min(nbco,zi(aipnco + zi(amail1+j+k-2)-1))
46          continue
45          continue
            do 47, i = 1, nbmpst, 1
            zi(aopnbn + i-1) = 2
            zi(aopnsp + i-1) = nbsp
            zi(aopnco + i-1) = nbco
47          continue
        else
        endif
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
    call jeecra(jexnum(ouerre, iocer), 'LONMAX', nbcmp, ' ')
    call jeveuo(jexnum(ouerre, iocer), 'E', aoerre)
    do 31, i = 1, nbcmp, 1
    zi(aoerre + i-1) = 0
31  continue
    30 end do
    call wkvect(ouvale, 'V V R', long*nbadr, aovale)
    call wkvect(oupadr, 'V V I', nbadr, aopadr)
    call jeecra(ouvale, 'DOCU', ibid, doco)
    zi(aopadr + 1-1) = 1
    do 60, i = 1, nbadr-1, 1
    zi(aopadr + i+1-1) = zi(aopadr + i-1) + long
    60 end do
    ptadr = 1
    do 100, ipart = 1, nbpart, 1
    icnxor = zi(acnxor + ipart-1)
    icnxex = zi(acnxex + ipart-1)
    nbsgpc = icnxex - icnxor + 1
    if (docu .eq. 'SGTD') then
        call rvechd(dim, epsi, ssch19, nbcmp, nbco,&
                    nbsp, zr(aparor+ icnxor-1), zr(aparex+icnxor-1), zi(amail1+icnxor-1),&
                    zi(amail2+icnxor-1), zi(afacor+icnxor-1), zi(afacex+icnxor- 1), nbsgpc,&
                    ptadr, zr(aovale))
    else if (docu .eq. 'ARCC') then
        call rvecha(dim, epsi, ssch19, nbcmp, nbco,&
                    nbsp, zr(aparor+ icnxor-1), zr(aparex+icnxor-1), zi(amail1+icnxor-1),&
                    zi(afacor+icnxor-1), zi(afacex+icnxor-1), nbsgpc, ptadr, zr(aovale))
    else if (docu .eq. 'SGT3') then
        call rvechs(ssch19, nbcmp, nbco, nbsp, zi(amail1),&
                    zi(vlcma3+ icnxor-1), zi(afacor+icnxor-1), zi(afacex+icnxor-1),&
                    zr(aparor+2*(icnxor-1)), zr(aparex+2*(icnxor-1)), nbsgpc, ptadr, zr(aovale),&
                    nbndf, clocf)
    else
!           /* AUTRE CAS COURBE */
    endif
    100 end do
    call jedema()
end subroutine
