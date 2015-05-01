subroutine rvechm(ssch19, sdlieu, sdeval)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
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
#include "asterfort/rvchlm.h"
#include "asterfort/wkvect.h"
    character(len=19) :: ssch19, sdlieu, sdeval
!
!**********************************************************************
!
!  OPERATION REALISEE
!  ------------------
!     OPERATION D' EXTRACTION DU POST-TRAITEMENT D' UNE COURBE OBTENUE
!     COMME REUNION FINIE DE MAILLES 1D
!
!  ARGUMENT EN ENTREE
!  ------------------
!
!     SSCH19 : NOM DU SOUS CHAMP DE GRANDEUR
!
!     SDLIEU : NOM DE LA SD REPRESENTANT LE LIEU
!
!
!  ARGUMENT EN SORTIE
!  ------------------
!
!     SDEVAL: NOM DE LA SD SOUS_CHAMP_GD PRODUITES
!            (DESCRIPTION : CF RVPSTE)
!
!**********************************************************************
!
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
    character(len=24) :: invale, inpadr, inpcmp, innoma, innugd, inpnco, inpnsp
    character(len=24) :: ouvale, oupadr, oupcmp, ouerre, ounoma, oupnbn, ounugd
    character(len=24) :: nrefe, nabsc, ndesc, nnume, ntab1, nnumnd, oupnco
    character(len=24) :: oupnsp
    character(len=15) :: nchmin
    character(len=14) :: nmail1, nmail2
    character(len=8) :: mailla, typmai, courbe
    character(len=4) :: docu
!
    integer :: aivale, aipadr, aipcmp, iocer, j, nbtcmp, adrnl, aopnco, aopnsp
    integer :: aopnbn, aovale, aopadr, aopcmp, aoerre, ml, long, aipnco, aipnsp
    integer :: arefe, adesc, nbcmp, anume, adrm, adrnd, i, inl
    integer :: amail1, amail2, achmin, adrin, adrou, isd, anumnd, atypm, atab1
    integer :: m2d, m2d1, m2d2, nbadr, nbmpst, nbnpst, nbocer, nd, n1
    integer :: iatyma
!
!==================== CORPS DE LA ROUTINE =============================
!
!-----------------------------------------------------------------------
    integer :: nbco, nbsp
!-----------------------------------------------------------------------
    call jemarq()
!
    ntab1 = '&&RVECHM.TABLE.CONTRI.M2'
    nnumnd = '&&RCECHM.NUM.NOEUD.LISTE'
    invale = ssch19//'.VALE'
    inpadr = ssch19//'.PADR'
    inpcmp = ssch19//'.PCMP'
    inpnco = ssch19//'.PNCO'
    inpnsp = ssch19//'.PNSP'
    innoma = ssch19//'.NOMA'
    innugd = ssch19//'.NUGD'
    ouvale = sdeval//'.VALE'
    oupnbn = sdeval//'.PNBN'
    oupnco = sdeval//'.PNCO'
    oupnsp = sdeval//'.PNSP'
    oupadr = sdeval//'.PADR'
    oupcmp = sdeval//'.PCMP'
    ounoma = sdeval//'.NOMA'
    ounugd = sdeval//'.NUGD'
    ouerre = sdeval//'.ERRE'
!
!
    nabsc = sdlieu//'.ABSC'
    nrefe = sdlieu//'.REFE'
    ndesc = sdlieu//'.DESC'
    nnume = sdlieu//'.NUME'
!
    call jelira(invale, 'DOCU', cval=docu)
    call jeveuo(nrefe, 'L', arefe)
    call jeveuo(ndesc, 'L', adesc)
    call jeveuo(nnume, 'L', anume)
!
    isd = zi(anume)
!
    call jelira(jexnum(nabsc, 1), 'LONMAX', nbnpst)
!
    nbmpst = nbnpst - 1
!
    call jeveuo(inpcmp, 'L', aipcmp)
    call jelira(inpcmp, 'LONMAX', nbtcmp)
    call wkvect(oupcmp, 'V V I', nbtcmp, aopcmp)
!
    nbcmp = 0
!
    do 20, i = 1, nbtcmp, 1
!
    nbcmp = nbcmp + min(1,zi(aipcmp + i-1))
!
    zi(aopcmp + i-1) = zi(aipcmp + i-1)
!
    20 end do
!
    call wkvect(ounoma, 'V V K8', 1, adrou)
    call jeveuo(innoma, 'L', adrin)
!
    mailla = zk8(adrin)
    zk8(adrou) = mailla
!
    call wkvect(ounugd, 'V V I', 1, adrou)
    call jeveuo(innugd, 'L', adrin)
!
    zi(adrou) = zi(adrin)
!
    courbe = zk8(arefe)
!
    nmail1 = courbe//'.MAIL1'
    nmail2 = courbe//'.MAIL2'
    nchmin = courbe//'.CHEMIN'
!
    call jeveuo(jexnum(nmail1, isd), 'L', amail1)
    call jelira(jexnum(nmail1, isd), 'LONMAX', n1)
    call jeveuo(jexnum(nmail2, isd), 'L', amail2)
    call jeveuo(jexnum(nchmin, isd), 'L', achmin)
!
    if (docu .eq. 'CHLM') then
!
        call wkvect(oupnbn, 'V V I', nbmpst, aopnbn)
        call wkvect(oupnco, 'V V I', nbmpst, aopnco)
        call wkvect(oupnsp, 'V V I', nbmpst, aopnsp)
        call jeveuo(inpnco, 'L', aipnco)
        call jeveuo(inpnsp, 'L', aipnsp)
!
        nbsp = 1000
        nbco = 1000
!
        do 40, i = 1, n1-1, 1
!
        m2d1 = zi(amail1 + i-1)
        m2d2 = zi(amail2 + i-1)
        nbco = min(zi(aipnco + m2d1-1),nbco)
        nbsp = min(zi(aipnsp + m2d1-1),nbsp)
!
        if (m2d2 .ne. 0) then
!
            nbco = min(zi(aipnco + m2d2-1),nbco)
            nbsp = min(zi(aipnsp + m2d2-1),nbsp)
!
        endif
!
40      continue
!
        do 41, i = 1, nbmpst, 1
!
        zi(aopnbn + i-1) = 2
        zi(aopnco + i-1) = nbco
        zi(aopnsp + i-1) = nbsp
!
41      continue
!
        long = 2*nbcmp*nbsp*nbco
        nbadr = nbmpst
        nbocer = nbmpst
!
    else
!
        long = nbcmp
        nbadr = nbnpst
        nbocer = nbnpst
!
    endif
!
    call jecrec(ouerre, 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                nbocer)
!
    do 30, iocer = 1, nbocer, 1
!
    call jecroc(jexnum(ouerre, iocer))
    call jeecra(jexnum(ouerre, iocer), 'LONMAX', nbcmp)
    call jeveuo(jexnum(ouerre, iocer), 'E', aoerre)
!
    do 31, i = 1, nbcmp, 1
!
    zi(aoerre + i-1) = 0
!
31  continue
!
    30 end do
!
    call wkvect(ouvale, 'V V R', long*nbadr, aovale)
    call wkvect(oupadr, 'V V I', nbadr, aopadr)
!
    zi(aopadr + 1-1) = 1
!
    do 60, i = 1, nbadr-1, 1
!
    zi(aopadr + i+1-1) = zi(aopadr + i-1) + long
!
    60 end do
!
!
    call jeveuo(inpadr, 'L', aipadr)
    call jeveuo(invale, 'L', aivale)
!
    if (docu .eq. 'CHNO') then
!
        do 100, inl = 1, nbnpst, 1
!
        call jenonu(jexnom(mailla//'.NOMNOE', zk8(adesc+inl-1)), nd)
!
        adrnd = zi(aipadr + nd-1)
        adrnl = zi(aopadr + inl-1)
!
        do 110, j = 1, nbcmp, 1
!
        zr(aovale + adrnl-1 + j-1) = zr(aivale + adrnd-1 + j- 1)
!
110      continue
!
100      continue
!
        call jeecra(ouvale, 'DOCU', cval='CHNO')
!
    else if (docu .eq. 'CHLM') then
!
        ml = 1
        m2d = 1
!
        call wkvect(nnumnd, 'V V I', nbnpst, anumnd)
        call wkvect(ntab1, 'V V R', 3*nbcmp*nbco*nbsp, atab1)
!
        do 200, i = 1, nbnpst, 1
!
        call jenonu(jexnom(mailla//'.NOMNOE', zk8(adesc + i-1)), zi(anumnd + i-1))
!
200      continue
!
        long = long/2
!
300      continue
        if (ml .lt. nbnpst) then
!
            m2d1 = zi(amail1 + m2d-1)
            m2d2 = zi(amail2 + m2d-1)
            adrm = zi(aopadr + ml-1)
!
!
            call jeveuo(mailla//'.TYPMAIL', 'L', iatyma)
            atypm=iatyma-1+m2d1
            call jenuno(jexnum('&CATA.TM.NOMTM', zi(atypm)), typmai)
!
            call rvchlm(ssch19, m2d1, zi(anumnd + ml-1), 2, nbcmp,&
                        nbco, nbsp, zr(aovale + adrm-1))
!
            if (m2d2 .gt. 0) then
!
                call rvchlm(ssch19, m2d2, zi(anumnd + ml-1), 2, nbcmp,&
                            nbco, nbsp, zr(atab1))
!
                do 310, j = 1, 2*long, 1
!
                if (zr(atab1 + j-1) .eq. r8vide()) goto 310
                zr(aovale+adrm-1+j-1) = 0.5d0*( zr(aovale+adrm-1+j- 1)+ zr(atab1 + j-1) )
!
!
310              continue
!
!
            endif
!
            ml = ml + 1
            adrm = zi(aopadr + ml-1)
!
            if ((typmai .eq. 'TRIA6') .or. (typmai .eq. 'QUAD8') .or. (typmai .eq. 'QUAD9')) then
!
                do 311, j = 1, long, 1
!
                zr(aovale+adrm-1+j-1) = zr(aovale+adrm-1-long +j- 1)
!
311              continue
!
!
                call rvchlm(ssch19, m2d1, zi(anumnd + ml+1-1), 1, nbcmp,&
                            nbco, nbsp, zr(aovale + adrm-1 + long))
!
!
                if (m2d2 .gt. 0) then
!
                    call rvchlm(ssch19, m2d2, zi(anumnd + ml+1-1), 1, nbcmp,&
                                nbco, nbsp, zr(atab1))
!
!
                    do 320, j = 1, long, 1
!
                    if (zr(atab1 + j-1) .eq. r8vide()) goto 320
                    zr(aovale+adrm+long-1+j-1) = 0.5d0* (&
                                                 zr( aovale+adrm+long-1+j-1)+ zr(atab1 + j-1))
!
!
320                  continue
!
!
                endif
!
                ml = ml + 1
!
            endif
!
            m2d = m2d + 1
!
!
            goto 300
!
        endif
!
        call jeecra(ouvale, 'DOCU', cval='CHLM')
!
        call jedetr(ntab1)
        call jedetr(nnumnd)
!
    else
!
!       /* AUTRE TYPE DE CHAMPS */
!
    endif
!
    call jedema()
end subroutine
