subroutine rvechs(ssch19, nbcp, nbco, nbsp, ma,&
                  vlc, for, fex, rsor, rsex,&
                  n, ptadr, val, nbndf, clocf)
    implicit none
!
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/rvchl3.h"
#include "asterfort/rvchn3.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
    character(len=19) :: ssch19
    integer :: nbcp, nbco, nbsp, n, ptadr, for(*), fex(*)
    integer :: ma(*), vlc(*), nbndf(6, *), clocf(6, 4, *)
    real(kind=8) :: rsor(*), rsex(*), val(*)
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
!     EVALUATION DE CMP LE LONG D' UN SGT_3D EN UN MORCEAUX
!     ------------------------------------------------------------------
! IN  SSCH19 : K : NOM DU SOUS_CHAM_GD
! IN  NBCP   : I : NOMBRE DE CMP
! IN  NBOP   : I : NOMBRE DE COUCHE
! IN  NBSP   : I : NOMBRE DE SOUS-POINT
! IN  MA     : I : TABLE DES MAILLES 3D RENCONTREES
! IN  VLC    : I : POINTEUR DES SGT_ELEM SUR LES MAILLES 3D RENCONTREES
! IN  FOR    : I : TABLES FACES CONTENANT ORIGINES   DES SGT_ELEM
! IN  FEX    : I : TABLES FACES CONTENANT EXTREMITES DES SGT_ELEM
! IN  RSOR   : R : TABLES CO_REF DES ORIGINES   DES SGT_ELEM
! IN  RSEX   : R : TABLES CO_REF DES EXTREMITES DES SGT_ELEM
! IN  N      : I : NOMBRE DE SGT_ELEM
! VAR PTADR  : I : POINTEUR SUR LE PREMIER ELEMENT LIBRE DE VAL
! OUT VAL    : R : TABLEAU DES VALEUR DES CMPS (TOUS MORCEAUX)
! IN  NBNDF  : I : TABLE(1..6,1..3)
!            :   :    T(I,J) = NB_ND DE LA FACE I DU TYPE DE MAILLE J
! IN  CLOCF  : I : TABLE(1..6,1..4,1..3)
!            :   :    T(I,J,K) = NUM_LOC (DANS CONNEC_MAILLE)
!            :   :    ND_LOC I DE LA FACE J DE LA MAILLE DE TYPE K
!     ------------------------------------------------------------------
!
!
!
    character(len=24) :: nconec, npadr, nvale
    character(len=8) :: nmaila, ktypm
    character(len=4) :: docu
!
    integer :: apnco, apnsp, apnbn, aconec, vlccnc
    integer :: m, face(2), i, j, nbpt, avale, apadr, itypm, nbma, aux, iatyma
    real(kind=8) :: cref(2, 2)
!
!==================== CORPS DE LA ROUTINE =============================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    call wkvect('&&RVECHS.TABLE.AUX', 'V V R', 4*nbco*nbsp*nbcp, aux)
    call jelira(ssch19//'.VALE', 'DOCU', i, docu)
    if (docu .eq. 'CHNO') then
        apnco = 0
        apnsp = 0
        apnbn = 0
    else
        call jeveuo(ssch19//'.PNCO', 'L', apnco)
        call jeveuo(ssch19//'.PNSP', 'L', apnsp)
        call jeveuo(ssch19//'.PNBN', 'L', apnbn)
    endif
    npadr = ssch19//'.PADR'
    nvale = ssch19//'.VALE'
    call jeveuo(npadr, 'L', apadr)
    call jeveuo(nvale, 'L', avale)
    call jeveuo(ssch19//'.NOMA', 'L', i)
    nmaila = zk8(i)
    nconec = nmaila//'.CONNEX         '
    call jeveuo(jexnum(nconec, 1), 'L', aconec)
    call jeveuo(jexatr(nconec, 'LONCUM'), 'L', vlccnc)
    if (docu .eq. 'CHNO') then
        nbma = 1
        nbpt = 1
        do 100, i = 1, n, 1
        m = ma(vlc(i))
        call jeveuo(nmaila//'.TYPMAIL', 'L', iatyma)
        j=iatyma-1+m
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(j)), ktypm)
        face(1) = for(i)
        cref(1,1) = rsor(2*(i-1)+1)
        cref(2,1) = rsor(2*(i-1)+2)
        if (ktypm(1:5) .eq. 'TETRA') then
            itypm = 1
        else if (ktypm(1:5) .eq. 'PENTA') then
            itypm = 2
        else if (ktypm(1:4) .eq. 'HEXA') then
            itypm = 3
        else
            call u2mesk('F', 'POSTRELE_18', 1, ktypm)
        endif
        call rvchn3(zr(avale), zi(apadr), ma(vlc(i)), itypm, nbpt,&
                    nbcp, face, cref, nbndf, clocf,&
                    zi(aconec), zi(vlccnc), val, ptadr, zr(aux))
100      continue
        m = ma(vlc(n))
        call jeveuo(nmaila//'.TYPMAIL', 'L', iatyma)
        j=iatyma-1+m
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(j)), ktypm)
        face(1) = fex(n)
        cref(1,1) = rsex(2*(n-1)+1)
        cref(2,1) = rsex(2*(n-1)+2)
        if (ktypm(1:5) .eq. 'TETRA') then
            itypm = 1
        else if (ktypm(1:5) .eq. 'PENTA') then
            itypm = 2
        else if (ktypm(1:4) .eq. 'HEXA') then
            itypm = 3
        else
            call u2mesk('F', 'POSTRELE_18', 1, ktypm)
        endif
        call rvchn3(zr(avale), zi(apadr), ma(vlc(n)), itypm, nbpt,&
                    nbcp, face, cref, nbndf, clocf,&
                    zi(aconec), zi(vlccnc), val, ptadr, zr(aux))
    else
        nbpt = 2
        do 200, i = 1, n, 1
        m = ma(vlc(i))
        nbma = vlc(i+1) - vlc(i)
        call jeveuo(nmaila//'.TYPMAIL', 'L', iatyma)
        j=iatyma-1+m
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(j)), ktypm)
        face(1) = for(i)
        face(2) = fex(i)
        cref(1,1) = rsor(2*(i-1)+1)
        cref(2,1) = rsor(2*(i-1)+2)
        cref(1,2) = rsex(2*(i-1)+1)
        cref(2,2) = rsex(2*(i-1)+2)
        if (ktypm(1:5) .eq. 'TETRA') then
            itypm = 1
        else if (ktypm(1:5) .eq. 'PENTA') then
            itypm = 2
        else if (ktypm(1:4) .eq. 'HEXA') then
            itypm = 3
        else
            call u2mesk('F', 'POSTRELE_18', 1, ktypm)
        endif
        call rvchl3(zr(avale), zi(apadr), zi(apnsp), zi(apnbn), ma( vlc(i)),&
                    nbma, itypm, nbco, nbsp, nbpt,&
                    nbcp, face, cref, nbndf, clocf,&
                    zi(aconec), zi(vlccnc), val, ptadr, zr(aux))
200      continue
    endif
    call jedetr('&&RVECHS.TABLE.AUX')
    call jedema()
end subroutine
