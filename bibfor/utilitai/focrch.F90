subroutine focrch(nomfon, resu, noeud, parax, paray,&
                  base, int, intitu, ind, listr,&
                  sst, nsst, ier)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/fointe.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "blas/dcopy.h"
    integer :: nsst, int, ind, ier
    character(len=1) :: base
    character(len=16) :: parax, paray
    character(len=8) :: sst, noeud
    character(len=19) :: nomfon, resu, listr
    character(len=24) :: intitu

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
!     RECUPERATION D'UNE FONCTION DANS UNE STRUCTURE "TRAN_GENE"
!     POUR UN NOEUD DE CHOC
!     ------------------------------------------------------------------
! IN  : NOMFON : NOM DE LA FONCTION
! IN  : RESU   : NOM DE LA STRUCTURE RESULTAT
! IN  : NOEUD  : NOEUD DE CHOC
! IN  : PARAX  : PARAMETRE DE LA FONCTION EN X
! IN  : PARAY  : PARAMETRE DE LA FONCTION EN Y
! IN  : BASE   : 'GLOBALE'
! IN  : INT    : PRISE EN COMPTE D'UN NOM DE LIAISON
! IN  : INTITU : NOM D'UNE LIAISON
! IN  : IND    : PRISE EN COMPTE D'UNE LISTE DE PARAMETRES
! IN  : LISTR  : LISTE DE PARAMETRES
! IN  : SST    : NOM DE LA SOUS-STRUCTURE
! IN  : NSST   : PRISE EN COMPTE DU NOM D'UNE SOUS-STRUCTURE
! OUT : IER    : CODE RETOUR, = 0 : OK
!     ------------------------------------------------------------------
    character(len=8) :: k8b
    character(len=16) :: nomcmd
    character(len=19) :: fonct1, fonct2
!     ----------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ic, inl, ie, ival,  jinst, iparax
    integer ::   jparx, jpary, jval, jvalx, jvaly, start, nbvint, iparay
    integer :: lfon, lg, lpro, lval, nbnoli, nbinst, nbpara
    integer :: nbval, jvint
    character(len=24), pointer :: nlname(:) => null()
    integer, pointer :: desc(:) => null()
    integer, pointer :: vindx(:) => null()

!-----------------------------------------------------------------------
    call jemarq()
    ier = 999
    call getres(k8b, k8b, nomcmd)
!
    call jeveuo(resu//'.DESC', 'L', vi=desc)
    nbnoli = desc(3)
!
    call jelira(resu//'.DISC', 'LONUTI', nbinst)
    call jeveuo(resu//'.DISC', 'L', jinst)
!
    call jeveuo(resu(1:16)//'.NL.INTI', 'L', vk24=nlname)
!
    ic = 1
    if (int .ne. 0) then
        do 2 inl = 1, nbnoli
            if (nlname((inl-1)*5+1) .eq. intitu) goto 4
 2      continue
        call utmess('A', 'UTILITAI_86', sk=intitu)
        goto 999
 4      continue
        if (nsst .eq. 0) then
            if (nlname((inl-1)*5+2)(1:8) .eq. noeud) goto 16
            ic = 2
            if (nlname((inl-1)*5+3)(1:8) .eq. noeud) goto 16
            lg = max(1,lxlgut(noeud))
            call utmess('A', 'UTILITAI_87', sk=noeud(1:lg))
            goto 999
        else
            if (nlname((inl-1)*5+4)(1:8) .eq. sst) goto 116
            if (nlname((inl-1)*5+5)(1:8) .eq. sst) goto 116
            call utmess('A', 'UTILITAI_88')
            goto 999
116         continue
            if (nlname((inl-1)*5+2) .ne. noeud .and. nlname((inl-1)*5+3) .ne. noeud) then
                lg = max(1,lxlgut(noeud))
                call utmess('A', 'UTILITAI_89', sk=noeud(1:lg))
                goto 999
            endif
            if (nlname((inl-1)*5+2)(1:8) .eq. noeud .and. nlname((inl-1)*5+4)(1:8) .eq. sst) goto 16
            ic = 2
            if (nlname((inl-1)*5+3)(1:8) .eq. noeud .and. nlname((inl-1)*5+5)(1:8) .eq. sst) &
            goto 16
            lg = max(1,lxlgut(noeud))
            call utmess('A', 'UTILITAI_90', sk=noeud(1:lg))
            goto 999
        endif
    endif
!     --- RECHERCHE DU NOEUD_1 DE CHOC ---
    do inl = 1, nbnoli
        if (nlname((inl-1)*5+2)(1:8) .eq. noeud) goto 16
    end do
!     --- RECHERCHE DU NOEUD_2 DE CHOC ---
    ic = 2
    do inl = 1, nbnoli
        if (nlname((inl-1)*5+3)(1:8) .eq. noeud) goto 16
    end do
    lg = max(1,lxlgut(noeud))
    call utmess('A', 'UTILITAI_87', sk=noeud(1:lg))
    goto 999
16  continue
!
    call jeveuo(resu(1:16)//'.NL.VINT', 'L', jvint)
    call jeveuo(resu(1:16)//'.NL.VIND', 'L', vi=vindx)
    start = vindx(inl)-1
    nbvint = vindx(nbnoli+1)-1
!
    if (parax(1:4) .eq. 'INST') then
        jvalx = jinst
        goto 20
    else if (parax(1:2).eq.'FN' ) then 
        jparx = jvint + start
    else if (parax(1:3).eq.'FT1') then 
        jparx = jvint + start + 1
    else if (parax(1:3).eq.'FT2') then 
        jparx = jvint + start + 2
    else if (parax(1:2).eq.'VN' ) then 
        jparx = jvint + start + 3
    else if (parax(1:3).eq.'VT1') then 
        jparx = jvint + start + 4
    else if (parax(1:3).eq.'VT2') then 
        jparx = jvint + start + 5
    else if (parax(1:5).eq.'DXLOC') then
        if (ic .eq. 1) then
            jparx = jvint + start + 6
        else
            jparx = jvint + start + 9
        endif
    else if (parax(1:5).eq.'DYLOC') then
        if (ic .eq. 1) then
            jparx = jvint + start + 7
        else
            jparx = jvint + start + 10
        endif
    else if (parax(1:5).eq.'DZLOC') then
        if (ic .eq. 1) then
            jparx = jvint + start + 8
        else
            jparx = jvint + start + 11
        endif
    else if (parax(1:4).eq.'VINT') then
        read(parax(5:7),'(I2)') iparax
        jparx = jvint + start + iparax - 1
    else 
        lg = max(1,lxlgut(parax(1:8)))
        call utmess('A', 'UTILITAI_91', sk=parax(1:lg))
        goto 999
    endif
    call wkvect('&&FOCRCH.PARAX', 'V V R', nbinst, jvalx)
    call dcopy(nbinst, zr(jparx), nbvint, zr(jvalx), 1)
20  continue
!
    if (paray(1:4) .eq. 'INST') then
        jvaly = jinst
        goto 22
    else if (paray(1:2).eq.'FN' ) then
        jpary = jvint + start
    else if (paray(1:3).eq.'FT1') then
        jpary = jvint + start + 1
    else if (paray(1:3).eq.'FT2') then
        jpary = jvint + start + 2
    else if (paray(1:2).eq.'VN' ) then
        jpary = jvint + start + 3
    else if (paray(1:3).eq.'VT1') then
        jpary = jvint + start + 4
    else if (paray(1:3).eq.'VT2') then
        jpary = jvint + start + 5
    else if (paray(1:5).eq.'DXLOC') then
        if (ic .eq. 1) then
            jpary = jvint + start + 6
        else
            jpary = jvint + start + 9
        endif
    else if (paray(1:5).eq.'DYLOC') then
        if (ic .eq. 1) then
            jpary = jvint + start + 7
        else
            jpary = jvint + start + 10
        endif
    else if (paray(1:5).eq.'DZLOC') then
        if (ic .eq. 1) then
            jpary = jvint + start + 8
        else
            jpary = jvint + start + 11
        endif
    else if (paray(1:4).eq.'VINT') then
        read(paray(5:7),'(I3)') iparay
        jpary = jvint + start + iparay - 1
    else 
        lg = max(1,lxlgut(paray(1:8)))
        call utmess('A', 'UTILITAI_91', sk=paray(1:lg))
        goto 999
    endif
    call wkvect('&&FOCRCH.PARAY', 'V V R', nbinst, jvaly)
    call dcopy(nbinst, zr(jpary), nbvint, zr(jvaly), 1)
22  continue
!
    if (ind .eq. 0) then
        ASSERT(lxlgut(nomfon).le.24)
        call wkvect(nomfon//'.PROL', base//' V K24', 6, lpro)
        zk24(lpro) = 'FONCTION'
        zk24(lpro+1) = 'LIN LIN '
        zk24(lpro+2) = parax
        zk24(lpro+3) = paray
        zk24(lpro+4) = 'EE'
        zk24(lpro+5) = nomfon
!
        nbval = nbinst * 2
        call wkvect(nomfon//'.VALE', base//' V R', nbval, lval)
        lfon = lval + nbinst
        do 30 ival = 0, nbinst-1
            zr(lval+ival) = zr(jvalx+ival)
            zr(lfon+ival) = zr(jvaly+ival)
30      continue
        ier = 0
!
    else
        fonct1 = '&&FOCRCH.FONCT1'
        ASSERT(lxlgut(fonct1).le.24)
        call wkvect(fonct1//'.PROL', 'V V K24', 6, lpro)
        zk24(lpro) = 'FONCTION'
        zk24(lpro+1) = 'LIN LIN '
        zk24(lpro+2) = 'INST'
        zk24(lpro+3) = parax
        zk24(lpro+4) = 'EE'
        zk24(lpro+5) = fonct1
        nbval = nbinst * 2
        call wkvect(fonct1//'.VALE', 'V V R', nbval, lval)
        lfon = lval + nbinst
        do 100 ival = 0, nbinst-1
            zr(lval+ival) = zr(jinst+ival)
            zr(lfon+ival) = zr(jvalx+ival)
100     continue
!
        fonct2 = '&&FOCRCH.FONCT2'
        ASSERT(lxlgut(fonct2).le.24)
        call wkvect(fonct2//'.PROL', 'V V K24', 6, lpro)
        zk24(lpro) = 'FONCTION'
        zk24(lpro+1) = 'LIN LIN '
        zk24(lpro+2) = 'INST'
        zk24(lpro+3) = paray
        zk24(lpro+4) = 'EE'
        zk24(lpro+5) = fonct2
        nbval = nbinst * 2
        call wkvect(fonct2//'.VALE', 'V V R', nbval, lval)
        lfon = lval + nbinst
        do 110 ival = 0, nbinst-1
            zr(lval+ival) = zr(jinst+ival)
            zr(lfon+ival) = zr(jvaly+ival)
110     continue
!
        call jeveuo(listr//'.VALE', 'L', jval)
        call jelira(listr//'.VALE', 'LONUTI', nbpara)
!
        ASSERT(lxlgut(nomfon).le.24)
        call wkvect(nomfon//'.PROL', base//' V K24', 6, lpro)
        zk24(lpro) = 'FONCTION'
        zk24(lpro+1) = 'LIN LIN '
        zk24(lpro+2) = parax
        zk24(lpro+3) = paray
        zk24(lpro+4) = 'EE'
        zk24(lpro+5) = nomfon
!
        nbval = nbpara * 2
        call wkvect(nomfon//'.VALE', base//' V R', nbval, lval)
        lfon = lval + nbpara
        do 120 ival = 0, nbpara-1
            call fointe('F ', fonct1, 1, 'INST', zr(jval+ival),&
                        zr(lval+ ival), ie)
            call fointe('F ', fonct2, 1, 'INST', zr(jval+ival),&
                        zr(lfon+ ival), ie)
120      continue
!
        call jedetr(fonct1//'.PROL')
        call jedetr(fonct1//'.VALE')
        call jedetr(fonct2//'.PROL')
        call jedetr(fonct2//'.VALE')
        ier = 0
    endif
    if (parax(1:4) .ne. 'INST') call jedetr('&&FOCRCH.PARAX')
    if (paray(1:4) .ne. 'INST') call jedetr('&&FOCRCH.PARAY')
!
999  continue
    call jedema()
end subroutine
