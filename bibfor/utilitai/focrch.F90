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
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
#include "blas/dcopy.h"
    integer :: nsst, int, ind, ier
    character(len=1) :: base
    character(len=16) :: parax, paray
    character(len=8) :: sst, noeud, intitu
    character(len=19) :: nomfon, resu, listr
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
    integer :: jsst
    character(len=8) :: k8b
    character(len=16) :: nomcmd
    character(len=19) :: fonct1, fonct2
!     ----------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ic, ichoc, idec, ie, ival, jdesc, jinst
    integer :: jinti, jncho, jparx, jpary, jval, jvalx, jvaly
    integer :: lfon, lg, lpro, lval, nbchoc, nbinst, nbpara
    integer :: nbval
!-----------------------------------------------------------------------
    call jemarq()
    ier = 9999
    call getres(k8b, k8b, nomcmd)
!
    call jeveuo(resu//'.DESC', 'L', jdesc)
    nbchoc = zi(jdesc+2)
    call jelira(resu//'.DISC', 'LONUTI', nbinst, k8b)
    call jeveuo(resu//'.DISC', 'L', jinst)
    call jeveuo(resu//'.NCHO', 'L', jncho)
    call jeveuo(resu//'.INTI', 'L', jinti)
    if (nsst .ne. 0) call jeveuo(resu//'.SST', 'L', jsst)
    ic = 1
    if (int .ne. 0) then
        do 2 ichoc = 1, nbchoc
            if (zk8(jinti+ichoc-1) .eq. intitu) goto 4
 2      continue
        call u2mesk('A', 'UTILITAI_86', 1, intitu)
        goto 9999
 4      continue
        if (nsst .eq. 0) then
            if (zk8(jncho+ichoc-1) .eq. noeud) goto 16
            ic = 2
            if (zk8(jncho+nbchoc+ichoc-1) .eq. noeud) goto 16
            lg = max(1,lxlgut(noeud))
            call u2mesk('A', 'UTILITAI_87', 1, noeud(1:lg))
            goto 9999
        else
            if (zk8(jsst+ichoc-1) .eq. sst) goto 116
            if (zk8(jsst+nbchoc+ichoc-1) .eq. sst) goto 116
            call u2mess('A', 'UTILITAI_88')
            goto 9999
116          continue
            if (zk8(jncho+ichoc-1) .ne. noeud .and. zk8(jncho+nbchoc+ ichoc-1) .ne. noeud) then
                lg = max(1,lxlgut(noeud))
                call u2mesk('A', 'UTILITAI_89', 1, noeud(1:lg))
                goto 9999
            endif
            if (zk8(jncho+ichoc-1) .eq. noeud .and. zk8(jsst+ichoc-1) .eq. sst) goto 16
            ic = 2
            if (zk8(jncho+nbchoc+ichoc-1) .eq. noeud .and. zk8(jsst+ nbchoc+ichoc-1) .eq. sst) &
            goto 16
            lg = max(1,lxlgut(noeud))
            call u2mesk('A', 'UTILITAI_90', 1, noeud(1:lg))
            goto 9999
        endif
    endif
!     --- RECHERCHE DU NOEUD_1 DE CHOC ---
    do 10 ichoc = 1, nbchoc
        if (zk8(jncho+ichoc-1) .eq. noeud) goto 16
10  end do
!     --- RECHERCHE DU NOEUD_2 DE CHOC ---
    ic = 2
    do 12 ichoc = 1, nbchoc
        if (zk8(jncho+nbchoc+ichoc-1) .eq. noeud) goto 16
12  end do
    lg = max(1,lxlgut(noeud))
    call u2mesk('A', 'UTILITAI_87', 1, noeud(1:lg))
    goto 9999
16  continue
!
    if (parax(1:4) .eq. 'INST') then
        jvalx = jinst
        goto 20
    else if (parax(1:2).eq.'FN') then
        call jeveuo(resu//'.FCHO', 'L', jparx)
        idec = 1 + 3*(ichoc-1)
    else if (parax(1:3).eq.'FT1') then
        call jeveuo(resu//'.FCHO', 'L', jparx)
        idec = 2 + 3*(ichoc-1)
    else if (parax(1:3).eq.'FT2') then
        call jeveuo(resu//'.FCHO', 'L', jparx)
        idec = 3 + 3*(ichoc-1)
    else if (parax(1:2).eq.'VN') then
        call jeveuo(resu//'.VCHO', 'L', jparx)
        idec = 1 + 3*(ichoc-1)
    else if (parax(1:3).eq.'VT1') then
        call jeveuo(resu//'.VCHO', 'L', jparx)
        idec = 2 + 3*(ichoc-1)
    else if (parax(1:3).eq.'VT2') then
        call jeveuo(resu//'.VCHO', 'L', jparx)
        idec = 3 + 3*(ichoc-1)
    else if (parax(1:5).eq.'DXLOC') then
        call jeveuo(resu//'.DLOC', 'L', jparx)
        if (ic .eq. 1) then
            idec = 1 + 3*(ichoc-1)
        else
            idec = 3*nbchoc*nbinst + 1 + 3*(ichoc-1)
        endif
    else if (parax(1:5).eq.'DYLOC') then
        call jeveuo(resu//'.DLOC', 'L', jparx)
        if (ic .eq. 1) then
            idec = 2 + 3*(ichoc-1)
        else
            idec = 3*nbchoc*nbinst + 2 + 3*(ichoc-1)
        endif
    else if (parax(1:5).eq.'DZLOC') then
        call jeveuo(resu//'.DLOC', 'L', jparx)
        if (ic .eq. 1) then
            idec = 3 + 3*(ichoc-1)
        else
            idec = 3*nbchoc*nbinst + 3 + 3*(ichoc-1)
        endif
    else
        lg = max(1,lxlgut(parax(1:8)))
        call u2mesk('A', 'UTILITAI_91', 1, parax(1:lg))
        goto 9999
    endif
    call wkvect('&&FOCRCH.PARAX', 'V V R', nbinst, jvalx)
    call dcopy(nbinst, zr(jparx+idec-1), 3*nbchoc, zr(jvalx), 1)
20  continue
!
    if (paray(1:4) .eq. 'INST') then
        jvaly = jinst
        goto 22
    else if (paray(1:2).eq.'FN') then
        call jeveuo(resu//'.FCHO', 'L', jpary)
        idec = 1 + 3*(ichoc-1)
    else if (paray(1:3).eq.'FT1') then
        call jeveuo(resu//'.FCHO', 'L', jpary)
        idec = 2 + 3*(ichoc-1)
    else if (paray(1:3).eq.'FT2') then
        call jeveuo(resu//'.FCHO', 'L', jpary)
        idec = 3 + 3*(ichoc-1)
    else if (paray(1:2).eq.'VN') then
        call jeveuo(resu//'.VCHO', 'L', jpary)
        idec = 1 + 3*(ichoc-1)
    else if (paray(1:3).eq.'VT1') then
        call jeveuo(resu//'.VCHO', 'L', jpary)
        idec = 2 + 3*(ichoc-1)
    else if (paray(1:3).eq.'VT2') then
        call jeveuo(resu//'.VCHO', 'L', jpary)
        idec = 3 + 3*(ichoc-1)
    else if (paray(1:5).eq.'DXLOC') then
        call jeveuo(resu//'.DLOC', 'L', jpary)
        if (ic .eq. 1) then
            idec = 1 + 3*(ichoc-1)
        else
            idec = 3*nbchoc*nbinst + 1 + 3*(ichoc-1)
        endif
    else if (paray(1:5).eq.'DYLOC') then
        call jeveuo(resu//'.DLOC', 'L', jpary)
        if (ic .eq. 1) then
            idec = 2 + 3*(ichoc-1)
        else
            idec = 3*nbchoc*nbinst + 2 + 3*(ichoc-1)
        endif
    else if (paray(1:5).eq.'DZLOC') then
        call jeveuo(resu//'.DLOC', 'L', jpary)
        if (ic .eq. 1) then
            idec = 3 + 3*(ichoc-1)
        else
            idec = 3*nbchoc*nbinst + 3 + 3*(ichoc-1)
        endif
    else
        lg = max(1,lxlgut(paray(1:8)))
        call u2mesk('A', 'UTILITAI_91', 1, paray(1:lg))
        goto 9999
    endif
    call wkvect('&&FOCRCH.PARAY', 'V V R', nbinst, jvaly)
    call dcopy(nbinst, zr(jpary+idec-1), 3*nbchoc, zr(jvaly), 1)
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
100      continue
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
110      continue
!
        call jeveuo(listr//'.VALE', 'L', jval)
        call jelira(listr//'.VALE', 'LONUTI', nbpara, k8b)
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
9999  continue
    call jedema()
end subroutine
