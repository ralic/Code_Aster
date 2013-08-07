subroutine foimpr(nomf, impr, iul, ind, fonins)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/foec1c.h"
#include "asterfort/foec1f.h"
#include "asterfort/foec1n.h"
#include "asterfort/foec2c.h"
#include "asterfort/foec2f.h"
#include "asterfort/foec2n.h"
#include "asterfort/fointc.h"
#include "asterfort/fointe.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=*) :: nomf, fonins
    integer :: impr, iul, ind
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
!     ROUTINE D'IMPRESSION D'UNE FONCTION SUR UN FICHIER
!     ----------------------------------------------------------------
!
    character(len=19) :: nomfon, nomf1, listr
    character(len=24) :: prol, vale, para
    character(len=24) :: nompar, nomres, titr
    integer :: nbpu
    character(len=8) :: nompu
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ideb, ifin, ii, iret, ival, jval
    integer :: lfon, lfon1, lnova, lprol, lprol1, ltitr, lval
    integer :: lval1, nbfonc, nbnova, nbtitr, nbv, nbv2
    integer :: nbval
    real(kind=8) :: resuim, resure
!-----------------------------------------------------------------------
    call jemarq()
    if (impr .le. 0) goto 9999
    if (iul .le. 0) then
        call u2mess('A', 'UTILITAI2_7')
        goto 9999
    endif
    listr = fonins
    nomf1 = '&&FOIMPR'
!
!     --- NOM DE LA FONCTION A EDITER ---
    nomfon = nomf
    prol = nomfon//'.PROL'
    vale = nomfon//'.VALE'
    para = nomfon//'.PARA'
    titr = nomfon//'.TITR'
!
!     --- IMPRESSION DU TITRE ---
    write(iul,'(/,80(''-''))')
    call jeexin(titr, iret)
    if (iret .ne. 0) then
        call jeveuo(titr, 'L', ltitr)
        call jelira(titr, 'LONMAX', nbtitr)
        do 10 i = 1, nbtitr
            write(iul,*) zk80(ltitr+i-1)
10      continue
    endif
!
!     --- CAS D'UNE FONCTION "FORMULE" ---
    call jeexin(nomfon//'.NOVA', iret)
    if (iret .ne. 0 .and. ind .ne. 0) then
        call jeveuo(nomfon//'.NOVA', 'L', lnova)
        call jelira(nomfon//'.NOVA', 'LONUTI', nbnova)
        if (nbnova .ne. 1) then
            call u2mess('A', 'UTILITAI2_8')
            goto 9999
        endif
        call jeveuo(listr//'.VALE', 'L', jval)
        call jelira(listr//'.VALE', 'LONUTI', nbval)
        nbv = 2 * nbval
        call wkvect(nomf1//'.VALE', 'V V R8', nbv, lval1)
        lfon1 = lval1 + nbval
        do 100 ival = 0, nbval-1
            zr(lval1+ival) = zr(jval+ival)
            call fointe('F ', nomfon, nbnova, zk8(lnova), zr(lval1+ival),&
                        zr(lfon1+ival), iret)
100      continue
!
        ASSERT(lxlgut(nomf1).le.24)
        call wkvect(nomf1//'.PROL', 'V V K24', 6, lprol1)
        zk24(lprol1) = 'FONCTION'
        zk24(lprol1+1) = 'LIN LIN '
        zk24(lprol1+2) = zk8(lnova)
        zk24(lprol1+3) = 'TOUTRESU'
        zk24(lprol1+4) = 'EE'
        zk24(lprol1+5) = nomf1
!
        call foec1f(iul, nomfon, zk24(lprol1), nbval, 'RIEN')
        if (impr .ge. 2) then
            ideb = 1
            ifin = min( 10 ,nbval )
            if (impr .ge. 3) ifin = nbval
            nompar = zk24(lprol1+2)
            nomres = zk24(lprol1+3)
            call foec2f(iul, zr(lval1), nbval, ideb, ifin,&
                        nompar, nomres)
        endif
        call jedetr(nomf1//'.PROL')
        call jedetr(nomf1//'.VALE')
        goto 9999
    endif
!
!     --- INFORMATIONS COMPLEMENTAIRES POUR L'EDITION ---
    call jeveuo(prol, 'L', lprol)
    nompar = zk24(lprol+2)
    nomres = zk24(lprol+3)
!
    if (zk24(lprol) .eq. 'CONSTANT' .or. zk24(lprol) .eq. 'FONCTION') then
!
!        --- NOMBRE DE VALEURS DE LA FONCTION ---
        if (ind .ne. 0) then
            call jelira(listr//'.VALE', 'LONUTI', nbval)
        else
            call jelira(vale, 'LONUTI', nbval)
            nbval= nbval/2
        endif
!
        call foec1f(iul, nomfon, zk24(lprol), nbval, 'RIEN')
        if (impr .ge. 2) then
            call jeveuo(vale, 'L', lval)
            if (ind .ne. 0) then
                call jeveuo(listr//'.VALE', 'L', jval)
                nbv2 = 2 * nbval
                call wkvect(nomf1//'.VALE', 'V V R8', nbv2, lval)
                lfon = lval + nbval
                do 200 ival = 0, nbval-1
                    zr(lval+ival) = zr(jval+ival)
                    call fointe('F ', nomfon, 1, nompar, zr(lval+ival),&
                                zr(lfon+ival), iret)
200              continue
            endif
            ideb = 1
            ifin = min( 10 ,nbval )
            if (impr .ge. 3) ifin = nbval
            call foec2f(iul, zr(lval), nbval, ideb, ifin,&
                        nompar, nomres)
            if (ind .ne. 0) then
                call jedetr(nomf1//'.PROL')
                call jedetr(nomf1//'.VALE')
            endif
        endif
!
    else if (zk24(lprol) .eq. 'NAPPE   ') then
!
        para = nomfon//'.PARA'
        call jelira(para, 'LONMAX', nbfonc)
        call foec1n(iul, nomfon, zk24(lprol), nbfonc, 'RIEN')
        if (impr .ge. 2) then
            call jeveuo(para, 'L', lval)
            ASSERT(ind.eq.0)
            call foec2n(iul, zk24(lprol), zr(lval), vale, nbfonc,&
                        impr)
        endif
!
    else if (zk24(lprol).eq.'FONCT_C ') then
!
        nbpu = 1
        nompu = ' '
        call jelira(vale, 'LONUTI', nbval)
        nbval= nbval/3
        call foec1c(iul, nomfon, zk24(lprol), nbval, 'RIEN')
        if (impr .ge. 2) then
            call jeveuo(vale, 'L', lval)
            if (ind .ne. 0) then
                call jeveuo(listr//'.VALE', 'L', jval)
                call jelira(listr//'.VALE', 'LONUTI', nbval)
                nbv2 = 3 * nbval
                call wkvect(nomf1//'.VALE', 'V V R8', nbv2, lval)
                lfon = lval + nbval
                ii = 0
                do 300 ival = 0, nbval-1
                    zr(lval+ival) = zr(jval+ival)
                    call fointc('F', nomfon, nbpu, nompu, zr(lval+ival),&
                                resure, resuim, iret)
                    zr(lfon+ii) = resure
                    ii = ii + 1
                    zr(lfon+ii) = resuim
                    ii = ii + 1
300              continue
            endif
            ideb = 1
            ifin = min( 10 ,nbval )
            if (impr .ge. 3) ifin = nbval
            call foec2c(iul, zr(lval), nbval, ideb, ifin,&
                        nompar, nomres)
            if (ind .ne. 0) then
                call jedetr(nomf1//'.PROL')
                call jedetr(nomf1//'.VALE')
            endif
        endif
!
    else if (zk24(lprol).eq.'INTERPRE') then
        call u2mesk('A', 'UTILITAI2_10', 1, zk24(lprol))
!
    else
        call u2mesk('A', 'UTILITAI2_11', 1, zk24(lprol))
!
    endif
9999  continue
    call jedema()
end subroutine
