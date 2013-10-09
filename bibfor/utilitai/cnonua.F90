subroutine cnonua(nx, chno, lno, nuage)
    implicit none
#include "jeveux.h"
#include "asterfort/crenua.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbec.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nx
    character(len=*) :: chno, lno, nuage
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
!     PASSAGE D'UNE SD CHAM_NO A UNE SD NUAGE
!
! IN  NX     : DIMENSION D'ESPACE DU NUAGE (1,2 OU 3)
! IN  CHNO   : NOM DE LA SD CHAM_NO
! IN  LNO    : LISTE DES NOEUDS A PRENDRE EN COMPTE
! OUT NUAGE  : SD NUAGE PRODUITE
!     ------------------------------------------------------------------
    integer :: gd, jdesc, num, ncmpmx, iad, nec, kcomp
    integer :: iaec, jrefe, np, kcoor, jlno, i, ibid, kvale, itype
    integer :: nc, iec, icmp, ianueq, iaprno, j, ino, ncmp, icompt
    integer :: jnuai, jnuax, jnuav, jnual, ival, k, ieq
    character(len=4) :: type
    character(len=8) :: noma, nomgd
    character(len=19) :: kchno, klno, knuage, nonu
    logical :: lnual, prem
!     ------------------------------------------------------------------
!
    call jemarq()
    kchno = chno
    klno = lno
    knuage = nuage
    lnual = .false.
!
    call jeveuo(kchno//'.DESC', 'L', jdesc)
    gd = zi(jdesc-1+1)
    num = zi(jdesc-1+2)
    call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', ncmpmx)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', iad)
    call jenuno(jexnum('&CATA.GD.NOMGD', gd), nomgd)
    nec = nbec(gd)
    call wkvect('&&CNONUA.NOMCMP', 'V V I', ncmpmx, kcomp)
    call wkvect('&&CNONUA.ENT_COD', 'V V I', nec, iaec)
!
    call jeveuo(kchno//'.REFE', 'L', jrefe)
    noma = zk24(jrefe-1+1) (1:8)
    nonu = zk24(jrefe-1+2) (1:19)
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=np)
    call jeveuo(noma//'.COORDO    .VALE', 'L', kcoor)
!
    if (klno .ne. ' ') then
        call jelira(klno, 'LONUTI', np)
        call jeveuo(klno, 'L', jlno)
    else
        call wkvect('&&CNONUA.NOEUD', 'V V I', np, jlno)
        do i = 1, np
            zi(jlno+i-1) = i
        end do
    endif
!
    call jelira(kchno//'.VALE', 'TYPE', cval=type)
    call jeveuo(kchno//'.VALE', 'L', kvale)
    if (type(1:1) .eq. 'R') then
        itype = 1
    else if (type(1:1) .eq. 'C') then
        itype = 2
    else
        call utmess('F', 'UTILITAI_36')
    endif
!
!
!     --SI LE CHAMP EST A REPRESENTATION CONSTANTE ---
!     ---------------------------------------------------
    if (num .lt. 0) then
        nc = -num
        do iec = 1, nec
            zi(iaec+iec-1) = zi(jdesc-1+2+iec)
        end do
        do icmp = 1, ncmpmx
            if (exisdg(zi(iaec) , icmp )) zi(kcomp+icmp-1) = icmp
        end do
    else
!
!
!     --- SI LE CHAMP EST DECRIT PAR 1 "PRNO" ---
!     ---------------------------------------------------
        prem = .true.
        call jeveuo(nonu//'.NUEQ', 'L', ianueq)
        call jenonu(jexnom(nonu//'.LILI', '&MAILLA'), ibid)
        call jeveuo(jexnum(nonu//'.PRNO', ibid), 'L', iaprno)
        do j = 1, np
            ino = zi(jlno+j-1)
            ncmp = zi(iaprno-1+ (ino-1)*(nec+2)+2 )
            if (ncmp .eq. 0) goto 110
            do iec = 1, nec
                zi(iaec+iec-1) = zi(iaprno-1+ (ino-1)*(nec+2)+2+iec )
            end do
            icompt = 0
            do icmp = 1, ncmpmx
                if (exisdg(zi(iaec) , icmp )) then
                    icompt = icompt + 1
                    zi(kcomp+icmp-1) = icmp
                endif
            end do
            if (prem) then
                nc = icompt
                prem = .false.
            else
                if (nc .ne. icompt) then
                    nc = max( nc , icompt )
                    lnual = .true.
                endif
            endif
110         continue
        end do
    endif
!
    call crenua(nuage, nomgd, np, nx, nc,&
                lnual)
!
!     --- .NUAI ---
!
    call jeveuo(knuage//'.NUAI', 'E', jnuai)
    zi(jnuai ) = np
    zi(jnuai+1) = nx
    zi(jnuai+2) = nc
    zi(jnuai+3) = gd
    zi(jnuai+4) = itype
    icmp = 0
    do i = 1, ncmpmx
        if (zi(kcomp+i-1) .ne. 0) then
            icmp = icmp + 1
            zi(jnuai+5+icmp-1) = zi(kcomp+i-1)
        endif
    end do
!
!     --- .NUAX ---
!
    call jeveuo(knuage//'.NUAX', 'E', jnuax)
    do i = 1, np
        do j = 1, nx
            zr(jnuax-1+nx*(i-1)+j) = zr(kcoor-1+3*(i-1)+j)
        end do
    end do
!
!     --- .NUAV ---
!
    call jeveuo(knuage//'.NUAV', 'E', jnuav)
!
!     --- .NUAL ---
!
    if (lnual) call jeveuo(knuage//'.NUAL', 'E', jnual)
!
!     --SI LE CHAMP EST A REPRESENTATION CONSTANTE ---
!
    if (num .lt. 0) then
        ncmp = -num
        do j = 1, np
            ino = zi(jlno+j-1)
            ival = ncmp * ( ino - 1 )
            icompt = 0
            do icmp = 1, ncmpmx
                if (exisdg(zi(iaec) , icmp )) then
                    icompt = icompt + 1
                    k = nc*(j-1) + icompt
                    if (itype .eq. 1) then
                        zr(jnuav+k-1) = zr(kvale-1+ival+icmp)
                    else
                        zc(jnuav+k-1) = zc(kvale-1+ival+icmp)
                    endif
                endif
            end do
        end do
    else
!
!     --- SI LE CHAMP EST DECRIT PAR 1 "PRNO" ---
!
        call jeveuo(nonu//'.NUEQ', 'L', ianueq)
        call jenonu(jexnom(nonu//'.LILI', '&MAILLA'), ibid)
        call jeveuo(jexnum(nonu//'.PRNO', ibid), 'L', iaprno)
        do j = 1, np
            ino = zi(jlno+j-1)
            ival = zi(iaprno-1+ (ino-1)*(nec+2)+1 )
            ncmp = zi(iaprno-1+ (ino-1)*(nec+2)+2 )
            if (ncmp .eq. 0) goto 210
            do iec = 1, nec
                zi(iaec+iec-1) = zi(iaprno-1+ (ino-1)*(nec+2)+2+iec )
            end do
            icompt = 0
            do icmp = 1, ncmpmx
                if (exisdg(zi(iaec) , icmp )) then
                    icompt = icompt + 1
                    ieq = zi(ianueq-1+ival-1+icompt)
                    k = nc*(j-1) + icompt
                    if (lnual) zl(jnual+k-1) = .true.
                    if (itype .eq. 1) then
                        zr(jnuav+k-1) = zr(kvale-1+ieq)
                    else
                        zc(jnuav+k-1) = zc(kvale-1+ieq)
                    endif
                endif
            end do
210         continue
        end do
    endif
!
    call jedetr('&&CNONUA.NOMCMP')
    call jedetr('&&CNONUA.ENT_COD')
    call jedetr('&&CNONUA.NOEUD')
    call jedema()
end subroutine
