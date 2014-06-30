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
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
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
    integer :: gd,  num, ncmpmx, iad, nec
    integer ::   np,  jlno, i, ibid, kvale, itype
    integer :: nc, iec, icmp, ianueq, iaprno, j, ino, ncmp, icompt
    integer ::   jnuav, jnual, ival, k, ieq
    character(len=4) :: type
    character(len=8) :: noma, nomgd
    character(len=19) :: kchno, klno, knuage, nonu
    logical(kind=1) :: lnual, prem
    integer, pointer :: ent_cod(:) => null()
    integer, pointer :: nomcmp(:) => null()
    real(kind=8), pointer :: nuax(:) => null()
    integer, pointer :: desc(:) => null()
    integer, pointer :: nuai(:) => null()
    real(kind=8), pointer :: nkcoor(:) => null()
    character(len=24), pointer :: refe(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
    kchno = chno
    klno = lno
    knuage = nuage
    lnual = .false.
!
    call jeveuo(kchno//'.DESC', 'L', vi=desc)
    gd = desc(1)
    num = desc(2)
    call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', ncmpmx)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', iad)
    call jenuno(jexnum('&CATA.GD.NOMGD', gd), nomgd)
    nec = nbec(gd)
    AS_ALLOCATE(vi=nomcmp, size=ncmpmx)
    AS_ALLOCATE(vi=ent_cod, size=nec)
!
    call jeveuo(kchno//'.REFE', 'L', vk24=refe)
    noma = refe(1) (1:8)
    nonu = refe(2) (1:19)
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=np)
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=nkcoor)
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
            ent_cod(iec) = desc(2+iec)
        end do
        do icmp = 1, ncmpmx
            if (exisdg(ent_cod, icmp )) nomcmp(icmp) = icmp
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
                ent_cod(iec) = zi(iaprno-1+ (ino-1)*(nec+2)+2+iec )
            end do
            icompt = 0
            do icmp = 1, ncmpmx
                if (exisdg(ent_cod, icmp )) then
                    icompt = icompt + 1
                    nomcmp(icmp) = icmp
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
    call jeveuo(knuage//'.NUAI', 'E', vi=nuai)
    nuai(1) = np
    nuai(2) = nx
    nuai(3) = nc
    nuai(4) = gd
    nuai(5) = itype
    icmp = 0
    do i = 1, ncmpmx
        if (nomcmp(i) .ne. 0) then
            icmp = icmp + 1
            nuai(1+5+icmp-1) = nomcmp(i)
        endif
    end do
!
!     --- .NUAX ---
!
    call jeveuo(knuage//'.NUAX', 'E', vr=nuax)
    do i = 1, np
        do j = 1, nx
            nuax(nx*(i-1)+j) = nkcoor(3*(i-1)+j)
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
                if (exisdg(ent_cod, icmp )) then
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
                ent_cod(iec) = zi(iaprno-1+ (ino-1)*(nec+2)+2+iec )
            end do
            icompt = 0
            do icmp = 1, ncmpmx
                if (exisdg(ent_cod, icmp )) then
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
    AS_DEALLOCATE(vi=nomcmp)
    AS_DEALLOCATE(vi=ent_cod)
    call jedetr('&&CNONUA.NOEUD')
    call jedema()
end subroutine
