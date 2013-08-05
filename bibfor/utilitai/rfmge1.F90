subroutine rfmge1(modgen)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/getvis.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsutnu.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=*) :: modgen
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     OPERATEUR "RECU_FONCTION"  MOT CLE "RESU_GENE"
!                                CONCEPT MODE_GENE
!     ------------------------------------------------------------------
    integer :: n1, ncmp, iret, jordr, lpro, lvar, lfon, nbordr, im, iord, iad
    integer :: jvale, jrefe, jdeeq, jnume, nbmode, i, istru, ibid
    real(kind=8) :: epsi
    character(len=4) :: interp(2)
    character(len=8) :: k8b, crit, mode
    character(len=14) :: nugene
    character(len=16) :: k16b, nomcmd, typcon, nomcha, npara
    character(len=19) :: noch19, nomfon, knume
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
!
    call getres(nomfon, typcon, nomcmd)
!
    interp(1) = 'NON '
    interp(2) = 'NON '
!
    call getvtx(' ', 'CRITERE', 1, iarg, 1,&
                crit, n1)
    call getvr8(' ', 'PRECISION', 1, iarg, 1,&
                epsi, n1)
    call getvtx(' ', 'INTERPOL', 1, iarg, 2,&
                interp, n1)
    if (n1 .eq. 1) interp(2) = interp(1)
!
    knume = '&&RFMGE1.NUME_ORDR'
    call rsutnu(modgen, ' ', 1, knume, nbordr,&
                epsi, crit, iret)
    if (iret .ne. 0) then
        call u2mess('F', 'UTILITAI4_11')
    endif
    call jeveuo(knume, 'L', jordr)
!
!     --- CREATION DE LA FONCTION ---
!
    ASSERT(lxlgut(nomfon).le.24)
    call wkvect(nomfon//'.PROL', 'G V K24', 6, lpro)
    zk24(lpro) = 'FONCTION        '
    zk24(lpro+1) = interp(1)//interp(2)
    zk24(lpro+2) = 'FREQ            '
    zk24(lpro+4) = 'EE              '
    zk24(lpro+5) = nomfon
!
    call wkvect(nomfon//'.VALE', 'G V R', 2*nbordr, lvar)
    lfon = lvar + nbordr - 1
!
    call getvtx(' ', 'NOM_PARA_RESU', 1, iarg, 1,&
                npara, n1)
    if (n1 .ne. 0) then
        zk24(lpro+3) = npara
        do 200 iord = 1, nbordr
            call rsadpa(modgen, 'L', 1, 'FREQ', zi(jordr+iord-1),&
                        0, iad, k8b)
            zr(lvar-1+iord) = zr(iad)
            call rsadpa(modgen, 'L', 1, npara, zi(jordr+iord-1),&
                        0, iad, k8b)
            zr(lfon-1+iord) = zr(iad)
200      continue
        goto 9999
    endif
!
    call getvtx(' ', 'NOM_CHAM', 1, iarg, 1,&
                nomcha, n1)
    call getvis(' ', 'NUME_CMP_GENE', 1, iarg, 1,&
                ncmp, n1)
!
    zk24(lpro+3) = nomcha
!
    do 100 iord = 1, nbordr
!
        call rsexch('F', modgen, nomcha, zi(jordr+iord-1), noch19,&
                    iret)
        call rsadpa(modgen, 'L', 1, 'FREQ', zi(jordr+iord-1),&
                    0, iad, k8b)
        zr(lvar+iord) = zr(iad)
!
        call jeveuo(noch19//'.VALE', 'L', jvale)
        call jelira(noch19//'.VALE', 'TYPE', ibid, k16b)
        if (k16b(1:1) .ne. 'R') then
            call u2mess('F', 'UTILITAI4_17')
        endif
!
        call jeveuo(noch19//'.REFE', 'L', jrefe)
        mode = zk24(jrefe)(1:8)
        if (mode .eq. '        ') then
            nugene = zk24(jrefe+1)(1:14)
            call jeveuo(nugene//'.NUME.DEEQ', 'L', jdeeq)
            call jeveuo(nugene//'.NUME.NEQU', 'L', jnume)
            nbmode = zi(jnume)
            im = 0
            do 110 i = 1, nbmode
                istru = zi(jdeeq+2*(i-1)+2-1)
                if (istru .lt. 0) goto 110
                im = im + 1
                if (im .eq. ncmp) goto 114
110          continue
            call u2mess('F', 'UTILITAI4_14')
114          continue
            im = i
        else
            im = ncmp
        endif
!
        zr(lfon+iord) = zr(jvale+im-1)
!
100  end do
!
9999  continue
!
    call jedetr(knume)
!
    call jedema()
end subroutine
