subroutine rvchn1(deplaz, nomjv, nbno, numnd, pgl)
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedupo.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbec.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvgl.h"
!
    integer :: nbno, numnd(*)
    character(len=*) :: deplaz, nomjv
    real(kind=8) :: pgl(3, 3)
! ----------------------------------------------------------------------
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
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: ibid, gd, iec, nec, ncmpmx, icompt, ino, icmp, jprno
    integer :: iad, tabec(10), iavald, nunoe, numdx, numdy, numdz, numdrx
    integer :: numdry, numdrz, nuddl
    real(kind=8) :: valed(3), vald(3), valer(3), valr(3)
    character(len=8) :: k8b, nomcmp
    character(len=19) :: prno, depla
    integer, pointer :: nueq(:) => null()
!     ------------------------------------------------------------------
    call jemarq()
!
    depla = deplaz
!
    call dismoi('PROF_CHNO', deplaz, 'CHAM_NO', repk=prno)
    call dismoi('NUM_GD', deplaz, 'CHAM_NO', repi=gd)
    call dismoi('NOM_GD', deplaz, 'CHAM_NO', repk=k8b)
    if (k8b(1:6) .ne. 'DEPL_R') then
        call utmess('F', 'POSTRELE_17')
    endif
!
    call jenonu(jexnom(prno//'.LILI', '&MAILLA'), ibid)
    call jeveuo(jexnum(prno//'.PRNO', ibid), 'L', jprno)
    call jeveuo(prno//'.NUEQ', 'L', vi=nueq)
!
    nec = nbec( gd )
    if (nec .gt. 10) then
        call utmess('F', 'POSTRELE_53')
    endif
    call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', iad)
    call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', ncmpmx)
!
    call jedupo(depla//'.VALE', 'V', nomjv, .false._1)
    call jeveuo(nomjv, 'E', iavald)
!
    do ino = 1, nbno
        nunoe = numnd(ino)
        do iec = 1, nec
            tabec(iec)= zi(jprno-1+(nunoe-1)*(nec+2)+2+iec )
        end do
        numdx = 0
        numdy = 0
        numdz = 0
        numdrx = 0
        numdry = 0
        numdrz = 0
        valed(1) = 0.0d0
        valed(2) = 0.0d0
        valed(3) = 0.0d0
        valer(1) = 0.0d0
        valer(2) = 0.0d0
        valer(3) = 0.0d0
        icompt = 0
        do icmp = 1, ncmpmx
            if (exisdg(tabec,icmp)) then
                icompt = icompt + 1
                nomcmp = zk8(iad-1+icmp)
                nuddl = nueq(1+zi(jprno+(nec+2)*(nunoe-1))-1)+ icompt-1
                if (nomcmp .eq. 'DX') then
                    numdx = nuddl
                    valed(1) = zr(iavald-1+numdx)
                else if (nomcmp .eq. 'DY') then
                    numdy = nuddl
                    valed(2) = zr(iavald-1+numdy)
                else if (nomcmp .eq. 'DZ') then
                    numdz = nuddl
                    valed(3) = zr(iavald-1+numdz)
                else if (nomcmp .eq. 'DRX') then
                    numdrx = nuddl
                    valer(1) = zr(iavald-1+numdrx)
                else if (nomcmp .eq. 'DRY') then
                    numdry = nuddl
                    valer(2) = zr(iavald-1+numdry)
                else if (nomcmp .eq. 'DRZ') then
                    numdrz = nuddl
                    valer(3) = zr(iavald-1+numdrz)
                endif
            endif
        end do
        if ((numdx+numdy+numdz) .eq. 0) goto 22
        call utpvgl(1, 3, pgl, valed, vald)
        if (numdx .ne. 0) zr(iavald-1+numdx) = vald(1)
        if (numdy .ne. 0) zr(iavald-1+numdy) = vald(2)
        if (numdz .ne. 0) zr(iavald-1+numdz) = vald(3)
 22     continue
        if ((numdrx+numdry+numdrz) .eq. 0) goto 30
        call utpvgl(1, 3, pgl, valer, valr)
        if (numdrx .ne. 0) zr(iavald-1+numdrx) = valr(1)
        if (numdry .ne. 0) zr(iavald-1+numdry) = valr(2)
        if (numdrz .ne. 0) zr(iavald-1+numdrz) = valr(3)
 30     continue
    end do
!
    call jedema()
end subroutine
