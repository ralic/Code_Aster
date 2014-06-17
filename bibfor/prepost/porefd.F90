subroutine porefd(trange, noeu, cmp, nomrez)
    implicit none
#include "jeveux.h"
#include "asterfort/foc1ma.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    character(len=*) :: trange, noeu, cmp, nomrez
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
!     POST-TRAITEMENT DE "RELA_EFFO_DEPL"
!
! ----------------------------------------------------------------------
    integer ::      nbinst, nbred, inume, jdepl
    integer ::    i, nbmax, ii, ic, imax, nbpara
    parameter    ( nbpara = 8 )
    real(kind=8) :: para(nbpara), xmax, temd, temf, temm
    complex(kind=8) :: c16b
    character(len=8) :: nomres, typara(nbpara), valek(3)
    character(len=16) :: nopara(nbpara)
    character(len=19) :: nomk19
    character(len=24) :: nomk24
    real(kind=8), pointer :: deplmax(:) => null()
    real(kind=8), pointer :: instmax(:) => null()
    integer, pointer :: nlin(:) => null()
    character(len=24), pointer :: redn(:) => null()
    real(kind=8), pointer :: disc(:) => null()
    integer, pointer :: desc(:) => null()
    integer, pointer :: redc(:) => null()
    real(kind=8), pointer :: redd(:) => null()
!
    data nopara / 'RELATION' , 'NOEUD'      , 'CMP',&
     &              'PHASE'    , 'INST_INIT'  , 'INST_FIN',&
     &              'MAXI'     , 'INST_MAXI'  /
    data typara / 'K8' , 'K8' , 'K8' , 'I' , 'R' , 'R' , 'R' , 'R' /
!     ------------------------------------------------------------------
!
    call jemarq()
    c16b=(0.d0,0.d0)
    nomk19 = ' '
    nomk19(1:8) = trange
    nomk24 = ' '
    nomk24(1:8) = noeu
    nomk24(9:16) = cmp
    nomres = nomrez
!
    call tbcrsd(nomres, 'G')
    call tbajpa(nomres, nbpara, nopara, typara)
!
    call jeveuo(nomk19//'.DESC', 'L', vi=desc)
    call jeveuo(nomk19//'.REDN', 'L', vk24=redn)
    call jeveuo(nomk19//'.REDC', 'L', vi=redc)
    call jeveuo(nomk19//'.REDD', 'L', vr=redd)
    call jeveuo(nomk19//'.DISC', 'L', vr=disc)
    call jelira(nomk19//'.DISC', 'LONUTI', nbinst)
    nbred = desc(4)
!
    do 10 inume = 0, nbred-1
        if (redn(inume+1)(1:16) .eq. nomk24) goto 12
10  end do
    call utmess('F', 'PREPOST4_57')
!
12  continue
    valek(1) = redn(inume+1)(17:24)
    valek(2) = noeu
    valek(3) = cmp
!
!     --- RECHERCHE DU MAXIMUM DE LA FONCTION ---
    call wkvect('&&POREFD.DEPL', 'V V R', nbinst, jdepl)
    AS_ALLOCATE(vi=nlin, size=nbinst)
    AS_ALLOCATE(vr=instmax, size=nbinst)
    AS_ALLOCATE(vr=deplmax, size=nbinst)
    do 14 i = 0, nbinst-1
        zr(jdepl+i) = redd(1+inume+nbred*i)
        nlin(1+i) = redc(1+inume+nbred*i)
14  end do
    call foc1ma(nbinst, disc, zr(jdepl), nbmax, instmax,&
deplmax)
!
!     --- RECHERCHE DES PHASES NON-LINEAIRE ---
    do 18 i = 0, nbinst-1
        if (nlin(1+i) .eq. 1) goto 20
18  end do
    goto 500
!
20  continue
!
    ii = 0
    ic = 0
    do 30 i = 0, nbinst-1
        if (nlin(1+i) .eq. 1 .and. ic .eq. 0) then
            xmax = zr(jdepl+i)
            imax = i
            ic = 1
            ii = ii + 1
            temd = disc(1+i)
        else if (nlin(1+i) .eq. 1) then
            if (abs(zr(jdepl+i)) .gt. abs(xmax)) then
                xmax = zr(jdepl+i)
                imax = i
            endif
        else if (nlin(1+i).eq.0 .and. ic.eq.1) then
            ic = 0
            temf = disc(i)
            temm = disc(imax+1)
            para(1) = temd
            para(2) = temf
            para(3) = xmax
            para(4) = temm
            call tbajli(nomres, nbpara, nopara, [ii], para,&
                        [c16b], valek, 0)
        endif
30  end do
    if (ic .eq. 1) then
        temf = disc(nbinst)
        temm = disc(imax+1)
        para(1) = temd
        para(2) = temf
        para(3) = xmax
        para(4) = temm
        call tbajli(nomres, nbpara, nopara, [ii], para,&
                    [c16b], valek, 0)
    endif
!
500  continue
    call jedetr('&&POREFD.DEPL')
    AS_DEALLOCATE(vi=nlin)
    AS_DEALLOCATE(vr=instmax)
    AS_DEALLOCATE(vr=deplmax)
!
    call jedema()
end subroutine
