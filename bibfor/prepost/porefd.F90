subroutine porefd(trange, noeu, cmp, nomrez)
    implicit none
#include "jeveux.h"
#include "asterfort/foc1ma.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nlget.h"
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
!
!     POST-TRAITEMENT DE "RELA_EFFO_DEPL"
!
! ----------------------------------------------------------------------
    integer ::      nbpt, nbred, inume, jdepl, nbnoli, nbvint, inl, start
    integer ::    i, nbmax, ii, ic, imax, nbpara
    parameter    ( nbpara = 8 )
    real(kind=8) :: para(nbpara), xmax, temd, temf, temm
    complex(kind=8) :: c16b
    character(len=8) :: nomres, typara(nbpara), valek(3)
    character(len=16) :: nopara(nbpara), nomk16
    character(len=19) :: nomk19
    character(len=24) :: identifier
    real(kind=8), pointer :: deplmax(:) => null()
    real(kind=8), pointer :: instmax(:) => null()
    integer, pointer :: nlin(:) => null()
    real(kind=8), pointer :: disc(:) => null()
    integer, pointer :: desc(:) => null()
    integer, pointer :: rdindx(:) => null()
!
    integer          , pointer :: nltype(:) => null()
    integer          , pointer :: vindx (:) => null()
    character(len=24), pointer :: nlname(:) => null()
    real(kind=8)     , pointer :: vint  (:) => null()
!
    data nopara / 'RELATION' , 'NOEUD'      , 'CMP',&
     &              'PHASE'    , 'INST_INIT'  , 'INST_FIN',&
     &              'MAXI'     , 'INST_MAXI'  /
    data typara / 'K8' , 'K8' , 'K8' , 'I' , 'R' , 'R' , 'R' , 'R' /
!     ------------------------------------------------------------------
!
    call jemarq()
    c16b=(0.d0,0.d0)
    nomk16 = '                '
    nomk19 = '                   '
    nomk19(1:8) = trange
    nomk16(1:8) = trange
    nomres = nomrez
!
    call tbcrsd(nomres, 'G')
    call tbajpa(nomres, nbpara, nopara, typara)
!
    call jeveuo(nomk19//'.DESC', 'L', vi=desc)
    nbnoli = desc(3)

    call jeveuo(nomk19//'.DISC', 'L', vr=disc)
    call jelira(nomk19//'.DISC', 'LONUTI', nbpt)

    call jeveuo(nomk16//'.NL.TYPE', 'L', vi  =nltype)
    call jeveuo(nomk16//'.NL.VIND', 'L', vi  =vindx)
    call jeveuo(nomk16//'.NL.INTI', 'L', vk24=nlname)
    call jeveuo(nomk16//'.NL.VINT', 'L', vr  =vint)
    nbvint = vindx(nbnoli+1)-1

    AS_ALLOCATE(vi=rdindx, size=nbnoli)
    nbred = 0
    do i = 1, nbnoli
        if (nltype(i).eq.NL_FX_RELATIONSHIP) then
            nbred = nbred + 1
            rdindx(nbred) = i
        end if
    end do

    do inume = 1, nbred
        i = rdindx(inume)
        identifier = nlname((i-1)*5+1)
        if ((identifier(1:8).eq.noeu) .and. (identifier(9:16).eq.cmp)) goto 12
    end do
    call utmess('F', 'PREPOST4_57')

12  continue
    AS_DEALLOCATE(vi=rdindx)
    inl = i
!
    valek(1) = identifier(17:24)
    valek(2) = noeu
    valek(3) = cmp
!
!     --- RECHERCHE DU MAXIMUM DE LA FONCTION ---
    call wkvect('&&POREFD.DEPL', 'V V R', nbpt, jdepl)
    AS_ALLOCATE(vi=nlin, size=nbpt)
    AS_ALLOCATE(vr=instmax, size=nbpt)
    AS_ALLOCATE(vr=deplmax, size=nbpt)
    start = vindx(inl)
    do i = 1, nbpt
        zr(jdepl-1+i) =      vint((i-1)*nbvint+start-1+1)
        nlin(i)       = nint(vint((i-1)*nbvint+start-1+3))
    end do

    call jelibe(nomk16//'.NL.TYPE')
    call jelibe(nomk16//'.NL.VIND')
    call jelibe(nomk16//'.NL.INTI')
    call jelibe(nomk16//'.NL.VINT')

    call foc1ma(nbpt, disc, zr(jdepl), nbmax, instmax,&
                deplmax)
!
!     --- RECHERCHE DES PHASES NON-LINEAIRE ---
    do i = 0, nbpt-1
        if (nlin(1+i) .eq. 1) goto 20
    end do
    goto 500
!
20  continue
!
    ii = 0
    ic = 0
    do i = 0, nbpt-1
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
    end do
    if (ic .eq. 1) then
        temf = disc(nbpt)
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
