subroutine usupu2(nbpt, nbpair, coef, ang, isupp,&
                  nbinst, temps, puusur, vustub, vusob,&
                  pus, pmoye, pourpu, poupre)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     CALCULE LA PUISSANCE D'USURE AU SENS D'ARCHARD
!                    PU  =  FN * VT
!
! OUT : PUUSUR : PUISSANCE USURE
!-----------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/impus.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/nlget.h"
#include "asterfort/stapu2.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
    character(len=8) :: noeu
    character(len=16) :: nomk16
    character(len=19) :: trange
    integer :: nbpair, nbinst
    real(kind=8) :: coef(*), ang(*), temps(*)
    real(kind=8) :: vustub(nbpair, nbinst), vusob(nbpair, nbinst)
    real(kind=8) :: pus(*), pmoye, pourpu(*), poupre(*)
!-----------------------------------------------------------------------
    integer :: ichoc, idebut, ifin, ifires, isupp, j
    integer :: nbtot, jwk1, i, ic
    integer :: jwk2, jwk3, jwk4, jwk5, jwk6, lg
    integer :: n1, n2, n3, n4, nbchoc, nbloc
    integer :: nbpas, nbpt, nbval, nt, nbvint, dec, nbnoli
    real(kind=8) :: puusur, tdebut, tfin, tmax, tmin
!
    integer, pointer :: chindx(:) => null()
    real(kind=8), pointer :: fcho(:) => null()
    real(kind=8), pointer :: dloc(:) => null()
    real(kind=8), pointer :: disc(:) => null()
    character(len=8), pointer :: ncho(:) => null()
    integer, pointer :: desc(:) => null()
    real(kind=8), pointer :: vcho(:) => null()
    real(kind=8), pointer :: vint(:) => null()
    integer          , pointer :: nltype(:) => null()
    integer          , pointer :: vindx (:) => null()
    character(len=24), pointer :: nlname(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    ifires = iunifi('RESULTAT')
    nbpt = 0
!
    call getvid(' ', 'RESU_GENE', scal=trange, nbret=nt)
    if (nt.eq.0) goto 999

    call jeveuo(trange//'.DESC', 'L', vi=desc)
    nbnoli = desc(3)

    nomk16 = trange(1:16)
    call jeveuo(nomk16//'.NL.TYPE', 'L', vi  =nltype)
    call jeveuo(nomk16//'.NL.VIND', 'L', vi  =vindx)
    call jeveuo(nomk16//'.NL.INTI', 'L', vk24=nlname)
    call jeveuo(nomk16//'.NL.VINT', 'L', vr  =vint)
    nbvint = vindx(nbnoli+1)-1
!
    AS_ALLOCATE(vi=chindx, size=nbnoli)

    nbchoc = 0
    do i = 1, nbnoli
        if (nltype(i).eq.NL_CHOC) then
            nbchoc = nbchoc + 1
            chindx(nbchoc) = i
        end if
    end do

    if (nbchoc.eq.0) call utmess('F', 'PREPOST4_84')

    nbtot = nbchoc

    call jeveuo(trange//'.DISC', 'L'     , vr=disc)
    call jelira(trange//'.DISC', 'LONMAX', nbpt)

    AS_ALLOCATE(vr =fcho, size=  3*nbtot*nbpt)
    AS_ALLOCATE(vr =dloc, size=2*3*nbtot*nbpt)
    AS_ALLOCATE(vr =vcho, size=  3*nbtot*nbpt)
    AS_ALLOCATE(vk8=ncho, size=  2*nbtot)

    do ic = 1, nbchoc
        i = chindx(ic)
        do j = 1, nbpt
            fcho((j-1)*3*nbtot+(ic-1)*3+1)     = vint((j-1)*nbvint+vindx(i)-1+1)
            fcho((j-1)*3*nbtot+(ic-1)*3+2)     = vint((j-1)*nbvint+vindx(i)-1+2)
            fcho((j-1)*3*nbtot+(ic-1)*3+3)     = vint((j-1)*nbvint+vindx(i)-1+3)

            dloc((j-1)*3*nbtot+(ic-1)*3+1)     = vint((j-1)*nbvint+vindx(i)-1+4)
            dloc((j-1)*3*nbtot+(ic-1)*3+2)     = vint((j-1)*nbvint+vindx(i)-1+5)
            dloc((j-1)*3*nbtot+(ic-1)*3+3)     = vint((j-1)*nbvint+vindx(i)-1+6)
            dec = 3*nbtot*nbpt
            dloc(dec+(j-1)*3*nbtot+(ic-1)*3+1) = vint((j-1)*nbvint+vindx(i)-1+7)
            dloc(dec+(j-1)*3*nbtot+(ic-1)*3+2) = vint((j-1)*nbvint+vindx(i)-1+8)
            dloc(dec+(j-1)*3*nbtot+(ic-1)*3+3) = vint((j-1)*nbvint+vindx(i)-1+9)

            vcho((j-1)*3*nbtot+(ic-1)*3+1)     = vint((j-1)*nbvint+vindx(i)-1+10)
            vcho((j-1)*3*nbtot+(ic-1)*3+2)     = vint((j-1)*nbvint+vindx(i)-1+11)
            vcho((j-1)*3*nbtot+(ic-1)*3+3)     = vint((j-1)*nbvint+vindx(i)-1+12)
        end do
        ncho(ic)       = nlname((i-1)*5+2)(1:8)
        ncho(nbtot+ic) = nlname((i-1)*5+3)(1:8)
    end do


    call getvr8(' ', 'PUIS_USURE', scal=puusur, nbret=n1)
    if (n1 .ne. 0) then
        call impus(ifires, 0, puusur)
        goto 999
    endif
!

    call getvis(' ', 'NB_BLOC', scal=nbloc, nbret=n1)
    if (n1 .eq. 0) nbloc = 1
    call getvr8(' ', 'INST_INIT', scal=tdebut, nbret=n2)
    call getvr8(' ', 'INST_FIN', scal=tfin, nbret=n3)
    call getvtx(' ', 'NOEUD', scal=noeu, nbret=n4)
!
!           --- RECHERCHE DU NOEUD DE CHOC ---
    do 10 ichoc = 1, nbchoc
        if (ncho(ichoc) .eq. noeu) goto 12
10  continue

    lg = max(1,lxlgut(noeu))
    call utmess('F', 'UTILITAI_87', sk=noeu(1:lg))
    goto 999

12  continue
!
    tmax = disc(nbpt)
    tmin = disc(1)

    if (n2 .eq. 0) then
        tdebut = tmin
    else
        if (tdebut .lt. tmin) tdebut = tmin
    endif
    if (n3 .eq. 0) then
        tfin = tmax
    else
        if (tfin .gt. tmax) tfin = tmax
    endif
    if (tdebut .ge. tfin) then
        call utmess('F', 'PREPOST4_47')
    endif

    do 14 j = 1, nbpt
        if (disc(j) .ge. tdebut) then
            idebut = j
            goto 15
        endif
14  continue
15  continue

    do 16 j = 1, nbpt
        if (disc(j) .ge. tfin) then
            ifin = j
            goto 17
        endif
16  continue
17  continue

    nbpas = ifin - idebut + 1
    if (nbloc .eq. 0) nbloc = 1
    nbval = nbpas / nbloc
!
    call wkvect('&&USURPU.WK1', 'V V R', nbpt, jwk1)
    call wkvect('&&USURPU.WK2', 'V V R', nbpt, jwk2)
    call wkvect('&&USURPU.WK3', 'V V R', nbpt, jwk3)
!
    call wkvect('&&USURPU.WK4', 'V V R', nbpt, jwk4)
    call wkvect('&&USURPU.WK5', 'V V R', nbpt, jwk5)
    call wkvect('&&USURPU.WK6', 'V V R', nbpt, jwk6)
!
    call stapu2(nbchoc, nbpt, nbpair, disc, fcho,&
                vcho, dloc, coef, ang, zr(jwk1),&
                zr(jwk2), zr(jwk3), zr( jwk4), zr(jwk5), zr(jwk6),&
                idebut, nbloc, nbval, ichoc, isupp,&
                nbinst, temps, puusur, vustub, vusob,&
                pus, pmoye, pourpu, poupre)
!
    call jedetr('&&USURPU.WK1')
    call jedetr('&&USURPU.WK2')
    call jedetr('&&USURPU.WK3')
    call jedetr('&&USURPU.WK4')
    call jedetr('&&USURPU.WK5')
    call jedetr('&&USURPU.WK6')

    AS_DEALLOCATE(vi=chindx)
    AS_DEALLOCATE(vr =dloc)
    AS_DEALLOCATE(vr =fcho)
    AS_DEALLOCATE(vr =vcho)
    AS_DEALLOCATE(vk8=ncho)

999 continue
    call jedema()
end subroutine
