subroutine ssmau2(nomu, option)
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
    implicit none
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/crmeam.h"
#include "asterfort/crmema.h"
#include "asterfort/dismoi.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mtdsc2.h"
#include "asterfort/mtdscr.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: nomu
    character(len=*) :: option
! ----------------------------------------------------------------------
!     BUT:
!          CONDENSATION DE LA MATRICE DE MASSE (OU D'AMORTISSEMENT)
!          D'UN MACR_ELEM_STAT:
!          CALCUL DE MP_EE = M_EE + PHI_EI*M_II*PHI_IE
!                            + M_EI*PHI_IE + PHI_EI*M_IE
!   ATTENTION LE PHI_IE D'ASTER EST L'OPPOSE DE CELUI DE LA FORMULE
!
!     IN: NOMU   : NOM DU MACR_ELEM_STAT
!         OPTION : 'MASS_MECA' OU 'AMOR_MECA'
!
!     OUT:   / NOMU.MAEL_MASS_VALE  SI 'MASS_MECA'
!            / NOMU.MAEL_AMOR_VALE  SI 'AMOR_MECA'
!
! ----------------------------------------------------------------------
!
!
    integer :: i, scdi, schc, iblo
    character(len=8) :: promes
    logical(kind=1) :: mostru
!
    character(len=16) :: optio2
    character(len=19) :: nu, matas, stock
!
!
!-----------------------------------------------------------------------
    integer :: iampee, iaphi0, iaphie, iascdi,   iatmi0
    integer :: iatmie, iblold, iblph, ii, iiblph, j
    integer :: jblph,  jjblph,  jualf, k, kk
    integer :: lgblph, lmat, nblph, nddle, nddli, nlblph
    character(len=24), pointer :: refa(:) => null()
    integer, pointer :: desm(:) => null()
    integer, pointer :: scib(:) => null()
    integer, pointer :: vschc(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    optio2 = option
    nu = nomu
    nu = nu(1:14)//'.NUME'
    stock = nu(1:14)//'.SLCS'
!
    if (optio2(1:9) .eq. 'MASS_MECA') then
        matas = nomu//'.MASSMECA'
!
    else if (optio2(1:9).eq.'AMOR_MECA') then
        matas = nomu//'.AMORMECA'
!
    else
        ASSERT(.false.)
    endif
!
!     -- MOSTRU=.TRUE. : CAS MODIFICATION STRUCTURALE
    mostru = .true.
    call dismoi('NOM_PROJ_MESU', nomu, 'MACR_ELEM_STAT', repk=promes)
    if (promes .eq. ' ') mostru = .false.
!
!
    call jeveuo(nomu//'.DESM', 'E', vi=desm)
    nddle = desm(4)
    nddli = desm(5)
!
!
!     -- ALLOCATION DE TMP_IE (TEMPORAIRE LIKE PHI_IE )
!     -------------------------------------------------------
    call jelira(nomu//'.PHI_IE', 'LONMAX', lgblph)
    call jelira(nomu//'.PHI_IE', 'NMAXOC', nblph)
    nlblph=lgblph/nddli
!
    call jecrec(nomu//'.TMP_IE', 'V V R', 'NU', 'DISPERSE', 'CONSTANT',&
                nblph)
    call jeecra(nomu//'.TMP_IE', 'LONMAX', lgblph)
    do j = 1, nblph
        call jecroc(jexnum(nomu//'.TMP_IE', j))
        call jeveuo(jexnum(nomu//'.TMP_IE', j), 'E', iatmi0)
        call jelibe(jexnum(nomu//'.TMP_IE', j))
    end do
!
    if (optio2(1:9) .eq. 'MASS_MECA') then
        call wkvect(nomu//'.MAEL_MASS_VALE', 'G V R', (nddle*(nddle+1)/ 2), iampee)
    else if ((optio2(1:9).eq.'AMOR_MECA').and.(mostru)) then
        call wkvect(nomu//'.MAEL_AMOR_VALE', 'G V R', (nddle*(nddle+1)/ 2), iampee)
    else
        ASSERT(.false.)
    endif
!
    if (mostru) then
!       CREATION DE LA MATRICE POUR MODIFICATION STRUCTURALE
        if (optio2(1:9) .eq. 'MASS_MECA') call crmema(promes, iampee)
        if (optio2(1:9) .eq. 'AMOR_MECA') call crmeam(promes, iampee)
!
    else
!
!     -- ALLOCATION DE MP_EE  ET INITIALISATION PAR M_EE:
!     ---------------------------------------------------
!
        call mtdscr(matas)
        call jeveuo(matas(1:19)//'.&INT', 'E', lmat)
        call mtdsc2(zk24(zi(lmat+1)), 'SCDI', 'L', iascdi)
        call jeveuo(zk24(zi(lmat+1)) (1:19)//'.REFA', 'L', vk24=refa)
        call jeveuo(refa(2) (1:14)//'.SLCS.SCHC', 'L', vi=vschc)
        call jeveuo(stock//'.SCIB', 'L', vi=scib)
!
        iblold = 0
        do j = 1, nddle
            iblo = scib(nddli+j)
            scdi = zi(iascdi-1+nddli+j)
            schc = vschc(nddli+j)
            if (iblo .ne. iblold) then
                if (iblold .gt. 0) call jelibe(jexnum(matas//'.UALF', iblold))
                call jeveuo(jexnum(matas//'.UALF', iblo), 'L', jualf)
            endif
            iblold = iblo
!CDIR$ IVDEP
            do i = max(1, j+1-schc), j
                ii = (j-1)*j/2 + i
                zr(iampee-1+ii) = zr(jualf-1+scdi+i-j)
            end do
!
        end do
        if (iblold .gt. 0) call jelibe(jexnum(matas//'.UALF', iblold))
!
!     -- CALCUL DE MP_EE = MP_EE + M_EI*PHI_IE + PHI_EI*M_IE :
!     --------------------------------------------------------
        iblold = 0
        do j = 1, nddle
            iblo = scib(nddli+j)
            scdi = zi(iascdi-1+nddli+j)
            schc = vschc(nddli+j)
            if (iblo .ne. iblold) then
                if (iblold .gt. 0) call jelibe(jexnum(matas//'.UALF', iblold))
                call jeveuo(jexnum(matas//'.UALF', iblo), 'L', jualf)
            endif
            iblold = iblo
!
            i = 0
            do iblph = 1, nblph
                call jeveuo(jexnum(nomu//'.PHI_IE', iblph), 'L', iaphi0)
                do iiblph = 1, nlblph
                    i = i + 1
                    if (i .gt. j) then
                        call jelibe(jexnum(nomu//'.PHI_IE', iblph))
                        goto 70
!
                    endif
                    iaphie = iaphi0 + (iiblph-1)*nddli
                    ii = (j-1)*j/2 + i
                    kk = 0
!CDIR$ IVDEP
                    do k = nddli + j + 1 - schc, nddli
                        kk = kk + 1
                        zr(iampee-1+ii) = zr(iampee-1+ii) - zr( iaphie-1+k)*zr(jualf-1+scdi-schc+&
                                          & kk)
                    end do
                end do
                call jelibe(jexnum(nomu//'.PHI_IE', iblph))
            end do
 70         continue
!
!
!        SYMETRIE:
            i = 0
            do iblph = 1, nblph
                if (i+nlblph .lt. j) then
                    i = i + nlblph
                    goto 100
!
                endif
                call jeveuo(jexnum(nomu//'.PHI_IE', iblph), 'L', iaphi0)
                do iiblph = 1, nlblph
                    i = i + 1
                    if (i .lt. j) goto 90
                    if (i .gt. nddle) then
                        call jelibe(jexnum(nomu//'.PHI_IE', iblph))
                        goto 110
!
                    endif
                    iaphie = iaphi0 + (iiblph-1)*nddli
                    ii = (i* (i-1)/2) + j
                    kk = 0
!CDIR$ IVDEP
                    do k = nddli + j + 1 - schc, nddli
                        kk = kk + 1
                        zr(iampee-1+ii) = zr(iampee-1+ii) - zr( iaphie-1+k)*zr(jualf-1+scdi-schc+&
                                          & kk)
                    end do
 90                 continue
                end do
                call jelibe(jexnum(nomu//'.PHI_IE', iblph))
100             continue
            end do
110         continue
!
        end do
!
!
        if (iblold .gt. 0) call jelibe(jexnum(matas//'.UALF', iblold))
!
!
!     -- CALCUL DE TMP_IE = M_II*PHI_IE :
!     -----------------------------------
        i = 0
        do iblph = 1, nblph
            call jeveuo(jexnum(nomu//'.PHI_IE', iblph), 'L', iaphi0)
            call jeveuo(jexnum(nomu//'.TMP_IE', iblph), 'E', iatmi0)
            do iiblph = 1, nlblph
                i = i + 1
                if (i .gt. nddle) goto 170
                iaphie = iaphi0 + (iiblph-1)*nddli
                iatmie = iatmi0 + (iiblph-1)*nddli
!
                iblold = 0
                do j = 1, nddli
                    iblo = scib(j)
                    scdi = zi(iascdi-1+j)
                    schc = vschc(j)
                    if (iblo .ne. iblold) then
                        if (iblold .gt. 0) call jelibe(jexnum(matas// '.UALF', iblold))
                        call jeveuo(jexnum(matas//'.UALF', iblo), 'L', jualf)
                    endif
                    iblold = iblo
!
                    kk = 0
!CDIR$ IVDEP
                    do k = j + 1 - schc, j
                        kk = kk + 1
                        zr(iatmie-1+j) = zr(iatmie-1+j) - zr(iaphie-1+ k)*zr(jualf-1+scdi-schc+kk&
                                         &)
                    end do
                    kk = 0
!CDIR$ IVDEP
                    do k = j + 1 - schc, j - 1
                        kk = kk + 1
                        zr(iatmie-1+k) = zr(iatmie-1+k) - zr(iaphie-1+ j)*zr(jualf-1+scdi-schc+kk&
                                         &)
                    end do
                end do
                if (iblold .gt. 0) call jelibe(jexnum(matas//'.UALF', iblold))
!
            end do
170         continue
            call jelibe(jexnum(nomu//'.PHI_IE', iblph))
            call jelibe(jexnum(nomu//'.TMP_IE', iblph))
        end do
!
!
!     -- CALCUL DE MP_EE = MP_EE + PHI_EI*TMP_IE:
!     -------------------------------------------
!
        i = 0
        do iblph = 1, nblph
            call jeveuo(jexnum(nomu//'.PHI_IE', iblph), 'L', iaphi0)
            do iiblph = 1, nlblph
                i = i + 1
                if (i .gt. nddle) goto 240
                iaphie = iaphi0 + (iiblph-1)*nddli
                j = 0
                do jblph = 1, nblph
                    call jeveuo(jexnum(nomu//'.TMP_IE', jblph), 'L', iatmi0)
                    do jjblph = 1, nlblph
                        j = j + 1
                        if (j .gt. i) goto 210
                        iatmie = iatmi0 + (jjblph-1)*nddli
                        ii = (i-1)*i/2 + j
!CDIR$ IVDEP
                        do k = 1, nddli
                            zr(iampee-1+ii) = zr(iampee-1+ii) - zr(iaphie-1+k)*zr(iatmie-1+k)
                        end do
                    end do
210                 continue
                    call jelibe(jexnum(nomu//'.TMP_IE', iblph))
                end do
            end do
240         continue
            call jelibe(jexnum(nomu//'.PHI_IE', iblph))
        end do
!
    endif
!
    call jedetr(nomu//'.TMP_IE')
    call jedema()
end subroutine
