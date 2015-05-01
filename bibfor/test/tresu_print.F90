subroutine tresu_print(refer, legend, llab, nbref, rela,&
                       tole, ssigne, refr, valr, refi,&
                       vali, refc, valc, ignore, compare)
    implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/bool_to_int.h"
#include "asterc/testresu_print.h"
!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
!
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
!
! person_in_charge: mathieu.courtois at edf.fr
!
    character(len=16), intent(in) :: refer
    character(len=16), intent(in) :: legend
    aster_logical, intent(in) :: llab
    integer, intent(in) :: nbref
    character(len=*), intent(in) :: rela
    real(kind=8), intent(in) :: tole
    character(len=*), intent(in), optional :: ssigne
    real(kind=8), intent(in), optional :: refr(nbref)
    real(kind=8), intent(in), optional :: valr
    integer, intent(in), optional :: refi(nbref)
    integer, intent(in), optional :: vali
    complex(kind=8), intent(in), optional :: refc(nbref)
    complex(kind=8), intent(in), optional :: valc
    aster_logical, intent(in), optional :: ignore
    real(kind=8), intent(in), optional :: compare
!
!   Interface d'appel à la fonction d'impression en C/Python pour les TEST_RESU
!   Quand plusieurs valeurs de référence sont fournis, on conserve la plus proche
!   de la valeur calculée.
!
    real(kind=8) :: arefr
    real(kind=8) :: avalr, minvr, tmpr, minvc, tmpc
    integer :: arefi
    integer :: avali, minvi, tmpi
    integer :: i, imin
    complex(kind=8) :: arefc
    complex(kind=8) :: avalc
    real(kind=8) :: arg_cmp
    aster_logical :: skip, isrela, valabs
    integer :: typ
!
    valabs = .false.
    if (present(ssigne)) then
        valabs = ssigne .eq. 'OUI'
    endif
!
    typ = 0
    ASSERT(UN_PARMI3(refr, refi, refc))
    ASSERT(UN_PARMI3(valr, vali, valc))
    ASSERT(ENSEMBLE2(refr, valr))
    ASSERT(ENSEMBLE2(refi, vali))
    ASSERT(ENSEMBLE2(refc, valc))
!
    arefr = 0.d0
    avalr = 0.d0
    if (present(refr)) then
        typ = 1
        avalr = valr
        arefr = refr(1)
        if (valabs) then
            avalr = abs(avalr)
            arefr = abs(arefr)
        endif
        minvr = abs(avalr - arefr)
        imin = 1
        do i = 1, nbref - 1
            arefr = refr(i+1)
            if (valabs) then
                arefr = abs(arefr)
            endif
            tmpr = abs(avalr - arefr)
            if (tmpr .lt. minvr) then
                tmpr = minvr
                imin = i + 1
            endif
        end do
        arefr = refr(imin)
        if (valabs) then
            arefr = abs(arefr)
        endif
    endif
!
    arefi = 0
    avali = 0
    if (present(refi)) then
        typ = 2
        avali = vali
        arefi = refi(1)
        if (valabs) then
            avali = abs(avali)
            arefi = abs(arefi)
        endif
        minvi = abs(avali - arefi)
        imin = 1
        do i = 1, nbref - 1
            arefi = refi(i+1)
            if (valabs) then
                arefi = abs(arefi)
            endif
            tmpi = abs(avali - arefi)
            if (tmpi .lt. minvi) then
                tmpi = minvi
                imin = i + 1
            endif
        end do
        arefi = refi(imin)
        if (valabs) then
            arefi = abs(arefi)
        endif
    endif
!
    arefc = dcmplx(0.d0, 0.d0)
    avalc = dcmplx(0.d0, 0.d0)
    if (present(refc)) then
        typ = 3
        avalc = valc
        arefc = refc(1)
        if (valabs) then
            avalc = abs(avalc)
            arefc = abs(arefc)
        endif
        minvc = abs(avalc - arefc)
        imin = 1
        do i = 1, nbref - 1
            arefc = refc(i+1)
            if (valabs) then
                arefc = abs(arefc)
            endif
            tmpc = abs(avalc - arefc)
            if (tmpc .lt. minvc) then
                tmpc = minvc
                imin = i + 1
            endif
        end do
        arefc = refc(imin)
        if (valabs) then
            arefc = abs(arefc)
        endif
    endif
!
    ASSERT(typ.ge.1 .and. typ.le.3)
!
    isrela = rela(1:4) .eq. 'RELA'
    skip = .false.
    if (present(ignore)) then
        skip = ignore
    endif
!
    arg_cmp = 1.d0
    if (present(compare)) then
        arg_cmp = compare
    endif
!
    call testresu_print(refer, legend, bool_to_int(llab), bool_to_int(skip), &
                        bool_to_int(isrela), &
                        tole, typ, arefr, avalr, arefi,&
                        avali, arefc, avalc, arg_cmp)
!
end subroutine tresu_print
