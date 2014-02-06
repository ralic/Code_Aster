subroutine tresu_print(refer, legend, llab, skip, rela, &
                       tole, refr, valr, refi, vali, &
                       refc, valc, compare)
    implicit none
!
! COPYRIGHT (C) 1991 - 2014  EDF R&D                  WWW.CODE-ASTER.ORG
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
    logical, intent(in) :: llab
    logical, intent(in) :: skip
    logical, intent(in) :: rela
    real(kind=8), intent(in) :: tole
    real(kind=8), intent(in), optional :: refr
    real(kind=8), intent(in), optional :: valr
    integer, intent(in), optional :: refi
    integer, intent(in), optional :: vali
    complex(kind=8), intent(in), optional :: refc
    complex(kind=8), intent(in), optional :: valc
    real(kind=8), intent(in), optional :: compare
!
!   Interface d'appel à la fonction d'impression en C/Python pour les TEST_RESU
!
#include "asterfort/assert.h"
#include "asterc/testresu_print.h"
!
    real(kind=8) :: arg_refr
    real(kind=8) :: arg_valr
    integer :: arg_refi
    integer :: arg_vali
    complex(kind=8) :: arg_refc
    complex(kind=8) :: arg_valc
    real(kind=8) :: arg_cmp
    integer :: typ
!
    typ = 0
    ASSERT(UN_PARMI3(refr, refi, refc))
    ASSERT(UN_PARMI3(valr, vali, valc))
    ASSERT(ENSEMBLE2(refr, valr))
    ASSERT(ENSEMBLE2(refi, vali))
    ASSERT(ENSEMBLE2(refc, valc))
!
    arg_refr = 0.d0
    arg_valr = 0.d0
    if (present(refr)) then
        typ = 1
        arg_refr = refr
        arg_valr = valr
    endif
!
    arg_refi = 0
    arg_vali = 0
    if (present(refi)) then
        typ = 2
        arg_refi = refi
        arg_vali = vali
    endif
!
    arg_refc = dcmplx(0.d0)
    arg_valc = dcmplx(0.d0)
    if (present(refc)) then
        typ = 3
        arg_refc = refc
        arg_valc = valc
    endif
!
    arg_cmp = 1.d0
    if (present(compare)) then
        arg_cmp = compare
    endif
!
    ASSERT(typ.ge.1 .and. typ.le.3)
!
    call testresu_print(refer, legend, llab, skip, rela, &
                        tole, typ, arg_refr, arg_valr, arg_refi, &
                        arg_vali, arg_refc, arg_valc, arg_cmp)
!
end subroutine tresu_print
