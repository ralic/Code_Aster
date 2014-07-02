subroutine tresu_ordgrd(valr, ignore, compare, mcf, iocc)
    implicit none
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/getvr8.h"
    real(kind=8), intent(in) :: valr
    aster_logical, intent(out) :: ignore
    real(kind=8), intent(out) :: compare
    character(len=*), intent(in), optional :: mcf
    integer, intent(in), optional :: iocc
! ======================================================================
! COPYRIGHT (C) 1991 - 2014  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mathieu.courtois@edf.fr
!
!   Read the ORDRE_GRANDEUR keyword in the value of VALE_CALC(=valr) is null
!
!   Optional keyword:
!   - By default, mcf=' '. If it is present, iocc is mandatory.
!
    integer :: nord, uioc
    character(len=24) :: umcf
!
    ASSERT(ENSEMBLE2(mcf, iocc))
    if (absent(mcf)) then
        umcf = ' '
        uioc = 0
    else
        umcf = mcf
        uioc = iocc
    endif
!
    ignore = .false.
    compare = 1.d0
    if (abs(valr) .le. r8prem()) then
        call getvr8(umcf, 'ORDRE_GRANDEUR', iocc=iocc, nbval=0, nbret=nord)
        if (nord .eq. 0) then
            ignore = .true.
        else
            call getvr8(umcf, 'ORDRE_GRANDEUR', iocc=iocc, scal=compare)
        endif
    endif
!
end subroutine tresu_ordgrd
