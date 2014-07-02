subroutine tresu_tole(tole, prec, mcf, iocc)
    implicit none
    real(kind=8), intent(out) :: tole
    real(kind=8), intent(out), optional :: prec
    character(len=*), intent(in), optional :: mcf
    integer, intent(in), optional :: iocc
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
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/getvr8.h"
#include "asterfort/utmess.h"
!
!   Read the TOLE_MACHINE keyword
!
!   This keyword contains one or two values. In this case, the first one is the
!   tolerance of the non regression test and the second is the precision used
!   to find the time (or another parameter) in the result data structure.
!
!   Optional keywords:
!   - Only tole is mandatory.
!   - By default, mcf=' '. If it is present, iocc is mandatory.
!
    integer :: np, uioc
    real(kind=8) :: epsir(2)
    character(len=24) :: umcf
!   to print the message only once
    aster_logical, save :: ipass = .false.
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
    epsir(1) = 1.d-6
    epsir(2) = 1.d-6
!
    call getvr8(umcf, 'TOLE_MACHINE', iocc=uioc, nbval=0, nbret=np)
    np = -np
    if (np .eq. 1) then
        call getvr8(umcf, 'TOLE_MACHINE', iocc=uioc, scal=epsir(1))
        epsir(2) = epsir(1)
    else if (np .eq. 2) then
        call getvr8(umcf, 'TOLE_MACHINE', iocc=uioc, nbval=2, vect=epsir)
    else
        ASSERT(np .eq. 0)
    endif
!
#ifdef TEST_STRICT
!   Does not use TOLE_MACHINE (except for the parameter) if TEST_STRICT
    epsir(1) = 1.d-6
    if (.not. ipass) then
        call utmess('I', 'TEST0_6', sr=epsir(1))
    endif
#endif
    ipass = .true.
!
    tole = epsir(1)
    if (present(prec)) then
        prec = epsir(2)
    endif
end subroutine
