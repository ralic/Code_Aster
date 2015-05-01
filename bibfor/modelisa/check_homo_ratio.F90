subroutine check_homo_ratio(cara, vale, nval)
    implicit none
    character(len=*), intent(in) :: cara(*)
    real(kind=8), intent(in) :: vale(*)
    integer, intent(in) :: nval
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   AFFE_CARA_ELEM
!   Check the consistency of properties R1, R2, EP1, EP2 for the beam elements
!   with homothetic section
!
#include "asterf_debug.h"
#include "asterfort/utmess.h"
! ----------------------------------------------------------------------
    integer, parameter :: nk = 4
    character(len=3) :: tcar(nk)
    real(kind=8) :: tval(nk), rratio, eratio, homo
    real(kind=8) :: valr(6)
    integer :: nv, i, j
    character(len=3) :: carpou(nk)
!
    data carpou /'R1', 'R2', 'EP1', 'EP2'/
!
!   copy the properties is the expected order
    nv = min(4, nval)
    tval = 0.d0
    tcar = '   '
    do i = 1, nv
        do j = 1, nk
            if (cara(i) .eq. carpou(j)) then
                tcar(j) = cara(i)
                tval(j) = vale(i)
                exit
            endif
        end do
    end do
!
    if (tcar(1)(1:1).eq. ' ')then
        call utmess('F', 'MODELISA5_54', sk=carpou(1))
    endif
    if (tcar(2)(1:1).eq. ' ')then
        call utmess('F', 'MODELISA5_54', sk=carpou(2))
    endif
!
!   default: EPi = Ri
    if (tcar(3)(1:1) .eq. ' ') then
        tval(3) = tval(1)
    endif
    if (tcar(4)(1:1) .eq. ' ') then
        tval(4) = tval(2)
    endif
#ifdef __DEBUG_ALL__
    print *, MARKER, "POUT/CERCL/CARA:", tval(1), tval(2), tval(3), tval(4)
#endif
!
    rratio = tval(2) / tval(1)
    eratio = tval(4) / tval(3)
    homo = abs((rratio - eratio) / rratio)
#ifdef __DEBUG_ALL__
    print *, MARKER, "POUT/CERCL/HOMO:", rratio, eratio, homo
#endif
    if (homo .gt. 1.0d-2) then
        valr(1:4) = tval(1:4)
        valr(5) = rratio
        valr(6) = eratio
        call utmess('A', 'POUTRE0_4', nr=6, valr=valr)
    endif
!
end subroutine
