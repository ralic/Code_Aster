function houxgb(xx, n)
! ======================================================================
! COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
!--------------------------------------------------------
! ELEMENT SHB8-PS A.COMBESCURE, S.BAGUET INSA LYON 2003 /
!-------------------------------------------------------
    implicit none
#include "asterfort/assert.h"
    real(kind=8) :: xx(24)
    integer :: n
    real(kind=8) :: houxgb
!
    call assert((n.eq.1).or.(n.eq.2).or.(n.eq.3).or.(n.eq.4))
!
    if (n .eq. 1) then
        houxgb = xx(1) + xx(4) - xx(7) - xx(10) - xx(13) - xx(16) + xx(19) + xx(22)
    else if (n.eq.2) then
        houxgb = xx(1) - xx(4) - xx(7) + xx(10) - xx(13) + xx(16) + xx(19) - xx(22)
    else if (n.eq.3) then
        houxgb = xx(1) - xx(4) + xx(7) - xx(10) + xx(13) - xx(16) + xx(19) - xx(22)
    else if (n.eq.4) then
        houxgb = - xx(1) + xx(4) - xx(7) + xx(10) + xx(13) - xx(16) + xx(19) - xx(22)
    endif
!
end function
