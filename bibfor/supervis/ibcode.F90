subroutine ibcode(ncode)
    implicit none
!
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
!
!   Return 1 if CODE=_F(...) is present in DEBUT or CODE='OUI' in POURSUITE else return 0.
!
#include "asterc/getexm.h"
#include "asterc/getfac.h"
#include "asterc/jdcset.h"
#include "asterfort/getvtx.h"
!
    integer, intent(out) :: ncode
!
    character(len=3) :: repons
    integer :: ier
!
    ncode = 0
    ier = getexm(' ', 'CODE')
    if (ier .eq. 1) then
!       cas de POURSUITE
        call getvtx(' ', 'CODE', scal=repons)
        if (repons .eq. 'OUI') then
            ncode = 1
        endif
    else
!       cas de DEBUT
        call getfac('CODE', ncode)
    endif
    call jdcset('icode', ncode)
end subroutine ibcode
