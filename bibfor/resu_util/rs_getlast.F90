subroutine rs_getlast(result_, nume_last, inst_last)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsorac.h"
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=*), intent(in) :: result_
    integer, intent(out) :: nume_last
    real(kind=8), optional, intent(out) :: inst_last
!
! --------------------------------------------------------------------------------------------------
!
! Results datastructure - Utility
!
! Get last index stored in results datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  result           : name of results datastructure
! Out nume_last        : last index stored in results datastructure
! Out inst_last        : last time stored in results datastructure
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: result
    character(len=8) :: k8bid
    complex(kind=8) :: c16bid
    integer :: list(1), iret, jinst
    real(kind=8) :: r8bid
!
! --------------------------------------------------------------------------------------------------
!
    result    = result_
    nume_last = 0
    call rsorac(result, 'DERNIER', 0  , r8bid, k8bid,&
                c16bid, 0.d0     , ' ', list , 1    ,&
                iret)
    if (iret .eq. 1) then
        nume_last = list(1)
    endif
    if (present(inst_last)) then
        call rsadpa(result, 'L', 1, 'INST', nume_last,&
                    0, sjv=jinst)
        inst_last = zr(jinst)
    endif

end subroutine
