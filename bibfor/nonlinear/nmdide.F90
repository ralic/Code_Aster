subroutine nmdide(l_reuse, result, nume_last, inst_last)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8vide.h"
#include "asterfort/rs_getlast.h"
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: result
    aster_logical, intent(in) :: l_reuse
    integer, intent(out) :: nume_last
    real(kind=8), intent(out) :: inst_last
!
! --------------------------------------------------------------------------------------------------
!
! Non-linear algorithm - Initial state management
!
! Last time in result datastructure if initial state given
!
! --------------------------------------------------------------------------------------------------
!
! In  result           : name of result datastructure (EVOL_NOLI)
! In  l_reuse          : .true. if reuse results datastructure
! Out nume_last        : last index stored in results datastructure
!                        0 if not reuse
! Out inst_last        : last time stored in results datastructure
!                        r8vide if not reuse
!
! --------------------------------------------------------------------------------------------------
!
    nume_last = 0
    inst_last = r8vide()
!
    if (l_reuse) then
        call rs_getlast(result, nume_last, inst_last = inst_last)
    endif
!
end subroutine
