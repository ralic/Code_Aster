subroutine mmexcl(type_inte  , pair_type  , i_poin_elem, ndexfr,&
                  l_node_excl, l_excl_frot)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/isdeco.h"
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
    integer, intent(in) :: type_inte
    integer, intent(in) :: pair_type
    integer, intent(in) :: i_poin_elem
    integer, intent(in) :: ndexfr
    aster_logical, intent(out) :: l_node_excl
    aster_logical, intent(out) :: l_excl_frot
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue method - Get informations for excluded contact point
!
! --------------------------------------------------------------------------------------------------
!
! In  type_inte        : type of integration scheme
! In  pair_type        : type of pairing
! In  i_poin_elem      : index of point in the element
! In  ndexfr           : coded integer for friction nodes
! Out l_node_excl      : .true. if contact point is excluded
! Out l_excl_frot      : .true. if contact point is excluded for friction
!
! --------------------------------------------------------------------------------------------------
!
    integer :: lnexfr(9)
    aster_logical :: l_tole_appa, l_tole_exte
!
! --------------------------------------------------------------------------------------------------
!
    l_tole_appa = .true.
    l_tole_exte = .true.
    l_node_excl = .false.
    l_excl_frot = .false.
!
! - From pairing
!
    if (pair_type .eq. -2) then
        l_tole_appa = .false.
    else if (pair_type.eq.-3) then
        l_tole_exte = .false.
    endif
!
! - SANS_GROUP_NO nodes
!
    if (pair_type .eq. -1) then
        ASSERT(type_inte.eq.1)
        l_node_excl = .true.
    endif
!
! - TOLE_APPA projection
!
    if (.not.l_tole_appa) then
        l_node_excl = .true.
    endif
!
! - TOLE_EXTE projection
!
    if (.not. l_tole_exte) then
        l_node_excl = .true.
    endif
!
! - Excluded for friction
!
    if (i_poin_elem .le. 9) then
        call isdeco([ndexfr], lnexfr, 9)
        if (lnexfr(i_poin_elem) .eq. 1) then
            l_excl_frot = .true.
        endif
    endif
!
end subroutine
