!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
#include "asterf_types.h"
!
interface
    subroutine mmmbca(mesh     , sddyna   , iter_newt, ds_contact    , sdstat        ,&
                      hval_incr, hval_algo, time_curr, loop_cont_node, loop_cont_conv)
        use NonLin_Datastructure_type
        character(len=8), intent(in) :: mesh
        character(len=19), intent(in) :: sddyna
        integer, intent(in) :: iter_newt
        type(NL_DS_Contact), intent(inout) :: ds_contact
        character(len=24), intent(in) :: sdstat
        character(len=19), intent(in) :: hval_incr(*)
        character(len=19), intent(in) :: hval_algo(*)
        real(kind=8), intent(in) :: time_curr
        aster_logical, intent(out) :: loop_cont_conv
        integer, intent(out) :: loop_cont_node
    end subroutine mmmbca
end interface
