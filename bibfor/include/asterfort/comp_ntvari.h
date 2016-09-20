!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine comp_ntvari(model_ , compor_cart_, compor_list_, compor_info,&
                           nt_vari, nb_vari_maxi, nb_zone     , v_exte)
        use NonLin_Datastructure_type
        character(len=8), optional, intent(in) :: model_
        character(len=19), optional, intent(in) :: compor_cart_
        character(len=16), optional, intent(in) :: compor_list_(20)
        character(len=19), intent(in) :: compor_info
        integer, intent(out) :: nt_vari
        integer, intent(out) :: nb_vari_maxi
        integer, intent(out) :: nb_zone
        type(NL_DS_ComporExte), pointer, intent(out) :: v_exte(:)
    end subroutine comp_ntvari
end interface
