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
    subroutine nmdoct(mesh  , list_load       , sdcont_defi     , sdunil_defi , l_cont,&
                      l_unil, ligrel_link_cont, ligrel_link_xfem, sd_iden_rela)
        character(len=8), intent(in) :: mesh
        character(len=24), intent(out) :: sdcont_defi
        character(len=24), intent(out) :: sdunil_defi
        character(len=19), intent(in) :: list_load
        aster_logical, intent(out) :: l_cont
        aster_logical, intent(out) :: l_unil
        character(len=19), intent(out) :: ligrel_link_cont
        character(len=19), intent(out) :: ligrel_link_xfem
        character(len=24), intent(out) :: sd_iden_rela
    end subroutine nmdoct
end interface
