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
    subroutine aplcfb(mesh        , newgeo        , sdappa      , i_zone        , pair_tole,&
                      nb_elem_mast, list_elem_mast, nb_elem_slav, list_elem_slav, &
                      nb_pair_zone, list_pair_zone)
        character(len=8), intent(in) :: mesh
        character(len=19), intent(in) :: newgeo
        character(len=19), intent(in) :: sdappa
        integer, intent(in) :: i_zone
        real(kind=8), intent(in) :: pair_tole
        integer, intent(in) :: nb_elem_mast
        integer, intent(in) :: list_elem_mast(nb_elem_mast)
        integer, intent(in) :: nb_elem_slav
        integer, intent(in) :: list_elem_slav(nb_elem_slav)
        integer, intent(inout) :: nb_pair_zone
        integer, pointer, intent(inout) :: list_pair_zone(:)
    end subroutine aplcfb
end interface
