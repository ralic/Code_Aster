!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine xrell2(tabl_node     , nb_dim      , nb_edgez, tabl_ptin, tabl_scor,&
                      l_create_group, sdline_crack, l_pilo)
        integer, intent(in) :: nb_edgez
        integer, intent(in) :: nb_dim
        integer, intent(in) :: tabl_node(3, nb_edgez)
        real(kind=8), intent(in) :: tabl_ptin(nb_dim, nb_edgez)
        real(kind=8), intent(in) :: tabl_scor(nb_edgez)
        character(len=14), intent(in) :: sdline_crack
        aster_logical, intent(in) :: l_create_group
        aster_logical, intent(in) :: l_pilo
    end subroutine xrell2
end interface
