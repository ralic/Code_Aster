!
! COPYRIGHT (C) 1991 - 2017  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine romModeProd(nb_matr  , l_matr_name, l_matr_type, prod_mode,&
                           mode_type, v_modec    , v_moder)
        integer, intent(in) :: nb_matr
        character(len=8), intent(in) :: l_matr_name(:)
        character(len=1), intent(in) :: l_matr_type(:)
        character(len=24), intent(in) :: prod_mode(:)
        character(len=1), intent(in) :: mode_type
        complex(kind=8), pointer, optional, intent(in) :: v_modec(:)
        real(kind=8), pointer, optional, intent(in) :: v_moder(:)
    end subroutine romModeProd
end interface
