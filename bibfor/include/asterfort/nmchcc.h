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
    subroutine nmchcc(list_func_acti, nb_matr    , list_matr_type, list_calc_opti, list_asse_opti,&
                      list_l_asse  , list_l_calc)
        integer, intent(in) :: list_func_acti(*)
        integer, intent(inout) :: nb_matr
        character(len=6), intent(inout)  :: list_matr_type(20)
        character(len=16), intent(inout) :: list_calc_opti(20)
        character(len=16), intent(inout) :: list_asse_opti(20)
        aster_logical, intent(inout) :: list_l_asse(20)
        aster_logical, intent(inout) :: list_l_calc(20)
    end subroutine nmchcc
end interface
