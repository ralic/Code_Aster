subroutine nmextv(nb_cmp_vale, func_name, v_cmp_name, v_cmp_vale, nb_vale,&
                  vale_resu)
!
implicit none
!
#include "asterfort/fointe.h"
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
    integer, intent(in) :: nb_cmp_vale
    character(len=8), intent(in) :: func_name
    character(len=8), intent(in) :: v_cmp_name(*)
    real(kind=8), intent(in) :: v_cmp_vale(*)
    real(kind=8), intent(out) :: vale_resu(*)
    integer, intent(out) :: nb_vale
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Field extraction datastructure
!
! Extract component value or apply function between several components
!
! --------------------------------------------------------------------------------------------------
!
! In  nb_cmp_vale      : number of components to evaluate
! In  func_name        : name of function to evaluate (' ' if not function)
! In  v_cmp_name       : list of name of components
! In  v_cmp_vale       : list of value of components
! Out vale_resu        : list of result values
! Out nb_vale          : number of result values (one if function)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_cmp_vale, icode
    real(kind=8) :: valr
!
! --------------------------------------------------------------------------------------------------
!
    nb_vale = 0
!
    if (func_name .eq. ' ') then
        do i_cmp_vale = 1, nb_cmp_vale
            vale_resu(i_cmp_vale) = v_cmp_vale(i_cmp_vale)
        end do
        nb_vale = nb_cmp_vale
    else
        call fointe('FM', func_name, nb_cmp_vale, v_cmp_name, v_cmp_vale,&
                    valr, icode)
        vale_resu(1) = valr
        nb_vale = 1
    endif
!
end subroutine
