subroutine nmexti(node_name, field    , nb_cmp, list_cmp, type_extr_cmp,&
                  nb_vale  , vale_resu)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmextv.h"
#include "asterfort/posddl.h"
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
    character(len=8), intent(in) :: node_name
    character(len=19), intent(in) :: field
    integer, intent(in) :: nb_cmp
    character(len=24), intent(in) :: list_cmp
    character(len=8), intent(in) :: type_extr_cmp
    integer, intent(out) :: nb_vale
    real(kind=8), intent(out) :: vale_resu(*)
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Field extraction datastructure
!
! Extract value(s) at node
!
! --------------------------------------------------------------------------------------------------
!
! In  node_name        : name of node
! In  field            : name of field
! In  nb_cmp           : number of components
! In  list_cmp         : name of object contains list of components
! In  type_extr_cmp    : type of extraction for components
! Out vale_resu        : list of result values
! Out nb_vale          : number of result values (one if function)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_para_maxi
    parameter    (nb_para_maxi=20)
    character(len=8) :: v_cmp_name(nb_para_maxi)
    real(kind=8) :: v_cmp_vale(nb_para_maxi)
!
    integer :: nb_cmp_vale
    integer :: i_cmp_vale, i_cmp
    character(len=8) :: cmp_name
    integer :: node_nume, dof_nume
    real(kind=8), pointer :: vale(:) => null()
    character(len=8), pointer :: v_list_cmp(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    i_cmp_vale = 1
    ASSERT(nb_cmp.le.nb_para_maxi)
!
! - Access to field
!
    call jeveuo(field //'.VALE', 'L', vr=vale)
!
! - Get name of components
!
    call jeveuo(list_cmp, 'L', vk8 = v_list_cmp)
    do i_cmp = 1, nb_cmp
        v_cmp_name(i_cmp) = v_list_cmp(i_cmp)
    end do
!
! - Get value of components
!
    do i_cmp = 1, nb_cmp
        cmp_name = v_cmp_name(i_cmp)
        call posddl('CHAM_NO', field, node_name, cmp_name, node_nume,&
                    dof_nume)
        if ((node_nume.ne.0) .and. (dof_nume.ne.0)) then
            v_cmp_vale(i_cmp_vale) = vale(dof_nume)
            i_cmp_vale = i_cmp_vale + 1
        endif
    end do
    nb_cmp_vale = i_cmp_vale - 1
!
! - Evaluation
!
    call nmextv(nb_cmp_vale, type_extr_cmp, v_cmp_name, v_cmp_vale, nb_vale,&
                vale_resu)
    ASSERT(nb_vale.le.nb_cmp)
!
end subroutine
