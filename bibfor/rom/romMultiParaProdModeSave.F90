subroutine romMultiParaProdModeSave(ds_multipara, ds_empi   ,&
                                    i_mode      )
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
#include "asterfort/romModeSave.h"
#include "asterfort/jeveuo.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
    type(ROM_DS_MultiPara), intent(in) :: ds_multipara
    type(ROM_DS_Empi), intent(in) :: ds_empi
    integer, intent(in) :: i_mode
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Save products matrix x mode
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_multipara     : datastructure for multiparametric problems
! In  ds_empi          : datastructure for empiric modes
! In  i_mode           : index of empiric mode
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: base, model
    integer :: nb_equa, nb_matr, i_matr
    character(len=24) :: field_name = ' ', field_iden_prod, field_refe
    character(len=1) :: syst_type, nume_prod, matr_type
    character(len=8) :: matr_name
    complex(kind=8), pointer :: v_prodc(:) => null()
    real(kind=8), pointer :: v_prodr(:) => null()
    integer :: jv_desc_matr
!
! --------------------------------------------------------------------------------------------------
!
    base           = ds_empi%base
    model          = ds_empi%model
    nb_equa        = ds_empi%nb_equa
    field_name     = ds_empi%field_name
    field_refe     = ds_empi%field_refe
    syst_type      = ds_multipara%syst_type
    nb_matr        = ds_multipara%nb_matr
!
! - Save them
!  
    do i_matr = 1, nb_matr
        matr_name = ds_multipara%matr_name(i_matr)
        matr_type = ds_multipara%matr_type(i_matr)
        call jeveuo(matr_name(1:8)//'           .&INT', 'L', jv_desc_matr)
        write(nume_prod,'(I1)') i_matr
        field_iden_prod = 'PROD_BASE_MATR_'//nume_prod
        if (matr_type .eq. 'C') then
            if (syst_type .eq. 'C') then
                call jeveuo(ds_multipara%prod_mode(i_matr)(1:19)//'.VALE', 'L',&
                            vc = v_prodc)
                call romModeSave(base      , i_mode         , model     ,&
                                 field_name, field_iden_prod, field_refe, nb_equa,&
                                 mode_vectc_ = v_prodc)
            elseif (syst_type .eq. 'R') then
                call jeveuo(ds_multipara%prod_mode(i_matr)(1:19)//'.VALE', 'L',&
                            vr = v_prodr)
                call romModeSave(base      , i_mode         , model     ,&
                                 field_name, field_iden_prod, field_refe, nb_equa,&
                                 mode_vectc_ = v_prodc)
            else
                ASSERT(.false.)
            endif
        elseif (matr_type .eq. 'R') then
            if (syst_type .eq. 'C') then
                call jeveuo(ds_multipara%prod_mode(i_matr)(1:19)//'.VALE', 'L',&
                            vc = v_prodc)
                call romModeSave(base      , i_mode         , model     ,&
                                 field_name, field_iden_prod, field_refe, nb_equa,&
                                 mode_vectc_ = v_prodc)
            elseif (syst_type .eq. 'R') then
                call jeveuo(ds_multipara%prod_mode(i_matr)(1:19)//'.VALE', 'L', vr =&
                            v_prodr)
                call romModeSave(base      , i_mode         , model     ,&
                                 field_name, field_iden_prod, field_refe, nb_equa,&
                                 mode_vectr_ = v_prodr)
            else
                ASSERT(.false.)
            endif
        endif
    end do
!
end subroutine
