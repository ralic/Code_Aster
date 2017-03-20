subroutine dbr_main_rb(ds_para_rb, ds_empi)
!
use Rom_Datastructure_type
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/mcmult.h"
#include "asterfort/mtdefs.h"
#include "asterfort/mtdscr.h"
#include "asterfort/mtcmbl.h"
#include "asterfort/preres.h"
#include "asterfort/vtcrem.h"
#include "asterfort/copisd.h"
#include "asterfort/romModeProd.h"
#include "asterfort/resoud.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/romModeSave.h"
#include "asterfort/dbr_calcrb_prep.h"
#include "asterfort/dbr_calcrb_solv.h"
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
    type(ROM_DS_ParaDBR_RB), intent(in) :: ds_para_rb
    type(ROM_DS_Empi), intent(inout) :: ds_empi
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Compute
!
! Main subroutine to compute empiric modes - For RB methods
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_para_rb       : datastructure for parameters (RB)
! IO  ds_empi          : datastructure for empiric modes
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_matr, nb_equa, i_mode, i_matr
    character(len=19) :: maprec, solver, vect_zero, syst_solu, crgc, syst_2mbr, syst_matr
    complex(kind=8), pointer :: v_modec(:) => null()
    real(kind=8), pointer :: v_moder(:) => null()
    character(len=8) :: base, model
    character(len=24) :: field_name = ' ', field_refe = ' ', field_iden = ' '
    character(len=1) :: syst_type
    complex(kind=8), pointer :: v_prodc(:) => null()
    real(kind=8), pointer :: v_prodr(:) => null()
    integer :: jv_desc_matr
    character(len=1) :: nume_prod, matr_type
    character(len=8) :: matr
!
! --------------------------------------------------------------------------------------------------
!
    vect_zero      = ds_para_rb%vect_zero
    maprec         = '&&OP0053.MAPREC'
    syst_solu      = ds_para_rb%syst_solu
    crgc           = '&&OP0053.GCPC'
    solver         = ds_para_rb%solver
    syst_matr      = ds_para_rb%syst_matr
    syst_2mbr      = ds_para_rb%syst_2mbr
    nb_equa        = ds_empi%nb_equa
    base           = ds_empi%base
    model          = ds_empi%model
    field_name     = ds_empi%field_name
    field_refe     = syst_2mbr
    nb_matr        = ds_para_rb%ds_multipara%nb_matr
    syst_type      = ds_para_rb%ds_multipara%syst_type

    ds_empi%nb_mode = 1
    i_mode          = 1
!
! - Prepare matrix and second member
!
    call dbr_calcrb_prep(ds_para_rb)
!
! - Solve linear system
!
    call dbr_calcrb_solv(ds_para_rb)
!
! - Save mode
!
    field_iden     = 'DEPL'
    if (syst_type .eq. 'C') then
        call jeveuo(syst_solu(1:19)//'.VALE', 'L', vc=v_modec)
        call romModeSave(base       , i_mode    , model, &
                         field_name , field_iden, field_refe, nb_equa,&
                         mode_vectc_ = v_modec)
    elseif (syst_type .eq. 'R') then
        call jeveuo(syst_solu(1:19)//'.VALE', 'L', vr=v_moder)
        call romModeSave(base       , i_mode    , model, &
                         field_name , field_iden, field_refe, nb_equa,&
                         mode_vectr_ = v_moder)
    else
        ASSERT(.false.)
    endif
!
! - Compute products matrix x mode
!  
    call romModeProd(nb_matr   ,&
                     ds_para_rb%ds_multipara%matr_name,&
                     ds_para_rb%ds_multipara%matr_type,&
                     ds_para_rb%ds_multipara%prod_mode,&
                     syst_type , v_modec  , v_moder)
!
! - Save products matrix x mode
!  
    do i_matr = 1, nb_matr
        matr      = ds_para_rb%ds_multipara%matr_name(i_matr)
        matr_type = ds_para_rb%ds_multipara%matr_type(i_matr)
        call jeveuo(matr(1:8)//'           .&INT', 'L', jv_desc_matr)
        write(nume_prod,'(I1)') i_matr
        field_iden = 'PROD_BASE_MATR_'//nume_prod
        if (matr_type .eq. 'C') then
            if (syst_type .eq. 'C') then
                call jeveuo(ds_para_rb%ds_multipara%prod_mode(i_matr)(1:19)//'.VALE', 'L',&
                            vc = v_prodc)
                call romModeSave(base      , i_mode    , model     ,&
                                 field_name, field_iden, field_refe, nb_equa,&
                                 mode_vectc_ = v_prodc)
            elseif (syst_type .eq. 'R') then
                call jeveuo(ds_para_rb%ds_multipara%prod_mode(i_matr)(1:19)//'.VALE', 'L',&
                            vr = v_prodr)
                call romModeSave(base      , i_mode    , model     ,&
                                 field_name, field_iden, field_refe, nb_equa,&
                                 mode_vectc_ = v_prodc)
            else
                ASSERT(.false.)
            endif
        elseif (matr_type .eq. 'R') then
            if (syst_type .eq. 'C') then
                call jeveuo(ds_para_rb%ds_multipara%prod_mode(i_matr)(1:19)//'.VALE', 'L',&
                            vc = v_prodc)
                call romModeSave(base      , i_mode    , model     ,&
                                 field_name, field_iden, field_refe, nb_equa,&
                                 mode_vectc_ = v_prodc)
            elseif (syst_type .eq. 'R') then
                call jeveuo(ds_para_rb%ds_multipara%prod_mode(i_matr)(1:19)//'.VALE', 'L', vr =&
                            v_prodr)
                call romModeSave(base      , i_mode    , model     ,&
                                 field_name, field_iden, field_refe, nb_equa,&
                                 mode_vectr_ = v_prodr)
            else
                ASSERT(.false.)
            endif
        endif
    end do
!
end subroutine
