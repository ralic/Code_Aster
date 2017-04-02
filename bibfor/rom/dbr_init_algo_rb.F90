subroutine dbr_init_algo_rb(ds_para_rb)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
#include "asterfort/dismoi.h"
#include "asterfort/mtdefs.h"
#include "asterfort/mtdscr.h"
#include "asterfort/vtcrem.h"
#include "asterfort/copisd.h"
#include "asterfort/jelira.h"
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
    type(ROM_DS_ParaDBR_RB), intent(inout) :: ds_para_rb
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Initializations
!
! Init algorithm - For RB methods
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_para_rb       : ddatastructure for parameters (RB)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=19) :: syst_matr, syst_2mbr, vect_zero
    character(len=1) :: syst_matr_type, syst_2mbr_type, matr_type, vect_type
    integer :: i_matr, nb_matr
    aster_logical :: l_coefm_cplx, l_coefv_cplx
    character(len=8) :: matr_name = ' '
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM2_41')
    endif
!
! - Get "representative" matrix
!
    matr_name = ds_para_rb%ds_multipara%matr_name(1)
    nb_matr   = ds_para_rb%ds_multipara%nb_matr
!
! - Get parameters
!
    syst_matr      = ds_para_rb%syst_matr
    syst_2mbr      = ds_para_rb%syst_2mbr
    vect_zero      = ds_para_rb%vect_zero
!
! - Evaluate type of resultant matrix
!
    syst_matr_type = 'R'
    do i_matr = 1, nb_matr
        matr_type    = ds_para_rb%ds_multipara%matr_type(i_matr)
        l_coefm_cplx = ds_para_rb%ds_multipara%l_coefm_cplx(i_matr)
        if (matr_type .eq. 'C') then
            syst_matr_type = 'C'
            exit
        endif
        if (l_coefm_cplx) then
            syst_matr_type = 'C'
            exit
        endif
    end do
!
! - Evaluate type of second member
!
    syst_2mbr_type = 'R'
    vect_type      = ds_para_rb%ds_multipara%vect_type
    l_coefv_cplx   = ds_para_rb%ds_multipara%l_coefv_cplx
    if (vect_type .eq. 'C') then
        syst_2mbr_type = 'C'
    endif
    if (l_coefv_cplx) then
        syst_2mbr_type = 'C'
    endif
!
! - Consistent
!
    if (syst_matr_type .eq. 'C') then
        syst_2mbr_type = 'C'
    endif
    if (syst_2mbr_type .eq. 'C') then
        syst_matr_type = 'C'
    endif
!
! - Save types
!
    ds_para_rb%syst_matr_type = syst_matr_type
    ds_para_rb%syst_2mbr_type = syst_2mbr_type
    if (niv .ge. 2) then
        if (syst_matr_type .eq. 'C') then
            call utmess('I', 'ROM2_14')
        elseif (syst_matr_type .eq. 'R') then
            call utmess('I', 'ROM2_15')
        endif
        if (syst_2mbr_type .eq. 'C') then
            call utmess('I', 'ROM2_16')
        elseif (syst_2mbr_type .eq. 'R') then
            call utmess('I', 'ROM2_17')
        endif 
    endif
!
! - Create resultant matrix
!
    call mtdefs(syst_matr, matr_name, 'V', syst_matr_type)
    call mtdscr(syst_matr)
!
! - Prepare second member
!
    call vtcrem(syst_2mbr, syst_matr, 'V', syst_2mbr_type)
!
! - Prepare null vector
!
    call copisd('CHAMP_GD', 'V', syst_2mbr, vect_zero)
!
end subroutine
