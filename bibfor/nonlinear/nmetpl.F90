subroutine nmetpl(ds_inout)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
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
    type(NL_DS_InOut), intent(inout) :: ds_inout
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Input/output datastructure
!
! Update name of field in algorithm
!
! This utiliy is required for "hat" variables
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_inout         : datastructure for input/output management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_field, i_field
    character(len=24) :: algo_name_old, algo_name_new
    character(len=6) :: hat_type, hat_vari
!
! --------------------------------------------------------------------------------------------------
!
    nb_field = ds_inout%nb_field
!
    do i_field = 1, nb_field
        algo_name_old = ds_inout%field(i_field)%algo_name
        if (algo_name_old(1:3) .eq. '#H#') then
            hat_type = algo_name_old(4:9)
            hat_vari = algo_name_old(11:16)
            if (hat_type .eq. 'VALINC') then
                if (hat_vari .eq. 'TEMP') then
                    ASSERT(.false.)
                endif
                algo_name_new = algo_name_old
                algo_name_new(14:16) = 'PLU'
                ds_inout%field(i_field)%algo_name = algo_name_new
            endif
        endif
    end do
!
end subroutine
