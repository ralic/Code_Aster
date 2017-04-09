subroutine romGreedyResiNormCalc(i_coef, nb_equa, ds_para_rb)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "blas/zdotc.h"
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
    integer, intent(in) :: i_coef
    integer, intent(in) :: nb_equa
    type(ROM_DS_ParaDBR_RB), intent(inout) :: ds_para_rb
!
! --------------------------------------------------------------------------------------------------
!
! Greedy algorithm
!
! Normalization of residual
!
! --------------------------------------------------------------------------------------------------
!
! In  i_coef           : index of coefficient
! In  nb_equa          : number of equations
! IO  ds_para_rb       : datastructure for parameters (RB)
!
! --------------------------------------------------------------------------------------------------
!
    complex(kind=8), pointer :: v_resic(:) => null()
    complex(kind=8) :: normc
    character(len=1) :: resi_type
    character(len=24) :: resi_vect
!
! --------------------------------------------------------------------------------------------------
!
    resi_type = ds_para_rb%resi_type
    resi_vect = ds_para_rb%resi_vect
!
! - Compute norm of residual
!
    if (resi_type .eq. 'C') then
        call jeveuo(resi_vect(1:19)//'.VALE', 'L', vc = v_resic)
        normc = zdotc(nb_equa, v_resic, 1, v_resic, 1)
        ds_para_rb%resi_norm(i_coef) = real(sqrt(real(normc)))/ds_para_rb%resi_refe
    else
        ASSERT(.false.)
    endif
!
end subroutine
