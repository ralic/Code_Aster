subroutine romMultiParaCoefCompute(ds_empi       , ds_multipara,&
                                   syst_2mbr_type, syst_2mbr   , solveROM,&
                                   i_mode_until  , i_mode_coef ,&
                                   coef_redu     , i_coef_)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/romMultiParaCoefOneCompute.h"
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
    type(ROM_DS_Empi), intent(in) :: ds_empi
    type(ROM_DS_MultiPara), intent(inout) :: ds_multipara
    character(len=1), intent(in) :: syst_2mbr_type
    character(len=19), intent(in) :: syst_2mbr
    type(ROM_DS_Solve), intent(in) :: solveROM
    integer, intent(in) :: i_mode_until
    integer, intent(in) :: i_mode_coef
    character(len=24), intent(in) :: coef_redu
    integer, optional, intent(in) :: i_coef_
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Compute reduced coefficients
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_empi          : datastructure for empiric modes
! IO  ds_multipara     : datastructure for multiparametric problems
! In  syst_2mbr_type   : type of second member (R or C)
! In  syst_2mbr        : second member
! In  ds_solveROM      : datastructure to solve systems (ROM)
! In  i_mode_until     : last mode until compute
! In  i_mode_coef      : index of mode to compute coefficients
! In  coef_redu        : name of object to save reduced coefficients
! In  i_coef_          : index of coefficient
!
! NB: if not(i_coef) : all coefficients for the mode
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: i_coef_b, i_coef_e, nb_mode, i_mode, i_coef, nb_vari_coef
    complex(kind=8), pointer :: v_coef_redu(:) => null()
    complex(kind=8), pointer :: v_syst_solu(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM2_45')
    endif
!
! - Get parameters
!
    nb_mode      = solveROM%syst_size
    nb_vari_coef = ds_multipara%nb_vari_coef
!
! - Access to objects
!
    if (syst_2mbr_type .eq. 'C') then
        call jeveuo(solveROM%syst_solu, 'L', vc = v_syst_solu)
        call jeveuo(coef_redu, 'E', vc = v_coef_redu)
    else
        ASSERT(.false.)
    endif
    ASSERT(i_mode_until .le. nb_mode)
    ASSERT(i_mode_coef  .le. nb_mode)
!
! - Select coefficient
!
    if (present(i_coef_)) then
        i_coef_b = i_coef_
        i_coef_e = i_coef_
    else
        i_coef_b = 1
        i_coef_e = nb_vari_coef
    endif
!
! - Compute reduced coefficients
!
    do i_coef = i_coef_b, i_coef_e
        if (niv .ge. 2) then
            call utmess('I', 'ROM2_46', si = i_coef)
        endif
! ----- Compute reduced coefficients for one evaluation of coefficient
        call romMultiParaCoefOneCompute(ds_empi       , ds_multipara,&
                                        syst_2mbr_type, syst_2mbr   , solveROM,&
                                        i_mode_until  , i_mode_coef , i_coef)
! ----- Copy coefficients
        do i_mode = 1, i_mode_until
            v_coef_redu(nb_vari_coef*(i_mode-1)+i_coef) = v_syst_solu(i_mode)
        end do
    end do
!
end subroutine
