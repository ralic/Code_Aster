subroutine romMultiParaCoefOneCompute(ds_empi       , ds_multipara,&
                                      syst_2mbr_type, syst_2mbr   , solveROM,&
                                      i_mode_until  , i_mode_coef , i_coef)
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
#include "asterfort/romEvalCoef.h"
#include "asterfort/romMultiParaROMMatrCreate.h"
#include "asterfort/romMultiParaROM2mbrCreate.h"
#include "asterfort/romSolveROMSystSolve.h"
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
    integer, intent(in) :: i_coef
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Compute reduced coefficients for one evaluation of coefficient
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
! In  i_coef           : index of coefficient
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nb_mode, i_mode, vali(2)
    complex(kind=8), pointer :: v_syst_solu(:) => null()
    complex(kind=8), pointer :: v_syst_matr(:) => null()
    real(kind=8) :: valr(2)
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM2_51', si = i_coef)
    endif
!
! - Get parameters
!
    nb_mode = solveROM%syst_size
!
! - Access to objects
!
    if (syst_2mbr_type .eq. 'C') then
        call jeveuo(solveROM%syst_solu, 'L', vc = v_syst_solu)
    else
        ASSERT(.false.)
    endif
    ASSERT(i_mode_until .le. nb_mode)
    ASSERT(i_mode_coef  .le. nb_mode)
!
! - Initialization of matrix
!
    call jeveuo(solveROM%syst_matr, 'E', vc = v_syst_matr)
    v_syst_matr(1:nb_mode*nb_mode) = dcmplx(0.d0,0.d0)
!
! - Evaluate coefficients
!
    call romEvalCoef(ds_multipara, l_init = .false._1,&
                     i_mode_coef_ = i_mode_coef, i_coef_ = i_coef)
!
! - Compute reduced second member
!
    call romMultiParaROM2mbrCreate(ds_empi       , ds_multipara, i_coef,&
                                   syst_2mbr_type, syst_2mbr   , solveROM%syst_2mbr)
!
! - Compute reduced matrix
!
    call romMultiParaROMMatrCreate(ds_empi  , ds_multipara, i_coef,&
                                   solveROM%syst_matr)
!
! - Solve reduced system
!
    call romSolveROMSystSolve(solveROM, size_to_solve_ = i_mode_until)
!
! - Debug print
!
    if (niv .ge. 2) then
        do i_mode = 1, i_mode_until
            valr(1) = real(v_syst_solu(i_mode))
            valr(2) = dimag(v_syst_solu(i_mode))
            vali(1) = i_mode
            vali(2) = i_coef
            call utmess('I', 'ROM2_52', ni = 2, vali = vali, nr = 2, valr = valr)
        end do
    endif
!
end subroutine
