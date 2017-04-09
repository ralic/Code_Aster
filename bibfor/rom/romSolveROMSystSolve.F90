subroutine romSolveROMSystSolve(ds_solve, size_to_solve_)
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
#include "asterfort/zgauss.h"
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
    type(ROM_DS_Solve), intent(in) :: ds_solve
    integer, optional, intent(in) :: size_to_solve_
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Solve system (ROM)
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_solve         : datastructure to solve systems (ROM)
! In  size_to_solve    : current size of system to solve
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=19) :: syst_matr, syst_2mbr, syst_solu
    character(len=1) :: syst_type
    integer :: nhrs, syst_size, size_to_solve
    complex(kind=8), pointer :: v_syst_matr(:) => null()
    complex(kind=8), pointer :: v_syst_2mbr(:) => null()
    complex(kind=8), pointer :: v_syst_solu(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM2_44')
    endif
!
! - Initializations
!   
    nhrs    = 1
!
! - Get parameters
!
    syst_solu      = ds_solve%syst_solu
    syst_matr      = ds_solve%syst_matr
    syst_2mbr      = ds_solve%syst_2mbr
    syst_size      = ds_solve%syst_size
    syst_type      = ds_solve%syst_matr_type
    if (present(size_to_solve_)) then
        size_to_solve = size_to_solve_
    else
        size_to_solve = syst_size
    endif
    ASSERT(size_to_solve .le. syst_size)
!
! - Access to objects
!
    if (syst_type .eq. 'C') then
        call jeveuo(syst_matr, 'L', vc = v_syst_matr)
        call jeveuo(syst_2mbr, 'L', vc = v_syst_2mbr)
        call jeveuo(syst_solu, 'E', vc = v_syst_solu)
    else
        ASSERT(.false.)
    endif
!
! - Solve system
!
    if (syst_type .eq. 'C') then
        call zgauss(v_syst_matr, v_syst_2mbr, size_to_solve, nhrs, v_syst_solu)
    else
        ASSERT(.false.)
    endif
!
end subroutine
