subroutine romSolveDOMSystCreate(syst_matr_type, syst_2mbr_type, syst_type,&
                                 matr_repr     , ds_solve)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
#include "asterfort/mtdefs.h"
#include "asterfort/mtdscr.h"
#include "asterfort/vtcrem.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
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
    character(len=1), intent(in) :: syst_matr_type
    character(len=1), intent(in) :: syst_2mbr_type
    character(len=1), intent(in) :: syst_type
    character(len=8), intent(in) :: matr_repr
    type(ROM_DS_Solve), intent(inout) :: ds_solve
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Create objects to solve system (DOM)
!
! --------------------------------------------------------------------------------------------------
!
! In  syst_matr_type   : type of matrix (real or complex)
! In  syst_2mbr_type   : type of second member (real or complex)
! In  syst_type        : global type of system (real or complex)
! In  matr_repr        : representative matrix
! IO  ds_solve         : datastructure to solve systems
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=19) :: syst_matr, syst_2mbr, vect_zero, syst_solu
    integer :: syst_size
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM2_27')
    endif
!
! - Get parameters
!
    syst_matr = ds_solve%syst_matr
    syst_2mbr = ds_solve%syst_2mbr
    vect_zero = ds_solve%vect_zero
    syst_solu = ds_solve%syst_solu
!
! - Create resultant matrix
!
    call mtdefs(syst_matr, matr_repr, 'V', syst_matr_type)
    call mtdscr(syst_matr)
    call dismoi('NB_EQUA', syst_matr, 'MATR_ASSE', repi=syst_size)
!
! - Prepare second member
!
    call vtcrem(syst_2mbr, syst_matr, 'V', syst_2mbr_type)
!
! - Prepare solution
!
    call vtcrem(syst_solu, syst_matr, 'V', syst_type)
!
! - Prepare null vector
!
    call copisd('CHAMP_GD', 'V', syst_solu, vect_zero)
!
! - Save parameters
!
    ds_solve%syst_size      = syst_size
    ds_solve%syst_type      = syst_type
    ds_solve%syst_matr_type = syst_matr_type
    ds_solve%syst_2mbr_type = syst_2mbr_type
!
end subroutine
