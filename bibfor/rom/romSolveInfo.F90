subroutine romSolveInfo(ds_solve)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
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
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Informations about objects to solve system
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_solve         : datastructure to solve systems
!
! --------------------------------------------------------------------------------------------------
!
    character(len=1) :: syst_matr_type, syst_2mbr_type, syst_type
!
! --------------------------------------------------------------------------------------------------
!
    syst_matr_type = ds_solve%syst_matr_type
    syst_2mbr_type = ds_solve%syst_2mbr_type
    syst_type      = ds_solve%syst_type
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
    if (syst_type .eq. 'C') then
        call utmess('I', 'ROM2_36')
    elseif (syst_type .eq. 'R') then
        call utmess('I', 'ROM2_37')
    endif 
    call utmess('I', 'ROM2_35', si = ds_solve%syst_size)
!
end subroutine
