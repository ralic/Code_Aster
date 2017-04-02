subroutine romSolveDSInit(type_syst, ds_solve)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/infniv.h"
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
    character(len=3), intent(in) :: type_syst
    type(ROM_DS_Solve), intent(out) :: ds_solve
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction - Initializations
!
! Initialisation of datastructure to solve systems
!
! --------------------------------------------------------------------------------------------------
!
! In  type_syst        : type of system
! Out ds_solve         : datastructure to solve systems
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_50', sk = type_syst)
    endif
!
    ds_solve%syst_matr       = '&&'//type_syst//'.MATR'
    ds_solve%syst_2mbr       = '&&'//type_syst//'.SECMBR'
    ds_solve%syst_solu       = '&&'//type_syst//'.SOLUTI'
    ds_solve%vect_zero       = '&&'//type_syst//'.VEZERO'
    ds_solve%syst_size       = 0
    ds_solve%syst_matr_type  = ' '
    ds_solve%syst_2mbr_type  = ' '
    ds_solve%syst_type       = ' '
!
end subroutine
