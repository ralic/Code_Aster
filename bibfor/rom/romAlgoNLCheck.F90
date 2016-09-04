subroutine romAlgoNLCheck(phenom        , mesh_algo, ds_algorom,&
                          l_line_search_)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
#include "asterfort/jeexin.h"
#include "asterfort/jenonu.h"
#include "asterfort/jexnom.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=4), intent(in) :: phenom
    character(len=8), intent(in) :: mesh_algo
    type(ROM_DS_AlgoPara), intent(in) :: ds_algorom
    aster_logical, intent(in), optional :: l_line_search_
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction - Solving non-linear problem
!
! Check ROM algorithm datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  phenom           : phenomenon (MECA/THER)
! In  mesh_algo        : mesh from *_non_line
! In  ds_algorom       : datastructure for ROM parameters
! In  l_line_search    : .true. if line search
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: iret, nb_mode
    character(len=8) :: mesh_base = ' '
    character(len=24) :: field_type = ' '
    character(len=24) :: grnode_int
    aster_logical :: l_hrom
    type(ROM_DS_Empi) :: ds_empi
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_30')
    endif
!
! - Get parameters
!
    ds_empi    = ds_algorom%ds_empi
    l_hrom     = ds_algorom%l_hrom
    grnode_int = ds_algorom%grnode_int
!
! - Check mesh
!
    mesh_base = ds_empi%mesh
    if (mesh_base .ne. mesh_algo) then
        call utmess('F', 'ROM5_31')
    endif
!
! - Check field in base
!
    field_type = ds_empi%field_type
    if (phenom .eq. 'THER') then
        if (field_type .ne. 'TEMP') then
            call utmess('F', 'ROM5_32')
        endif
    elseif (phenom .eq. 'MECA') then
        if (field_type .ne. 'DEPL') then
            call utmess('F', 'ROM5_32')
        endif
    else
        ASSERT(.false.)
    endif
!
! - Check group of nodes
!
    if (l_hrom) then
        call jeexin(mesh_algo//'.GROUPENO', iret)
        if (iret .eq. 0) then
            call utmess('F', 'ROM5_33', sk = grnode_int)
        else
            call jenonu(jexnom(mesh_algo//'.GROUPENO', grnode_int), iret)
            if (iret .eq. 0) then
                call utmess('F', 'ROM5_33', sk = grnode_int)
            endif
        endif
    endif
!
! - Check empiric base
!
    nb_mode = ds_empi%nb_mode
    if (nb_mode .lt. 1) then
        call utmess('F', 'ROM5_35')
    endif
!
! - Function exclusion
!
    if (phenom .eq. 'THER') then
        if (l_line_search_) then
            call utmess('F', 'ROM5_34')
        endif
    elseif (phenom .eq. 'MECA') then
!
    else
        ASSERT(.false.)
    endif   
!
end subroutine
