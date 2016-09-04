subroutine romAlgoNLInit(phenom        , mesh, nume_dof, result, ds_algorom,&
                         l_line_search_)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/infniv.h"
#include "asterfort/romEquationListCreate.h"
#include "asterfort/romBaseCopy.h"
#include "asterfort/romBaseTruncation.h"
#include "asterfort/romAlgoNLCheck.h"
#include "asterfort/romAlgoNLTableCreate.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
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
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: nume_dof
    character(len=8), intent(in) :: result
    type(ROM_DS_AlgoPara), intent(inout) :: ds_algorom
    aster_logical, intent(in), optional :: l_line_search_
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction - Solving non-linear problem
!
! Init ROM algorithm datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  phenom           : phenomenon (MECA/THER)
! In  mesh             : name of mesh
! In  nume_dof         : name of numbering (NUME_DDL)
! In  result           : name of datastructure for results
! IO  ds_algorom       : datastructure for ROM parameters
! In  l_line_search    : .true. if line search
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    aster_logical :: l_hrom
    integer :: nb_mode
    character(len=8) :: base_rid
    character(len=24) :: gamma = ' '
    real(kind=8), pointer :: v_gamma(:) => null()   
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_37')
    endif
!
! - Get parameters
!
    l_hrom  = ds_algorom%l_hrom
    nb_mode = ds_algorom%ds_empi%nb_mode
!
! - Check ROM algorithm datastructure
!
    call romAlgoNLCheck(phenom, mesh, ds_algorom, l_line_search_)
!
! - Prepare the list of equations at interface
!
    if (l_hrom) then
        call romEquationListCreate(ds_algorom%ds_empi   , nume_dof, ds_algorom%grnode_int,&
                                   ds_algorom%v_equa_int)
    endif
!
! - Truncation of empirical modes on RID
!
    if (l_hrom) then
        base_rid = '&&TRUNC'
        call romBaseCopy(ds_algorom%ds_empi, base_rid, ds_algorom%ds_empi_rid)
        call romBaseTruncation(ds_algorom%ds_empi, nume_dof, 'V', ds_algorom%ds_empi_rid)
    endif
!
! - Create object for reduced coordinates
!
    gamma = '&&GAMMA'
    call wkvect(gamma, 'V V R', nb_mode, vr = v_gamma)
    ds_algorom%gamma = gamma
!
! - Create table for the reduced coordinates
!
    call romAlgoNLTableCreate(result, ds_algorom)
!
end subroutine
