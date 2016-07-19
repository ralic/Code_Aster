subroutine nxlect(result     , model     , ther_crit_i, ther_crit_r, ds_inout,&
                  ds_algopara, result_dry, compor     , l_dry      , l_line_search)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/ntdcom.h"
#include "asterfort/nxdocc.h"
#include "asterfort/nxdocn.h"
#include "asterfort/nxdomt.h"
#include "asterfort/ReadInOut.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: result
    character(len=24), intent(in) :: model
    integer, intent(inout) :: ther_crit_i(*)
    real(kind=8), intent(inout) :: ther_crit_r(*)
    type(NL_DS_InOut), intent(inout) :: ds_inout
    type(NL_DS_AlgoPara), intent(inout) :: ds_algopara
    character(len=8), intent(out) :: result_dry
    character(len=24), intent(out) :: compor
    aster_logical, intent(out) :: l_dry
    aster_logical, intent(out) :: l_line_search
!
! --------------------------------------------------------------------------------------------------
!
! Thermics - Init
!
! Read parameters
!
! --------------------------------------------------------------------------------------------------
!
! In  result           : name of datastructure for results
! In  model            : name of model
! IO  ther_crit_i      : criteria for algorithm (integer)
! IO  ther_crit_r      : criteria for algorithm (real)
! IO  ds_inout         : datastructure for input/output management
! IO  ds_algopara      : datastructure for algorithm parameters
! Out result_dry       : name of datastructure for results (drying)
! Out compor           : name of <CARTE> COMPOR
! Out l_dry            : .true. if drying (concrete)
! Out l_line_search    : .true. if line search
!
! --------------------------------------------------------------------------------------------------
!
    result_dry = ' '
    compor     = ' '
!
! - Create comportment <CARTE>
!
    call nxdocc(model, compor)
!
! - Read parameters for algorithm management
!
    call nxdomt(ds_algopara)
!
! - Read parameters for drying
!
    call ntdcom(result_dry)
!
! - Read convergence criteria
!
    call nxdocn(ther_crit_i, ther_crit_r)
!
! - Read parameters for input/output management
!
    call ReadInOut('THER', result, ds_inout)
!
! - Drying
!
    l_dry = result_dry(1:1) .ne. ' '
!
! - Line search
!
    l_line_search = ds_algopara%line_search%iter_maxi .gt. 0
!
end subroutine
