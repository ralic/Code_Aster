subroutine nxlect(l_ther_nonl, list_load  , solver    , ther_para_i, ther_para_r,&
                  ther_crit_i, ther_crit_r, result_dry, matcst     , coecst     ,&
                  result     , model      , mate      , cara_elem  , compor     )
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getres.h"
#include "asterfort/cresol.h"
#include "asterfort/nxdocc.h"
#include "asterfort/ntdcom.h"
#include "asterfort/ntdomt.h"
#include "asterfort/ntdoth.h"
#include "asterfort/nxdocn.h"
#include "asterfort/nxdomt.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jessica.haelewyn at edf.fr
!
    aster_logical, intent(in) :: l_ther_nonl
    character(len=19), intent(inout) :: list_load
    character(len=19), intent(in) :: solver
    integer, intent(inout) :: ther_para_i(*)
    integer, intent(inout) :: ther_crit_i(*)
    real(kind=8), intent(inout) :: ther_para_r(*)
    real(kind=8), intent(inout) :: ther_crit_r(*)
    character(len=8), intent(out) :: result_dry
    aster_logical, intent(out) :: matcst
    aster_logical, intent(out) :: coecst
    character(len=24), intent(out) :: result
    character(len=24), intent(out) :: model
    character(len=24), intent(out) :: mate
    character(len=24), intent(out) :: cara_elem
    character(len=24), intent(out) :: compor
!
! --------------------------------------------------------------------------------------------------
!
! Thermics - Initializations
!
! Read parameters
!
! --------------------------------------------------------------------------------------------------
!
! In  l_temp_nonl      : .true. if THER_NON_LINE
! IO  list_load        : name for list of loads
! In  solver           : name of datastructure for linear solver parameters
! IO  ther_para_i      : parameters for algorithm (integer)
! IO  ther_para_r      : parameters for algorithm (real)
! IO  ther_crit_i      : criteria for algorithm (integer)
! IO  ther_crit_r      : criteria for algorithm (real)
! Out result_dry       : name of datastructure for results (drying)
! Out matcst           : .true. if constant material parameters
! Out coecst           : .true. if constant rigidity matrix
! Out result           : name of datastructure for results
! Out model            : name of model
! Out mate             : name of material characteristics (field)
! Out cara_elem        : name of datastructure for elementary parameters (CARTE)
! Out compor           : name of <CARTE> COMPOR
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: k16bid
    character(len=8) :: k8bid
!
! --------------------------------------------------------------------------------------------------
!
    result_dry = ' '
    compor     = ' '
!
! - Get datastructure for results
!
    call getres(result, k16bid, k8bid)
!
! - Read parameters
!
    call ntdoth(model, mate, cara_elem, list_load,&
                matcst_ = matcst, coecst_ = coecst )
!
! - Create comportment <CARTE>
!
    if (l_ther_nonl) then
        call nxdocc(model, compor)
    endif
!
! - Get parameters for linear solver
!
    call cresol(solver)
!
! - Get algorithm parameters and criteria
!
    if (l_ther_nonl) then
        call nxdomt(ther_para_i, ther_para_r)
        call ntdcom(result_dry)
        call nxdocn(ther_crit_i, ther_crit_r)
    else
        call ntdomt(ther_para_r)
    endif
!
end subroutine
