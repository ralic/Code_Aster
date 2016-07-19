subroutine ntdata(list_load, solver, matcst   , coecst  , result,&
                  model    , mate  , cara_elem, ds_inout, theta )
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getres.h"
#include "asterfort/cresol.h"
#include "asterfort/ntdoth.h"
#include "asterfort/ntdomt.h"
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
    character(len=19), intent(inout) :: list_load
    character(len=19), intent(in) :: solver
    aster_logical, intent(out) :: matcst
    aster_logical, intent(out) :: coecst
    character(len=8), intent(out) :: result
    character(len=24), intent(out) :: model
    character(len=24), intent(out) :: mate
    character(len=24), intent(out) :: cara_elem
    type(NL_DS_InOut), intent(inout) :: ds_inout
    real(kind=8), intent(out) :: theta
!
! --------------------------------------------------------------------------------------------------
!
! Thermics - Initializations
!
! Read parameters (linear)
!
! --------------------------------------------------------------------------------------------------
!
! Out matcst           : .true. if constant material parameters
! Out coecst           : .true. if constant rigidity matrix
! Out result           : name of datastructure for results
! Out model            : name of model
! Out mate             : name of material characteristics (field)
! Out cara_elem        : name of datastructure for elementary parameters (CARTE)
! IO  ds_inout         : datastructure for input/output management
! Out theta            : value for PARM_THETA
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: k16bid
    character(len=8) :: k8bid
!
! --------------------------------------------------------------------------------------------------
!

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
! - Get parameters for linear solver
!
    call cresol(solver)
!
! - Get algorithm parameters and criteria
!
    call ntdomt(theta)
!
! - Read parameters for input/output management
!
    call ReadInOut('THER', result, ds_inout)
!
end subroutine
