subroutine dbr_DSInit(ds_para)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/infniv.h"
#include "asterfort/romBaseDSInit.h"
#include "asterfort/romLineicBaseDSInit.h"
#include "asterfort/romSnapDSInit.h"
#include "asterfort/dbr_paraPODDSInit.h"
#include "asterfort/dbr_paraRBDSInit.h"
#include "asterfort/dbr_paraDSInit.h"
#include "asterfort/romMultiParaDSInit.h"
#include "asterfort/romSolveDSInit.h"
#include "asterfort/romMultiCoefDSInit.h"
#include "asterfort/romVariParaDSInit.h"
#include "asterfort/romEvalCoefDSInit.h"
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
    type(ROM_DS_ParaDBR), intent(out) :: ds_para
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Initializations
!
! Initialization of datastructures
!
! --------------------------------------------------------------------------------------------------
!
! Out ds_para          : datastructure for parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    type(ROM_DS_Snap) :: ds_snap
    type(ROM_DS_Empi) :: ds_empi
    type(ROM_DS_LineicNumb)  :: ds_lineicnumb
    type(ROM_DS_ParaDBR_POD) :: ds_para_pod
    type(ROM_DS_ParaDBR_RB)  :: ds_para_rb
    type(ROM_DS_Solve)       :: ds_solveROM, ds_solveDOM
    type(ROM_DS_MultiPara)   :: ds_multipara
    type(ROM_DS_MultiCoef)   :: ds_multicoef_v, ds_multicoef_m
    type(ROM_DS_VariPara)    :: ds_varipara
    type(ROM_DS_EvalCoef)    :: ds_evalcoef
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_9')
    endif
!
! - Initialization of datastructure for lineic base numbering
!
    call romLineicBaseDSInit(ds_lineicnumb)
!
! - Initialization of datastructure for snapshot selection
!
    call romSnapDSInit(ds_snap)
!
! - Initialization of datastructure for empiric modes
!
    call romBaseDSInit(ds_lineicnumb, ds_empi)
!
! - Initialisation of datastructure for solving problems
!
    call romSolveDSInit('ROM', ds_solveROM)
    call romSolveDSInit('DOM', ds_solveDOM)
!
! - Initialisation of datastructure for multiparametric problems - Coefficients
!
    call romMultiCoefDSInit('V', ds_multicoef_v)
    call romMultiCoefDSInit('M', ds_multicoef_m)
!
! - Initializations of variation of parameters for multiparametric problems
!
    call romVariParaDSInit(ds_varipara)
!
! - Initializations of evaluation of coefficients for multiparametric problems
!
    call romEvalCoefDSInit(ds_evalcoef)
!
! - Initialisation of datastructure for multiparametric problems
!
    call romMultiParaDSInit(ds_multicoef_v, ds_multicoef_m, ds_varipara, ds_evalcoef,&
                            ds_multipara)
!
! - Initialization of datastructures for POD parameters
!
    call dbr_paraPODDSInit(ds_snap, ds_para_pod)
!
! - Initialization of datastructures for RB parameters
!
    call dbr_paraRBDSInit(ds_multipara, ds_solveDOM, ds_solveROM, ds_para_rb)    
!
! - Initialization of datastructures for parameters
!
    call dbr_paraDSInit(ds_empi, ds_para_pod, ds_para_rb, ds_para)
!
end subroutine
