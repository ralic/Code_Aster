subroutine op0053()
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/infmaj.h"
#include "asterfort/titre.h"
#include "asterfort/dbr_chck.h"
#include "asterfort/dbr_DSInit.h"
#include "asterfort/dbr_init_base.h"
#include "asterfort/dbr_init_algo.h"
#include "asterfort/dbr_para_info.h"
#include "asterfort/dbr_read.h"
#include "asterfort/dbr_main.h"
#include "asterfort/dbr_clean.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!
!
! --------------------------------------------------------------------------------------------------
!
!   DEFI_BASE_REDUITE
!
! --------------------------------------------------------------------------------------------------
!
    type(ROM_DS_ParaDBR) :: ds_para
!
! --------------------------------------------------------------------------------------------------
!
    call titre()
    call infmaj()
!
! - Initialization of datastructures
!
    call dbr_DSInit(ds_para)
!
! - Read parameters
!
    call dbr_read(ds_para)
!
! - Prepare datastructure for empiric modes
!
    call dbr_init_base(ds_para)
!
! - Check parameters
!
    call dbr_chck(ds_para)
!
! - Init algorithm
!
    call dbr_init_algo(ds_para)
!
! - Print informations
!
    call dbr_para_info(ds_para)
!
! - Compute the POD by the main function
!
    call dbr_main(ds_para)
!
! - Clean datastructures
!
    call dbr_clean(ds_para)
!
end subroutine
