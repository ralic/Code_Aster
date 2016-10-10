subroutine op0053()
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/infmaj.h"
#include "asterfort/titre.h"
#include "asterfort/dbr_chck.h"
#include "asterfort/dbr_ini0.h"
#include "asterfort/dbr_read.h"
#include "asterfort/dbr_rnum.h"
#include "asterfort/dbr_main.h"
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
    character(len=8) :: result
!
! --------------------------------------------------------------------------------------------------
!
    call titre()
    call infmaj()
!
! - Initialization of datastructures
!
    call dbr_ini0(ds_para)
!
! - Read parameters
!
    call dbr_read(ds_para, result)
!
! - Check parameters
!
    call dbr_chck(result, ds_para)
!
! - Create numbering of nodes for the lineic model
!
    if (ds_para%ds_empi%base_type .eq. 'LINEIQUE') then
        call dbr_rnum(ds_para%ds_empi)
    endif
!
! - Compute the POD by the main function
!
    call dbr_main(ds_para)
!
! - Clean
!
    if (ds_para%ds_empi%base_type .eq. 'LINEIQUE') then
       AS_DEALLOCATE(vi = ds_para%ds_empi%ds_lineic%v_nume_pl)
       AS_DEALLOCATE(vi = ds_para%ds_empi%ds_lineic%v_nume_sf)
    endif
!
end subroutine
