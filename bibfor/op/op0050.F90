subroutine op0050()
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/infmaj.h"
#include "asterfort/titre.h"
#include "asterfort/ddr_ini0.h"
#include "asterfort/ddr_chck.h"
#include "asterfort/ddr_read.h"
#include "asterfort/ddr_main.h"
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
! ----------------------------------------------------------------------
!
!   DEFI_DOMAINE_REDUIT
!
! ----------------------------------------------------------------------
!
    type(ROM_DS_ParaDDR) :: ds_para
!
! ----------------------------------------------------------------------
!
    call titre()
    call infmaj()
!
! - Create datastructure
!
    call ddr_ini0(ds_para)
!
! - Read parameters
!
    call ddr_read(ds_para)
!
! - Some checks
!
    call ddr_chck(ds_para)
!
! - Compute EIM
!   
    call ddr_main(ds_para)
!
end subroutine
