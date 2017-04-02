subroutine dbr_calcrb_prep(ds_para_rb)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/romMultiParaCreateSyst.h"
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
    type(ROM_DS_ParaDBR_RB), intent(in) :: ds_para_rb
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Compute
!
! Prepare matrix and second member
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_para_rb       : datastructure for parameters (RB)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=1) :: syst_2mbr_type
    character(len=19) :: syst_matr, syst_2mbr
!
! --------------------------------------------------------------------------------------------------
!
    syst_matr      = ds_para_rb%syst_matr
    syst_2mbr      = ds_para_rb%syst_2mbr
    syst_2mbr_type = ds_para_rb%syst_2mbr_type
!
! - Create system
!
    call romMultiParaCreateSyst(ds_para_rb%ds_multipara, syst_matr, syst_2mbr_type, syst_2mbr)
!
end subroutine
