subroutine comp_meca_info(ds_compor_prep)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterc/getfac.h"
#include "asterfort/comp_meca_init.h"
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
! aslint: disable=W1003
! person_in_charge: mickael.abbas at edf.fr
!
    type(NL_DS_ComporPrep), intent(out) :: ds_compor_prep
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Create datastructure to prepare comportement
!
! --------------------------------------------------------------------------------------------------
!
! Out ds_compor_prep   : datastructure to prepare comportement
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: keywordfact
    integer :: nb_info_comp, nbocc_compor
    type(NL_DS_Compor) :: ds_compor
!
! --------------------------------------------------------------------------------------------------
!
    nbocc_compor = 0
    keywordfact  = 'COMPORTEMENT'
    call getfac(keywordfact, nbocc_compor)
!
! - Initializations
!
    ds_compor_prep%v_comp => null()
    ds_compor_prep%v_exte => null()
!
! - Number of comportement information
!
    if (nbocc_compor .eq. 0) then
        nb_info_comp = 1
    else
        nb_info_comp = nbocc_compor
    endif
!
! - Save number of comportments
!
    ds_compor_prep%nb_comp = nbocc_compor
!
! - Allocate comportment informations objects 
!
    allocate(ds_compor_prep%v_comp(nb_info_comp))
!
! - Allocate comportment informations objects (external: UMAT and MFront)
!
    allocate(ds_compor_prep%v_exte(nb_info_comp))
!
! - If nothing in COMPORTEMENT: all is elastic
!
    call comp_meca_init(ds_compor)
    if (nbocc_compor .eq. 0) then
        ds_compor_prep%v_comp(1) = ds_compor
        ds_compor_prep%v_comp(1)%rela_comp = 'ELAS'
        ds_compor_prep%v_comp(1)%defo_comp = 'PETIT'
        ds_compor_prep%v_comp(1)%type_comp = 'COMP_ELAS'
        ds_compor_prep%v_comp(1)%type_cpla = 'ANALYTIQUE'
    endif
!
end subroutine
