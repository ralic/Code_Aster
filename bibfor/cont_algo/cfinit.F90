subroutine cfinit(ds_contact, nume_inst)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfdisl.h"
#include "asterfort/isfonc.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mmbouc.h"
#include "asterfort/mminit.h"
#include "asterfort/vtzero.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    type(NL_DS_Contact), intent(in) :: ds_contact
    integer, intent(in) :: nume_inst
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Discrete methods - Initializations for current time step
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! In  nume_inst        : index of current step time
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_reac_geom(3)
    character(len=24) :: sdcont_clreac
    aster_logical, pointer :: v_sdcont_clreac(:) => null()
    character(len=24) :: sdcont_autoc1, sdcont_autoc2
!
! --------------------------------------------------------------------------------------------------
!

!
! - Datastructure for contact solving
!
    sdcont_clreac = ds_contact%sdcont_solv(1:14)//'.REAL'
    call jeveuo(sdcont_clreac, 'E', vl = v_sdcont_clreac)
    sdcont_autoc1 = ds_contact%sdcont_solv(1:14)//'.REA1'
    sdcont_autoc2 = ds_contact%sdcont_solv(1:14)//'.REA2'
!
! - Geometric parameters
!
    l_reac_geom(1) = .true.
    l_reac_geom(2) = .false.
    l_reac_geom(3) = .true.
    if (cfdisl(ds_contact%sdcont_defi,'REAC_GEOM_SANS')) then
        if (nume_inst .ne. 1) then
            l_reac_geom(1) = .false.
            l_reac_geom(3) = .false.
        endif
    endif
!
! - Geometric loop counter initialization
!
    call mmbouc(ds_contact, 'GEOM', 'INIT')
!
! - First geometric loop counter
!    
    call mmbouc(ds_contact, 'GEOM', 'INCR')
!
! - Vector initialization for REAC_GEOM
!
    call vtzero(sdcont_autoc1)
    call vtzero(sdcont_autoc2)
!
! - Save parameters
!
    v_sdcont_clreac(1) = l_reac_geom(1)
    v_sdcont_clreac(2) = l_reac_geom(2)
    v_sdcont_clreac(3) = l_reac_geom(3)
!
end subroutine
