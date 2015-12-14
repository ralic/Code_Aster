subroutine mminit(mesh  , ds_contact, sddyna  , hat_valinc, sdtime,&
                  sdstat, sdnume    , nume_dof, nume_inst)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/copisd.h"
#include "asterfort/jeveuo.h"
#include "asterfort/misazl.h"
#include "asterfort/mm_cycl_init.h"
#include "asterfort/mmbouc.h"
#include "asterfort/mmopti.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmchex.h"
#include "asterfort/mmapin.h"
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
    character(len=8), intent(in) :: mesh
    type(NL_DS_Contact), intent(inout) :: ds_contact
    character(len=19), intent(in) :: hat_valinc(*)
    character(len=24), intent(in) :: sdtime
    character(len=24), intent(in) :: sdstat
    character(len=19), intent(in) :: sddyna
    character(len=19), intent(in) :: sdnume    
    character(len=24), intent(in) :: nume_dof
    integer, intent(in) :: nume_inst
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue method - Initializations
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! IO  ds_contact       : datastructure for contact management
! In  hat_valinc       : hat variable for algorithm fields
! In  nume_dof         : name of numbering object (NUME_DDL)
! In  sdtime           : datastructure for timers
! In  sdstat           : datastructure for statistics
! In  sddyna           : datastructure for dynamic
! In  sdnume           : name of dof positions datastructure
! In  nume_inst        : index of current step time
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_dyna, l_cont_allv, l_step_first
    character(len=19) :: sdcont_depgeo, sdcont_deplam, sdcont_depini
    character(len=19) :: sdcont_vitini, sdcont_accini
    character(len=19) :: disp_prev, acce_curr, vite_curr
    character(len=24) :: sdcont_tabfin
    real(kind=8), pointer :: v_sdcont_tabfin(:) => null()
    character(len=24) :: sdcont_etatct
    real(kind=8), pointer :: v_sdcont_etatct(:) => null()
    integer :: ztabf, zetat
    integer :: ipc, nb_inte_poin
!
! --------------------------------------------------------------------------------------------------
!
    l_cont_allv  = cfdisl(ds_contact%sdcont_defi,'ALL_VERIF')
    l_dyna       = ndynlo(sddyna,'DYNAMIQUE')
    ztabf        = cfmmvd('ZTABF')
    zetat        = cfmmvd('ZETAT')
    nb_inte_poin = cfdisi(ds_contact%sdcont_defi,'NTPC' )
!
! - Using *_INIT options (like SEUIL_INIT)
!
    l_step_first = nume_inst .eq. 1
!
! - Geometric loop counter initialization
!
    call mmbouc(ds_contact, 'Geom', 'Init_Counter')
!
! - First geometric loop counter
!
    call mmbouc(ds_contact, 'Geom', 'Incr_Counter')
!
! - Get field names in hat-variables
!
    call nmchex(hat_valinc, 'VALINC', 'DEPMOI', disp_prev)
    call nmchex(hat_valinc, 'VALINC', 'VITPLU', vite_curr)
    call nmchex(hat_valinc, 'VALINC', 'ACCPLU', acce_curr)
!
! - Lagrangians initialized (LAMBDA TOTAUX)
!
    sdcont_depini = ds_contact%sdcont_solv(1:14)//'.INIT'
    call copisd('CHAMP_GD', 'V', disp_prev, sdcont_depini)
    call misazl(sdnume, disp_prev)
    if (l_dyna) then
        call misazl(sdnume, acce_curr)
        call misazl(sdnume, vite_curr)
    endif
!
! - Management of status for time cut
!
    sdcont_tabfin = ds_contact%sdcont_solv(1:14)//'.TABFIN'
    sdcont_etatct = ds_contact%sdcont_solv(1:14)//'.ETATCT'
    call jeveuo(sdcont_tabfin, 'E', vr = v_sdcont_tabfin)
    call jeveuo(sdcont_etatct, 'L', vr = v_sdcont_etatct)
    do ipc = 1, nb_inte_poin
        v_sdcont_tabfin(ztabf*(ipc-1)+23) = v_sdcont_etatct(zetat*(ipc-1)+1)
        v_sdcont_tabfin(ztabf*(ipc-1)+17) = v_sdcont_etatct(zetat*(ipc-1)+2)
        v_sdcont_tabfin(ztabf*(ipc-1)+18) = v_sdcont_etatct(zetat*(ipc-1)+3)
    end do
!
! - Save speed and acceleration
!
    sdcont_vitini = ds_contact%sdcont_solv(1:14)//'.VITI'
    sdcont_accini = ds_contact%sdcont_solv(1:14)//'.ACCI'
    if (l_dyna) then
        call copisd('CHAMP_GD', 'V', vite_curr, sdcont_vitini)
        call copisd('CHAMP_GD', 'V', acce_curr, sdcont_accini)
    endif
!
! - Save displacements for geometric loop
!
    sdcont_depgeo = ds_contact%sdcont_solv(1:14)//'.DEPG'
    call copisd('CHAMP_GD', 'V', disp_prev, sdcont_depgeo)
!
! - Save displacements for friction loop
!
    sdcont_deplam = ds_contact%sdcont_solv(1:14)//'.DEPF'
    call copisd('CHAMP_GD', 'V', disp_prev, sdcont_deplam)
!
! - Initial pairing
!
    call mmapin(mesh, ds_contact, nume_dof, sdtime, sdstat)
!
! - Initial options
!
    if (.not.l_cont_allv.and.l_step_first) then
        call mmopti(mesh, ds_contact)
    endif
!
end subroutine
