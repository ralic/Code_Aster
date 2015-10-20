subroutine cfmxme(nume_dof, sddyna, ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/mm_cycl_crsd.h"
#include "asterfort/mm_cycl_init.h"
#include "asterfort/ndynlo.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcreb.h"
#include "asterfort/wkvect.h"
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
    character(len=24), intent(in) :: nume_dof
    character(len=19), intent(in) :: sddyna
    type(NL_DS_Contact), intent(in) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue method - Create datastructures for CONTINUE method
!
! --------------------------------------------------------------------------------------------------
!
! In  nume_dof         : name of numbering object (NUME_DDL)
! In  sddyna           : name of dynamic solving datastructure
! In  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nt_cont_poin
    aster_logical :: l_dyna, l_inte_node
    character(len=24) :: sdcont_etatct
    real(kind=8), pointer :: v_sdcont_etatct(:) => null()
    character(len=24) :: sdcont_tabfin
    real(kind=8), pointer :: v_sdcont_tabfin(:) => null()
    character(len=24) :: sdcont_apjeu
    real(kind=8), pointer :: v_sdcont_apjeu(:) => null()
    character(len=24) :: sdcont_vitini, sdcont_accini
    integer :: ztabf, zetat
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> . Create contact datastructures for CONTINUE method'
    endif
!
! - Get parameters
!
    nt_cont_poin = cfdisi(ds_contact%sdcont_defi,'NTPC')
    l_dyna       = ndynlo(sddyna,'DYNAMIQUE')
!
! - Create datastructure for general informations about contact
!
    sdcont_tabfin = ds_contact%sdcont_solv(1:14)//'.TABFIN'
    ztabf = cfmmvd('ZTABF')
    call wkvect(sdcont_tabfin, 'V V R', ztabf*nt_cont_poin+1, vr = v_sdcont_tabfin)
    v_sdcont_tabfin(1) = nt_cont_poin
!
! - Create fields for dynamic management
!
    if (l_dyna) then
        sdcont_vitini = ds_contact%sdcont_solv(1:14)//'.VITI'
        sdcont_accini = ds_contact%sdcont_solv(1:14)//'.ACCI'
        call vtcreb(sdcont_vitini, 'V', 'R', nume_ddlz = nume_dof)
        call vtcreb(sdcont_accini, 'V', 'R', nume_ddlz = nume_dof)
    endif
!
! - Create datastructure to save contact states (step cutting management)
!
    zetat         = cfmmvd('ZETAT')
    sdcont_etatct = ds_contact%sdcont_solv(1:14)//'.ETATCT'
    call wkvect(sdcont_etatct, 'V V R', zetat*nt_cont_poin, vr = v_sdcont_etatct)
!
! - Create datastructure for cycling detection and treatment
!
    call mm_cycl_crsd(ds_contact)
    call mm_cycl_init(ds_contact)
!
! - Create datastructure to save gaps
!
    sdcont_apjeu = ds_contact%sdcont_solv(1:14)//'.APJEU'
    call wkvect(sdcont_apjeu, 'V V R', nt_cont_poin, vr = v_sdcont_apjeu)
!
! - Warning if not node integration (=> no CONT_NOEU)
!
    l_inte_node = cfdisl(ds_contact%sdcont_defi,'ALL_INTEG_NOEUD')
    if (.not.l_inte_node) then
        call utmess('A', 'CONTACT3_16')
    endif
!
end subroutine
