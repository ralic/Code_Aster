subroutine cfmxsd(mesh_     , model_, nume_dof, list_func_acti, sddyna,&
                  ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfcrsd.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmap.h"
#include "asterfort/cfmmma.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/cfmxme.h"
#include "asterfort/cfmxr0.h"
#include "asterfort/infdbg.h"
#include "asterfort/wkvect.h"
#include "asterfort/xxmxme.h"
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
    character(len=*), intent(in) :: mesh_
    character(len=*), intent(in) :: model_
    character(len=24), intent(in) :: nume_dof
    integer, intent(in) :: list_func_acti(*)
    character(len=19), intent(in) :: sddyna
    type(NL_DS_Contact), intent(in) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Prepare contact solving datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  nume_dof         : name of numbering object (NUME_DDL)
! In  list_func_acti   : list of active functionnalities
! In  sddyna           : name of dynamic solving datastructure
! In  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdcont_defi, sdcont_solv
    integer :: ifm, niv
    character(len=8) :: model, mesh
    integer :: zbouc
    integer :: nb_cont_zone
    aster_logical :: l_cont_disc, l_cont_cont, l_cont_xfem, l_cont_allv
    character(len=14) :: nume_dof_frot
    character(len=24) :: sdcont_crnudd
    aster_logical, pointer :: v_sdcont_crnudd(:) => null()
    character(len=24) :: sdcont_maxdep
    real(kind=8), pointer :: v_sdcont_maxdep(:) => null()
    character(len=24) :: sdcont_nosdco
    character(len=24), pointer :: v_sdcont_nosdco(:) => null()
    character(len=24) :: sdcont_mboucl
    integer, pointer :: v_sdcont_mboucl(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> Create contact datastructures for solving'
    endif
!
! - Initializations
!
    sdcont_defi   = ds_contact%sdcont_defi
    sdcont_solv   = ds_contact%sdcont_solv
    nume_dof_frot = '&&CFMXSD.NUMDF'
    nb_cont_zone  = cfdisi(sdcont_defi,'NZOCO')
    model         = model_
    mesh          = mesh_
!
! - Contact method
!
    l_cont_xfem = cfdisl(sdcont_defi,'FORMUL_XFEM')
    l_cont_cont = cfdisl(sdcont_defi,'FORMUL_CONTINUE')
    l_cont_disc = cfdisl(sdcont_defi,'FORMUL_DISCRETE')
    l_cont_allv = cfdisl(sdcont_defi,'ALL_VERIF')
!
! - Create CONT_NOEU datastructure
!
    call cfmxr0(sdcont_defi, sdcont_solv, mesh)
!
! - Create pairing datastructure
!
    if (l_cont_cont.or.l_cont_disc) then
        call cfmmap(mesh, sdcont_defi, sdcont_solv)
    endif
!
! - Create loop counters datastructure
!
    zbouc         = cfmmvd('ZBOUC')
    sdcont_mboucl = sdcont_solv(1:14)//'.MBOUCL'
    call wkvect(sdcont_mboucl, 'V V I', zbouc, vi = v_sdcont_mboucl)
!
! - Create datastructure for datastructure names
!
    sdcont_nosdco = sdcont_solv(1:14)//'.NOSDCO'
    call wkvect(sdcont_nosdco, 'V V K24', 4, vk24 = v_sdcont_nosdco)
    v_sdcont_nosdco(1) = nume_dof_frot
    v_sdcont_nosdco(2) = ds_contact%ligrel_elem_cont
    v_sdcont_nosdco(3) = ds_contact%ligrel_elem_cont
    v_sdcont_nosdco(4) = ds_contact%iden_rela
!
! - Create datastructure for renumbering flag
!
    if (l_cont_cont) then
        sdcont_crnudd = sdcont_solv(1:14)//'.NUDD'
        call wkvect(sdcont_crnudd, 'V V L', 1, vl = v_sdcont_crnudd)
        if (l_cont_allv) then
            v_sdcont_crnudd(1) = .false.
        else
            v_sdcont_crnudd(1) = .true.
        endif
    endif
!
! - Create datastructure for geometric loop parameter
!
    sdcont_maxdep = sdcont_solv(1:14)//'.MAXD'
    call wkvect(sdcont_maxdep, 'V V R', 1, vr = v_sdcont_maxdep)
    v_sdcont_maxdep(1) = -1.d0
!
! - Create datastructures for solving
!
    if (.not.l_cont_allv) then
!
! ----- Create datastructures for DISCRETE/CONTINUE methods
!
        if (l_cont_cont.or.l_cont_disc) then
            call cfmmma(sdcont_defi, sdcont_solv)
        endif
!
! ----- Create datastructures for DISCRETE method
!
        if (l_cont_disc) then
            call cfcrsd(mesh, nume_dof, sdcont_defi, sdcont_solv)
        endif
!
! ----- Create datastructures for CONTINUE method
!
        if (l_cont_cont) then
            call cfmxme(nume_dof, sddyna, sdcont_defi, sdcont_solv)
        endif
!
! ----- Create datastructures for XFEM method
!
        if (l_cont_xfem) then
            call xxmxme(mesh, model, list_func_acti, sdcont_defi, sdcont_solv)
        endif
    endif
!
end subroutine
