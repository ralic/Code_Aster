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
#include "asterfort/cfmxme.h"
#include "asterfort/cfmxr0.h"
#include "asterfort/cfmxr0_lac.h"
#include "asterfort/infdbg.h"
#include "asterfort/lac_crsd.h"
#include "asterfort/wkvect.h"
#include "asterfort/xxmxme.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=*), intent(in) :: mesh_
    character(len=*), intent(in) :: model_
    character(len=24), intent(in) :: nume_dof
    integer, intent(in) :: list_func_acti(*)
    character(len=19), intent(in) :: sddyna
    type(NL_DS_Contact), intent(inout) :: ds_contact
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
! IO  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=8) :: model, mesh
    integer :: nb_cont_zone
    aster_logical :: l_cont_disc, l_cont_cont, l_cont_xfem, l_cont_allv, l_cont_lac
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
    nb_cont_zone  = cfdisi(ds_contact%sdcont_defi,'NZOCO')
    model         = model_
    mesh          = mesh_
!
! - Contact method
!
    l_cont_xfem = cfdisl(ds_contact%sdcont_defi,'FORMUL_XFEM')
    l_cont_cont = cfdisl(ds_contact%sdcont_defi,'FORMUL_CONTINUE')
    l_cont_disc = cfdisl(ds_contact%sdcont_defi,'FORMUL_DISCRETE')
    l_cont_lac  = cfdisl(ds_contact%sdcont_defi,'FORMUL_LAC')
    l_cont_allv = cfdisl(ds_contact%sdcont_defi,'ALL_VERIF')
!
! - Create CONT_NOEU datastructure
!
    if (l_cont_cont .or. l_cont_disc .or. l_cont_xfem) then
        call cfmxr0(mesh, ds_contact)
    endif
!
! - Create pairing datastructure
!
    if (l_cont_cont .or. l_cont_disc .or. l_cont_lac) then
        call cfmmap(mesh, ds_contact)
    endif
!
! - Create datastructures for solving
!
    if (.not.l_cont_allv) then
!
! ----- Create datastructures for DISCRETE/CONTINUE methods
!
        if (l_cont_cont .or. l_cont_disc) then
            call cfmmma(ds_contact)
        endif
!
! ----- Create datastructures for DISCRETE method
!
        if (l_cont_disc) then
            call cfcrsd(mesh, nume_dof, ds_contact)
        endif
!
! ----- Create datastructures for CONTINUE method
!
        if (l_cont_cont) then
            call cfmxme(nume_dof, sddyna, ds_contact)
        endif
!
! ----- Create datastructures for LAC method
!
        if (l_cont_lac) then
            call lac_crsd(ds_contact)
        endif
!
! ----- Create datastructures for XFEM method
!
        if (l_cont_xfem) then
            call xxmxme(mesh, model, list_func_acti, ds_contact)
        endif
    endif
!
! - Create CONT_ELEM datastructure
!
    if (l_cont_lac) then
        call cfmxr0_lac(mesh, ds_contact)
    endif
!
end subroutine
