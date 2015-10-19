subroutine nmctcg(model   , mesh, ds_contact, sdstat, sdtime,&
                  nume_dof)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/xmctcg.h"
#include "asterfort/mmctcg.h"
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
    character(len=24), intent(in) :: model
    type(NL_DS_Contact), intent(inout) :: ds_contact
    character(len=24), intent(in) :: nume_dof
    character(len=24), intent(in) :: sdtime
    character(len=24), intent(in) :: sdstat
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Continue/XFEM method
!
! Geometric loop: geometric actualisation and pairing 
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! IO  ds_contact       : datastructure for contact management
! In  nume_dof         : name of numbering object (NUME_DDL)
! In  sdtime           : datastructure for timers
! In  sdstat           : datastructure for statistics
!
! --------------------------------------------------------------------------------------------------
!
    integer :: cont_form
    aster_logical :: l_cont_allv
!
! --------------------------------------------------------------------------------------------------
!
    cont_form   = cfdisi(ds_contact%sdcont_defi,'FORMULATION')
    l_cont_allv = cfdisl(ds_contact%sdcont_defi,'ALL_VERIF')
!
! - Geometric loop: geometric actualisation and pairing
!
    if (.not.l_cont_allv) then
        if (cont_form .eq. 2) then
            call mmctcg(mesh , ds_contact, nume_dof, sdstat, sdtime)
        elseif (cont_form .eq. 3) then
            call xmctcg(model, mesh, ds_contact, sdstat, sdtime)
        endif
    endif
!
end subroutine
