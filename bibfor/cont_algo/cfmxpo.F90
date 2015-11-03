subroutine cfmxpo(mesh  , model_   , ds_contact, nume_inst  , sddisc,&
                  sdstat, hval_algo, hval_incr , hval_veasse)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdeco.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmxre.h"
#include "asterfort/cfverl.h"
#include "asterfort/mmdeco.h"
#include "asterfort/xmdeco.h"
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
    character(len=*), intent(in) :: model_
    character(len=24), intent(in) :: sdstat
    type(NL_DS_Contact), intent(in) :: ds_contact
    integer, intent(in) :: nume_inst
    character(len=19), intent(in) :: sddisc
    character(len=19), intent(in) :: hval_algo(*)
    character(len=19), intent(in) :: hval_incr(*)
    character(len=19), intent(in) :: hval_veasse(*)
!
! --------------------------------------------------------------------------------------------------
!
! Contact
!
! Post-treatment for contact
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  sdstat           : datastructure for statistics
! In  ds_contact       : datastructure for contact management
! In  nume_inst        : index of current time step
! In  sddisc           : datastructure for discretization
! In  hval_algo        : hat-variable for algorithms fields
! In  hval_incr        : hat-variable for incremental values fields
! In  hval_veasse      : hat-variable for vectors (node fields)
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_cont_cont, l_cont_disc, l_cont_xfem, l_all_verif, l_cont_lac
!
! --------------------------------------------------------------------------------------------------
!
    l_cont_cont = cfdisl(ds_contact%sdcont_defi,'FORMUL_CONTINUE')
    l_cont_disc = cfdisl(ds_contact%sdcont_defi,'FORMUL_DISCRETE')
    l_cont_xfem = cfdisl(ds_contact%sdcont_defi,'FORMUL_XFEM')
    l_cont_lac  = .false._1
!   l_cont_lac  = cfdisl(ds_contact%sdcont_defi, 'FORMUL_LAC')
    l_all_verif = cfdisl(ds_contact%sdcont_defi,'ALL_VERIF') 
!
! - Time step cut management
!
    if (.not.l_all_verif) then
        if (l_cont_disc) then
            call cfdeco(ds_contact)
        else if (l_cont_cont) then
            call mmdeco(ds_contact)
        else if (l_cont_xfem) then
            call xmdeco(ds_contact)
        else if (l_cont_lac) then
            ASSERT(.false.)
        endif
    endif
!
! - Check normals
!
    if (l_cont_cont .or. l_cont_disc) then
        call cfverl(ds_contact)
    endif
!
! - Save post-treatment fields for contact
!
    call cfmxre(mesh  , model_   , sdstat   , ds_contact , nume_inst,&
                sddisc, hval_algo, hval_incr, hval_veasse)
!
end subroutine
