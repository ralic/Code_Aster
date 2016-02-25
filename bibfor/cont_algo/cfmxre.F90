subroutine cfmxre(mesh  , model_   , ds_measure, ds_contact , nume_inst,&
                  sddisc, hval_algo, hval_incr , hval_veasse)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmve.h"
#include "asterfort/cfresu.h"
#include "asterfort/cnscno.h"
#include "asterfort/diinst.h"
#include "asterfort/dismoi.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mmmcpt.h"
#include "asterfort/mmmres.h"
#include "asterfort/nmchex.h"
#include "asterfort/xmmres.h"
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
    character(len=8), intent(in) :: mesh
    character(len=*), intent(in) :: model_
    type(NL_DS_Measure), intent(inout) :: ds_measure
    type(NL_DS_Contact), intent(in) :: ds_contact
    integer, intent(in) :: nume_inst
    character(len=19), intent(in) :: sddisc
    character(len=19), intent(in) :: hval_algo(*)
    character(len=19), intent(in) :: hval_incr(*)
    character(len=19), intent(in) :: hval_veasse(*)
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Post-treatment
!
! All methods - Save post-treatment fields for contact
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! IO  ds_measure       : datastructure for measure and statistics management
! In  ds_contact       : datastructure for contact management
! In  nume_inst        : index of current time step
! In  sddisc           : datastructure for discretization
! In  hval_algo        : hat-variable for algorithms fields
! In  hval_incr        : hat-variable for incremental values fields
! In  hval_veasse      : hat-variable for vectors (node fields)
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_cont_cont, l_cont_disc, l_cont_xfem, l_cont_lac
    aster_logical :: l_cont_exiv, l_all_verif, l_inte_node
    character(len=19) :: disp_iter, disp_cumu_inst, disp_curr
    real(kind=8) :: time_curr, time_prev, time_incr
    character(len=19) :: prno
    character(len=8) :: model
    character(len=19) :: cnsinr, cnsper, cnoinr
    integer :: ibid
!
! --------------------------------------------------------------------------------------------------
!
    model = model_
!
! - Contact parameters
!
    l_cont_cont = cfdisl(ds_contact%sdcont_defi,'FORMUL_CONTINUE')
    l_cont_disc = cfdisl(ds_contact%sdcont_defi,'FORMUL_DISCRETE')
    l_cont_xfem = cfdisl(ds_contact%sdcont_defi,'FORMUL_XFEM')
    l_cont_lac  = .false._1
!   l_cont_lac  = cfdisl(ds_contact%sdcont_defi, 'FORMUL_LAC')
    l_cont_exiv = cfdisl(ds_contact%sdcont_defi,'EXIS_VERIF')
    l_all_verif = cfdisl(ds_contact%sdcont_defi,'ALL_VERIF') 
    l_inte_node = cfdisl(ds_contact%sdcont_defi,'ALL_INTEG_NOEUD')
!
! - Get fields name
!
    cnsinr = ds_contact%fields_cont_node
    cnoinr = ds_contact%field_cont_node
    cnsper = ds_contact%field_cont_perc
!
! - Times
!
    time_prev = diinst(sddisc,nume_inst-1)
    time_curr = diinst(sddisc,nume_inst)
    time_incr = time_curr - time_prev
!
! - Get fields
!
    call nmchex(hval_algo, 'SOLALG', 'DDEPLA', disp_iter)
    call nmchex(hval_algo, 'SOLALG', 'DEPDEL', disp_cumu_inst)
    call nmchex(hval_incr, 'VALINC', 'DEPPLU', disp_curr)
!
! - Create fields
!
    if (.not. l_all_verif) then
!
! ----- Create fields
!
        if (l_cont_xfem) then
            call xmmres(disp_cumu_inst, model, hval_veasse, cnsinr)
        else if (l_cont_cont) then
            if (l_inte_node) then
                call mmmres(mesh       , time_incr, ds_contact, disp_cumu_inst, sddisc,&
                            hval_veasse, cnsinr   , cnsper)
            endif
        else if (l_cont_disc) then
            call cfresu(time_incr, sddisc, ds_contact, disp_cumu_inst, disp_iter,&
                        cnsinr   , cnsper)
        else if (l_cont_lac) then   
            ASSERT(.false.)
        else
            ASSERT(.false.)
        endif
!
! ----- Number of contact links
!
        if (l_cont_cont) then
            call mmmcpt(mesh, ds_measure, ds_contact, cnsinr)
        endif
    endif
!
! - Create fields - No-computed contact
!
    if (l_cont_exiv) then
        call cfmmve(mesh, ds_contact, hval_incr, time_curr)
    endif
!
! - Transform fields
!
    call dismoi('PROF_CHNO', cnoinr, 'CHAM_NO', repk=prno, arret='C')
    call cnscno(cnsinr, prno, 'NON', 'V', cnoinr, 'F', ibid)
!
end subroutine
