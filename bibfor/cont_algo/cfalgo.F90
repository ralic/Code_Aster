subroutine cfalgo(mesh          , ds_measure, resi_glob_rela, iter_newt,&
                  solver        , nume_dof  , matr_asse     , disp_iter,&
                  disp_cumu_inst, ds_contact, ctccvg        )
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/algocg.h"
#include "asterfort/algocl.h"
#include "asterfort/algoco.h"
#include "asterfort/algocp.h"
#include "asterfort/algogl.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfeven.h"
#include "asterfort/cfpost.h"
#include "asterfort/cfprep.h"
#include "asterfort/fro2gd.h"
#include "asterfort/frogdp.h"
#include "asterfort/frolgd.h"
#include "asterfort/fropgd.h"
#include "asterfort/infdbg.h"
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
    type(NL_DS_Measure), intent(inout) :: ds_measure
    real(kind=8), intent(in) :: resi_glob_rela
    integer, intent(in) :: iter_newt
    character(len=19), intent(in) :: solver
    character(len=14), intent(in) :: nume_dof
    character(len=19), intent(in) :: matr_asse
    character(len=19), intent(in) :: disp_iter
    character(len=19), intent(in) :: disp_cumu_inst
    type(NL_DS_Contact), intent(inout) :: ds_contact 
    integer, intent(out) :: ctccvg 
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Discrete methods - Solve contact (pairing and algorithm)
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! IO  ds_measure       : datastructure for measure and statistics management
! In  resi_glob_rela   : current value of RESI_GLOB_RELA
! In  iter_newt        : index of current Newton iteration
! In  solver           : datastructure for solver parameters
! In  nume_dof         : name of numbering object (NUME_DDL)
! In  matr_asse        : matrix
! In  disp_iter        : displacement iteration
! In  disp_cumu_inst   : displacement increment from beginning of current time
! IO  ds_contact       : datastructure for contact management
! Out ctccvg           : output code for contact algorithm
!                        -1 - No solving
!                         0 - OK
!                        +1 - Maximum contact iteration
!                        +2 - Singular contact matrix
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: algo_cont, algo_frot, model_ndim
    aster_logical :: l_gliss, l_first_geom, l_wait_conv
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ... DEBUT DE LA RESOLUTION DU CONTACT'
    endif
!
! - Initializations
!
    ctccvg      = 0
    l_wait_conv = .false._1
!
! - Get contact parameters
!
    algo_cont  = cfdisi(ds_contact%sdcont_defi, 'ALGO_CONT')
    algo_frot  = cfdisi(ds_contact%sdcont_defi, 'ALGO_FROT')
    l_gliss    = cfdisl(ds_contact%sdcont_defi, 'CONT_DISC_GLIS')
    model_ndim = cfdisi(ds_contact%sdcont_defi, 'NDIM' )
!
! - First geometric loop
!    
    l_first_geom = ds_contact%l_first_geom
!
! - Preparation of contact solving
!
    call cfprep(mesh, matr_asse, disp_iter, disp_cumu_inst, ds_contact)
!
! - Print
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ...... DEBUT DE REALISATION DU CALCUL'
    endif
!
! - Select algorithm
!
    if (algo_cont .eq. 4) then
        if (algo_frot .eq. 0) then
            call algocp(ds_measure, ds_contact%sdcont_solv, nume_dof, matr_asse)
        else if (algo_frot.eq.1) then
            call frogdp(ds_measure, ds_contact%sdcont_solv, nume_dof, matr_asse, resi_glob_rela)
        else
            ASSERT(.false.)
        endif
    else if (algo_cont.eq.1) then
        if (l_gliss) then
            call algogl(ds_measure, ds_contact%sdcont_defi, ds_contact%sdcont_solv,&
                        solver, matr_asse             , mesh                  ,&
                        ctccvg)
        else
            call algoco(ds_measure, ds_contact%sdcont_defi, ds_contact%sdcont_solv,&
                        solver, matr_asse             , mesh                  ,&
                        ctccvg)
        endif
    else if (algo_cont.eq.2) then
        if (algo_frot .eq. 0) then
            call algocg(ds_measure, ds_contact%sdcont_defi, ds_contact%sdcont_solv,&
                        solver, matr_asse             , ctccvg)
        else
            ASSERT(.false.)
        endif
    else if (algo_cont.eq.5) then
        if (algo_frot .eq. 0) then
            call algocl(ds_measure, ds_contact%sdcont_defi, ds_contact%sdcont_solv,&
                        solver, matr_asse             , mesh                  ,&
                        ctccvg, l_wait_conv)
        else if (algo_frot.eq.1) then
            call fropgd(ds_measure, ds_contact%sdcont_defi, ds_contact%sdcont_solv,&
                        solver, nume_dof              , matr_asse             ,&
                        mesh  , resi_glob_rela        , disp_cumu_inst        ,&
                        ctccvg, l_wait_conv)
        else if (algo_frot.eq.2) then
            if (model_ndim .eq. 2) then
                call fro2gd(ds_measure, ds_contact%sdcont_defi, ds_contact%sdcont_solv,&
                            solver, matr_asse             , mesh                  ,&
                            ctccvg)
            else if (model_ndim.eq.3) then
                call frolgd(ds_measure  , ds_contact%sdcont_defi, ds_contact%sdcont_solv,&
                            solver      , nume_dof              , matr_asse             ,&
                            mesh        , resi_glob_rela        , disp_cumu_inst        ,&
                            l_first_geom, ctccvg)
            else
                ASSERT(.false.)
            endif
        else
            ASSERT(.false.)
        endif
    else
        ASSERT(.false.)
    endif
!
! - Print
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ...... FIN DE REALISATION DU CALCUL'
    endif
!
! - Post-treatment
!
    call cfpost(mesh, disp_iter, ds_contact, ctccvg)
!
! - Set events
!
    if (iter_newt .eq. 0) then
        call cfeven('INI', ds_contact)
    endif
    call cfeven('FIN', ds_contact)
!
! - Print
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ... FIN DE LA RESOLUTION DU CONTACT'
    endif
!
! - Wait contact convergence
!
    ds_contact%l_wait_conv = l_wait_conv
    ASSERT(ctccvg.ge.0)
!
end subroutine
