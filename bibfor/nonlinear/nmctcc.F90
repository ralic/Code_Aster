subroutine nmctcc(mesh      , model_, mate  , nume_inst, sddyna   ,&
                  sderro    , sdstat, sddisc, hval_incr, hval_algo,&
                  ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/infdbg.h"
#include "asterfort/mmbouc.h"
#include "asterfort/mm_cycl_flip.h"
#include "asterfort/mmstat.h"
#include "asterfort/nmcrel.h"
#include "asterfort/nmimck.h"
#include "asterfort/utmess.h"
#include "asterfort/xmmbca.h"
#include "asterfort/xmtbca.h"
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
    character(len=24), intent(in) :: model_
    character(len=24), intent(in) :: mate
    integer, intent(in) :: nume_inst
    character(len=19), intent(in) :: sddyna
    character(len=24), intent(in) :: sderro
    character(len=24), intent(in) :: sdstat
    character(len=19), intent(in) :: sddisc
    character(len=19), intent(in) :: hval_incr(*)
    character(len=19), intent(in) :: hval_algo(*)
    type(NL_DS_Contact), intent(inout) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue methods - Evaluate convergence for contact loop
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  mate             : name of material characteristics (field)
! In  nume_inst        : index of current time step
! In  sddyna           : dynamic parameters datastructure
! In  sderro           : datastructure for errors during algorithm
! In  sdstat           : datastructure for statistics
! In  sddisc           : datastructure for time discretization
! In  hval_incr        : hat-variable for incremental values fields
! In  hval_algo        : hat-variable for algorithms fields
! IO  ds_contact       : datastructure for contact management
! Out loop_cont_conv   : .true. if contact loop converged
! Out loop_cont_node   : number of contact state changing
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    aster_logical :: l_cont_xfem_gg, l_cont_cont, l_cont_xfem, l_frot, l_erro_cont
    integer :: nb_cont_poin, iter_cont_mult, iter_cont_maxi
    integer :: loop_cont_count
    character(len=8) :: model
    integer :: iter_newt
    aster_logical :: cycl_flip, loop_cont_conv
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECANONLINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ALGORITHME DES CONTRAINTES ACTIVES'
    endif
!
! - Initializations
!
    model          = model_(1:8)
    loop_cont_conv = .false.
    l_erro_cont    = .false.
    iter_newt      = -1
!
! - Get contact parameters
!
    l_cont_cont    = cfdisl(ds_contact%sdcont_defi, 'FORMUL_CONTINUE')
    l_cont_xfem    = cfdisl(ds_contact%sdcont_defi, 'FORMUL_XFEM')
    l_frot         = cfdisl(ds_contact%sdcont_defi, 'FROTTEMENT')
    l_cont_xfem_gg = cfdisl(ds_contact%sdcont_defi, 'CONT_XFEM_GG')
    nb_cont_poin   = cfdisi(ds_contact%sdcont_defi, 'NTPC')
    iter_cont_mult = cfdisi(ds_contact%sdcont_defi, 'ITER_CONT_MULT')
!
! - Compute convergence criterion
! 
    if (iter_cont_mult .eq. -1) then
        iter_cont_maxi = cfdisi(ds_contact%sdcont_defi, 'ITER_CONT_MAXI')
    else
        iter_cont_maxi = iter_cont_mult*nb_cont_poin
    endif
!
! - Management of contact loop
!
    if (l_cont_xfem) then
        if (l_cont_xfem_gg) then
            call xmtbca(mesh, hval_incr, ds_contact)
        else
            call xmmbca(mesh, model, mate, hval_incr, ds_contact)
        endif
    else if (l_cont_cont) then
        call mmstat(mesh  , iter_newt, nume_inst, sddyna    , sdstat,&
                    sddisc, hval_incr, hval_algo, ds_contact)
    else
        ASSERT(.false.)
    endif
!
! - Flip-flop: forced convergence
!
    if (l_cont_cont) then
        call mm_cycl_flip(ds_contact, cycl_flip)
        if (cycl_flip) then
            call mmbouc(ds_contact, 'Cont', 'Set_Convergence')
        endif
    endif
!
! - State of contact loop
!
    call mmbouc(ds_contact, 'Cont', 'Read_Counter'  , loop_cont_count)
    call mmbouc(ds_contact, 'Cont', 'Is_Convergence', loop_state_ = loop_cont_conv)
!
! - Convergence of contact loop
!
    if ((.not.loop_cont_conv) .and. (loop_cont_count .eq. iter_cont_maxi)) then
        if (l_frot .and. l_cont_xfem) then
! --------- XFEM+friction: forced convergence
            call utmess('A', 'CONTACT3_86')
            call mmbouc(ds_contact, 'Cont', 'Set_Convergence')
        else
            l_erro_cont    = .true.
            loop_cont_conv = .false.
        endif
    endif
!
! - Error management
!
    call nmcrel(sderro, 'ERRE_CTCC', l_erro_cont)
    if (loop_cont_conv) then
        call nmcrel(sderro, 'DIVE_FIXC', .false._1)
    else
        call nmcrel(sderro, 'DIVE_FIXC', .true._1)
    endif
!
end subroutine
