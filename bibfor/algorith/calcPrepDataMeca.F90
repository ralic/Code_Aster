subroutine calcPrepDataMeca(model          , mate          , cara_elem,&
                            disp_prev      , disp_cumu_inst, vari_prev, sigm_prev,&
                            time_prev      , time_curr     ,&
                            ds_constitutive, varc_refe     ,&
                            hval_incr      , hval_algo     ,&
                            merigi         , vediri        , vefint   , veforc   ,&
                            vevarc_prev    , vevarc_curr   )
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/nmchai.h"
#include "asterfort/nmch1p.h"
#include "asterfort/nmch2p.h"
#include "asterfort/dismoi.h"
#include "asterfort/nmcha0.h"
#include "asterfort/nmchex.h"
#include "asterfort/mvnume.h"
#include "asterfort/chpver.h"
#include "asterfort/nmvcle.h"
#include "asterfort/jeexin.h"
#include "asterfort/vrcomp.h"
#include "asterfort/utmess.h"
#include "asterfort/gcncon.h"
#include "asterfort/nmvcre.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: mate
    character(len=24), intent(in) :: cara_elem
    character(len=19), intent(in) :: disp_prev
    character(len=19), intent(in) :: disp_cumu_inst
    character(len=19), intent(in) :: vari_prev
    character(len=19), intent(in) :: sigm_prev
    real(kind=8), intent(in) :: time_prev
    real(kind=8), intent(in) :: time_curr
    type(NL_DS_Constitutive), intent(in) :: ds_constitutive
    character(len=24), intent(out) :: varc_refe
    character(len=19), intent(out) :: hval_incr(:)
    character(len=19), intent(out) :: hval_algo(:)
    character(len=19), intent(out) :: merigi
    character(len=19), intent(out) :: vediri
    character(len=19), intent(out) :: vefint
    character(len=19), intent(out) :: veforc
    character(len=19), intent(out) :: vevarc_prev
    character(len=19), intent(out) :: vevarc_curr
!
! --------------------------------------------------------------------------------------------------
!
! Command CALCUL
!
! Prepare data for mechanics
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of model
! In  mate             : name of material characteristics (field)
! In  cara_elem        : name of elementary characteristics (field)
! In  disp_prev        : displacement at beginning of current step
! In  disp_cumu_inst   : displacement increment from beginning of step
! In  vari_prev        : internal variables at beginning of step
! In  sigm_prev        : stress at beginning of step
! In  time_prev        : time at beginning of step
! In  time_curr        : time at end of step
! In  ds_constitutive  : datastructure for constitutive laws management
! Out varc_refe        : name of reference command variables vector
! Out hval_incr        : hat-variable for incremental values fields
! Out hval_algo        : hat-variable for algorithms fields
! Out merigi           : name of elementary for tangent matrix
! Out vediri           : name of elementary for reaction (Lagrange) vector
! Out vefint           : name of elementary for internal forces vector (RAPH_MECA)
! Out veforc           : name of elementary for forces vector (FORC_NODA)
! Out vevarc_prev      : name of elementary for external state variables at beginning of step
! Out vevarc_curr      : name of elementary for external state variables at end of step
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret
    character(len=19) :: disp_curr, varc_prev, varc_curr, sigm_curr, vari_curr, ligrmo
!
! --------------------------------------------------------------------------------------------------
!
    varc_refe = '&&OP0026.COMREF'
!
! - Create "hat-variables"
!
    call nmch1p(hval_incr)
    call nmch2p(hval_algo)
!
! - Get LIGREL
!
    call dismoi('NOM_LIGREL', model, 'MODELE', repk=ligrmo)
!
! - Put displacements in "hat-variables"
!
    call nmcha0('VALINC', 'DEPMOI', disp_prev     , hval_incr)
    call nmcha0('SOLALG', 'DEPDEL', disp_cumu_inst, hval_algo)
!
! - Compute current displacements
!
    if (disp_prev .ne. ' ') then
        call nmchex(hval_incr, 'VALINC', 'DEPPLU', disp_curr)
        call mvnume(disp_prev, disp_cumu_inst, disp_curr)
    endif
!
! - Check and put stress in "hat-variables"
!
    if (sigm_prev .ne. ' ') then
        call chpver('F', sigm_prev, 'ELGA', 'SIEF_R', iret)
        call nmcha0('VALINC', 'SIGMOI', sigm_prev, hval_incr)
    endif
!
! - Check and put internal variables in "hat-variables"
!
    if (vari_prev .ne. ' ') then
        call chpver('F', vari_prev, 'ELGA', 'VARI_R', iret)
        call nmcha0('VALINC', 'VARMOI', vari_prev, hval_incr)
    endif
!
! - Get command variables
!
    call nmchex(hval_incr, 'VALINC', 'COMMOI', varc_prev)
    call nmchex(hval_incr, 'VALINC', 'COMPLU', varc_curr)
!
! - Prepare command variables
!
    call nmvcle(model, mate, cara_elem, time_curr, varc_curr)
    call nmvcle(model, mate, cara_elem, time_prev, varc_prev)
    call nmvcre(model, mate, cara_elem, varc_refe)
!
! - Checking number of internal variables
!
    call jeexin(ds_constitutive%compor(1:19)//'.CESD', iret)
    if (iret .gt. 0 .and. vari_prev .ne. ' ') then
        call vrcomp(ds_constitutive%compor, vari_prev, ligrmo, iret)
        if (iret .eq. 1) then
            call utmess('F', 'CALCUL1_5')
        endif
    endif
!
! - Datastructures name (automatic génération)
!
    call gcncon('_', sigm_curr)
    call gcncon('_', vari_curr)
    call gcncon('_', merigi)
    call gcncon('_', vefint)
    call gcncon('_', vediri)
    call gcncon('_', veforc)
    call gcncon('_', vevarc_prev)
    call gcncon('_', vevarc_curr)
    call gcncon('_', ds_constitutive%comp_error)
!
! - Changeing names of variables
!
    call nmcha0('VALINC', 'SIGPLU', sigm_curr, hval_incr)
    call nmcha0('VALINC', 'VARPLU', vari_curr, hval_incr)
!
end subroutine
