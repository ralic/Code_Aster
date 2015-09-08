subroutine nmvcfo(type_comp, model    , mate     , cara_elem, compor,&
                  varc_refe, hval_incr, vect_elem)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/nmvarc_prep.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/nmvccc.h"
#include "asterfort/memare.h"
#include "asterfort/nmvcd2.h"
#include "asterfort/nmchex.h"
#include "asterfort/reajre.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=1), intent(in) :: type_comp
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: mate
    character(len=24), intent(in) :: varc_refe
    character(len=24), intent(in) :: cara_elem
    character(len=24), intent(in) :: compor
    character(len=19), intent(in) :: hval_incr(*)
    character(len=19), intent(in) :: vect_elem
!
! --------------------------------------------------------------------------------------------------
!
! Nonlinear mechanics (algorithm)
!
! Command variables - Vector for reference (residual evaluation)
!
! --------------------------------------------------------------------------------------------------
!
! In  type_comp      : type of computation
!                      '-' - Previous step
!                      '+' - Current step
! In  model          : name of model
! In  mate           : name of material characteristics (field)
! In  cara_elem      : name of elementary characteristics (field)
! In  varc_refe      : name of reference command variables vector
! In  compor         : name of comportment definition (field)
! In  hval_incr      : hat-variable for incremental values
! In  vect_elem      : name of elementary vectors
!
! --------------------------------------------------------------------------------------------------
!
    integer :: mxchin, mxchout, nbin, nbout
    parameter    (mxchout=2, mxchin=31)
    character(len=8) :: lpaout(mxchout), lpain(mxchin)
    character(len=19) :: lchout(mxchout), lchin(mxchin)
!
    aster_logical :: exis_temp, exis_hydr, exis_ptot, exis_sech, exis_epsa
    aster_logical :: exis_meta_zirc, exis_meta_acier, exis_meta, calc_meta
    character(len=19) :: sigm_prev, vari_prev, varc_prev, varc_curr
    integer :: iret
!
! --------------------------------------------------------------------------------------------------
!
!
! - Get fields from hat-variables - Begin of time step
!
    call nmchex(hval_incr, 'VALINC', 'SIGMOI', sigm_prev)
    call nmchex(hval_incr, 'VALINC', 'VARMOI', vari_prev)
    call nmchex(hval_incr, 'VALINC', 'COMMOI', varc_prev)
    call nmchex(hval_incr, 'VALINC', 'COMPLU', varc_curr)
!
! - Command variables affected
!
    call nmvcd2('HYDR'   , mate, exis_hydr)
    call nmvcd2('PTOT'   , mate, exis_ptot)
    call nmvcd2('SECH'   , mate, exis_sech)
    call nmvcd2('EPSA'   , mate, exis_epsa)
    call nmvcd2('M_ZIRC' , mate, exis_meta_zirc)
    call nmvcd2('M_ACIER', mate, exis_meta_acier)
    call nmvcd2('TEMP'   , mate, exis_temp)
    exis_meta = exis_temp .and. (exis_meta_zirc.or.exis_meta_acier)
    calc_meta = .false.
    if (exis_meta.and.type_comp.eq.'+') then
        calc_meta = .true.
    endif
!
! - Prepare elementary vectors
!
    call jeexin(vect_elem(1:19)//'.RELR', iret)
    if (iret .eq. 0) then
        call memare('V', vect_elem, model, mate, cara_elem,&
                    'CHAR_MECA')
    endif
    call jedetr(vect_elem(1:19)// '.RELR')
    call reajre(vect_elem, ' ', 'V')
!
! - Fields preparation of elementary vectors
!
    call nmvarc_prep(type_comp, model    , cara_elem, mate     , varc_refe,&
                     compor   , exis_temp, mxchin   , nbin     , lpain    ,&
                     lchin    , mxchout  , nbout    , lpaout   , lchout   ,&
                     sigm_prev, vari_prev, varc_prev, varc_curr)
!
! - Computation of elementary vectors
!
    call nmvccc(model    , nbin     , nbout    , lpain    , lchin    ,&
                lpaout   , lchout   , exis_temp, exis_hydr, exis_ptot,&
                exis_sech, exis_epsa, calc_meta, vect_elem)
!
end subroutine
