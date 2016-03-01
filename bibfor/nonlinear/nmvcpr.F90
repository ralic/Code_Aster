subroutine nmvcpr(modelz   , mate , cara_elem      , varc_refe      , compor   ,&
                  hval_incr, base_, vect_elem_curr_, vect_elem_prev_, nume_dof_,&
                  cnvcpr_)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/assvec.h"
#include "asterfort/nmvarc_prep.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/memare.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmvccc.h"
#include "asterfort/nmvcd2.h"
#include "asterfort/reajre.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=*), intent(in) :: modelz
    character(len=24), intent(in) :: mate
    character(len=24), intent(in) :: varc_refe
    character(len=24), intent(in) :: cara_elem
    character(len=24), intent(in) :: compor
    character(len=19), intent(in) :: hval_incr(*)
    character(len=1), optional, intent(in) :: base_
    character(len=*), optional, intent(in) :: vect_elem_curr_
    character(len=*), optional, intent(in) :: vect_elem_prev_
    character(len=24), optional, intent(in) :: nume_dof_
    character(len=24), optional, intent(in) :: cnvcpr_
!
! --------------------------------------------------------------------------------------------------
!
! Nonlinear mechanics (algorithm)
!
! Command variables - Second member for prediction
!
! --------------------------------------------------------------------------------------------------
!
! In  model          : name of model
! In  mate           : name of material characteristics (field)
! In  cara_elem      : name of elementary characteristics (field)
! In  varc_refe      : name of reference command variables vector
! In  compor         : name of comportment definition (field)
! In  hval_incr      : hat-variable for incremental values
! In  base           : JEVEUX base to create objects
! In  nume_dof       : numbering of dof
! In  vect_elem_prev : elementary vector for previous command variables
! In  vect_elem_curr : elementary vector for current command variables
! In  cnvcpr         : name of second member for command variables
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
    real(kind=8) :: coef_vect(2)
    character(len=19) :: vect_elem(2)
    character(len=19) :: sigm_prev, vari_prev, varc_prev, varc_curr
    character(len=24) :: model
    integer :: iret
    character(len=1)  :: base
    character(len=19) :: vect_elem_curr, vect_elem_prev
    character(len=24) :: nume_dof, cnvcpr
!
! --------------------------------------------------------------------------------------------------
!
    model = modelz
    base  = 'V'
    if (present(base_)) then
        base = base_
    endif
    vect_elem_prev = '&&VEVCOM'
    if (present(vect_elem_prev_)) then
        vect_elem_prev = vect_elem_prev_
    endif 
    vect_elem_curr = '&&VEVCOP'
    if (present(vect_elem_curr_)) then
        vect_elem_curr = vect_elem_curr_
    endif
    if (present(cnvcpr_)) then
        cnvcpr   = cnvcpr_
        ASSERT(present(nume_dof_))
        nume_dof = nume_dof_
    endif

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
!
! - Prepare elementary vectors
!
    if (present(vect_elem_prev_)) then
        call jeexin(vect_elem_prev//'.RELR', iret)
        if (iret .eq. 0) then
            call memare(base, vect_elem_prev, model, mate, cara_elem,&
                        'CHAR_MECA')
        endif
        call jedetr(vect_elem_prev//'.RELR')
        call reajre(vect_elem_prev, ' ', base)
    endif
    if (present(vect_elem_curr_)) then
        call jeexin(vect_elem_curr//'.RELR', iret)
        if (iret .eq. 0) then
            call memare(base, vect_elem_curr, model, mate, cara_elem,&
                        'CHAR_MECA')
        endif
        call jedetr(vect_elem_curr//'.RELR')
        call reajre(vect_elem_curr, ' ', base)
    endif
!
! - Fields preparation of elementary vectors - Previous
!
    if (present(vect_elem_prev_)) then
        call nmvarc_prep('-'      , model    , cara_elem, mate     , varc_refe,&
                         compor   , exis_temp, mxchin   , nbin     , lpain    ,&
                         lchin    , mxchout  , nbout    , lpaout   , lchout   ,&
                         sigm_prev, vari_prev, varc_prev, varc_curr)
!
! - Computation of elementaty vectors - Previous
! - For metallurgy: already incremental
!
        calc_meta = .false.
        call nmvccc(model    , nbin     , nbout    , lpain    , lchin         ,&
                    lpaout   , lchout   , exis_temp, exis_hydr, exis_ptot     ,&
                    exis_sech, exis_epsa, calc_meta, base     , vect_elem_prev)
    endif
!
! - Fields preparation of elementary vectors - Current
!
    if (present(vect_elem_curr_)) then
        call nmvarc_prep('+'      , model    , cara_elem, mate     , varc_refe,&
                         compor   , exis_temp, mxchin   , nbin     , lpain    ,&
                         lchin    , mxchout  , nbout    , lpaout   , lchout   ,&
                         sigm_prev, vari_prev, varc_prev, varc_curr)
!
! - Computation of elementary vectors - Current
!
        calc_meta = exis_meta
        call nmvccc(model    , nbin     , nbout    , lpain    , lchin         ,&
                    lpaout   , lchout   , exis_temp, exis_hydr, exis_ptot     ,&
                    exis_sech, exis_epsa, calc_meta, base     , vect_elem_curr)
    endif
!
! - Assembling
!
    if (present(cnvcpr_)) then
        coef_vect(1) = +1.d0
        coef_vect(2) = -1.d0
        vect_elem(1) = vect_elem_curr
        vect_elem(2) = vect_elem_prev
        call assvec(base, cnvcpr, 2, vect_elem, coef_vect,&
                    nume_dof, ' ', 'ZERO', 1)
    endif
!
end subroutine
