subroutine dtmforc_rede(nl_ind, sd_dtm_, sd_nl_, buffdtm, buffnl,&
                        depl, fext )

    implicit none
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
!
! dtmforc_rede : Calculates the force/displacement localized force at 
!                the current step (t)
!
!       nl_ind           : nonlinearity index (for sd_nl access)
!       sd_dtm_, buffdtm : dtm data structure and its buffer
!       sd_nl_ , buffnl  : nl  data structure and its buffer
!       depl             : structural modal displacement at instant t
!       fext             : projected total non-linear force
!
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/dtmget.h"
#include "asterfort/fointe.h"
#include "asterfort/nlget.h"
#include "asterfort/nlsav.h"
#include "asterfort/vecini.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
!
!   -0.1- Input/output arguments
    integer               , intent(in)  :: nl_ind
    character(len=*)      , intent(in)  :: sd_dtm_
    character(len=*)      , intent(in)  :: sd_nl_
    integer     , pointer , intent(in)  :: buffdtm(:)
    integer     , pointer , intent(in)  :: buffnl (:)
    real(kind=8), pointer , intent(in)  :: depl   (:)
    real(kind=8), pointer , intent(out) :: fext   (:)
!
!   -0.2- Local variables
    character(len=8)  :: sd_dtm, sd_nl, fonc, comp
    integer           :: im, ier, icomp, nbmode, saredi
    integer           :: start, finish
    real(kind=8)      :: seuil, force
!
    integer         , pointer :: vindx(:) => null()
    real(kind=8)    , pointer :: dplred(:) => null()
    real(kind=8)    , pointer :: vint(:) => null()
    real(kind=8)    , pointer :: fext0(:)   => null()
!
!   0 - Initializations
    sd_dtm = sd_dtm_
    sd_nl  = sd_nl_
!
    call nlget(sd_nl, _INTERNAL_VARS      , vr=vint, buffer=buffnl)
    call nlget(sd_nl, _INTERNAL_VARS_INDEX, vi=vindx, buffer=buffnl)
    start  = vindx(nl_ind)
!
    call nlget (sd_nl , _FX_FONCT      , iocc = nl_ind, kscal=fonc, buffer=buffnl )
    call nlget (sd_nl , _CMP_NAME      , iocc = nl_ind, kscal=comp, buffer=buffnl )
    call dtmget(sd_dtm, _NB_MODES      , iscal= nbmode,             buffer=buffdtm)
    call nlget (sd_nl , _MODAL_DEPL_NO1, iocc = nl_ind, vr=dplred,  buffer=buffnl )
!
    if (comp(1:2) .eq. 'DX' ) icomp = 1
    if (comp(1:2) .eq. 'DY' ) icomp = 2
    if (comp(1:2) .eq. 'DZ' ) icomp = 3
    if (comp(1:3) .eq. 'DRX') icomp = 4
    if (comp(1:3) .eq. 'DRY') icomp = 5
    if (comp(1:3) .eq. 'DRZ') icomp = 6
!
!seuil : deplacement suivant icomp en repere physique
!        (equivalent tophys)
    seuil = 0.d0
    do im = 1, nbmode
        seuil = seuil + dplred((im-1)*6+icomp) * depl(im)
    end do
!
    saredi = 1
!
    call fointe('F ', fonc, 1, [comp], [seuil],&
                force, ier)

    if (abs(force) .le. r8prem()) saredi = 0
!
    AS_ALLOCATE(vr=fext0, size=nbmode)
    call vecini(nbmode, 0.d0, fext0)

!fext0 : force en repere generalise (equivalent togene)
    do im = 1, nbmode
        fext0(im) = fext0(im) + dplred((im-1)*6+icomp) * force
    end do

    do im = 1, nbmode
        fext(im) = fext(im) + fext0(im)
    end do
!
    AS_DEALLOCATE(vr=fext0)

! --------------------------------------------------------------------------------------------------
!   --- Internal variables, storage
!
    finish = vindx(nl_ind+1)
    ASSERT((finish-start).eq.NBVARINT_FXRL)

    vint(start  ) = seuil
    vint(start+1) = force
    vint(start+2) = 1.d0 * saredi


end subroutine
