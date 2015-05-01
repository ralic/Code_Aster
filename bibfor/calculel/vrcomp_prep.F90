subroutine vrcomp_prep(vari, vari_r,&
                       compor_curr, compor_curr_r,&
                       compor_prev, compor_prev_r)
!
implicit none
!
#include "asterfort/carces.h"
#include "asterfort/cesred.h"
#include "asterfort/cestas.h"
#include "asterfort/celces.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/utmess.h"
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
!
    character(len=*), intent(in) :: vari
    character(len=19), intent(out) :: vari_r
    character(len=*), intent(in)  :: compor_curr
    character(len=19), intent(out) :: compor_curr_r
    character(len=*), intent(in)  :: compor_prev
    character(len=19), intent(out) :: compor_prev_r
!
! --------------------------------------------------------------------------------------------------
!
! Check compatibility of comportments
!
! Prepare fields
!
! --------------------------------------------------------------------------------------------------
!
! In  vari          : internal variable
! Out vari_r        : reduced field for internal variable
! In  compor_curr   : current comportment
! Out compor_curr_r : reduced field for current comportment
! In  compor_prev   : previous comportment
! Out compor_prev_r : reduced field for previous comportment
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: coto
    integer :: iret
!
! --------------------------------------------------------------------------------------------------
!
    coto          = '&&VRCOMP.COTO'
    vari_r        = '&&VRCOMP.VARI_R'
    compor_curr_r = '&&VRCOMP.COPP'
!
! - Create reduced CARTE on current comportement
!
    call carces(compor_curr, 'ELEM', ' ', 'V', coto,&
                'A', iret)
    call cesred(coto, 0, [0], 1, 'RELCOM',&
                'V', compor_curr_r)
    call detrsd('CHAM_ELEM_S', coto)
!
! - Create reduced field for internal variables
!
    call celces(vari, 'V', vari_r)
    call cestas(vari_r)
!
! - Create reduced CARTE on previous comportement
!
    if (compor_prev.eq.' ') then
        compor_prev_r = ' '
    else
        compor_prev_r = '&&VRCOMP.COPM'
        call carces(compor_prev, 'ELEM', ' ', 'V', coto,&
                    'A', iret)
        call cesred(coto, 0, [0], 1, 'RELCOM',&
                    'V', compor_prev_r)
        call detrsd('CHAM_ELEM_S', coto)
    endif
!
end subroutine
