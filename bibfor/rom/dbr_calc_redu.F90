subroutine dbr_calc_redu(nb_snap, m, q, v, nb_mode, v_gamma)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
#include "asterfort/assert.h"
#include "blas/dgemm.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    integer              , intent(in)  :: nb_snap
    integer              , intent(in)  :: m
    real(kind=8), pointer, intent(in)  :: q(:)
    real(kind=8), pointer, intent(in)  :: v(:)
    integer              , intent(in)  :: nb_mode
    real(kind=8), pointer, intent(out) :: v_gamma(:)
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Compute
!
! Compute reduced coordinates
!
! --------------------------------------------------------------------------------------------------
!
! In  nb_snap          : number of snapshots for compute reduced coordinates
! In  q                : pointer to snapshots matrix
! In  m                : number of lines
! In  v                : singular vectors 
! In  nb_mode          : number of modes
! Out v_gamma          : pointer to reduced coordinates
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    real(kind=8), pointer :: v_pod(:) => null()
    integer :: ieq, i_mode
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM7_12')
    endif
!
! - Get parameters
!
    ASSERT(nb_snap .gt. 0)
!
! - Compute reduced coordinates
!
    AS_ALLOCATE(vr = v_pod  , size = m*nb_mode)
    AS_ALLOCATE(vr = v_gamma, size = nb_mode*nb_snap)
    do i_mode = 1, nb_mode
        do ieq = 1, m
            v_pod(ieq+m*(i_mode-1)) = v(ieq+m*(i_mode-1))
        enddo
    enddo
    call dgemm('T', 'N', nb_mode, nb_snap, m, 1.d0, v_pod, m, q, m, 0.d0, v_gamma, nb_mode)
    AS_DEALLOCATE(vr = v_pod)
!
end subroutine
