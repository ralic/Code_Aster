subroutine dbr_calcpod_svd2(p, incr_end, g, s, b, nb_sing)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
#include "blas/dgesvd.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
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
    integer, intent(in) :: p
    integer, intent(in) :: incr_end
    real(kind=8), pointer, intent(in) :: g(:)
    real(kind=8), intent(out), pointer :: b(:)
    real(kind=8), intent(out), pointer :: s(:)
    integer, intent(out) :: nb_sing
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Compute
!
! Compute empiric modes by SVD
!
! --------------------------------------------------------------------------------------------------
!
! In  incr_end         : total number of snapshots
! In  p                : number of snapshots computed (<= total number of snaps)
! In  g                : pointer to [g] matrix
! Out s                : singular values 
! Out b                : singular vectors
! Out nb_sing          : total number of singular values
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: lda, lwork
    real(kind=8), pointer :: w(:)    => null()
    real(kind=8), pointer :: work(:) => null()
    integer(kind=4) :: info
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_7')
    endif
!
! - Prepare parameters for LAPACK
!
    lda     = max(1, p)
    nb_sing = min(p, incr_end)
    lwork   = max(1,3*nb_sing+lda,5*nb_sing)
    AS_ALLOCATE(vr = b, size = p*nb_sing)
    AS_ALLOCATE(vr = s, size = nb_sing)
    AS_ALLOCATE(vr = work, size = lwork)
!
! - Compute SVD: Q = V S Wt
!
    call dgesvd('S', 'N', p, incr_end, g,&
                lda, s, b, p, w,&
                1, work, lwork, info)
    if (info .ne. 0) then
        call utmess('F', 'ROM5_8')
    endif
    if (niv .ge. 2) then
        call utmess('I', 'ROM7_10', si = 8*lwork)
    endif
!
! - Clean
!
    AS_DEALLOCATE(vr = work)
!
end subroutine
