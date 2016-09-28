subroutine dbr_calc_svd(ds_empi, ds_snap, q, s, v, nb_sing)
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
    type(ROM_DS_Empi), intent(in) :: ds_empi
    type(ROM_DS_Snap), intent(in) :: ds_snap
    real(kind=8), pointer, intent(inout) :: q(:)
    real(kind=8), intent(out), pointer :: v(:)
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
! In  ds_empi          : datastructure for empiric modes
! In  ds_snap          : datastructure for snapshot selection
! In  q                : pointer to [q] matrix
! Out s                : singular values 
! Out v                : singular vectors 
! Out nb_sing          : total number of singular values
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: m, n
    integer :: nb_equa, nb_slice, nb_snap, nb_cmp
    integer :: lda, lwork
    character(len=8)  :: base_type
    real(kind=8), pointer :: w(:) => null()
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
! - Init
!
    nb_sing      = 0
    v            => null()
    s            => null()
!
! - Get parameters
!
    nb_snap      = ds_snap%nb_snap
    base_type    = ds_empi%base_type
    nb_slice     = ds_empi%ds_lineic%nb_slice
    nb_equa      = ds_empi%nb_equa
    nb_cmp       = ds_empi%nb_cmp
!
! - Prepare parameters for LAPACK
!
    if (base_type .eq. 'LINEIQUE') then
        m      = nb_equa/nb_slice
        n      = nb_slice*nb_snap        
    else
        m      = nb_equa
        n      = nb_snap
    endif
    lda     = max(1, m)
    nb_sing = min(m, n)
    lwork   = max(1,3*nb_sing+lda,5*nb_sing)
    AS_ALLOCATE(vr = v, size = m*nb_sing)
    AS_ALLOCATE(vr = s, size = nb_sing)
    AS_ALLOCATE(vr = work, size = lwork)
!
! - Compute SVD: Q = V S Wt
!
    call dgesvd('S', 'N', m, n, q,&
                lda, s, v, m, w,&
                1, work, lwork, info)
    if (info .ne. 0) then
        call utmess('F', 'ROM5_8')
    endif
!
! - Clean
!
    AS_DEALLOCATE(vr = work)
!
end subroutine
