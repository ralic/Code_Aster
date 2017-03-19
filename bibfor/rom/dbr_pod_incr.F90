subroutine dbr_pod_incr(l_reuse, nb_mode_maxi, ds_empi, ds_para_pod,&
                        q, s, v, nb_mode, nb_snap_redu)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/norm_frobenius.h"
#include "asterfort/dbr_calcpod_svd2.h"
#include "asterfort/dbr_calcpod_sele.h"
#include "asterfort/romTableSave.h"
#include "asterfort/romTableCreate.h"
#include "asterfort/romBaseRead.h"
#include "asterfort/ltnotb.h"
#include "asterfort/tbexve.h"
#include "asterfort/jeveuo.h"
#include "asterfort/detrsd.h"
#include "asterfort/rsexch.h"
#include "blas/dgemm.h"
#include "blas/dgesv.h"
#include "blas/dgesvd.h"
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
    aster_logical, intent(in) :: l_reuse
    integer, intent(in) :: nb_mode_maxi
    type(ROM_DS_Empi), intent(inout) :: ds_empi
    type(ROM_DS_ParaDBR_POD) , intent(in) :: ds_para_pod
    real(kind=8), pointer, intent(inout) :: q(:)
    real(kind=8), pointer, intent(out)   :: s(:)
    real(kind=8), pointer, intent(out)   :: v(:)
    integer, intent(out) :: nb_mode
    integer, intent(out) :: nb_snap_redu
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Compute
!
! Incremental POD method
!
! --------------------------------------------------------------------------------------------------
!  
! In  l_reuse          : .true. if reuse
! In  nb_mode_maxi     : maximum number of emprical modes
! IO  ds_empi          : datastructure for empiric modes
! In  ds_para_pod      : datastructure for parameters (POD)
! IO  q                : pointer to snapshots matrix (be modified after SVD)
! Out s                : singular values 
! Out v                : singular vectors 
! Out nb_mode          : number of modes selected
! Out nb_snap_redu     : number of snapshots used in incremental algorithm
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: incr_ini, incr_end, i_equa, i_snap, p, i_incr, k, i_mode
    integer :: nb_equa, nb_snap, nb_sing
    real(kind=8) :: tole_incr, tole_svd
    character(len=8)  :: base_type, base
    real(kind=8) :: norm_q, norm_r
    integer(kind=4) :: info
    real(kind=8), pointer :: qi(:)   => null()
    real(kind=8), pointer :: ri(:)   => null()
    real(kind=8), pointer :: rt(:)   => null()
    real(kind=8), pointer :: vt(:)   => null()
    real(kind=8), pointer :: g(:)    => null()
    real(kind=8), pointer :: gt(:)   => null()
    real(kind=8), pointer :: kv(:)   => null()
    real(kind=8), pointer :: kt(:)   => null()
    integer(kind=4), pointer :: IPIV(:) => null()
    real(kind=8), pointer :: b(:)    => null()
    real(kind=8), pointer :: v_gamma(:)    => null()
    character(len=19) :: tabl_name, tabl_name_r
    character(len=24) :: typval
    integer :: nbval, iret
    real(kind=8), pointer :: v_gm(:) => null()
    character(len=24) :: mode = '&&IPOD_MODE'
    real(kind=8), pointer :: v_mode(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
!
! - Get parameters
!
    nb_equa      = ds_empi%nb_equa
    base         = ds_empi%base
    tabl_name    = ds_para_pod%tabl_name
    base_type    = ds_para_pod%base_type
    nb_snap      = ds_para_pod%ds_snap%nb_snap
    tole_incr    = ds_para_pod%tole_incr
    tole_svd     = ds_para_pod%tole_svd
    ASSERT(base_type .eq. '3D')
!
! - Allocate objects
!
    nb_snap      = nb_snap
    AS_ALLOCATE(vr = qi, size = nb_equa)
    AS_ALLOCATE(vr = ri, size = nb_equa)
    AS_ALLOCATE(vr = rt, size = nb_equa)
    if (l_reuse) then
        call romBaseRead(base, ds_empi)
        call ltnotb(base, 'COOR_REDUIT', tabl_name_r)
        call tbexve(tabl_name_r, 'COOR_REDUIT', '&&COORHR', 'V', nbval, typval)
        call jeveuo('&&COORHR', 'E', vr = v_gm)
        AS_ALLOCATE(vr = vt, size = nb_equa*(nb_snap+ds_empi%nb_mode))
        AS_ALLOCATE(vr = gt, size = (nb_snap+ds_empi%nb_mode)*(nb_snap+ds_empi%nb_snap))
        AS_ALLOCATE(vr = g , size = (nb_snap+ds_empi%nb_mode)*(nb_snap+ds_empi%nb_snap))
    else
        AS_ALLOCATE(vr = vt, size = nb_equa*nb_snap)
        AS_ALLOCATE(vr = gt, size = nb_snap*nb_snap)
        AS_ALLOCATE(vr = g , size = nb_snap*nb_snap)
    endif
!
! - Initialize algorithm
!
    if (l_reuse) then
        do i_mode = 1, ds_empi%nb_mode
            call rsexch(' ', ds_empi%base, ds_empi%field_type, i_mode, mode, iret)
            call jeveuo(mode(1:19)//'.VALE', 'E', vr = v_mode)
            do i_equa = 1, nb_equa
                vt(i_equa+nb_equa*(i_mode-1)) = v_mode(i_equa)
            end do
        enddo
        do i_equa = 1, ds_empi%nb_mode*ds_empi%nb_snap
            gt(i_equa) = v_gm(i_equa)
        enddo
        call detrsd('TABLE', tabl_name_r)
    else
        qi(1:nb_equa) = q(1:nb_equa)
        call norm_frobenius(nb_equa, qi, norm_q)
        vt(1:nb_equa) = qi(1:nb_equa)/norm_q
        gt(1)   = norm_q
    endif
    if (l_reuse) then
        p        = ds_empi%nb_mode
        incr_ini = ds_empi%nb_snap+1
        incr_end = nb_snap+ds_empi%nb_snap
    else
        p        = 1
        incr_ini = 2
        incr_end = nb_snap
    endif
!
! - Main algorithm
!
    do i_incr = incr_ini, incr_end
        if (l_reuse) then
            do i_equa = 1, nb_equa
                qi(i_equa) = q(i_equa+nb_equa*(i_incr-ds_empi%nb_snap-1))
            enddo
        else
            do i_equa = 1, nb_equa
                qi(i_equa) = q(i_equa+nb_equa*(i_incr-1))
            enddo
        endif
        call norm_frobenius(nb_equa, qi, norm_q)
        AS_ALLOCATE(vr  = kv  , size = p*p)
        AS_ALLOCATE(vr  = kt  , size = p)
        AS_ALLOCATE(vi4 = IPIV, size = p)
        call dgemm('T', 'N', p, p, nb_equa, 1.d0, vt, nb_equa, vt, nb_equa, 0.d0, kv, p)
        call dgemm('T', 'N', p, 1, nb_equa, 1.d0, vt, nb_equa, qi, nb_equa, 0.d0, kt, p)
        call dgesv(p, 1, kv, p, IPIV, kt, p, info)
        call dgemm('N', 'N', nb_equa, 1, p, 1.d0, vt, nb_equa, kt, p, 0.d0, rt, nb_equa)
        ri = qi - rt
        call norm_frobenius(nb_equa, ri, norm_r)
        if (norm_r/norm_q .ge. tole_incr) then
            do i_equa = 1, nb_equa
                vt(i_equa+nb_equa*p) = ri(i_equa)/norm_r
            enddo
            do i_snap = 1, p
                g(i_snap+(p+1)*(i_incr-1)) = kt(i_snap)
                do k = 1, i_incr-1
                    g(i_snap+(p+1)*(k-1))= gt(i_snap+p*(k-1))
                enddo
            enddo
            do k = 1, i_incr-1
                g((p+1)*k)= 0.d0
            enddo
            p = p +1
            g(p*i_incr)=norm_r
            do k = 1, p*i_incr
                gt(k) = g(k)
            enddo
        else
            do i_snap = 1, p
                gt(i_snap+p*(i_incr-1)) = kt(i_snap)
            enddo
        endif
        AS_DEALLOCATE(vr = kt)
        AS_DEALLOCATE(vr = kv)
        AS_DEALLOCATE(vi4 = IPIV)
    enddo
    AS_DEALLOCATE(vr = qi)
    AS_DEALLOCATE(vr = ri)
    AS_DEALLOCATE(vr = rt)
!
! - Prepare matrix
!
    do i_equa = 1, p*incr_end
        g(i_equa) = gt(i_equa)
    end do
!
! - Compute SVD: Q = V S Wt
!
    call dbr_calcpod_svd2(p, incr_end, g, s, b, nb_sing)
!
! - Select empiric modes
!
    call dbr_calcpod_sele(nb_mode_maxi, tole_svd, s, nb_sing, nb_mode)
!
! - Compute matrix V
!
    AS_ALLOCATE(vr = v, size = nb_equa*nb_mode)
    call dgemm('N', 'N', nb_equa, nb_mode, p, 1.d0, vt, nb_equa, b, p, 0.d0, v, nb_equa)
!
! - Compute reduced coordinates
!
    AS_ALLOCATE(vr = v_gamma, size = nb_mode*incr_end)
    call dgemm('T', 'N', nb_mode, incr_end, p, 1.d0, b, p, gt, p, 0.d0, v_gamma, nb_mode)
!
! - Save the reduced coordinates in a table
!
    if (l_reuse) then
        call romTableCreate(base, tabl_name)
    endif
    do i_snap = 1, incr_end
        call romTableSave(tabl_name  , nb_mode, v_gamma   ,&
                          nume_snap_ = i_snap)
    end do
!
! - Number of snapshots in empiric base
!
    nb_snap_redu = incr_end
    call utmess('I', 'ROM7_14', si = nb_snap_redu)
!
! - Clean
!
    AS_DEALLOCATE(vr = v_gamma)
    AS_DEALLOCATE(vr = vt)
    AS_DEALLOCATE(vr = gt)
    AS_DEALLOCATE(vr = g)
    AS_DEALLOCATE(vr = b)
!
end subroutine
