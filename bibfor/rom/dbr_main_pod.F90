subroutine dbr_main_pod(nb_mode_maxi, ds_para_pod, field_iden, ds_empi)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/dbr_calcpod_q.h"
#include "asterfort/dbr_calcpod_svd.h"
#include "asterfort/dbr_calcpod_sele.h"
#include "asterfort/dbr_calcpod_save.h"
#include "asterfort/dbr_calcpod_redu.h"
#include "asterfort/romTableSave.h"
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
    integer, intent(in) :: nb_mode_maxi
    character(len=24), intent(in) :: field_iden
    type(ROM_DS_ParaDBR_POD), intent(in) :: ds_para_pod
    type(ROM_DS_Empi), intent(inout) :: ds_empi
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Compute
!
! Main subroutine to compute empiric modes - For POD methods
!
! --------------------------------------------------------------------------------------------------
!
! In  nb_mode_maxi     : maximum number of emprical modes
! In  field_iden       : identificator of field (name in results datastructure)
! In  ds_para_pod      : datastructure for parameters (POD)
! IO  ds_empi          : datastructure for empiric modes
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_sing, nb_mode, nb_snap_redu, nb_line_svd, i_snap
    real(kind=8), pointer :: q(:) => null()
    real(kind=8), pointer :: v(:) => null()
    real(kind=8), pointer :: s(:) => null() 
    real(kind=8), pointer :: v_gamma(:) => null()
    character(len=19) :: tabl_name
!
! --------------------------------------------------------------------------------------------------
!
    tabl_name    = ds_para_pod%tabl_name
    nb_snap_redu = ds_para_pod%ds_snap%nb_snap
    nb_line_svd  = 0
!
! - Create snapshots matrix Q
!    
    call dbr_calcpod_q(ds_empi, ds_para_pod%ds_snap, q)
!
! - Compute empiric modes by SVD
!
    call dbr_calcpod_svd(ds_empi, ds_para_pod%ds_snap, q, s, v, nb_sing, nb_line_svd)
!
! - Select empiric modes
!
    call dbr_calcpod_sele(nb_mode_maxi, ds_para_pod%tole_svd, s, nb_sing, nb_mode)
!
! - Save empiric modes
! 
    call dbr_calcpod_save(ds_empi, nb_mode, nb_snap_redu, field_iden, s, v)
!
! - Compute reduced coordinates
!
    call dbr_calcpod_redu(nb_snap_redu, nb_line_svd, q, v, nb_mode, v_gamma)
!
! - Save the reduced coordinates in a table
!
    do i_snap = 1, nb_snap_redu
        call romTableSave(tabl_name  , nb_mode, v_gamma   ,&
                          nume_snap_ = i_snap)
    end do
!
! - Cleaning
!
    AS_DEALLOCATE(vr = q)
    AS_DEALLOCATE(vr = v)
    AS_DEALLOCATE(vr = s)
    AS_DEALLOCATE(vr = v_gamma)
!
end subroutine
