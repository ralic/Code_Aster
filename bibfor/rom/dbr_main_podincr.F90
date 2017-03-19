subroutine dbr_main_podincr(l_reuse, nb_mode_maxi, ds_para_pod, ds_empi)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
#include "asterfort/infniv.h"
#include "asterfort/dbr_calcpod_q.h"
#include "asterfort/dbr_pod_incr.h"
#include "asterfort/dbr_calcpod_save.h"
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
    aster_logical, intent(in) :: l_reuse
    integer, intent(in) :: nb_mode_maxi
    type(ROM_DS_ParaDBR_POD), intent(in) :: ds_para_pod
    type(ROM_DS_Empi), intent(inout) :: ds_empi
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Compute
!
! Main subroutine to compute empiric modes - Incremental POD method
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_para        : datastructure for parameters
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8), pointer :: q(:) => null()
    real(kind=8), pointer :: v(:) => null()
    real(kind=8), pointer :: s(:) => null()
    integer :: nb_mode, nb_snap_redu
!
! --------------------------------------------------------------------------------------------------
!

!
! - Create snapshots matrix Q
!    
    call dbr_calcpod_q(ds_empi, ds_para_pod%ds_snap, q)
!
! - Incremental POD method
!
    call dbr_pod_incr(l_reuse, nb_mode_maxi, ds_empi, ds_para_pod,&
                      q, s, v, nb_mode, nb_snap_redu)
!
! - Save empiric base
!
    call dbr_calcpod_save(ds_empi, nb_mode, nb_snap_redu, s, v)
!
! - Cleaning
!
    AS_DEALLOCATE(vr = q)
    AS_DEALLOCATE(vr = v)
    AS_DEALLOCATE(vr = s)
!
end subroutine
