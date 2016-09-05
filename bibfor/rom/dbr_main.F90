subroutine dbr_main(ds_para)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/dbr_calc_q.h"
#include "asterfort/dbr_calc_svd.h"
#include "asterfort/dbr_calc_sele.h"
#include "asterfort/dbr_calc_save.h"
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
    type(ROM_DS_ParaDBR), intent(in) :: ds_para
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Compute
!
! Main subroutine to compute empiric modes
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_para        : datastructure for parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_sing, nb_mode
    real(kind=8), pointer :: q(:) => null()
    real(kind=8), pointer :: v(:) => null()
    real(kind=8), pointer :: s(:) => null() 
!
! --------------------------------------------------------------------------------------------------
!
!
! - Create snapshots matrix Q
!    
    call dbr_calc_q(ds_para%ds_empi, ds_para%ds_snap, q)
!
! - Compute empiric modes by SVD
!
    call dbr_calc_svd(ds_para%ds_empi, ds_para%ds_snap, q, s, v, nb_sing)
!
! - Select empiric modes
!
    call dbr_calc_sele(ds_para, s, nb_sing, nb_mode)
!
! - Save empiric modes
! 
    call dbr_calc_save(ds_para%ds_empi, nb_mode, s, v)
!
! - Cleaning
!
    AS_DEALLOCATE(vr = q)
    AS_DEALLOCATE(vr = v)
    AS_DEALLOCATE(vr = s)
!
end subroutine
