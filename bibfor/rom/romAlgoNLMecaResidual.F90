subroutine romAlgoNLMecaResidual(v_fint, v_fext, ds_algorom, resi)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jelira.h"
#include "asterfort/rsexch.h"
#include "blas/ddot.h"
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
    real(kind=8), intent(in), pointer :: v_fint(:)
    real(kind=8), intent(in), pointer :: v_fext(:)
    type(ROM_DS_AlgoPara), intent(in) :: ds_algorom
    real(kind=8), intent(out) :: resi
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction - Solving non-linear problem MECHANICS
!
! Evaluate residuals in applying HYPER-REDUCTION
!
! --------------------------------------------------------------------------------------------------
!
! In  v_fint           : pointer to internal forces
! In  v_fint           : pointer to external forces
! In  ds_algorom       : datastructure for ROM parameters
! Out resi             : value for residual
!
! --------------------------------------------------------------------------------------------------
!
    type(ROM_DS_Empi) :: ds_empi
    aster_logical :: l_hrom
    character(len=8) :: base
    character(len=19) :: mode
    character(len=24) :: field_type
    integer :: i_equa, nb_equa, nb_mode, i_mode, iret
    real(kind=8) :: term
    real(kind=8), pointer :: v_mode(:)=> null()
    real(kind=8), pointer :: v_resi(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    resi = 0.d0
!
! - Get parameters
!
    l_hrom = ds_algorom%l_hrom
!
! - Get empirical basis
!
    if (l_hrom) then
        ds_empi = ds_algorom%ds_empi_rid
    else
        ds_empi = ds_algorom%ds_empi
    endif
    base       = ds_empi%base
    nb_equa    = ds_empi%nb_equa
    nb_mode    = ds_empi%nb_mode
    field_type = ds_empi%field_type
!
! - Compute equilibrium residual
!
    AS_ALLOCATE(vr=v_resi, size=nb_equa)
    do i_equa = 1, nb_equa
        v_resi(i_equa) = v_fint(i_equa) - v_fext(i_equa)
    enddo
!
! - Truncation of residual
!    
    if (l_hrom) then
        do i_equa = 1, nb_equa
            if (ds_algorom%v_equa_int(i_equa) .eq. 1) then
                v_resi(i_equa) = 0.d0
            endif
        enddo
    endif
!
! - Compute norm
!  
    do i_mode = 1, nb_mode
        call rsexch(' ', base, field_type, i_mode, mode, iret)
        call jeveuo(mode(1:19)//'.VALE', 'E', vr = v_mode)
        term = ddot(nb_equa, v_mode, 1, v_resi, 1)
        resi = max (resi, abs(term))
    enddo
!
! - Cleaning
!    
    AS_DEALLOCATE(vr=v_resi)
!
end subroutine
