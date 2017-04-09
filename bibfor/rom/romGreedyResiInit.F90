subroutine romGreedyResiInit(ds_para_rb)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "blas/zdotc.h"
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
    type(ROM_DS_ParaDBR_RB), intent(inout) :: ds_para_rb
!
! --------------------------------------------------------------------------------------------------
!
! Greedy algorithm
!
! Compute initial residual
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_para_rb       : datastructure for parameters (RB)
!
! --------------------------------------------------------------------------------------------------
!    
    integer :: ifm, niv
    integer :: nb_equa
    character(len=1) :: resi_type
    complex(kind=8), pointer :: v_2mbr_init(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM2_53')
    endif
!
! - Get parameters
!
    nb_equa   = ds_para_rb%solveDOM%syst_size
    resi_type = ds_para_rb%resi_type
!
! - Access to objects
!
    if (ds_para_rb%solveROM%syst_2mbr_type .eq. 'C') then
        call jeveuo(ds_para_rb%vect_2mbr_init(1:19)//'.VALE', 'L', vc = v_2mbr_init)
    else
        ASSERT(.false.)
    endif
!
! - Initial residual
!  
    if (resi_type .eq. 'C') then
        ds_para_rb%resi_refe = real(sqrt(real(zdotc(nb_equa, v_2mbr_init, 1, v_2mbr_init, 1))))
    else
        ASSERT(.false.)
    endif
!
! - Print norm of residual
!
    if (niv .ge. 2) then
        call utmess('I', 'ROM2_54', sr = ds_para_rb%resi_refe)
    endif
!
end subroutine
