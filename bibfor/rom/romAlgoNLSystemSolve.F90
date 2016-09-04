subroutine romAlgoNLSystemSolve(matr_asse, vect_2mbr, ds_algorom, vect_solu)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/infniv.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mgauss.h"
#include "asterfort/mrmult.h"
#include "asterfort/jelira.h"
#include "asterfort/rsexch.h"
#include "asterfort/vtaxpy.h"
#include "asterfort/vtzero.h"
#include "asterfort/utmess.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=24), intent(in) :: matr_asse
    character(len=24), intent(in) :: vect_2mbr
    type(ROM_DS_AlgoPara), intent(in) :: ds_algorom
    character(len=19), intent(in) :: vect_solu
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction - Solving non-linear problem
!
! Solve reduced system
!
! --------------------------------------------------------------------------------------------------
!
! In  matr_asse        : matrix
! In  vect_2mbr        : second member
! In  ds_algorom       : datastructure for ROM parameters
! In  vect_solu        : solution
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=24) :: gamma = ' ', field_type = ' '
    real(kind=8), pointer :: v_gamma(:) => null()
    real(kind=8), pointer :: v_vect_2mbr(:) => null()
    integer :: nb_equa_2mbr, nb_equa_matr, nb_equa, nb_mode
    integer :: i_mode, j_mode, i_equa
    integer :: jv_matr, iret
    aster_logical :: l_hrom, l_rom
    character(len=8) :: base
    character(len=19) :: mode
    real(kind=8) :: term1, term2, det, term
    real(kind=8), pointer :: v_matr_rom(:) => null()
    real(kind=8), pointer :: v_vect_rom(:) => null()
    real(kind=8), pointer :: v_mrmult(:) => null()
    real(kind=8), pointer :: v_mode(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_40')
    endif
!
! - Get parameters
!
    l_rom       = ds_algorom%l_rom
    l_hrom      = ds_algorom%l_hrom
    gamma       = ds_algorom%gamma
    ASSERT(l_rom)
!
! - Access to reduced coordinates
!
    call jeveuo(gamma, 'E', vr = v_gamma)
!
! - Access to second member
!
    call jeveuo(vect_2mbr(1:19)//'.VALE', 'E'     , vr = v_vect_2mbr)
    call jelira(vect_2mbr(1:19)//'.VALE', 'LONMAX', nb_equa_2mbr)
!
! - Access to matrix
!
    call jeveuo(matr_asse(1:19)//'.&INT', 'L', jv_matr)
    call dismoi('NB_EQUA', matr_asse, 'MATR_ASSE', repi = nb_equa_matr)
!
! - Select empiric base (RID or not ? )
!
    if (l_hrom) then
        base       = ds_algorom%ds_empi_rid%base
        nb_mode    = ds_algorom%ds_empi_rid%nb_mode
        nb_equa    = ds_algorom%ds_empi_rid%nb_equa
        field_type = ds_algorom%ds_empi_rid%field_type
    else
        base       = ds_algorom%ds_empi%base
        nb_mode    = ds_algorom%ds_empi%nb_mode
        nb_equa    = ds_algorom%ds_empi%nb_equa
        field_type = ds_algorom%ds_empi%field_type
    endif
    ASSERT(nb_equa .eq. nb_equa_2mbr)
    ASSERT(nb_equa .eq. nb_equa_matr)
!
! - Truncation of second member
!    
    if (l_hrom) then
        do i_equa = 1, nb_equa
            if (ds_algorom%v_equa_int(i_equa) .eq. 1) then
                v_vect_2mbr(i_equa) = 0.d0
            endif
        enddo
    endif
!
! - Allocate objects
!
    AS_ALLOCATE(vr = v_matr_rom, size = nb_mode*nb_mode)
    AS_ALLOCATE(vr = v_vect_rom, size = nb_mode)
    AS_ALLOCATE(vr = v_mrmult  , size = nb_equa)
!
! - Compute reduced objects
!
    do i_mode = 1, nb_mode
        call rsexch(' ', base, field_type, i_mode, mode, iret)
        call jeveuo(mode(1:19)//'.VALE', 'L', vr = v_mode)
        term1 = ddot(nb_equa, v_mode, 1, v_vect_2mbr, 1)
        v_vect_rom(i_mode) = term1
        call mrmult('ZERO', jv_matr, v_mode, v_mrmult, 1, .false._1)
        if (l_hrom) then
            do i_equa = 1, nb_equa
                if (ds_algorom%v_equa_int(i_equa) .eq. 1) then
                    v_mrmult(i_equa) = 0.d0
                endif
            end do
        endif
        do j_mode = 1, nb_mode
            call rsexch(' ', base, field_type, j_mode, mode, iret)
            call jeveuo(mode(1:19)//'.VALE', 'L', vr = v_mode)
            term2 = ddot(nb_equa, v_mode, 1, v_mrmult, 1)
            v_matr_rom(nb_mode*(i_mode-1)+j_mode) = term2
        end do 
    end do
!
! - Solve system
!    
    call mgauss('NFSP', v_matr_rom, v_vect_rom, nb_mode, nb_mode, 1, det, iret)
    call jeveuo(gamma, 'E', vr = v_gamma)
    v_gamma = v_gamma + v_vect_rom
!
! - Project in physical space
!    
    call vtzero(vect_solu)
    do i_mode = 1 , nb_mode
        term = v_vect_rom(i_mode)
        call rsexch(' ', base, field_type, i_mode, mode, iret)
        call vtaxpy(term, mode, vect_solu)
    enddo
!
! - Clean
!
    AS_DEALLOCATE(vr = v_matr_rom)
    AS_DEALLOCATE(vr = v_vect_rom)
    AS_DEALLOCATE(vr = v_mrmult  )
!
end subroutine
