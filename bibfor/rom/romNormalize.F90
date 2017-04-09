subroutine romNormalize(vect_type, vect_vale, nb_equa)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "blas/zdotc.h"
#include "asterfort/jeveuo.h"
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
    character(len=1), intent(in) :: vect_type
    character(len=19), intent(in) :: vect_vale
    integer, intent(in) :: nb_equa
!
! --------------------------------------------------------------------------------------------------
!
! Greedy algorithm
!
! Normalization of vector
!
! --------------------------------------------------------------------------------------------------
!
! In  vect_type        : type of vector (real or complex)
! In  vect_vale        : name of vector to normalize
! In  nb_equa          : number of equations
!
! --------------------------------------------------------------------------------------------------
!
    complex(kind=8), pointer :: v_valec(:) => null()
    complex(kind=8) :: normc
!
! --------------------------------------------------------------------------------------------------
!
    if (vect_type .eq. 'C') then
        call jeveuo(vect_vale(1:19)//'.VALE', 'E', vc = v_valec)
        normc = zdotc(nb_equa, v_valec, 1, v_valec, 1)
        v_valec(1:nb_equa) = v_valec(1:nb_equa) / normc
    else
        ASSERT(.false.)
    endif
!
end subroutine
