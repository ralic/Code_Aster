subroutine romGreedyResiMaxi(ds_para_rb, i_coef_maxi)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8gaem.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
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
    type(ROM_DS_ParaDBR_RB), intent(in) :: ds_para_rb
    integer, intent(out) :: i_coef_maxi
!
! --------------------------------------------------------------------------------------------------
!
! Greedy algorithm
!
! Get maximum of residual
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_para_rb       : datastructure for parameters (RB)
! Out i_coef_maxi      : index where residual is maximum
!
! --------------------------------------------------------------------------------------------------
!    
    integer :: i_coef, nb_coef
    real(kind=8) :: vale_maxi
!
! --------------------------------------------------------------------------------------------------
!
    vale_maxi   = -r8gaem()
    i_coef_maxi = 0
!
! - Get parameters
!
    nb_coef        = ds_para_rb%multipara%nb_vari_coef
!
! - Get maximum
!
    do i_coef = 1, nb_coef
        if (ds_para_rb%resi_norm(i_coef) .ge. vale_maxi) then
            vale_maxi   = ds_para_rb%resi_norm(i_coef)
            i_coef_maxi = i_coef
        endif
    end do
    !WRITE(6,*) 'Indice du residu max:  ',i_coef_maxi,ds_para_rb%resi_norm(i_coef_maxi)
!
end subroutine
