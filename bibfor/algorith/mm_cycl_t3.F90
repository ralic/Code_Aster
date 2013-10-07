subroutine mm_cycl_t3(sd_cont_solv, pres_frot_prev, &
                      dist_frot_prev, coef_frot, pres_frot, &
                      dist_frot, cycl_stat_curr)
!
    implicit     none
!
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=24), intent(in) :: sd_cont_solv
    real(kind=8), intent(in) :: pres_frot_prev(3)
    real(kind=8), intent(in) :: dist_frot_prev(3)
    real(kind=8), intent(in) :: coef_frot
    real(kind=8), intent(in) :: pres_frot(3)
    real(kind=8), intent(in) :: dist_frot(3)
    integer, intent(out) :: cycl_stat_curr
!
! --------------------------------------------------------------------------------------------------
!
! Contact (continue method)
!
! Automatic treatment for Sticking/sliding cycling
!
! --------------------------------------------------------------------------------------------------
!
!
! In  sd_cont_solv   : data structure for contact solving
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sd_cycl_his
    integer :: jcyhis
    integer :: cycl_type, idim
    real(kind=8) :: laug_frot_curr(3),laug_frot_prev(3)
    real(kind=8) :: nrese_curr, nrese_prev
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initialisations
!
    cycl_type = 3
    nrese_curr = 0.d0
    nrese_prev = 0.d0
!
! - Acces to cycling objects
!
    sd_cycl_his = sd_cont_solv(1:14)//'.CYCHIS'
    call jeveuo(sd_cycl_his,'E',jcyhis)
!
! - Augmented ratios
!
    laug_frot_prev(1) = pres_frot_prev(1) + coef_frot*dist_frot_prev(1)
    laug_frot_prev(2) = pres_frot_prev(2) + coef_frot*dist_frot_prev(2)
    laug_frot_prev(3) = pres_frot_prev(3) + coef_frot*dist_frot_prev(3)
    do idim = 1, 3
        nrese_prev = laug_frot_prev(idim)*laug_frot_prev(idim) + nrese_prev
    end do
    nrese_prev = sqrt(nrese_prev)
!
    laug_frot_curr(1) = pres_frot(1) + coef_frot*dist_frot(1)
    laug_frot_curr(2) = pres_frot(2) + coef_frot*dist_frot(2)
    laug_frot_curr(3) = pres_frot(3) + coef_frot*dist_frot(3)
    do idim = 1, 3
        nrese_curr = laug_frot_curr(idim)*laug_frot_curr(idim) + nrese_curr
    end do
    nrese_curr = sqrt(nrese_curr)
    cycl_stat_curr = -2
!
    call jedema()
end subroutine
