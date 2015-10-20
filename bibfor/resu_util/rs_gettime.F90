subroutine rs_gettime(result_, nume, inst)
!
implicit none
!
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/rsadpa.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=*), intent(in) :: result_
    integer, intent(in) :: nume
    real(kind=8), intent(out) :: inst
!
! --------------------------------------------------------------------------------------------------
!
! Results datastructure - Utility
!
! Get time at index stored in results datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  result           : name of results datastructure
! In  nume             : index to find in results datastructure
! Out inst             : time found in results datastructure
!                        
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: result
    integer :: j_inst
!
! --------------------------------------------------------------------------------------------------
!
    result    = result_
    inst      = r8vide()
    call rsadpa(result, 'L', 1, 'INST', nume,&
                0, sjv=j_inst)
    inst      = zr(j_inst)

end subroutine
