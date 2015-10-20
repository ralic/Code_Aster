subroutine rs_getnume(result_, inst, criter_, prec, nume,&
                      iret)
!
implicit none
!
#include "asterfort/rsorac.h"
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
    real(kind=8), intent(in) :: inst
    character(len=*), intent(in) :: criter_
    real(kind=8), intent(in) :: prec
    integer, intent(out) :: nume
    integer, intent(out) :: iret
!
! --------------------------------------------------------------------------------------------------
!
! Results datastructure - Utility
!
! Get index stored in results datastructure for a given time
!
! --------------------------------------------------------------------------------------------------
!
! In  result           : name of results datastructure
! In  inst             : time to find in results datastructure
! In  criter           : absolute/relative search
! In  prec             : precision to search time
! Out nume             : index stored in results datastructure for given time
! Out iret             : error code
!                        0 - Time not found
!                        1 - One time found
!                        2 - Several times found
!                        
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: result, k8bid
    complex(kind=8) :: c16bid
    integer :: tnum(1), ibid, nb_find
!
! --------------------------------------------------------------------------------------------------
!
    result    = result_
    nume      = 0
    iret      = 0
    call rsorac(result , 'INST', ibid   , inst, k8bid,&
                c16bid , prec  , criter_, tnum, 1    ,&
                nb_find)
    if (nb_find.lt.0) then
        iret = 2
    elseif (nb_find.eq.1) then
        iret = 1
        nume = tnum(1)
    elseif (nb_find.eq.0) then
        iret = 0
    endif

end subroutine
