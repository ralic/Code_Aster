!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
#include "asterf_types.h"
!
interface
    subroutine mdfedy(nbpal, nbmode, numpas, dt, dtsto,&
                      tcf, vrotat, dplmod, depgen, vitgen,&
                      fexgen, typal, finpal, cnpal, prdeff,&
                      conv, fsauv)
        integer :: nbmode
        integer :: nbpal
        integer :: numpas
        real(kind=8) :: dt
        real(kind=8) :: dtsto
        real(kind=8) :: tcf
        real(kind=8) :: vrotat
        real(kind=8) :: dplmod(nbpal, nbmode, *)
        real(kind=8) :: depgen(*)
        real(kind=8) :: vitgen(*)
        real(kind=8) :: fexgen(*)
        character(len=6) :: typal(20)
        character(len=3) :: finpal(20)
        character(len=8) :: cnpal(20)
        aster_logical :: prdeff
        real(kind=8) :: conv
        real(kind=8) :: fsauv(20, 3)
    end subroutine mdfedy
end interface
