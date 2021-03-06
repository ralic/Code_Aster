!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine dxmate(fami, df, dm, dmf, dc,&
                      dci, dmc, dfc, nno, pgl,&
                      multic, coupmf, t2iu, t2ui, t1ve)
        character(len=4) :: fami
        real(kind=8) :: df(3, 3)
        real(kind=8) :: dm(3, 3)
        real(kind=8) :: dmf(3, 3)
        real(kind=8) :: dc(2, 2)
        real(kind=8) :: dci(2, 2)
        real(kind=8) :: dmc(3, 2)
        real(kind=8) :: dfc(3, 2)
        integer :: nno
        real(kind=8) :: pgl(3, 3)
        integer :: multic
        aster_logical :: coupmf
        real(kind=8) :: t2iu(4)
        real(kind=8) :: t2ui(4)
        real(kind=8) :: t1ve(9)
    end subroutine dxmate
end interface
