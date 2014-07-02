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
    subroutine xadher(p, saut, lamb1, cstafr, cpenfr,&
                      algofr, vitang, pboul, kn, ptknp,&
                      ik, adher)
        real(kind=8) :: p(3, 3)
        real(kind=8) :: saut(3)
        real(kind=8) :: lamb1(3)
        real(kind=8) :: cstafr
        real(kind=8) :: cpenfr
        integer :: algofr
        real(kind=8) :: vitang(3)
        real(kind=8) :: pboul(3)
        real(kind=8) :: kn(3, 3)
        real(kind=8) :: ptknp(3, 3)
        real(kind=8) :: ik(3, 3)
        aster_logical :: adher
    end subroutine xadher
end interface
