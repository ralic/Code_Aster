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
    subroutine utin3d(igeom, nsomm, ino, ityp, inst,&
                      insold, k8cart, ltheta, niv, ifm,&
                      option, valfp, valfm, noe)
        integer :: igeom
        integer :: nsomm
        integer :: ino
        integer :: ityp
        real(kind=8) :: inst
        real(kind=8) :: insold
        character(len=8) :: k8cart
        aster_logical :: ltheta
        integer :: niv
        integer :: ifm
        integer :: option
        real(kind=8) :: valfp(9)
        real(kind=8) :: valfm(9)
        integer :: noe(9, 6, 3)
    end subroutine utin3d
end interface
