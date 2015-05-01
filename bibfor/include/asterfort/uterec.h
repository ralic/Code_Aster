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
    subroutine uterec(ndim, iflup, iflum, ino, mno,&
                      jno, nsomm, jac, term22, aux,&
                      ltheta, valthe, valunt, niv, ifm,&
                      xn, yn, zn, valhp, valhm,&
                      valtp, valtm, ityp, itemp, itemm,&
                      noe)
        integer :: ndim
        integer :: iflup
        integer :: iflum
        integer :: ino
        integer :: mno
        integer :: jno
        integer :: nsomm
        real(kind=8) :: jac(9)
        real(kind=8) :: term22
        real(kind=8) :: aux
        aster_logical :: ltheta
        real(kind=8) :: valthe
        real(kind=8) :: valunt
        integer :: niv
        integer :: ifm
        real(kind=8) :: xn(9)
        real(kind=8) :: yn(9)
        real(kind=8) :: zn(9)
        real(kind=8) :: valhp(9)
        real(kind=8) :: valhm(9)
        real(kind=8) :: valtp(9)
        real(kind=8) :: valtm(9)
        integer :: ityp
        integer :: itemp
        integer :: itemm
        integer :: noe(9, 6, 3)
    end subroutine uterec
end interface
