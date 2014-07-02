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
    subroutine lcconv(loi, yd, dy, ddy, ye,&
                      nr, itmax, toler, iter, intg,&
                      nmat, mater, r, rini, epstr,&
                      typess, essai, icomp, nvi, vind,&
                      vinf, vind1, indi, bnews, mtrac,&
                      lreli, iret)
        integer :: nvi
        integer :: nmat
        integer :: nr
        character(len=16) :: loi
        real(kind=8) :: yd(*)
        real(kind=8) :: dy(*)
        real(kind=8) :: ddy(*)
        real(kind=8) :: ye(nr)
        integer :: itmax
        real(kind=8) :: toler
        integer :: iter
        integer :: intg
        real(kind=8) :: mater(nmat, 2)
        real(kind=8) :: r(*)
        real(kind=8) :: rini(*)
        real(kind=8) :: epstr(6)
        integer :: typess
        real(kind=8) :: essai
        integer :: icomp
        real(kind=8) :: vind(nvi)
        real(kind=8) :: vinf(nvi)
        real(kind=8) :: vind1(nvi)
        integer :: indi(7)
        aster_logical :: bnews(3)
        aster_logical :: mtrac
        aster_logical :: lreli
        integer :: iret
    end subroutine lcconv
end interface
