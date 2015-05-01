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
    subroutine approj(sdappa, noma, newgeo, defico, posnom,&
                      dirapp, dir, itemax, epsmax, toleou,&
                      coorpt, posmam, iprojm, ksi1m, ksi2m,&
                      tau1m, tau2m, distm, vecpmm)
        character(len=19) :: sdappa
        character(len=8) :: noma
        character(len=19) :: newgeo
        character(len=24) :: defico
        integer :: posnom
        aster_logical :: dirapp
        real(kind=8) :: dir(3)
        integer :: itemax
        real(kind=8) :: epsmax
        real(kind=8) :: toleou
        real(kind=8) :: coorpt(3)
        integer :: posmam
        integer :: iprojm
        real(kind=8) :: ksi1m
        real(kind=8) :: ksi2m
        real(kind=8) :: tau1m(3)
        real(kind=8) :: tau2m(3)
        real(kind=8) :: distm
        real(kind=8) :: vecpmm(3)
    end subroutine approj
end interface
