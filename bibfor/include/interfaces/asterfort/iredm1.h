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
interface
    subroutine iredm1(masse, noma, basemo, nbmode, nbmods,&
                      iamor, mass, rigi, amored, freq,&
                      smass, srigi, samor, cmass, crigi,&
                      camor)
        character(len=8) :: masse
        character(len=8) :: noma
        character(len=8) :: basemo
        integer :: nbmode
        integer :: nbmods
        integer :: iamor
        real(kind=8) :: mass(*)
        real(kind=8) :: rigi(*)
        real(kind=8) :: amored(*)
        real(kind=8) :: freq(*)
        real(kind=8) :: smass(*)
        real(kind=8) :: srigi(*)
        real(kind=8) :: samor(*)
        real(kind=8) :: cmass(*)
        real(kind=8) :: crigi(*)
        real(kind=8) :: camor(*)
    end subroutine iredm1
end interface
