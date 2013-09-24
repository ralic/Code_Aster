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
    subroutine prexel(champ, ioc, mamax, nomax, ispmax,&
                      cmpmax, valmax, mamin, nomin, ispmin,&
                      cmpmin, valmin, maamax, noamax, isamax,&
                      cmamax, vaamax, maamin, noamin, isamin,&
                      cmamin, vaamin)
        character(len=*) :: champ
        integer :: ioc
        character(len=8) :: mamax
        character(len=8) :: nomax
        integer :: ispmax
        character(len=8) :: cmpmax
        real(kind=8) :: valmax
        character(len=8) :: mamin
        character(len=8) :: nomin
        integer :: ispmin
        character(len=8) :: cmpmin
        real(kind=8) :: valmin
        character(len=8) :: maamax
        character(len=8) :: noamax
        integer :: isamax
        character(len=8) :: cmamax
        real(kind=8) :: vaamax
        character(len=8) :: maamin
        character(len=8) :: noamin
        integer :: isamin
        character(len=8) :: cmamin
        real(kind=8) :: vaamin
    end subroutine prexel
end interface
