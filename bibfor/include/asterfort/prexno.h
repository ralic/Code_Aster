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
    subroutine prexno(champ, ioc, nomax, cmpmax, valmax,&
                      nomin, cmpmin, valmin, noamax, cmamax,&
                      vaamax, noamin, cmamin, vaamin)
        character(len=*) :: champ
        integer :: ioc
        character(len=8) :: nomax
        character(len=8) :: cmpmax
        real(kind=8) :: valmax
        character(len=8) :: nomin
        character(len=8) :: cmpmin
        real(kind=8) :: valmin
        character(len=8) :: noamax
        character(len=8) :: cmamax
        real(kind=8) :: vaamax
        character(len=8) :: noamin
        character(len=8) :: cmamin
        real(kind=8) :: vaamin
    end subroutine prexno
end interface
