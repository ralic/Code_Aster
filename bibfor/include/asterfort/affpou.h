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
interface
    subroutine affpou(tmp, tmpf, fcx, nom, isec,&
                      ivar, car, ncar, val, tab,&
                      exp, nbo, ioc, ier)
        character(len=24) :: tmp
        character(len=24) :: tmpf
        character(len=8) :: fcx
        character(len=24) :: nom
        integer :: isec
        integer :: ivar
        character(len=8) :: car(*)
        integer :: ncar
        real(kind=8) :: val(*)
        character(len=8) :: tab(*)
        character(len=8) :: exp(*)
        integer :: nbo
        character(len=6) :: ioc
        integer :: ier
    end subroutine affpou
end interface
