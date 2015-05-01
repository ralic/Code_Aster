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
    subroutine mazacp(option, ndimsi, epsm, deps, epsane,&
                      ee, mazars, varm, varp, sigp,&
                      dsidep)
        character(len=16) :: option
        integer :: ndimsi
        real(kind=8) :: epsm(*)
        real(kind=8) :: deps(*)
        real(kind=8) :: epsane
        real(kind=8) :: ee
        real(kind=8) :: mazars(*)
        real(kind=8) :: varm(*)
        real(kind=8) :: varp(*)
        real(kind=8) :: sigp(*)
        real(kind=8) :: dsidep(6, 6)
    end subroutine mazacp
end interface
