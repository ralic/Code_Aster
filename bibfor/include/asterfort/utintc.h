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
    subroutine utintc(zrino2, zrino1, zrjno2, zrjno1, x3,&
                      y3, inst, insold, k8cart, ltheta,&
                      nsomm, valfp, valfm, ifm, niv,&
                      option)
        real(kind=8) :: zrino2
        real(kind=8) :: zrino1
        real(kind=8) :: zrjno2
        real(kind=8) :: zrjno1
        real(kind=8) :: x3
        real(kind=8) :: y3
        real(kind=8) :: inst
        real(kind=8) :: insold
        character(len=8) :: k8cart
        logical(kind=1) :: ltheta
        integer :: nsomm
        real(kind=8) :: valfp(9)
        real(kind=8) :: valfm(9)
        integer :: ifm
        integer :: niv
        integer :: option
    end subroutine utintc
end interface
