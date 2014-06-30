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
    subroutine mmproj(alias, nno, ndim, coorma, coorpt,&
                      itemax, epsmax, toleou, dirapp, dir,&
                      ksi1, ksi2, tau1, tau2, iproj,&
                      niverr)
        character(len=8) :: alias
        integer :: nno
        integer :: ndim
        real(kind=8) :: coorma(27)
        real(kind=8) :: coorpt(3)
        integer :: itemax
        real(kind=8) :: epsmax
        real(kind=8) :: toleou
        logical(kind=1) :: dirapp
        real(kind=8) :: dir(3)
        real(kind=8) :: ksi1
        real(kind=8) :: ksi2
        real(kind=8) :: tau1(3)
        real(kind=8) :: tau2(3)
        integer :: iproj
        integer :: niverr
    end subroutine mmproj
end interface
