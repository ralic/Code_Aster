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
    subroutine mmgeom(ndim  ,nne   ,nnm   ,ffe   ,ffm   , &
                      geomae,geomam,tau1  ,tau2  , &
              norm  ,mprojn,mprojt,geome ,geomm )
        integer :: ndim
        integer :: nne
        integer :: nnm
        real(kind=8) :: ffe(9)
        real(kind=8) :: ffm(9)
        real(kind=8) :: geomae(9, 3)
        real(kind=8) :: geomam(9, 3)
        real(kind=8) :: tau1(3)
        real(kind=8) :: tau2(3)
        real(kind=8) :: norm(3)
        real(kind=8) :: mprojn(3, 3)
        real(kind=8) :: mprojt(3, 3)
        real(kind=8) :: geome(3)
        real(kind=8) :: geomm(3)
    end subroutine mmgeom
end interface
