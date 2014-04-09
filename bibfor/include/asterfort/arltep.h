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
    subroutine arltep(ndim  ,coors   ,npgs   ,kpgs  , &
                      nns   ,fctfs   , &
                      elrefc,nnc   ,coorc , &
                      fctfc ,dfdxc ,dfdyc ,dfdzc)
        integer :: ndim
        integer :: npgs
        integer :: kpgs
        integer :: nns
        integer :: nnc
        character(len=8) :: elrefc
        real(kind=8) :: coorc(ndim*nnc)
        real(kind=8) :: coors(ndim*nns)
        real(kind=8) :: fctfs(nns*npgs)
        real(kind=8) :: fctfc(ndim*ndim*nnc)
        real(kind=8) :: dfdxc(nnc)
        real(kind=8) :: dfdyc(nnc)
        real(kind=8) :: dfdzc(nnc)
    end subroutine arltep
end interface
