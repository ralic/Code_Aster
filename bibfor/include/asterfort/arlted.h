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
    subroutine arlted(ndim  , &
                      nns   ,jcoors, &
                      npgs  ,ivfs  ,idfdes,ipoids, &
                      elref1,ndml1   ,jcoor1, &
                      fcpig1    ,poijcs, &
                      dfdx1 ,dfdy1 ,dfdz1 )
        character(len=8) :: elref1
        integer :: ndim
        integer :: nns
        integer :: npgs
        integer :: ivfs
        integer :: ipoids
        integer :: idfdes
        integer :: ndml1
        integer :: jcoor1
        integer :: jcoors
        real(kind=8) :: poijcs(npgs)
        real(kind=8) :: fcpig1(npgs*ndim*ndim*ndml1)
        real(kind=8) :: dfdx1(npgs*ndim*ndim*ndml1)
        real(kind=8) :: dfdy1(npgs*ndim*ndim*ndml1)
        real(kind=8) :: dfdz1(npgs*ndim*ndim*ndml1)
    end subroutine arlted
end interface
