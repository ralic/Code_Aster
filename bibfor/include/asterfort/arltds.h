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
    subroutine arltds(nns   ,npgs  , &
                      ipoids,icoors,ivfs  ,idfdes, &
                      poijcs,fctfs ,dfdxs ,dfdys ,dfdzs)
        integer :: nns
        integer :: npgs
        integer :: icoors
        integer :: ipoids
        integer :: ivfs
        integer :: idfdes
        real(kind=8) :: poijcs(npgs)
        real(kind=8) :: fctfs(npgs*nns)
        real(kind=8) :: dfdxs(npgs*nns)
        real(kind=8) :: dfdys(npgs*nns)
        real(kind=8) :: dfdzs(npgs*nns)
    end subroutine arltds
end interface
