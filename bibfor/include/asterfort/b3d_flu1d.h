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
    subroutine b3d_flu1d(vsigma, e0, dt, eps10, veps10,&
                         e1, eta1, eta2, veps20, deps0,&
                         deps1, deps2, veps1f, veps2f)
        real(kind=8) :: vsigma
        real(kind=8) :: e0
        real(kind=8) :: dt
        real(kind=8) :: eps10
        real(kind=8) :: veps10
        real(kind=8) :: e1
        real(kind=8) :: eta1
        real(kind=8) :: eta2
        real(kind=8) :: veps20
        real(kind=8) :: deps0
        real(kind=8) :: deps1
        real(kind=8) :: deps2
        real(kind=8) :: veps1f
        real(kind=8) :: veps2f
    end subroutine b3d_flu1d
end interface 
