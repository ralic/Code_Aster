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
    subroutine dplvga(yate, rho11, rho12, r, t,&
                      kh, congem, dimcon, adcp11, adcp12,&
                      ndim, padp, dp11p1, dp11p2, dp12p1,&
                      dp12p2, dp21p1, dp21p2, dp11t, dp12t,&
                      dp21t)
        integer :: dimcon
        integer :: yate
        real(kind=8) :: rho11
        real(kind=8) :: rho12
        real(kind=8) :: r
        real(kind=8) :: t
        real(kind=8) :: kh
        real(kind=8) :: congem(dimcon)
        integer :: adcp11
        integer :: adcp12
        integer :: ndim
        real(kind=8) :: padp
        real(kind=8) :: dp11p1
        real(kind=8) :: dp11p2
        real(kind=8) :: dp12p1
        real(kind=8) :: dp12p2
        real(kind=8) :: dp21p1
        real(kind=8) :: dp21p2
        real(kind=8) :: dp11t
        real(kind=8) :: dp12t
        real(kind=8) :: dp21t
    end subroutine dplvga
end interface
