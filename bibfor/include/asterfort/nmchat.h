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
    subroutine nmchat(matel, mat, nbvar, memo, visc,&
                      plast, sigmdv, depsdv, pm, dp,&
                      ndimsi, dt, rpvp, qp, vim,&
                      idelta, n1, n2, beta1, beta2,&
                      dsidep)
        real(kind=8) :: matel(*)
        real(kind=8) :: mat(*)
        integer :: nbvar
        integer :: memo
        integer :: visc
        real(kind=8) :: plast
        real(kind=8) :: sigmdv(6)
        real(kind=8) :: depsdv(6)
        real(kind=8) :: pm
        real(kind=8) :: dp
        integer :: ndimsi
        real(kind=8) :: dt
        real(kind=8) :: rpvp
        real(kind=8) :: qp
        real(kind=8) :: vim(*)
        integer :: idelta
        real(kind=8) :: n1
        real(kind=8) :: n2
        real(kind=8) :: beta1
        real(kind=8) :: beta2
        real(kind=8) :: dsidep(6, 6)
    end subroutine nmchat
end interface
