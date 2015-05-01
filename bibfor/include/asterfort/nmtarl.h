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
    subroutine nmtarl(mode, ndimsi, mat, sigel, vim,&
                      epm, dp, sp, xi, dirdp,&
                      dirsp, dirxi, min, rho, ener)
        integer :: mode
        integer :: ndimsi
        real(kind=8) :: mat(*)
        real(kind=8) :: sigel(*)
        real(kind=8) :: vim(*)
        real(kind=8) :: epm(*)
        real(kind=8) :: dp
        real(kind=8) :: sp
        real(kind=8) :: xi
        real(kind=8) :: dirdp
        real(kind=8) :: dirsp
        real(kind=8) :: dirxi
        real(kind=8) :: min
        real(kind=8) :: rho
        real(kind=8) :: ener
    end subroutine nmtarl
end interface
