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
    subroutine mefrep(nbz, nbmod, nbcyl, nbgrp, numgrp,&
                      z, freq0, rho, visc, rint,&
                      phix, phiy, dcent, matma)
        integer :: nbz
        integer :: nbmod
        integer :: nbcyl
        integer :: nbgrp
        integer :: numgrp(*)
        real(kind=8) :: z(*)
        real(kind=8) :: freq0(*)
        real(kind=8) :: rho(*)
        real(kind=8) :: visc(*)
        real(kind=8) :: rint(*)
        real(kind=8) :: phix(*)
        real(kind=8) :: phiy(*)
        real(kind=8) :: dcent(*)
        real(kind=8) :: matma(*)
    end subroutine mefrep
end interface
