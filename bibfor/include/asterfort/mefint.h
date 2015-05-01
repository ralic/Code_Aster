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
    subroutine mefint(nbz, nbgrp, nbmod, nbnoe, nbddl,&
                      irot, numnog, nbnog, zint, defm,&
                      phix, phiy, z, num)
        integer :: nbnoe
        integer :: nbmod
        integer :: nbgrp
        integer :: nbz
        integer :: nbddl
        integer :: irot(3)
        integer :: numnog(nbgrp)
        integer :: nbnog(nbgrp)
        real(kind=8) :: zint(nbz, nbgrp)
        real(kind=8) :: defm(6*nbnoe, nbmod)
        real(kind=8) :: phix(nbz, nbgrp, nbmod)
        real(kind=8) :: phiy(nbz, nbgrp, nbmod)
        real(kind=8) :: z(*)
        integer :: num(nbz)
    end subroutine mefint
end interface
