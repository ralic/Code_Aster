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
    function mefin3(nbz, nbgrp, imod, icyl, jmod,&
                    jcyl, z, f1, f2, f3,&
                    g, h)
        integer :: nbgrp
        integer :: nbz
        integer :: imod
        integer :: icyl
        integer :: jmod
        integer :: jcyl
        real(kind=8) :: z(*)
        real(kind=8) :: f1(nbz*nbgrp, *)
        real(kind=8) :: f2(nbz*nbgrp, *)
        real(kind=8) :: f3(*)
        real(kind=8) :: g(*)
        real(kind=8) :: h(*)
        real(kind=8) :: mefin3
    end function mefin3
end interface
