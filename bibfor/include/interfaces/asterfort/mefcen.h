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
    subroutine mefcen(caelem, iequiv, nbcyl, nbz, irot,&
                      numnog, nbnog, nummag, numgrp, coor,&
                      cent, req, xint, yint, zint,&
                      rint, nbgrp)
        integer :: nbgrp
        integer :: nbz
        integer :: nbcyl
        character(len=19) :: caelem
        integer :: iequiv
        integer :: irot(3)
        integer :: numnog(*)
        integer :: nbnog(*)
        integer :: nummag(*)
        integer :: numgrp(*)
        real(kind=8) :: coor(*)
        real(kind=8) :: cent(2*nbcyl)
        real(kind=8) :: req(nbgrp)
        real(kind=8) :: xint(nbcyl)
        real(kind=8) :: yint(nbcyl)
        real(kind=8) :: zint(nbz, nbgrp)
        real(kind=8) :: rint(nbcyl)
    end subroutine mefcen
end interface
