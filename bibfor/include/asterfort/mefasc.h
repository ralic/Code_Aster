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
    subroutine mefasc(ndim, nbcyl, nbgrp, nbtron, numgrp,&
                      idir, igrp, som, rint, dcent,&
                      ficent, d, fi, a, b)
        integer :: nbtron
        integer :: nbcyl
        integer :: ndim(14)
        integer :: nbgrp
        integer :: numgrp(*)
        integer :: idir
        integer :: igrp
        real(kind=8) :: som(9)
        real(kind=8) :: rint(*)
        real(kind=8) :: dcent(nbcyl)
        real(kind=8) :: ficent(nbcyl)
        real(kind=8) :: d(nbcyl, nbcyl)
        real(kind=8) :: fi(nbcyl, nbcyl)
        real(kind=8) :: a(2*nbtron*(nbcyl+1), *)
        real(kind=8) :: b(*)
    end subroutine mefasc
end interface
