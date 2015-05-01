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
    subroutine mlnfmj(nb, n, p, frontl, frontu,&
                      frnl, frnu, adper, t1, t2,&
                      cl, cu)
        integer :: p
        integer :: nb
        integer :: n
        real(kind=8) :: frontl(*)
        real(kind=8) :: frontu(*)
        real(kind=8) :: frnl(*)
        real(kind=8) :: frnu(*)
        integer :: adper(*)
        real(kind=8) :: t1(p, nb, *)
        real(kind=8) :: t2(p, nb, *)
        real(kind=8) :: cl(nb, nb, *)
        real(kind=8) :: cu(nb, nb, *)
    end subroutine mlnfmj
end interface
