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
    subroutine mlnclj(nb, n, ll, m, it,&
                      p, frontl, frontu, frnl, frnu,&
                      adper, travl, travu, cl, cu)
        integer :: p
        integer :: nb
        integer :: n
        integer :: ll
        integer :: m
        integer :: it
        complex(kind=8) :: frontl(*)
        complex(kind=8) :: frontu(*)
        complex(kind=8) :: frnl(*)
        complex(kind=8) :: frnu(*)
        integer :: adper(*)
        complex(kind=8) :: travl(p, nb, *)
        complex(kind=8) :: travu(p, nb, *)
        complex(kind=8) :: cl(nb, nb, *)
        complex(kind=8) :: cu(nb, nb, *)
    end subroutine mlnclj
end interface
