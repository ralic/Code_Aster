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
    subroutine mlnclm(nb, n, p, frontl, frontu,&
                      adper, tu, tl, ad, eps,&
                      ier, cl, cu)
        integer :: nb
        integer :: n
        integer :: p
        complex(kind=8) :: frontl(*)
        complex(kind=8) :: frontu(*)
        integer :: adper(*)
        complex(kind=8) :: tu(*)
        complex(kind=8) :: tl(*)
        integer :: ad(*)
        real(kind=8) :: eps
        integer :: ier
        complex(kind=8) :: cl(nb, nb, *)
        complex(kind=8) :: cu(nb, nb, *)
    end subroutine mlnclm
end interface
