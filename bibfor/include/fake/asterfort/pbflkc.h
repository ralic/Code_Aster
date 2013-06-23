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
    subroutine pbflkc(umoy, rhof, hmoy, rmoy, long,&
                      cf0, mcf0, icoq, imod, nbm,&
                      rkip, tcoef, s1, s2, ki,&
                      lambda, kcalcu, passag)
        integer :: nbm
        real(kind=8) :: umoy
        real(kind=8) :: rhof
        real(kind=8) :: hmoy
        real(kind=8) :: rmoy
        real(kind=8) :: long
        real(kind=8) :: cf0
        real(kind=8) :: mcf0
        integer :: icoq
        integer :: imod
        real(kind=8) :: rkip
        real(kind=8) :: tcoef(10, nbm)
        real(kind=8) :: s1
        real(kind=8) :: s2
        complex(kind=8) :: ki(4, 3)
        complex(kind=8) :: lambda(3)
        complex(kind=8) :: kcalcu(3, 4)
        complex(kind=8) :: passag(3, 3)
    end subroutine pbflkc
end interface
