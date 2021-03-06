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
    subroutine bijsom(umoy, rhof, r1, r2, long,&
                      cf0, icoq, jcoq, jmod, nbm,&
                      rki, thetai, thetaj, tcoef, ysol,&
                      bij)
        integer :: nbm
        real(kind=8) :: umoy
        real(kind=8) :: rhof
        real(kind=8) :: r1
        real(kind=8) :: r2
        real(kind=8) :: long
        real(kind=8) :: cf0
        integer :: icoq
        integer :: jcoq
        integer :: jmod
        real(kind=8) :: rki
        real(kind=8) :: thetai
        real(kind=8) :: thetaj
        real(kind=8) :: tcoef(10, nbm)
        complex(kind=8) :: ysol(3, 101)
        complex(kind=8) :: bij
    end subroutine bijsom
end interface
