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
    function spect3(x, a, b, func, tol,&
                    coeff, xlc, vitn, defm, rhoe,&
                    nbp, im, jm)
        integer :: nbp
        real(kind=8) :: x
        real(kind=8) :: a
        real(kind=8) :: b
        real(kind=8) :: tol
        real(kind=8) :: coeff(*)
        real(kind=8) :: xlc
        real(kind=8) :: vitn(nbp, *)
        real(kind=8) :: defm(nbp, *)
        real(kind=8) :: rhoe(nbp, *)
        integer :: im
        integer :: jm
        real(kind=8) :: spect3
        interface
            function func(xx, y, xlc, vitn, rhoe,&
                          defm, nbp, im, jm)
                integer :: nbp
                real(kind=8) :: xx
                real(kind=8) :: y
                real(kind=8) :: xlc
                real(kind=8) :: vitn(nbp, *)
                real(kind=8) :: rhoe(nbp, *)
                real(kind=8) :: defm(nbp, *)
                integer :: im
                integer :: jm
                real(kind=8) :: func
            end function func
        end interface
    end function spect3
end interface
