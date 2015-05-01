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
    subroutine xtdepm(ndim, jnnm, jnne, ndeple, nsinge,&
                      nsingm, ffe, ffm, jdepde, rre,&
                      rrm, jddle, jddlm, nfhe, nfhm, lmulti,&
                      heavn, heavfa, ddeple, ddeplm)
        integer :: ndim
        integer :: jnnm(3)
        integer :: jnne(3)
        integer :: ndeple
        integer :: nsinge
        integer :: nsingm
        real(kind=8) :: ffe(20)
        real(kind=8) :: ffm(20)
        integer :: jdepde
        real(kind=8) :: rre
        real(kind=8) :: rrm
        integer :: jddle(2)
        integer :: jddlm(2)
        integer :: nfhe
        integer :: nfhm
        aster_logical :: lmulti
        integer :: heavn(*)
        integer :: heavfa(*)
        real(kind=8) :: ddeple(3)
        real(kind=8) :: ddeplm(3)
    end subroutine xtdepm
end interface
