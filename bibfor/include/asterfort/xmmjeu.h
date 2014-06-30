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
    subroutine xmmjeu(ndim, jnnm, jnne, ndeple, nsinge,&
                      nsingm, ffe, ffm, norm, jgeom,&
                      jdepde, jdepm, rre, rrm, jddle,&
                      jddlm, nfhe, nfhm, lmulti, heavfa,&
                      jeu)
        integer :: ndim
        integer :: jnnm(3)
        integer :: jnne(3)
        integer :: ndeple
        integer :: nsinge
        integer :: nsingm
        real(kind=8) :: ffe(20)
        real(kind=8) :: ffm(20)
        real(kind=8) :: norm(3)
        integer :: jgeom
        integer :: jdepde
        integer :: jdepm
        real(kind=8) :: rre
        real(kind=8) :: rrm
        integer :: jddle(2)
        integer :: jddlm(2)
        integer :: nfhe
        integer :: nfhm
        logical(kind=1) :: lmulti
        integer :: heavfa(*)
        real(kind=8) :: jeu
    end subroutine xmmjeu
end interface
