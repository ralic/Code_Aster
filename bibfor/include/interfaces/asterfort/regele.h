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
    subroutine regele(option, typmod, npi, ndim, dimuel,&
                      nddls, nddlm, nno, nnos, nnom,&
                      axi, regula, dimcon, ipoids, ipoid2,&
                      ivf, ivf2, idfde, idfde2, compor,&
                      geom, deplp, contp, imate, dimdef,&
                      matuu, vectu)
        integer :: dimdef
        integer :: dimcon
        integer :: dimuel
        integer :: ndim
        integer :: npi
        character(len=16) :: option
        character(len=8) :: typmod(2)
        integer :: nddls
        integer :: nddlm
        integer :: nno
        integer :: nnos
        integer :: nnom
        logical :: axi
        integer :: regula(6)
        integer :: ipoids
        integer :: ipoid2
        integer :: ivf
        integer :: ivf2
        integer :: idfde
        integer :: idfde2
        character(len=16) :: compor(*)
        real(kind=8) :: geom(ndim, *)
        real(kind=8) :: deplp(dimuel)
        real(kind=8) :: contp(dimcon*npi)
        integer :: imate
        real(kind=8) :: matuu(dimuel*dimuel)
        real(kind=8) :: vectu(dimuel)
    end subroutine regele
end interface
