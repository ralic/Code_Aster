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
    subroutine cabthm(nddls, nddlm, nno, nnos, nnom,&
                      dimuel, dimdef, ndim, kpi, ipoids,&
                      ipoid2, ivf, ivf2, idfde, idfde2,&
                      dfdi, dfdi2, geom, poids, poids2,&
                      b, nmec, yamec, addeme, yap1,&
                      addep1, yap2, addep2, yate, addete,&
                      np1, np2, axi)
        integer :: ndim
        integer :: dimdef
        integer :: dimuel
        integer :: nnos
        integer :: nno
        integer :: nddls
        integer :: nddlm
        integer :: nnom
        integer :: kpi
        integer :: ipoids
        integer :: ipoid2
        integer :: ivf
        integer :: ivf2
        integer :: idfde
        integer :: idfde2
        real(kind=8) :: dfdi(nno, 3)
        real(kind=8) :: dfdi2(nnos, 3)
        real(kind=8) :: geom(ndim, nno)
        real(kind=8) :: poids
        real(kind=8) :: poids2
        real(kind=8) :: b(dimdef, dimuel)
        integer :: nmec
        integer :: yamec
        integer :: addeme
        integer :: yap1
        integer :: addep1
        integer :: yap2
        integer :: addep2
        integer :: yate
        integer :: addete
        integer :: np1
        integer :: np2
        logical(kind=1) :: axi
    end subroutine cabthm
end interface
