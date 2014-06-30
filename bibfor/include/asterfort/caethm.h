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
    subroutine caethm(nomte, axi, perman, vf, typvf,&
                      typmod, modint, mecani, press1, press2,&
                      tempe, dimdep, dimdef, dimcon, nmec,&
                      np1, np2, ndim, nno, nnos,&
                      nnom, nface, npi, npg, nddls,&
                      nddlm, nddlfa, nddlk, dimuel, ipoids,&
                      ivf, idfde, ipoid2, ivf2, idfde2,&
                      npi2, jgano)
        character(len=16) :: nomte
        logical(kind=1) :: axi
        logical(kind=1) :: perman
        logical(kind=1) :: vf
        integer :: typvf
        character(len=8) :: typmod(2)
        character(len=3) :: modint
        integer :: mecani(5)
        integer :: press1(7)
        integer :: press2(7)
        integer :: tempe(5)
        integer :: dimdep
        integer :: dimdef
        integer :: dimcon
        integer :: nmec
        integer :: np1
        integer :: np2
        integer :: ndim
        integer :: nno
        integer :: nnos
        integer :: nnom
        integer :: nface
        integer :: npi
        integer :: npg
        integer :: nddls
        integer :: nddlm
        integer :: nddlfa
        integer :: nddlk
        integer :: dimuel
        integer :: ipoids
        integer :: ivf
        integer :: idfde
        integer :: ipoid2
        integer :: ivf2
        integer :: idfde2
        integer :: npi2
        integer :: jgano
    end subroutine caethm
end interface
