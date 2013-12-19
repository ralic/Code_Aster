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
    subroutine xcaehm(nomte, axi, perman, typmod, modint,&
                      mecani, press1, press2, tempe, dimdef,&
                      dimcon, nmec, np1, np2, ndim,&
                      nno, nnos, nnom, npi, npg,&
                      nddls, nddlm, dimuel, ipoids, ivf,&
                      idfde, ddld, ddlm, enrmec, nenr,&
                      dimenr, nnop, nnops, nnopm)
        character(len=16) :: nomte
        logical :: axi
        logical :: perman
        character(len=8) :: typmod(2)
        character(len=3) :: modint
        integer :: mecani(5)
        integer :: press1(7)
        integer :: press2(7)
        integer :: tempe(5)
        integer :: dimdef
        integer :: dimcon
        integer :: nmec
        integer :: np1
        integer :: np2
        integer :: ndim
        integer :: nno
        integer :: nnos
        integer :: nnom
        integer :: npi
        integer :: npg
        integer :: nddls
        integer :: nddlm
        integer :: dimuel
        integer :: ipoids
        integer :: ivf
        integer :: idfde
        integer :: ddld
        integer :: ddlm
        integer :: enrmec(3)
        integer :: nenr
        integer :: dimenr
        integer :: nnop
        integer :: nnops
        integer :: nnopm
    end subroutine xcaehm
end interface 
