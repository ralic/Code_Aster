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
    subroutine assthm(nno, nnos, nnom, npg, npi,&
                      ipoids, ipoid2, ivf, ivf2, idfde,&
                      idfde2, geom, crit, deplm, deplp,&
                      contm, contp, varim, varip, defgem,&
                      defgep, drds, drdsr, dsde, b,&
                      dfdi, dfdi2, r, sigbar, c,&
                      ck, cs, matuu, vectu, rinstm,&
                      rinstp, option, imate, mecani, press1,&
                      press2, tempe, dimdef, dimcon, dimuel,&
                      nbvari, nddls, nddlm, nmec, np1,&
                      np2, ndim, compor, typmod, axi,&
                      perman, modint, codret, angmas)
        integer :: ndim
        integer :: nbvari
        integer :: dimuel
        integer :: dimcon
        integer :: dimdef
        integer :: npi
        integer :: nnos
        integer :: nno
        integer :: nnom
        integer :: npg
        integer :: ipoids
        integer :: ipoid2
        integer :: ivf
        integer :: ivf2
        integer :: idfde
        integer :: idfde2
        real(kind=8) :: geom(ndim, nno)
        real(kind=8) :: crit(*)
        real(kind=8) :: deplm(dimuel)
        real(kind=8) :: deplp(dimuel)
        real(kind=8) :: contm(dimcon*npi)
        real(kind=8) :: contp(dimcon*npi)
        real(kind=8) :: varim(nbvari*npi)
        real(kind=8) :: varip(nbvari*npi)
        real(kind=8) :: defgem(dimdef)
        real(kind=8) :: defgep(dimdef)
        real(kind=8) :: drds(dimdef+1, dimcon)
        real(kind=8) :: drdsr(dimdef, dimcon)
        real(kind=8) :: dsde(dimcon, dimdef)
        real(kind=8) :: b(dimdef, dimuel)
        real(kind=8) :: dfdi(nno, 3)
        real(kind=8) :: dfdi2(nnos, 3)
        real(kind=8) :: r(dimdef+1)
        real(kind=8) :: sigbar(dimdef)
        real(kind=8) :: c(dimdef)
        real(kind=8) :: ck(dimdef)
        real(kind=8) :: cs(dimdef)
        real(kind=8) :: matuu(dimuel*dimuel)
        real(kind=8) :: vectu(dimuel)
        real(kind=8) :: rinstm
        real(kind=8) :: rinstp
        character(len=16) :: option
        integer :: imate
        integer :: mecani(5)
        integer :: press1(7)
        integer :: press2(7)
        integer :: tempe(5)
        integer :: nddls
        integer :: nddlm
        integer :: nmec
        integer :: np1
        integer :: np2
        character(len=16) :: compor(*)
        character(len=8) :: typmod(2)
        logical :: axi
        logical :: perman
        character(len=3) :: modint
        integer :: codret
        real(kind=8) :: angmas(3)
    end subroutine assthm
end interface 
