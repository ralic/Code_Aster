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
    subroutine dlnew0(result, force0, force1, iinteg, neq,&
                      istoc, iarchi, ifm, nbexci, nondp,&
                      nmodam, lamort, limped, lmodst, imat,&
                      masse, rigid, amort, nchar, nveca,&
                      liad, lifo, modele, mate, carele,&
                      charge, infoch, fomult, numedd, depla,&
                      vitea, accea, dep0, vit0, acc0,&
                      fexte, famor, fliai, depl1, vite1,&
                      acce1, psdel, fammo, fimpe, fonde,&
                      vien, vite, vita1, mltap, a0,&
                      a2, a3, a4, a5, a6,&
                      a7, a8, c0, c1, c2,&
                      c3, c4, c5, nodepl, novite,&
                      noacce, matres, maprec, solveu, criter,&
                      chondp, ener, vitini, vitent, valmod,&
                      basmod, veanec, vaanec, vaonde, veonde,&
                      dt, theta, tempm, temps, iforc2,&
                      tabwk1, tabwk2, archiv, nbtyar, typear,&
                      numrep)
        integer :: nbtyar
        integer :: nondp
        integer :: nbexci
        integer :: neq
        character(len=8) :: result
        character(len=19) :: force0
        character(len=19) :: force1
        integer :: iinteg
        integer :: istoc
        integer :: iarchi
        integer :: ifm
        integer :: nmodam
        logical(kind=1) :: lamort
        logical(kind=1) :: limped
        logical(kind=1) :: lmodst
        integer :: imat(3)
        character(len=8) :: masse
        character(len=8) :: rigid
        character(len=8) :: amort
        integer :: nchar
        integer :: nveca
        integer :: liad(*)
        character(len=24) :: lifo(*)
        character(len=24) :: modele
        character(len=24) :: mate
        character(len=24) :: carele
        character(len=24) :: charge
        character(len=24) :: infoch
        character(len=24) :: fomult
        character(len=24) :: numedd
        real(kind=8) :: depla(neq)
        real(kind=8) :: vitea(neq)
        real(kind=8) :: accea(neq)
        real(kind=8) :: dep0(*)
        real(kind=8) :: vit0(*)
        real(kind=8) :: acc0(*)
        real(kind=8) :: fexte(*)
        real(kind=8) :: famor(*)
        real(kind=8) :: fliai(*)
        real(kind=8) :: depl1(neq)
        real(kind=8) :: vite1(neq)
        real(kind=8) :: acce1(neq)
        real(kind=8) :: psdel(neq)
        real(kind=8) :: fammo(neq)
        real(kind=8) :: fimpe(neq)
        real(kind=8) :: fonde(neq)
        real(kind=8) :: vien(neq)
        real(kind=8) :: vite(neq)
        real(kind=8) :: vita1(neq)
        integer :: mltap(nbexci)
        real(kind=8) :: a0
        real(kind=8) :: a2
        real(kind=8) :: a3
        real(kind=8) :: a4
        real(kind=8) :: a5
        real(kind=8) :: a6
        real(kind=8) :: a7
        real(kind=8) :: a8
        real(kind=8) :: c0
        real(kind=8) :: c1
        real(kind=8) :: c2
        real(kind=8) :: c3
        real(kind=8) :: c4
        real(kind=8) :: c5
        character(len=8) :: nodepl(nbexci)
        character(len=8) :: novite(nbexci)
        character(len=8) :: noacce(nbexci)
        character(len=8) :: matres
        character(len=19) :: maprec
        character(len=19) :: solveu
        character(len=24) :: criter
        character(len=8) :: chondp(nondp)
        logical(kind=1) :: ener
        character(len=24) :: vitini
        character(len=24) :: vitent
        character(len=24) :: valmod
        character(len=24) :: basmod
        character(len=24) :: veanec
        character(len=24) :: vaanec
        character(len=24) :: vaonde
        character(len=24) :: veonde
        real(kind=8) :: dt
        real(kind=8) :: theta
        real(kind=8) :: tempm
        real(kind=8) :: temps
        integer :: iforc2
        real(kind=8) :: tabwk1(neq)
        real(kind=8) :: tabwk2(neq)
        integer :: archiv
        character(len=16) :: typear(nbtyar)
        integer :: numrep
    end subroutine dlnew0
end interface
