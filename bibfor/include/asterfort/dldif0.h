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
    subroutine dldif0(result, force1, neq, istoc, iarchi,&
                      ifm, lamort, imat, masse, rigid,&
                      amort, dep0, vit0, acc0, depl1,&
                      vite1, acce1, vite2, fexte, famor,&
                      fliai, nchar, nveca, liad, lifo,&
                      modele, ener, solveu, mate, carele,&
                      charge, infoch, fomult, numedd, dt,&
                      temps, tabwk0, tabwk1, archiv, nbtyar,&
                      typear, numrep)
        integer :: nbtyar
        integer :: neq
        character(len=8) :: result
        character(len=19) :: force1
        integer :: istoc
        integer :: iarchi
        integer :: ifm
        logical(kind=1) :: lamort
        integer :: imat(3)
        character(len=8) :: masse
        character(len=8) :: rigid
        character(len=8) :: amort
        real(kind=8) :: dep0(*)
        real(kind=8) :: vit0(*)
        real(kind=8) :: acc0(*)
        real(kind=8) :: depl1(neq)
        real(kind=8) :: vite1(neq)
        real(kind=8) :: acce1(neq)
        real(kind=8) :: vite2(neq)
        real(kind=8) :: fexte(*)
        real(kind=8) :: famor(*)
        real(kind=8) :: fliai(*)
        integer :: nchar
        integer :: nveca
        integer :: liad(*)
        character(len=24) :: lifo(*)
        character(len=24) :: modele
        logical(kind=1) :: ener
        character(len=19) :: solveu
        character(len=24) :: mate
        character(len=24) :: carele
        character(len=24) :: charge
        character(len=24) :: infoch
        character(len=24) :: fomult
        character(len=24) :: numedd
        real(kind=8) :: dt
        real(kind=8) :: temps
        real(kind=8) :: tabwk0(neq)
        real(kind=8) :: tabwk1(neq)
        integer :: archiv
        character(len=16) :: typear(nbtyar)
        integer :: numrep
    end subroutine dldif0
end interface
