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
    subroutine dltlec(result, modele, numedd, materi, mate,&
                      carael, carele, imat, masse, rigid,&
                      amort, lamort, nchar, nveca, lischa,&
                      charge, infoch, fomult, iaadve, ialifo,&
                      nondp, iondp, solveu, iinteg, t0,&
                      nume, baseno, numrep)
        character(len=8) :: result
        character(len=24) :: modele
        character(len=24) :: numedd
        character(len=8) :: materi
        character(len=24) :: mate
        character(len=8) :: carael
        character(len=24) :: carele
        integer :: imat(3)
        character(len=8) :: masse
        character(len=8) :: rigid
        character(len=8) :: amort
        logical :: lamort
        integer :: nchar
        integer :: nveca
        character(len=19) :: lischa
        character(len=24) :: charge
        character(len=24) :: infoch
        character(len=24) :: fomult
        integer :: iaadve
        integer :: ialifo
        integer :: nondp
        integer :: iondp
        character(len=19) :: solveu
        integer :: iinteg
        real(kind=8) :: t0
        integer :: nume
        character(len=8) :: baseno
        integer :: numrep
    end subroutine dltlec
end interface
