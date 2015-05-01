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
    subroutine rvtec2(releve, absc, itcopt, itsppt, coor,&
                      nomnoe, nbcmp, nbpoin, docu, nomtab,&
                      iocc, xnovar, ncheff, i1, ioc,&
                      isd)
        real(kind=8) :: releve(*)
        real(kind=8) :: absc(*)
        integer :: itcopt(*)
        integer :: itsppt(*)
        real(kind=8) :: coor(*)
        character(len=8) :: nomnoe(*)
        integer :: nbcmp
        integer :: nbpoin
        character(len=4) :: docu
        character(len=19) :: nomtab
        integer :: iocc
        character(len=24) :: xnovar
        character(len=16) :: ncheff
        integer :: i1
        integer :: ioc
        integer :: isd
    end subroutine rvtec2
end interface
