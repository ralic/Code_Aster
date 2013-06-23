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
    subroutine afddli(valr, valk, valc, prnm, nddla,&
                      fonree, nomn, ino, ddlimp, valimr,&
                      valimf, valimc, motcle, direct, dimens,&
                      mod, lisrel, nomcmp, nbcmp, icompt,&
                      lxfem, jnoxfl, jnoxfv, ch1, ch2,&
                      ch3, cnxinv)
        integer :: nddla
        real(kind=8) :: valr(*)
        character(len=8) :: valk(*)
        complex(kind=8) :: valc(*)
        integer :: prnm(*)
        character(len=4) :: fonree
        character(len=8) :: nomn
        integer :: ino
        integer :: ddlimp(nddla)
        real(kind=8) :: valimr(nddla)
        character(len=8) :: valimf(nddla)
        complex(kind=8) :: valimc(nddla)
        character(len=16) :: motcle(nddla)
        real(kind=8) :: direct(3)
        integer :: dimens
        character(len=8) :: mod
        character(len=19) :: lisrel
        character(len=8) :: nomcmp(*)
        integer :: nbcmp
        integer :: icompt(nddla)
        logical :: lxfem
        integer :: jnoxfl
        integer :: jnoxfv
        character(len=19) :: ch1
        character(len=19) :: ch2
        character(len=19) :: ch3
        character(len=19) :: cnxinv
    end subroutine afddli
end interface
