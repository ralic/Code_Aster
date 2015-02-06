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
    subroutine xddlim(modele, motcle, nomn, ino, valimr,&
                      valimc, valimf, fonree, icompt, lisrel,&
                      ndim, direct, jnoxfv, ch1, ch2,&
                      ch3, cnxinv, mesh, hea_no)
        character(len=8) :: modele
        character(len=8) :: motcle
        character(len=8) :: nomn
        integer :: ino
        real(kind=8) :: valimr
        complex(kind=8) :: valimc
        character(len=8) :: valimf
        character(len=4) :: fonree
        integer :: icompt
        character(len=19) :: lisrel
        integer :: ndim
        real(kind=8) :: direct(3)
        integer :: jnoxfv
        character(len=19) :: ch1
        character(len=19) :: ch2
        character(len=19) :: ch3
        character(len=19) :: hea_no
        character(len=19) :: cnxinv
        character(len=8), intent(in) :: mesh
    end subroutine xddlim
end interface
