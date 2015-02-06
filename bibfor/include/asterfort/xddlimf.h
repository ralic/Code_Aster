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
    subroutine xddlimf(modele, ino, cnxinv, jnoxfv, motcle,&
                       ch2, ndim, lsn, lst, valimr, valimf, valimc,&
                       fonree, lisrel, nomn, direct, class, mesh,&
                       hea_no)
        character(len=8) :: modele
        integer :: ino
        character(len=19) :: cnxinv
        integer :: jnoxfv
        character(len=8) :: motcle
        character(len=19) :: ch2
        integer :: ndim
        real(kind=8) :: lsn(4)
        real(kind=8) :: lst(4)
        real(kind=8) :: valimr
        character(len=8) :: valimf
        complex(kind=8) :: valimc
        character(len=4) :: fonree
        character(len=19) :: lisrel
        character(len=8), intent(in) :: mesh
        character(len=8) :: nomn
        real(kind=8) :: direct(3)
        aster_logical :: class
        character(len=19) :: hea_no
    end subroutine xddlimf
end interface
