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
    subroutine xdvois(typma, ino, noma, numa, jlsnd, jlsnl, jconx2,&
                      ch2 , lsn, nbmano, jma, adrma, ndim, coupee,&
                      nno, arete, milieu, lsno, voisin)
        character(len=8) :: typma
        integer :: ino
        character(len=8) :: noma
        integer :: numa
        integer :: jlsnd
        integer :: jlsnl
        character(len=19) :: ch2
        real(kind=8) :: lsn(4)
        integer :: nbmano
        integer :: jma
        integer :: adrma
        integer :: ndim
        aster_logical :: coupee
        character(len=8) :: arete
        aster_logical :: milieu
        real(kind=8) :: lsno(3)
        integer :: voisin(3)
        integer :: jconx2
        integer :: nno
    end subroutine xdvois
end interface
