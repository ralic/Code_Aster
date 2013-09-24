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
    subroutine lrfmed(resu, i, mfich, nomgd, typcha,&
                      option, param, nochmd, acces, nbordr,&
                      nnu, nis, nto, jnume, jlist,&
                      noma, nbcmpv, ncmpva, ncmpvm, prolz,&
                      iinst, crit, epsi, linoch, acce)
        character(len=8) :: resu
        integer :: i
        integer :: mfich
        character(len=8) :: nomgd
        character(len=8) :: typcha
        character(len=24) :: option
        character(len=8) :: param
        character(len=64) :: nochmd
        character(len=10) :: acces
        integer :: nbordr
        integer :: nnu
        integer :: nis
        integer :: nto
        integer :: jnume
        integer :: jlist
        character(len=8) :: noma
        integer :: nbcmpv
        character(len=24) :: ncmpva
        character(len=24) :: ncmpvm
        character(len=3) :: prolz
        integer :: iinst
        character(len=8) :: crit
        real(kind=8) :: epsi
        character(len=16) :: linoch(100)
        character(len=4) :: acce
    end subroutine lrfmed
end interface
