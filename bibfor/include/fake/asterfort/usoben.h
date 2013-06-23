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
    subroutine usoben(guidag, dimobs, obsuse, nco, rayo,&
                      thet, nbsect, parusu, typusu, nomt19,&
                      arete, arete2, rcarte, denc)
        character(len=8) :: guidag
        integer :: dimobs
        real(kind=8) :: obsuse(*)
        integer :: nco
        real(kind=8) :: rayo(*)
        real(kind=8) :: thet(*)
        integer :: nbsect
        real(kind=8) :: parusu(20, *)
        integer :: typusu(*)
        character(len=19) :: nomt19
        real(kind=8) :: arete
        real(kind=8) :: arete2
        real(kind=8) :: rcarte
        real(kind=8) :: denc
    end subroutine usoben
end interface
