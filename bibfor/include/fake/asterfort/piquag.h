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
    subroutine piquag(epsi, rip, rep, rit, ret,&
                      bet, eso, hso, h2, h3,&
                      l3, l4, l5, l6, tetaf,&
                      xmax, ymax, lmax, nt, mailla,&
                      nogrno, typsou)
        real(kind=8) :: epsi
        real(kind=8) :: rip
        real(kind=8) :: rep
        real(kind=8) :: rit
        real(kind=8) :: ret
        real(kind=8) :: bet
        real(kind=8) :: eso
        real(kind=8) :: hso
        real(kind=8) :: h2
        real(kind=8) :: h3
        real(kind=8) :: l3
        real(kind=8) :: l4
        real(kind=8) :: l5
        real(kind=8) :: l6
        real(kind=8) :: tetaf
        real(kind=8) :: xmax
        real(kind=8) :: ymax
        real(kind=8) :: lmax
        integer :: nt
        character(len=8) :: mailla
        character(len=24) :: nogrno
        character(len=8) :: typsou
    end subroutine piquag
end interface
