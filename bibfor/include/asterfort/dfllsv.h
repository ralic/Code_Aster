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
    subroutine dfllsv(lisifr, lisevr, lisevk, lisesu, isauve,&
                      even, action, submet, subaut, pasmin,&
                      nbrpas, niveau, pcplus, cmmaxi, delcol,&
                      durdec, penmax, cricmp, valere, nocham,&
                      nocmp)
        character(len=24) :: lisifr
        character(len=24) :: lisevr
        character(len=24) :: lisevk
        character(len=24) :: lisesu
        integer :: isauve
        character(len=16) :: even
        character(len=16) :: action
        character(len=16) :: submet
        character(len=16) :: subaut
        real(kind=8) :: pasmin
        integer :: nbrpas
        integer :: niveau
        real(kind=8) :: pcplus
        real(kind=8) :: cmmaxi
        real(kind=8) :: delcol
        real(kind=8) :: durdec
        real(kind=8) :: penmax
        character(len=16) :: cricmp
        real(kind=8) :: valere
        character(len=16) :: nocham
        character(len=16) :: nocmp
    end subroutine dfllsv
end interface
