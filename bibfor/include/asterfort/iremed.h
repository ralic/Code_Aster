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
    subroutine iremed(nomcon, ifichi, nocham, novcmp, partie,&
                      liordr, lresu, nbnoec, linoec, nbmaec,&
                      limaec, nomcmp, lvarie, carael)
        character(len=*) :: nomcon
        integer :: ifichi
        character(len=*) :: nocham
        character(len=*) :: novcmp
        character(len=*) :: partie
        character(len=*) :: liordr
        logical(kind=1) :: lresu
        integer :: nbnoec
        integer :: linoec(*)
        integer :: nbmaec
        integer :: limaec(*)
        character(len=*) :: nomcmp
        logical(kind=1) :: lvarie
        character(len=8) :: carael
    end subroutine iremed
end interface
