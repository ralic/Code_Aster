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
    subroutine irvari(ifi, nochmd, chanom, typech, modele,&
                      nbcmp, nomcmp, partie, numpt, instan,&
                      numord, nbmaec, limaec, noresu, carael,&
                      codret)
        integer :: ifi
        character(len=64) :: nochmd
        character(len=19) :: chanom
        character(len=8) :: typech
        character(len=8) :: modele
        integer :: nbcmp
        character(len=*) :: nomcmp(*)
        character(len=*) :: partie
        integer :: numpt
        real(kind=8) :: instan
        integer :: numord
        integer :: nbmaec
        integer :: limaec(*)
        character(len=8) :: noresu
        character(len=8) :: carael
        integer :: codret
    end subroutine irvari
end interface
