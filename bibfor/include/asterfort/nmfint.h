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
    subroutine nmfint(modele, mate, carele, comref, compor,&
                      carcri, fonact, iterat, sddyna, sdstat,&
                      sdtime, valinc, solalg, ldccvg, codere,&
                      vefint)
        character(len=24) :: modele
        character(len=24) :: mate
        character(len=24) :: carele
        character(len=24) :: comref
        character(len=24) :: compor
        character(len=24) :: carcri
        integer :: fonact(*)
        integer :: iterat
        character(len=19) :: sddyna
        character(len=24) :: sdstat
        character(len=24) :: sdtime
        character(len=19) :: valinc(*)
        character(len=19) :: solalg(*)
        integer :: ldccvg
        character(len=24) :: codere
        character(len=19) :: vefint
    end subroutine nmfint
end interface
