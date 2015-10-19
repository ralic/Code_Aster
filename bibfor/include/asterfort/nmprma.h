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
! aslint: disable=W1504
!
interface
    subroutine nmprma(modelz     , mate    , carele, compor, carcri,&
                      ds_algopara, lischa  , numedd, numfix, solveu,&
                      comref     , ds_print, sdstat, sdtime, sddisc,&
                      sddyna     , numins  , fonact, ds_contact,&
                      valinc     , solalg  , veelem, meelem, measse,&
                      maprec     , matass  , codere, faccvg, ldccvg)
        use NonLin_Datastructure_type
        character(len=*) :: modelz
        character(len=24) :: mate
        character(len=24) :: carele
        character(len=24) :: compor
        character(len=24) :: carcri
        type(NL_DS_AlgoPara), intent(in) :: ds_algopara
        character(len=19) :: lischa
        character(len=24) :: numedd
        character(len=24) :: numfix
        character(len=19) :: solveu
        character(len=24) :: comref
        type(NL_DS_Print), intent(inout) :: ds_print
        character(len=24) :: sdstat
        character(len=24) :: sdtime
        character(len=19) :: sddisc
        character(len=19) :: sddyna
        integer :: numins
        integer :: fonact(*)
        type(NL_DS_Contact), intent(inout) :: ds_contact
        character(len=19) :: valinc(*)
        character(len=19) :: solalg(*)
        character(len=19) :: veelem(*)
        character(len=19) :: meelem(*)
        character(len=19) :: measse(*)
        character(len=19) :: maprec
        character(len=19) :: matass
        character(len=24) :: codere
        integer :: faccvg
        integer :: ldccvg
    end subroutine nmprma
end interface
