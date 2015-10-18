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
    subroutine nmpost(modele , mesh    , numedd, numfix     , carele  ,&
                      compor , numins  , mate  , comref     , ds_inout,&
                      ds_contact, ds_algopara, fonact  ,&
                      carcri , ds_print, sdstat, sddisc     , sdtime  ,&
                      sd_obsv, sderro  , sddyna, sdpost     , valinc  ,&
                      solalg , meelem  , measse, veelem     , veasse  ,&
                      sdener , sdcriq  , eta   , lischa)
        use NonLin_Datastructure_type
        character(len=24) :: modele
        character(len=8), intent(in) :: mesh
        character(len=24) :: numedd
        character(len=24) :: numfix
        character(len=24) :: carele
        character(len=24) :: compor
        integer :: numins
        character(len=24) :: mate
        character(len=24) :: comref
        type(NL_DS_Contact), intent(in) :: ds_contact
        type(NL_DS_InOut), intent(in) :: ds_inout
        type(NL_DS_AlgoPara), intent(in) :: ds_algopara
        integer :: fonact(*)
        character(len=24) :: carcri
        type(NL_DS_Print), intent(in) :: ds_print
        character(len=24) :: sdstat
        character(len=19) :: sddisc
        character(len=24) :: sdtime
        character(len=19), intent(in) :: sd_obsv
        character(len=24) :: sderro
        character(len=24) :: sdieto
        character(len=19) :: sddyna
        character(len=19) :: lischa
        character(len=19) :: sdpost
        character(len=19) :: valinc(*)
        character(len=19) :: solalg(*)
        character(len=19) :: meelem(*)
        character(len=19) :: measse(*)
        character(len=19) :: veelem(*)
        character(len=19) :: veasse(*)
        character(len=19) :: sdener
        character(len=24) :: sdcriq
        real(kind=8) :: eta
    end subroutine nmpost
end interface
