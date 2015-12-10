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
    subroutine nminit(result , model , numedd   , numfix     , mate      ,&
                      compor , carele, list_load, ds_algopara, maprec    ,&
                      solveu , carcri, numins   , sdstat     , sddisc    ,&
                      sdnume , sdcrit, varc_refe, fonact     , mesh      ,&
                      sdpilo , sddyna, ds_print , sd_suiv    , sd_obsv   ,&
                      sdtime , sderro, sdpost   , ds_inout   , sdener    ,&
                      ds_conv, sdcriq, valinc   , solalg     , measse    ,&
                      veelem , meelem, veasse   , codere     , ds_contact)
        use NonLin_Datastructure_type
        type(NL_DS_InOut), intent(inout) :: ds_inout
        character(len=8) :: result
        character(len=24) :: model
        character(len=24) :: numedd
        character(len=24) :: numfix
        character(len=24) :: mate
        character(len=24) :: compor
        character(len=24) :: carele
        character(len=19) :: list_load
        character(len=19) :: maprec
        character(len=19) :: solveu
        character(len=24) :: carcri
        integer :: numins
        character(len=24) :: sdstat
        character(len=19) :: sddisc
        character(len=19) :: sdnume
        character(len=19) :: sdcrit
        character(len=24) :: varc_refe
        integer :: fonact(*)
        character(len=8) :: mesh
        character(len=19) :: sdpilo
        character(len=19) :: sddyna
        type(NL_DS_Print), intent(inout) :: ds_print
        character(len=24), intent(out) :: sd_suiv
        character(len=24) :: sdtime
        character(len=24) :: sderro
        character(len=19) :: sdpost
        character(len=19), intent(out) :: sd_obsv
        character(len=19) :: sdener
        type(NL_DS_Conv), intent(inout) :: ds_conv
        type(NL_DS_AlgoPara), intent(inout) :: ds_algopara
        character(len=24) :: sdcriq
        character(len=19) :: valinc(*)
        character(len=19) :: solalg(*)
        character(len=19) :: measse(*)
        character(len=19) :: veelem(*)
        character(len=19) :: meelem(*)
        character(len=19) :: veasse(*)
        character(len=24) :: codere
        type(NL_DS_Contact), intent(inout) :: ds_contact
    end subroutine nminit
end interface
