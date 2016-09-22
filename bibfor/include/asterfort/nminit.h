!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine nminit(mesh      , model     , mate       , cara_elem      , list_load ,&
                      numedd    , numfix    , ds_algopara, ds_constitutive, maprec    ,&
                      solver    , numins    , sddisc     , sdnume         , sdcrit    ,&
                      varc_refe , fonact    , sdpilo     , sddyna         , ds_print  ,&
                      sd_suiv   , sd_obsv   , sderro     , sdpost         , ds_inout  ,&
                      ds_energy , ds_conv   , sdcriq     , valinc         , solalg    ,&
                      measse    , veelem    , meelem     , veasse         , ds_contact,&
                      ds_measure, ds_algorom)
        use NonLin_Datastructure_type
        use Rom_Datastructure_type
        character(len=8), intent(in) :: mesh
        character(len=24), intent(in) :: model
        character(len=24), intent(in) :: mate
        character(len=24), intent(in) :: cara_elem
        character(len=19), intent(in) :: list_load
        character(len=24) :: numedd
        character(len=24) :: numfix
        type(NL_DS_AlgoPara), intent(inout) :: ds_algopara
        type(NL_DS_Constitutive), intent(inout) :: ds_constitutive
        character(len=19) :: maprec
        character(len=19), intent(in) :: solver
        integer :: numins
        character(len=19) :: sddisc
        character(len=19) :: sdnume
        character(len=19) :: sdcrit
        character(len=24) :: varc_refe
        integer :: fonact(*)
        character(len=19) :: sdpilo
        character(len=19) :: sddyna
        type(NL_DS_Print), intent(inout) :: ds_print
        character(len=24), intent(out) :: sd_suiv
        character(len=19), intent(out) :: sd_obsv
        character(len=24) :: sderro
        character(len=19) :: sdpost
        type(NL_DS_InOut), intent(inout) :: ds_inout
        type(NL_DS_Energy), intent(inout) :: ds_energy
        type(NL_DS_Conv), intent(inout) :: ds_conv
        character(len=24) :: sdcriq
        character(len=19) :: valinc(*)
        character(len=19) :: solalg(*)
        character(len=19) :: measse(*)
        character(len=19) :: veelem(*)
        character(len=19) :: meelem(*)
        character(len=19) :: veasse(*)
        type(NL_DS_Contact), intent(inout) :: ds_contact
        type(NL_DS_Measure), intent(inout) :: ds_measure
        type(ROM_DS_AlgoPara), intent(inout) :: ds_algorom
    end subroutine nminit
end interface
