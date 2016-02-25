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
interface
    subroutine nmarch(numins  , modele  , mate  , carele, fonact,&
                      carcri  , ds_print, sddisc, sdpost, sdcrit,&
                      ds_measure, sderro  , sddyna, sdpilo, ds_energy,&
                      ds_inout, sdcriq  )
        use NonLin_Datastructure_type
        integer :: numins
        character(len=24) :: modele
        character(len=24) :: mate
        character(len=24) :: carele
        integer :: fonact(*)
        character(len=24) :: carcri
        type(NL_DS_Print), intent(in) :: ds_print
        character(len=19) :: sddisc
        character(len=19) :: sdpost
        character(len=19) :: sdcrit
        type(NL_DS_Measure), intent(inout) :: ds_measure
        character(len=24) :: sderro
        character(len=19) :: sddyna
        character(len=19) :: sdpilo
        type(NL_DS_Energy), intent(in) :: ds_energy
        type(NL_DS_InOut), intent(in) :: ds_inout
        character(len=24) :: sdcriq
    end subroutine nmarch
end interface
