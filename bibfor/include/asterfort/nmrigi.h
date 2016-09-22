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
    subroutine nmrigi(modelz    , mate  , carele, ds_constitutive, sddyna,&
                      ds_measure, fonact, iterat, valinc         , solalg,&
                      comref    , meelem, veelem, optioz         , ldccvg)
        use NonLin_Datastructure_type
        character(len=*) :: modelz
        character(len=*) :: mate
        character(len=24) :: carele
        type(NL_DS_Constitutive), intent(in) :: ds_constitutive
        character(len=19) :: sddyna
        type(NL_DS_Measure), intent(inout) :: ds_measure
        integer :: fonact(*)
        integer :: iterat
        character(len=19) :: valinc(*)
        character(len=19) :: solalg(*)
        character(len=24) :: comref
        character(len=19) :: meelem(*)
        character(len=19) :: veelem(*)
        character(len=*) :: optioz
        integer :: ldccvg
    end subroutine nmrigi
end interface
