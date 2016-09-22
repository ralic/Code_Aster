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
    subroutine nmflma(typmat, mod45 , defo  , ds_algopara, modelz,&
                      mate  , carele, sddisc, sddyna     , fonact,&
                      numins, valinc, solalg, lischa     , comref,&
                      ds_contact, numedd, numfix,&
                      ds_constitutive, ds_measure, meelem,&
                      measse, veelem, nddle , ddlexc     , modrig,&
                      ldccvg, matass, matgeo)
        use NonLin_Datastructure_type
        character(len=16) :: typmat
        character(len=4) :: mod45
        integer :: defo
        type(NL_DS_AlgoPara), intent(in) :: ds_algopara
        character(len=*) :: modelz
        character(len=24) :: mate
        character(len=24) :: carele
        character(len=19) :: sddisc
        character(len=19) :: sddyna
        integer :: fonact(*)
        integer :: numins
        character(len=19) :: valinc(*)
        character(len=19) :: solalg(*)
        character(len=19) :: lischa
        character(len=24) :: comref
        type(NL_DS_Contact), intent(in) :: ds_contact
        character(len=24) :: numedd
        character(len=24) :: numfix
        type(NL_DS_Constitutive), intent(in) :: ds_constitutive
        type(NL_DS_Measure), intent(inout) :: ds_measure
        character(len=19) :: meelem(*)
        character(len=19) :: measse(*)
        character(len=19) :: veelem(*)
        integer :: nddle
        character(len=24) :: ddlexc
        character(len=16) :: modrig
        integer :: ldccvg
        character(len=19) :: matass
        character(len=19) :: matgeo
    end subroutine nmflma
end interface
