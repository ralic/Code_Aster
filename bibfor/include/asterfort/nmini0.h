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
    subroutine nmini0(list_func_acti, eta    , nume_inst  , matass  , zmeelm    ,&
                      zmeass        , zveelm , zveass     , zsolal  , zvalin    ,&
                      ds_print      , ds_conv, ds_algopara, ds_inout, ds_contact)
        use NonLin_Datastructure_type
        integer, intent(out) :: list_func_acti(*)
        character(len=19), intent(out) :: matass
        integer, intent(out) :: nume_inst
        real(kind=8), intent(out) :: eta
        integer, intent(in) :: zmeelm
        integer, intent(in) :: zmeass
        integer, intent(in) :: zveelm
        integer, intent(in) :: zveass
        integer, intent(in) :: zsolal
        integer, intent(in) :: zvalin
        type(NL_DS_Print), intent(out) :: ds_print
        type(NL_DS_Conv), intent(out) :: ds_conv
        type(NL_DS_AlgoPara), intent(out) :: ds_algopara
        type(NL_DS_InOut), intent(out) :: ds_inout
        type(NL_DS_Contact), intent(out) :: ds_contact
    end subroutine nmini0
end interface
