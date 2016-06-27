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
    subroutine cfmxre(mesh  , model_   , ds_measure, ds_contact , nume_inst,&
                      sddisc, hval_algo, hval_incr , hval_veasse)
        use NonLin_Datastructure_type
        character(len=8), intent(in) :: mesh
        character(len=*), intent(in) :: model_
        type(NL_DS_Measure), intent(inout) :: ds_measure
        type(NL_DS_Contact), intent(inout) :: ds_contact
        integer, intent(in) :: nume_inst
        character(len=19), intent(in) :: sddisc
        character(len=19), intent(in) :: hval_algo(*)
        character(len=19), intent(in) :: hval_incr(*)
        character(len=19), intent(in) :: hval_veasse(*)
    end subroutine cfmxre
end interface
