!
! COPYRIGHT (C) 1991 - 2017  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine romMultiParaCoefCompute(ds_empi       , ds_multipara,&
                                       syst_2mbr_type, syst_2mbr   , solveROM,&
                                       i_mode_until  , i_mode_coef ,&
                                       coef_redu     , i_coef_)
        use Rom_Datastructure_type
        type(ROM_DS_Empi), intent(in) :: ds_empi
        type(ROM_DS_MultiPara), intent(inout) :: ds_multipara
        character(len=1), intent(in) :: syst_2mbr_type
        character(len=19), intent(in) :: syst_2mbr
        type(ROM_DS_Solve), intent(in) :: solveROM
        integer, intent(in) :: i_mode_until
        integer, intent(in) :: i_mode_coef
        character(len=24), intent(in) :: coef_redu
        integer, optional, intent(in) :: i_coef_
    end subroutine romMultiParaCoefCompute
end interface
