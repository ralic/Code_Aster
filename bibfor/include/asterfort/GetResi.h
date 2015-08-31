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
    subroutine GetResi(ds_conv   , type  ,&
                       col_name_ , col_name_locus_, vale_calc_  , locus_calc_, user_para_,&
                       l_conv_   , event_type_    , l_resi_test_)
        use NonLin_Datastructure_type
        type(NL_DS_Conv), intent(in) :: ds_conv
        character(len=*), intent(in) :: type
        character(len=16), optional, intent(out) :: col_name_
        character(len=16), optional, intent(out) :: col_name_locus_
        real(kind=8), optional, intent(out) :: vale_calc_
        character(len=16), optional, intent(out) :: locus_calc_
        real(kind=8), optional, intent(out) :: user_para_
        aster_logical, optional, intent(out) :: l_conv_
        character(len=16), optional, intent(out)  :: event_type_
        aster_logical, optional, intent(out) :: l_resi_test_
    end subroutine GetResi
end interface
