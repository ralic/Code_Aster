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
    subroutine nmobse(meshz     , sd_obsv  , time,&
                      cara_elemz, modelz   , matez    , ds_constitutive, disp_curr,&
                      strx_curr , varc_curr, varc_refe)
        use NonLin_Datastructure_type
        character(len=*), intent(in) :: meshz
        character(len=19), intent(in) :: sd_obsv
        real(kind=8), intent(in) :: time
        character(len=*), optional, intent(in) :: modelz
        character(len=*), optional, intent(in) :: cara_elemz
        character(len=*), optional, intent(in) :: matez
        type(NL_DS_Constitutive), optional, intent(in) :: ds_constitutive
        character(len=*), optional, intent(in) :: disp_curr
        character(len=*), optional, intent(in) :: strx_curr
        character(len=*), optional, intent(in) :: varc_curr
        character(len=*), optional, intent(in) :: varc_refe
    end subroutine nmobse
end interface
