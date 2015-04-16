!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine vetnth(optioz, modelz, carelz, matcdz, instz,&
                      chtnz, compoz, tpchiz, tpchfz, chhyz,&
                      vecelz, veceiz, varc_curr)
        character(len=*) :: optioz
        character(len=*) :: modelz
        character(len=*) :: carelz
        character(len=*) :: matcdz
        character(len=*) :: instz
        character(len=*) :: chtnz
        character(len=*) :: compoz
        character(len=*) :: tpchiz
        character(len=*) :: tpchfz
        character(len=*) :: chhyz
        character(len=*) :: vecelz
        character(len=*) :: veceiz
        character(len=19), intent(in) :: varc_curr
    end subroutine vetnth
end interface
