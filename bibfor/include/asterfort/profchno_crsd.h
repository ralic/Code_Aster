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
    subroutine profchno_crsd(prof_chnoz , base      , nb_equa     , meshz      , nb_ligrz,&
                             nb_ecz     , gran_namez, prno_lengthz, l_coll_const)
        character(len=*), intent(in) :: prof_chnoz
        character(len=1), intent(in) :: base
        integer, intent(in) :: nb_equa
        character(len=*), optional, intent(in) :: meshz
        character(len=*), optional, intent(in) :: gran_namez
        integer, optional, intent(in) :: nb_ecz
        integer, optional, intent(in) :: nb_ligrz
        integer, optional, intent(in) :: prno_lengthz
        logical, optional, intent(in) :: l_coll_const
    end subroutine profchno_crsd
end interface
