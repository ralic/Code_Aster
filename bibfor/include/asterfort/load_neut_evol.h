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
    subroutine load_neut_evol(nb_type_neumz, type_calc  , time_curr, load_name, load_type_ligr,&
                              load_opti_r  , load_para_r, load_obje, nb_obje)
        integer, intent(in) :: nb_type_neumz
        character(len=4), intent(in) :: type_calc
        real(kind=8), intent(in) :: time_curr
        character(len=8), intent(in) :: load_name
        character(len=6), intent(out) :: load_type_ligr
        character(len=16), intent(out) :: load_opti_r
        character(len=8), intent(out) :: load_para_r(2)
        character(len=19), intent(out) :: load_obje(2)
        integer, intent(out) :: nb_obje
    end subroutine load_neut_evol
end interface
