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
    subroutine afrela(coef_real, coef_cplx, dof_name, node_name, repe_type,&
                      repe_defi, nbterm, vale_real, vale_cplx, vale_func,&
                      type_coef, vale_type, type_lagr, epsi, lisrez)
        integer, intent(in) :: nbterm
        real(kind=8), intent(in) :: coef_real(nbterm)
        complex(kind=8), intent(in) :: coef_cplx(nbterm)
        character(len=8), intent(in) :: dof_name(nbterm)
        character(len=8), intent(in) :: node_name(nbterm)
        integer, intent(in) :: repe_type(nbterm)
        real(kind=8), intent(in) :: repe_defi(3, nbterm)
        real(kind=8), intent(in) :: vale_real
        complex(kind=8), intent(in) :: vale_cplx
        character(len=*), intent(in) :: vale_func
        character(len=4), intent(in) :: type_coef
        character(len=4), intent(in) :: vale_type
        character(len=2), intent(in) :: type_lagr
        real(kind=8), intent(in) :: epsi
        character(len=*), intent(in) :: lisrez
    end subroutine afrela
end interface
