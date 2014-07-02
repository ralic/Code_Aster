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
#include "asterf_types.h"
!
interface
    subroutine afddli(model, gran_cmp_nb, gran_cmp_name, node_nume, node_name, &
                      prnm, repe_type, repe_defi, coef_type, cmp_nb, &
                      cmp_name, cmp_acti, vale_type, vale_real, vale_func, &
                      vale_cplx, cmp_count, list_rela, lxfem, jnoxfl, &
                      jnoxfv, ch_xfem_stat, ch_xfem_lnno, ch_xfem_ltno, connex_inv)
        character(len=8), intent(in) :: model
        integer, intent(in) :: gran_cmp_nb
        character(len=8), intent(in) :: gran_cmp_name(gran_cmp_nb)
        integer, intent(in) :: node_nume 
        character(len=8), intent(in) :: node_name
        integer, intent(in) :: prnm(*)
        integer, intent(in) :: repe_type
        real(kind=8), intent(in) :: repe_defi(3)
        character(len=4), intent(in) :: coef_type
        integer, intent(in) :: cmp_nb
        character(len=16), intent(in) :: cmp_name(cmp_nb)
        integer, intent(in) :: cmp_acti(cmp_nb)
        character(len=4), intent(in) :: vale_type
        real(kind=8), intent(in) :: vale_real(cmp_nb)
        character(len=8), intent(in) :: vale_func(cmp_nb)
        complex(kind=8), intent(in) ::  vale_cplx(cmp_nb)
        integer, intent(inout) :: cmp_count(cmp_nb)
        character(len=19), intent(in) :: list_rela
        aster_logical, intent(in) :: lxfem
        integer, intent(in) :: jnoxfl
        integer, intent(in) :: jnoxfv
        character(len=19), intent(in) :: connex_inv
        character(len=19), intent(in) :: ch_xfem_stat
        character(len=19), intent(in) :: ch_xfem_lnno
        character(len=19), intent(in) :: ch_xfem_ltno
    end subroutine afddli
end interface
