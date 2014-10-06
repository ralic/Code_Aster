subroutine xrelco(mesh   , nb_dim, sdline_crack, nb_rela_line, list_rela_line,&
                  nb_edge)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/afrela.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
    character(len=8), intent(in) :: mesh
    integer, intent(in) :: nb_dim
    character(len=14), intent(in) :: sdline_crack
    character(len=19), intent(in) :: list_rela_line
    integer, intent(out) :: nb_rela_line
    integer, intent(out) :: nb_edge
!
! --------------------------------------------------------------------------------------------------
!
! XFEM - Contact definition
!
! Create kinematic load
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh           : name of mesh
! In  nb_dim         : dimension of space
! In  sdline_crack   : name of datastructure of linear relations for crack
! In  list_rela_line : name of linear relation object
! Out nb_rela_line   : number of linear relation
! Out nb_edge        : number of "VITAL" edge
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbddl
    parameter  (nbddl=12)
    character(len=8) :: ddlc(nbddl)
!
    real(kind=8) :: vale_real, coef_real(6)
    integer :: ier, repe_type(8), i_edge
    integer :: node_nume(8), i_dim
    character(len=8) :: node_name(8), vale_func_dumm, cmp_name(8)
    complex(kind=8) :: coef_cplx_dumm, vale_cplx_dumm
    aster_logical :: l_mult_crack
    integer, pointer :: v_rela_node(:) => null()
    integer, pointer :: v_rela_cmp(:) => null()
!
    data ddlc /'LAGS_C','LAGS_F1','LAGS_F2',&
               'LAG2_C','LAG2_F1','LAG2_F2',&
               'LAG3_C','LAG3_F1','LAG3_F2',&
               'LAG4_C','LAG4_F1','LAG4_F2'/
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    vale_real      = 0.d0
    repe_type(1:8) = 0
    nb_rela_line   = 0
    nb_edge        = 0
!
! - Get access 
!
    call jeexin(sdline_crack, ier)
    if (ier .eq. 0) then
        nb_edge = 0
    else
        call jeveuo(sdline_crack, 'L', vi = v_rela_node)
        call jelira(sdline_crack, 'LONMAX', nb_edge)
        call jeexin(sdline_crack(1:14)//'_LAGR', ier)
        if (ier .eq. 0) then
            l_mult_crack = .false.
        else
            l_mult_crack = .true.
            call jeveuo(sdline_crack(1:14)//'_LAGR', 'L', vi = v_rela_cmp)
        endif
    endif
!
! - Total number of "VITAL" edges
!
    nb_edge = nb_edge/2
!
! - Create kinematic load
!
    do i_edge = 1, nb_edge
!
! ----- Get nodes of linear relation
!
        node_nume(1) = v_rela_node(2*(i_edge-1)+1)
        node_nume(2) = v_rela_node(2*(i_edge-1)+2)
        call jenuno(jexnum(mesh(1:8)//'.NOMNOE', node_nume(1)), node_name(1))
        call jenuno(jexnum(mesh(1:8)//'.NOMNOE', node_nume(2)), node_name(2))
!
! ----- Coefficients of linear relation
!
        coef_real(1) = 1.d0
        coef_real(2) = -1.d0
!
! ----- Set linear relation
!
        do i_dim = 1, nb_dim
            if (l_mult_crack) then
                cmp_name(1) = ddlc(3*(v_rela_cmp(2*(i_edge-1)+1)-1)+i_dim)
                cmp_name(2) = ddlc(3*(v_rela_cmp(2*(i_edge-1)+2)-1)+i_dim)
            else
                cmp_name(1) = ddlc(i_dim)
                cmp_name(2) = ddlc(i_dim)
            endif
            call afrela(coef_real, [coef_cplx_dumm], cmp_name, node_name, repe_type,&
                        [0.d0]   , 2, vale_real, vale_cplx_dumm, vale_func_dumm,&
                        'REEL', 'REEL', '12', 0.d0, list_rela_line)
            nb_rela_line = nb_rela_line + 1
        end do
    end do
!
    call jedema()
end subroutine
