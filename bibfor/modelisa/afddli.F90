subroutine afddli(model, gran_cmp_nb, gran_cmp_name, node_nume, node_name, &
                  prnm, repe_type, repe_defi, coef_type, cmp_nb, &
                  cmp_name, cmp_acti, vale_type, vale_real, vale_func, &
                  vale_cplx, cmp_count, list_rela, lxfem, jnoxfl, &
                  jnoxfv, ch_xfem_stat, ch_xfem_lnno, ch_xfem_ltno, connex_inv)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/indik8.h"
#include "asterfort/afrela.h"
#include "asterfort/assert.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/xddlim.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! aslint: disable=W1504
!
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
    logical, intent(in) :: lxfem
    integer, intent(in) :: jnoxfl
    integer, intent(in) :: jnoxfv
    character(len=19), intent(in) :: connex_inv
    character(len=19), intent(in) :: ch_xfem_stat
    character(len=19), intent(in) :: ch_xfem_lnno
    character(len=19), intent(in) :: ch_xfem_ltno
!
! --------------------------------------------------------------------------------------------------
!
! Loadings - Kinematic
!
! Apply simple kinematic relation
!
! --------------------------------------------------------------------------------------------------
!
! For i=1,cmp_nb
!    coef * component_i = vale_i on node (numnoe,nomnoe) with coef = 1 (real or cplx)
!
! Overload rule: last kinematic relation on the node kept
!
! In  model          : Name of model
! In  gran_cmp_nb    : number of component of <GRANDEUR> (as DEPL_R, TEMP_R, etc)
! In  gran_cmp_name  : names of component of <GRANDEUR> (as DEPL_R, TEMP_R, etc)
! In  node_nume      : number (in mesh) of the node
! In  node_name      : name of the node
! In  prnm(*)        : <GRANDEUR> on node
! In  repe_type      : If 0 -> global reference system
!                      If 2/3 -> local reference system give by repe_defi
! In  repe_defi      : local reference system
! In  coef_type      : type of coefficient (real or complex)
! In  lagr_type      : type of lagrange multpilers (position of Lagrange and physical dof)
! In  cmp_nb         : number of components
! In  cmp_name       : name of components
! In  cmp_acti       : 1 if component affected, 0 else
! In  vale_type      : affected value type
! In  vale_real      : affected value if real
! In  vale_func      : affected value if function
! In  vale_cplx      : affected value if complex
! In  cmp_count      : how many times components have been affected
! In  list_rela      : JEVEUX object liste_rela for aflrch.F90 subroutine
! In  l_xfem         : .true. if xfem
! In  connex_inv     : inverse connectivity (blank if not xfem)
! In  jnoxfl         : pointer on list of XFEM nodes
! In  jnoxfv         : pointer on list of XFEM nodes
! In  ch_xfem_stat   : status of nodes field (blank if not xfem)
! In  ch_xfem_lnno   : normal level-set field (blank if not xfem)
! In  ch_xfem_ltno   : tangent level-set field (blank if not xfem)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ibid, i_cmp, cmp_index
    character(len=2) :: lagr_type
    character(len=24) :: valkm(2)
    real(kind=8) :: coef_real_unit, rbid(3)
    complex(kind=8) :: coef_cplx_unit
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    coef_real_unit = 1.d0
    coef_cplx_unit = dcmplx(1.d0,0.d0)
    lagr_type = '12'
!
! - Loop on components
!
    do i_cmp = 1, cmp_nb
!
! ----- Is component exists on this node ?
!
        cmp_index = indik8(gran_cmp_name, cmp_name(i_cmp)(1:8), 1, gran_cmp_nb)
        ASSERT(cmp_index.gt.0)
        if (.not.exisdg(prnm,cmp_index)) then
            valkm(1)=cmp_name(i_cmp)
            valkm(2)=node_name
            call u2mesk('A','CALCULEL3_18',2,valkm)
            goto 25
        endif
!
! ----- Apply on component, XFEM case
!
        if (lxfem) then
            ASSERT(coef_type.eq.'REEL')
            if (zl(jnoxfl-1+2*node_nume) .and. cmp_name(i_cmp)(1:1) .eq. 'D') then
                call xddlim(model, cmp_name(i_cmp)(1:8), node_name, node_nume, &
                            vale_real(i_cmp), vale_cplx(i_cmp), vale_func(i_cmp), &
                            vale_type, cmp_count(i_cmp), list_rela,&
                            ibid, rbid, jnoxfv, ch_xfem_stat, ch_xfem_lnno,&
                            ch_xfem_ltno, connex_inv)
                goto 25
            endif
        endif
!
! ----- Count
!
        cmp_count(i_cmp) = cmp_count(i_cmp) + 1
!
! ----- Apply on active component
!
        ASSERT(cmp_acti(i_cmp).le.1)
        ASSERT(cmp_acti(i_cmp).ge.0)
        if (cmp_acti(i_cmp).eq.1) then
            call afrela(coef_real_unit, coef_cplx_unit, cmp_name(i_cmp)(1:8), node_name,  &
                        repe_type, repe_defi, 1, &
                        vale_real(i_cmp), vale_cplx(i_cmp), vale_func(i_cmp), &
                        coef_type, vale_type, lagr_type, 0.d0, list_rela)
        endif
!
25      continue
    end do
!
    call jedema()
end subroutine
