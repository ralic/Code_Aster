subroutine vtcreb(field_nodez , base      , type_scalz,&
                  nume_ddlz   ,&
                  meshz       , prof_chnoz, idx_gdz, nb_equa_inz,&
                  nb_equa_outz)
!
implicit none
!
#include "asterfort/wkvect.h"
#include "asterfort/dismoi.h"
#include "asterfort/jeecra.h"
#include "asterfort/sdchgd.h"
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
!
    character(len=*), intent(in) :: field_nodez
    character(len=1), intent(in) :: base
    character(len=*), intent(in) :: type_scalz
    character(len=*), optional, intent(in) :: nume_ddlz
    character(len=*), optional, intent(in) :: meshz
    character(len=*), optional, intent(in) :: prof_chnoz
    integer, optional, intent(in) :: nb_equa_inz
    integer, optional, intent(in) :: idx_gdz
    integer, optional, intent(out) :: nb_equa_outz
!
! --------------------------------------------------------------------------------------------------
!
! Field utility
!
! Create NODE field
!
! --------------------------------------------------------------------------------------------------
!
! In  field_node    : name of field
! In  base          : JEVEUX base to create field
! In  type_scal     : type of GRANDEUR (real or complex)
! With numbering:
!   In  nume_ddl    : name of numbering
! With complete informations:
!   In  mesh        : name of mesh
!   In  prof_chno   : name of PROF_CHNO
!   In  idx_gd      : index of GRANDEUR
!   In  nb_equa_in  : number of equations
!
!   Out nb_equa_out : number of equations
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: prof_chno, field_node
    character(len=8) :: mesh
    character(len=24) :: obj_refe, obj_vale, obj_desc
    integer :: idx_gd, nb_equa, j_vale
    character(len=24), pointer :: p_refe(:) => null()
    integer, pointer :: p_desc(:) => null()
    character(len=3) :: type_scal
!
! --------------------------------------------------------------------------------------------------
!
    field_node = field_nodez
    type_scal  = type_scalz
!
    obj_refe = field_node(1:19)//'.REFE'
    obj_vale = field_node(1:19)//'.VALE'
    obj_desc = field_node(1:19)//'.DESC'
!
! - Get parameters from NUME_DDL
!
    if (present(nume_ddlz)) then       
        call dismoi('NUM_GD_SI' , nume_ddlz, 'NUME_DDL', repi=idx_gd)
        call dismoi('NB_EQUA'   , nume_ddlz, 'NUME_DDL', repi=nb_equa)
        call dismoi('NOM_MAILLA', nume_ddlz, 'NUME_DDL', repk=mesh)
        call dismoi('PROF_CHNO' , nume_ddlz, 'NUME_DDL', repk=prof_chno)
    else
        idx_gd    = idx_gdz
        nb_equa   = nb_equa_inz
        prof_chno = prof_chnoz
        mesh      = meshz
    endif
!
! - Object .REFE
!
    call wkvect(obj_refe, base//' V K24', 4, vk24 = p_refe)
    p_refe(1) = mesh
    p_refe(2) = prof_chno
!
! - Object .DESC
!
    call wkvect(obj_desc, base//' V I', 2, vi = p_desc)
    call jeecra(obj_desc, 'DOCU', cval='CHNO')
    p_desc(1) = idx_gd
    p_desc(2) = 1
!
! - Object .VALE
!
    call wkvect(obj_vale, base//' V '//type_scal, nb_equa, j_vale)
    if (present(nb_equa_outz)) then
        nb_equa_outz = nb_equa
    endif
!
! - Change GRANDEUR
!
    if (type_scal.eq.'R'.or.type_scal.eq.'C'.or.type_scal.eq.'F') then
        call sdchgd(field_node, type_scal)
    endif
!
end subroutine
