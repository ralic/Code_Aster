subroutine pteddl(typesd   , resuz    , nb_cmp, list_cmp, nb_equa,&
                  tabl_equa, list_equa)
!
implicit none
!
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/select_dof.h"
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
    integer, intent(in) :: nb_cmp
    integer, intent(in) :: nb_equa
    character(len=*), intent(in) :: typesd
    character(len=*), intent(in) :: resuz
    character(len=8), target, intent(in) :: list_cmp(nb_cmp)
    integer, target, optional, intent(inout) :: tabl_equa(nb_equa, nb_cmp)
    integer, target, optional, intent(inout) :: list_equa(nb_equa)
!
! --------------------------------------------------------------------------------------------------
!
! In  typesd     : type of datastructure (chamno/nume_ddl)
! In  resu       : name of datastructure (chamno/nume_ddl)
! In  nb_cmp     : number of components
! In  list_cmp   : list of components
! In  nb_equa    : number of equations
! IO  tabl_equa  : table of equations
!      tabl_equa(IEQ,ICMP) =
!                   1 SI LE IEQ-EME CMP DE NUM A POUR NOM: list_cmp(ICMP)
!                   0 SINON
! IO  list_equa  : list of equations
!      list_equa(IEQ) =
!                   1 SI LE IEQ-EME CMP DE NUM A POUR NOM: list_cmp(ICMP)
!                   0 SINON
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: resu
    character(len=8), pointer :: list_cmp_p(:) => null()
    integer, pointer :: tabl_equa_p(:,:) => null()
    integer, pointer :: list_equa_p(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    resu = resuz
!
! - Table of equations
!
    if (present(tabl_equa)) then
        tabl_equa(1:nb_equa,1:nb_cmp) = 0
        tabl_equa_p => tabl_equa
    endif
!
! - List of equations
!
    if (present(list_equa)) then
        list_equa(1:nb_equa) = 0
        list_equa_p => list_equa
        ASSERT(nb_cmp.eq.1)
    endif
!
! - Get list of components
!
    list_cmp_p => list_cmp
!
! - Set dof in table
!
    if (typesd .eq. 'NUME_DDL') then
        if (present(tabl_equa)) then
            call select_dof(tabl_equa = tabl_equa_p, &
                               nume_ddlz = resu, &
                               nb_cmpz   = nb_cmp, list_cmp  = list_cmp_p)
        else
            call select_dof(list_equa = list_equa_p, &
                               nume_ddlz = resu, &
                               nb_cmpz   = nb_cmp, list_cmp  = list_cmp_p)
        endif
    else if (typesd .eq. 'CHAM_NO') then
        if (present(tabl_equa)) then
            call select_dof(tabl_equa = tabl_equa_p, &
                               chamnoz   = resu,&
                               nb_cmpz   = nb_cmp, list_cmp  = list_cmp_p)
        else
            call select_dof(list_equa = list_equa_p, &
                               chamnoz   = resu,&
                               nb_cmpz   = nb_cmp, list_cmp  = list_cmp_p)
        endif
    else
        ASSERT(.false.)
    endif
!
end subroutine
