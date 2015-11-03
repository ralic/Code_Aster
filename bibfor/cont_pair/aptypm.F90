subroutine aptypm(mesh     , elem_nume, elem_ndim, elem_nbnode, elem_type,&
                  elem_name)
!
implicit none
!
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/mmtypm.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: mesh
    integer, intent(in) :: elem_nume
    integer, intent(out) :: elem_ndim
    integer, intent(in) :: elem_nbnode
    character(len=8), intent(out) :: elem_type
    character(len=8), intent(out) :: elem_name
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing
!
! Get parameters for current element
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  elem_nume        : index of element in mesh datastructure
! Out elem_ndim        : dimension of element
! In  elem_nbnode      : number of nodes of element
! Out elem_type        : type of element
! Out elem_name        : name of element
!
! --------------------------------------------------------------------------------------------------
!
    call mmtypm(mesh, elem_nume, elem_nbnode, elem_type, elem_ndim)
    call jenuno(jexnum(mesh//'.NOMMAI', elem_nume), elem_name)
!
end subroutine
