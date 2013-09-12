subroutine matloc(mesh, connex_inv, keywordfact, iocc, node_nume,&
                  node_name, nb_repe_elem, list_repe_elem, matr_glob_loca)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterc/r8prem.h"
#include "asterfort/angvx.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/matrot.h"
#include "asterfort/u2mess.h"
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
!
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: connex_inv
    character(len=16), intent(in) :: keywordfact
    integer, intent(in) :: iocc
    character(len=8), intent(in) :: node_name
    integer, intent(in) :: node_nume
    integer, intent(in) :: nb_repe_elem
    integer, intent(in) :: list_repe_elem(*)
    real(kind=8), intent(out) :: matr_glob_loca(3, 3)
!
! --------------------------------------------------------------------------------------------------
!
! Loads affectation - DDL_POUTRE
!
! Computation of matrix of transition from global to local coordinate system at node
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh           : meshing
! In  connex_inv     : inverse connectivity of mesh (nodes -> elements)
! In  keywordfact    : factor keyword to read
! In  iocc           : factor keyword index in AFFE_CHAR_MECA
! In  node_nume      : number of node if mesh
! In  node_name      : name of node
! In  nb_repe_elem   : number of elements of local coordinate system
! In  list_elem      : list of elements of local coordinate sysem
! Out matr_glob_loca : matrix of transition from global to local coordinate system
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nocc, i, j, jadrm, jtypma, jadrvlc, jcoord
    integer :: nb_conn_elem, elem_nume
    integer :: node_nume_1, node_nume_2
    real(kind=8) :: vx(3), vy(3), vz(3), vecty(3), vxn, vyn, vyp, dgrd, angl_naut(3)
    real(kind=8) :: alpha, beta, gamma
    character(len=8) :: k8bid, type_elem, elem_name, valk(2)
    character(len=24) :: connex
    integer :: iarg
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    dgrd = r8dgrd()
!
! - Elements connected to node
!
    call jelira(jexnum(connex_inv, node_nume), 'LONMAX', nb_conn_elem, k8bid)
    call jeveuo(jexnum(connex_inv, node_nume), 'L', jadrm)
!
! - Access to mesh
!
    connex = mesh//'.CONNEX'
    call jeveuo(mesh//'.TYPMAIL', 'L', jtypma)
    call jeveuo(mesh//'.COORDO    .VALE', 'L', jcoord)
!
! - Get element connected to node
!
    if (nb_conn_elem .eq. 1) then
        elem_nume = zi(jadrm)
        call jenuno(jexnum(mesh//'.NOMMAI', elem_nume), elem_name)
    else
        if (nb_repe_elem .eq. 0) call u2mesk('F', 'CHARGES2_37', 1, node_name)
        if (nb_repe_elem .ne. 1) call u2mesk('F', 'CHARGES2_38', 1, node_name)
        do i = 1, nb_repe_elem
            elem_nume = list_repe_elem(i)
            do j = 1, nb_conn_elem
                if (zi(jadrm+j-1) .eq. elem_nume) goto 24
            enddo
        enddo
        call u2mesk('F', 'CHARGES2_39', 1, node_name)
24      continue
    endif
!
! - Check element type
!
    call jenuno(jexnum('&CATA.TM.NOMTM', zi(jtypma+elem_nume-1)), type_elem)
    call jenuno(jexnum(mesh//'.NOMMAI', elem_nume), elem_name)
    valk(2) = node_name
    valk(1) = elem_name
    if (type_elem(1:3) .ne. 'SEG') call u2mesk('F', 'CHARGES2_40', 2, valk)
!
! - Get nodes of element connected
!
    call jeveuo(jexnum(connex, elem_nume), 'L', jadrvlc)
    if (node_nume .eq. zi(jadrvlc)) then
        node_nume_1 = zi(jadrvlc+1)
        node_nume_2 = zi(jadrvlc)
    else
        node_nume_1 = zi(jadrvlc)
        node_nume_2 = zi(jadrvlc+1)
    endif
!
! - Construct colinear vector to element
!
    vx(1) = zr(jcoord+3*(node_nume_2-1) ) - zr(jcoord+3*(node_nume_1-1) )
    vx(2) = zr(jcoord+3*(node_nume_2-1)+1) - zr(jcoord+3*(node_nume_1-1)+1)
    vx(3) = zr(jcoord+3*(node_nume_2-1)+2) - zr(jcoord+3*(node_nume_1-1)+2)
    vxn = sqrt( vx(1)**2 + vx(2)**2 + vx(3)**2 )
    valk(2) = node_name
    valk(1) = elem_name
    if (vxn .le. r8prem()) call u2mesk('F', 'CHARGES2_41', 2, valk)
    vx(1) = vx(1) / vxn
    vx(2) = vx(2) / vxn
    vx(3) = vx(3) / vxn
!
! - Is VECT_Y ?
!
    call getvr8(keywordfact, 'VECT_Y', iocc=iocc, nbval=3, vect=vecty,&
                nbret=nocc)
    if (nocc .ne. 0) then
!
! ----- VECT_Y
!
        vy(1) = vecty(1)
        vy(2) = vecty(2)
        vy(3) = vecty(3)
!
! ----- Projection
!
        vyp = vx(1)*vy(1) + vx(2)*vy(2) + vx(3)*vy(3)
        vy(1) = vy(1) - vyp*vx(1)
        vy(2) = vy(2) - vyp*vx(2)
        vy(3) = vy(3) - vyp*vx(3)
        vyn = sqrt(vy(1)**2+vy(2)**2+vy(3)**2)
        vy(1) = vy(1) / vyn
        vy(2) = vy(2) / vyn
        vy(3) = vy(3) / vyn
!
! ----- Tangent vector
!
        vz(1) = vx(2)*vy(3) - vy(2)*vx(3)
        vz(2) = vx(3)*vy(1) - vy(3)*vx(1)
        vz(3) = vx(1)*vy(2) - vy(1)*vx(2)
!
! ----- Transformation matrix
!
        do i = 1, 3
            matr_glob_loca(1,i) = vx(i)
            matr_glob_loca(2,i) = vy(i)
            matr_glob_loca(3,i) = vz(i)
        enddo
        goto 999
    endif
!
! --- SI ANGL_VRIL
!
    call getvr8(keywordfact, 'ANGL_VRIL', iocc=iocc, scal=gamma, nbret=nocc)
    if (nocc .ne. 0) then
!
! ----- Get nautic angles
!
        call angvx(vx, alpha, beta)
        angl_naut(1) = alpha
        angl_naut(2) = beta
        angl_naut(3) = gamma * dgrd
!
! ----- Transformation matrix
!
        call matrot(angl_naut, matr_glob_loca)
        goto 999
    endif
!
999  continue
!
    call jedema()
!
end subroutine
