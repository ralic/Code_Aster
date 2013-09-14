subroutine drz02d(noma, type_vale, dist_mini, nb_node, list_node,&
                  type_lagr, lisrel)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/afrela.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
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
    character(len=8), intent(in) :: noma
    character(len=4), intent(in) :: type_vale
    real(kind=8), intent(in) :: dist_mini
    integer, intent(in) :: nb_node
    character(len=24), intent(in) :: list_node
    character(len=2), intent(in) :: type_lagr
    character(len=19), intent(in) :: lisrel
!
! --------------------------------------------------------------------------------------------------
!
! Loads - Affectation
!
! Apply transformtion - 2D with without nodes with DRZ dof
!
! --------------------------------------------------------------------------------------------------
!
! In  noma          : mesh
! In  type_vale     : type of affected value
! In  dist_mini     : minimum distance to detect nodes in same place
! In  nb_node       : number of nodes  applying translation
! In  list_node     : list of nodes applying translation
! In  tran          : vector defining translation
! In  type_lagr     : choosing lagrange multipliers position
! In  lisrel        : list of relations
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_no
    integer :: numnoe_m, numnoe_a, numnoe_b
    character(len=8) :: nomnoe_m, nomnoe_a, nomnoe_b
    integer :: jcoor, jliscc, jliscr, jlino
    integer :: jlisdi, jlisdl, jlisdm, jlisno
    integer :: nb_maxi, nb_term
    real(kind=8) :: d2, d21, un, x, x0, y, y0
    real(kind=8) :: vale_real
    complex(kind=8) :: vale_cplx
    character(len=8) :: vale_fonc
    real(kind=8) :: lab
    character(len=4) :: type_coef
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    vale_fonc = '&FOZERO'
    vale_real = 0.d0
    vale_cplx = (0.d0,0.d0)
    un = 1.d0
    type_coef = 'REEL'
    ASSERT(type_vale.ne.'COMP')
    ASSERT(dist_mini .gt. 0.d0)
!
! - Nodes coordinates
!
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
!
! - List of nodes to apply linear relation
!
    call jeveuo(list_node, 'L', jlino)
!
! - Working vectors
!
    nb_maxi = 5
    call wkvect('&&DRZ02D.LISNO', 'V V K8', nb_maxi, jlisno)
    call wkvect('&&DRZ02D.LISDDL', 'V V K8', nb_maxi, jlisdl)
    call wkvect('&&DRZ02D.COER', 'V V R', nb_maxi, jliscr)
    call wkvect('&&DRZ02D.COEC', 'V V C', nb_maxi, jliscc)
    call wkvect('&&DRZ02D.DIRECT', 'V V R', 3*nb_maxi, jlisdi)
    call wkvect('&&DRZ02D.DIME', 'V V I', nb_maxi, jlisdm)
!
! - First node in list for first reference node
!
    numnoe_a = zi(jlino+1-1)
    call jenuno(jexnum(noma//'.NOMNOE', numnoe_a), nomnoe_a)
!
! - Detect if all nodes are in same place
!
    do i_no = 2, nb_node
        numnoe_m = zi(jlino+i_no-1)
        x = zr(jcoor-1+3* (numnoe_m-1)+1) - zr(jcoor-1+3*(numnoe_a-1)+1)
        y = zr(jcoor-1+3* (numnoe_m-1)+2) - zr(jcoor-1+3*(numnoe_a-1)+2)
        numnoe_b = numnoe_m
        x0 = x
        y0 = y
        lab=sqrt(x*x+y*y)
        if (abs(lab) .gt. dist_mini) goto 40
    enddo
!
! - If all nodes are in same place
!
    do i_no = 2, nb_node
        numnoe_m = zi(jlino+i_no-1)
        call jenuno(jexnum(noma//'.NOMNOE', numnoe_m), nomnoe_m)
!
        nb_term = 2
        zr(jliscr+1-1) = un
        zr(jliscr+2-1) = -un
        zk8(jlisno+1-1) = nomnoe_m
        zk8(jlisno+2-1) = nomnoe_a
!
! ----- First relation: DX(M) - DX(A) = 0
!
        zk8(jlisdl+1-1) = 'DX'
        zk8(jlisdl+2-1) = 'DX'
!
! ----- Compute linear relation
!
        call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                    zr(jlisdi), nb_term, vale_real, vale_cplx, vale_fonc,&
                    type_coef, type_vale, type_lagr, 0.d0, lisrel)
!
! ----- First relation: DY(M) -DY(A) = 0
!
        zk8(jlisdl+1-1) = 'DY'
        zk8(jlisdl+2-1) = 'DY'
!
! ----- Compute linear relation
!
        call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                    zr(jlisdi), nb_term, vale_real, vale_cplx, vale_fonc,&
                    type_coef, type_vale, type_lagr, 0.d0, lisrel)
    end do
    goto 999
!
! - If all nodes are not in same place
!
40  continue
!
    call jenuno(jexnum(noma//'.NOMNOE', numnoe_b), nomnoe_b)
    do i_no = 2, nb_node
        numnoe_m = zi(jlino+i_no-1)
        call jenuno(jexnum(noma//'.NOMNOE', numnoe_m), nomnoe_m)
        if (numnoe_m .ne. numnoe_b) then
!
            x = zr(jcoor-1+3*(numnoe_m-1)+1) - zr(jcoor-1+3*(numnoe_a-1)+1)
            y = zr(jcoor-1+3*(numnoe_m-1)+2) - zr(jcoor-1+3*(numnoe_a-1)+2)
!
            nb_term = 5
            zk8(jlisno+1-1) = nomnoe_m
            zk8(jlisno+2-1) = nomnoe_a
            zk8(jlisno+3-1) = nomnoe_b
            zk8(jlisno+4-1) = nomnoe_a
            zk8(jlisno+5-1) = nomnoe_b
!
! --------- First relation: DX(M) + DX(A)*(-1+Y0*Y/(X0**2+Y0**2)) - DX(B)*Y*Y0/(X0**2+Y0**2)
! ---------                 + DY(B)*Y*X0/(X0**2+Y0**2) - DY(A)*Y*X0/(X0**2+Y0**2) = 0
!
            zk8(jlisdl+1-1) = 'DX'
            zk8(jlisdl+2-1) = 'DX'
            zk8(jlisdl+3-1) = 'DX'
            zk8(jlisdl+4-1) = 'DY'
            zk8(jlisdl+5-1) = 'DY'
            d2 = x0*x0 + y0*y0
            d21 = 1.d0/d2
            zr(jliscr+1-1) = un
            zr(jliscr+2-1) = -un + y0*y*d21
            zr(jliscr+3-1) = -y0*y*d21
            zr(jliscr+4-1) = -x0*y*d21
            zr(jliscr+5-1) = x0*y*d21
!
! --------- Compute linear relation
!
            call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                        zr(jlisdi), nb_term, vale_real, vale_cplx, vale_fonc,&
                        type_coef, type_vale, type_lagr, 0.d0, lisrel)
!
! --------- Second relation: DY(M) + DY(A)*(-1+X0*X/(X0**2+Y0**2)) + DX(B)*X*Y0/(X0**2+Y0**2)
! ---------                  - DX(A)*Y0*X/(X0**2+Y0**2) - DY(B)*X*X0/(X0**2+Y0**2) = 0
!
            zk8(jlisdl+1-1) = 'DY'
            zk8(jlisdl+2-1) = 'DY'
            zk8(jlisdl+3-1) = 'DX'
            zk8(jlisdl+4-1) = 'DX'
            zk8(jlisdl+5-1) = 'DY'
            zr(jliscr+1-1) = un
            zr(jliscr+2-1) = -un + x0*x*d21
            zr(jliscr+3-1) = y0*x*d21
            zr(jliscr+4-1) = -y0*x*d21
            zr(jliscr+5-1) = -x0*x*d21
!
! --------- Compute linear relation
!
            call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                        zr(jlisdi), nb_term, vale_real, vale_cplx, vale_fonc,&
                        type_coef, type_vale, type_lagr, 0.d0, lisrel)
        endif
    enddo
!
! - Third relation: -DX(A)*X0 - DY(A)*Y0 + DX(B)*X0 + DY(B)*Y0 = 0
!
    nb_term = 4
    zk8(jlisno+1-1) = nomnoe_a
    zk8(jlisno+2-1) = nomnoe_a
    zk8(jlisno+3-1) = nomnoe_b
    zk8(jlisno+4-1) = nomnoe_b
    zk8(jlisdl+1-1) = 'DX'
    zk8(jlisdl+2-1) = 'DY'
    zk8(jlisdl+3-1) = 'DX'
    zk8(jlisdl+4-1) = 'DY'
    zr(jliscr+1-1) = -x0
    zr(jliscr+2-1) = -y0
    zr(jliscr+3-1) = x0
    zr(jliscr+4-1) = y0
!
! - Compute linear relation
!
    call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                zr(jlisdi), nb_term, vale_real, vale_cplx, vale_fonc,&
                type_coef, type_vale, type_lagr, 0.d0, lisrel)
!
999  continue
!
    call jedetr('&&DRZ02D.LISNO')
    call jedetr('&&DRZ02D.LISDDL')
    call jedetr('&&DRZ02D.COER')
    call jedetr('&&DRZ02D.COEC')
    call jedetr('&&DRZ02D.DIRECT')
    call jedetr('&&DRZ02D.DIME')
!
    call jedema()
end subroutine
