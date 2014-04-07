subroutine drz02d(noma, type_vale, dist_mini, nb_node, list_node,&
                  type_lagr, lisrel, nom_noeuds, type_transf)
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
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
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
    character(len=8), intent(out) :: nom_noeuds(:)
    character(len=1), intent(out) :: type_transf
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
! Out nom_noeuds    : nom des noeuds "maitres" pour la relation
! Out type_transf   : type de la transformation
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_no
    integer :: numnoe_m, numnoe_a, numnoe_b
    character(len=8) :: nomnoe_m, nomnoe_a, nomnoe_b
    integer :: jcoor,   jlino

    integer :: nb_maxi, nb_term
    real(kind=8) :: d2, d21, un, x, x0, y, y0
    real(kind=8) :: vale_real
    complex(kind=8) :: vale_cplx
    character(len=8) :: vale_fonc
    real(kind=8) :: lab
    character(len=4) :: type_coef
    complex(kind=8), pointer :: coec(:) => null()
    real(kind=8), pointer :: coer(:) => null()
    integer, pointer :: dime(:) => null()
    real(kind=8), pointer :: direct(:) => null()
    character(len=8), pointer :: lisddl(:) => null()
    character(len=8), pointer :: lisno(:) => null()
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
    type_transf = '1'
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
    AS_ALLOCATE(vk8=lisno, size=nb_maxi)
    AS_ALLOCATE(vk8=lisddl, size=nb_maxi)
    AS_ALLOCATE(vr=coer, size=nb_maxi)
    AS_ALLOCATE(vc=coec, size=nb_maxi)
    AS_ALLOCATE(vr=direct, size=3*nb_maxi)
    AS_ALLOCATE(vi=dime, size=nb_maxi)
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
        coer(1) = un
        coer(2) = -un
        lisno(1) = nomnoe_m
        lisno(2) = nomnoe_a
!
! ----- First relation: DX(M) - DX(A) = 0
!
        lisddl(1) = 'DX'
        lisddl(2) = 'DX'
!
! ----- Compute linear relation
!
        call afrela(coer, coec, lisddl, lisno, dime,&
                    direct, nb_term, vale_real, vale_cplx, vale_fonc,&
                    type_coef, type_vale, type_lagr, 0.d0, lisrel)
!
! ----- First relation: DY(M) -DY(A) = 0
!
        lisddl(1) = 'DY'
        lisddl(2) = 'DY'
!
! ----- Compute linear relation
!
        call afrela(coer, coec, lisddl, lisno, dime,&
                    direct, nb_term, vale_real, vale_cplx, vale_fonc,&
                    type_coef, type_vale, type_lagr, 0.d0, lisrel)
    end do
    goto 999
!
! - If all nodes are not in same place
!
40  continue
!
    call jenuno(jexnum(noma//'.NOMNOE', numnoe_b), nomnoe_b)
    nom_noeuds(2) = nomnoe_b
    type_transf = '2'
    do i_no = 2, nb_node
        numnoe_m = zi(jlino+i_no-1)
        call jenuno(jexnum(noma//'.NOMNOE', numnoe_m), nomnoe_m)
        if (numnoe_m .ne. numnoe_b) then
!
            x = zr(jcoor-1+3*(numnoe_m-1)+1) - zr(jcoor-1+3*(numnoe_a-1)+1)
            y = zr(jcoor-1+3*(numnoe_m-1)+2) - zr(jcoor-1+3*(numnoe_a-1)+2)
!
            nb_term = 5
            lisno(1) = nomnoe_m
            lisno(2) = nomnoe_a
            lisno(3) = nomnoe_b
            lisno(4) = nomnoe_a
            lisno(5) = nomnoe_b
!
! --------- First relation: DX(M) + DX(A)*(-1+Y0*Y/(X0**2+Y0**2)) - DX(B)*Y*Y0/(X0**2+Y0**2)
! ---------                 + DY(B)*Y*X0/(X0**2+Y0**2) - DY(A)*Y*X0/(X0**2+Y0**2) = 0
!
            lisddl(1) = 'DX'
            lisddl(2) = 'DX'
            lisddl(3) = 'DX'
            lisddl(4) = 'DY'
            lisddl(5) = 'DY'
            d2 = x0*x0 + y0*y0
            d21 = 1.d0/d2
            coer(1) = un
            coer(2) = -un + y0*y*d21
            coer(3) = -y0*y*d21
            coer(4) = -x0*y*d21
            coer(5) = x0*y*d21
!
! --------- Compute linear relation
!
            call afrela(coer, coec, lisddl, lisno, dime,&
                        direct, nb_term, vale_real, vale_cplx, vale_fonc,&
                        type_coef, type_vale, type_lagr, 0.d0, lisrel)
!
! --------- Second relation: DY(M) + DY(A)*(-1+X0*X/(X0**2+Y0**2)) + DX(B)*X*Y0/(X0**2+Y0**2)
! ---------                  - DX(A)*Y0*X/(X0**2+Y0**2) - DY(B)*X*X0/(X0**2+Y0**2) = 0
!
            lisddl(1) = 'DY'
            lisddl(2) = 'DY'
            lisddl(3) = 'DX'
            lisddl(4) = 'DX'
            lisddl(5) = 'DY'
            coer(1) = un
            coer(2) = -un + x0*x*d21
            coer(3) = y0*x*d21
            coer(4) = -y0*x*d21
            coer(5) = -x0*x*d21
!
! --------- Compute linear relation
!
            call afrela(coer, coec, lisddl, lisno, dime,&
                        direct, nb_term, vale_real, vale_cplx, vale_fonc,&
                        type_coef, type_vale, type_lagr, 0.d0, lisrel)
        endif
    enddo
!
! - Third relation: -DX(A)*X0 - DY(A)*Y0 + DX(B)*X0 + DY(B)*Y0 = 0
!
    nb_term = 4
    lisno(1) = nomnoe_a
    lisno(2) = nomnoe_a
    lisno(3) = nomnoe_b
    lisno(4) = nomnoe_b
    lisddl(1) = 'DX'
    lisddl(2) = 'DY'
    lisddl(3) = 'DX'
    lisddl(4) = 'DY'
    coer(1) = -x0
    coer(2) = -y0
    coer(3) = x0
    coer(4) = y0
!
! - Compute linear relation
!
    call afrela(coer, coec, lisddl, lisno, dime,&
                direct, nb_term, vale_real, vale_cplx, vale_fonc,&
                type_coef, type_vale, type_lagr, 0.d0, lisrel)
!
999 continue
    nom_noeuds(1) = nomnoe_a
!
    AS_DEALLOCATE(vk8=lisno)
    AS_DEALLOCATE(vk8=lisddl)
    AS_DEALLOCATE(vr=coer)
    AS_DEALLOCATE(vc=coec)
    AS_DEALLOCATE(vr=direct)
    AS_DEALLOCATE(vi=dime)
!
    call jedema()
end subroutine
