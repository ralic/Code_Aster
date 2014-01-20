subroutine drz12d(noma, ligrmo, type_vale, nb_node, list_node,&
                  cmp_index_drz, type_lagr, lisrel)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/indik8.h"
#include "asterfort/afrela.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
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
    character(len=8), intent(in) :: noma
    character(len=19), intent(in) :: ligrmo
    character(len=4), intent(in) :: type_vale
    integer, intent(in) :: nb_node
    character(len=24), intent(in) :: list_node
    character(len=2), intent(in) :: type_lagr
    integer, intent(in) :: cmp_index_drz
    character(len=19), intent(in) :: lisrel
!
! --------------------------------------------------------------------------------------------------
!
! Loads - Affectation
!
! Apply transformation - 2D with at least one node with DRZ dof
!
! --------------------------------------------------------------------------------------------------
!
! In  noma          : mesh
! In  ligrmo        : <LIGREL> of model
! In  type_vale     : type of affected value
! In  nb_node       : number of nodes  applying translation
! In  list_node     : list of nodes applying translation
! In  tran          : vector defining translation
! In  cmp_index_drz : index in DEPL_R <GRANDEUR> for DRZ
! In  type_lagr     : choosing lagrange multipliers position
! In  lisrel        : list of relations
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_no
    integer :: jcoor
    integer ::   jprnm
    integer :: nbec
    integer :: jlino, numnoe_m, numnoe_a
    integer :: nb_maxi, nb_term
    real(kind=8) :: un, x, y
    real(kind=8) :: vale_real
    complex(kind=8) :: vale_cplx
    character(len=8) :: vale_fonc
    character(len=4) :: type_coef
    character(len=8) :: nomg, nomnoe_m, nomnoe_a
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
!
! - Information about <GRANDEUR>
!
    nomg = 'DEPL_R'
    call dismoi('NB_EC', nomg, 'GRANDEUR', repi=nbec)
    ASSERT(nbec.le.10)
    call jeveuo(ligrmo//'.PRNM', 'L', jprnm)
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
    nb_maxi = 3
    AS_ALLOCATE(vk8=lisno, size=nb_maxi)
    AS_ALLOCATE(vk8=lisddl, size=nb_maxi)
    AS_ALLOCATE(vr=coer, size=nb_maxi)
    AS_ALLOCATE(vc=coec, size=nb_maxi)
    AS_ALLOCATE(vr=direct, size=3*nb_maxi)
    AS_ALLOCATE(vi=dime, size=nb_maxi)
!
! - First node with DRZ node (reference)
!
    do i_no = 1, nb_node
        numnoe_m = zi(jlino+i_no-1)
        if (exisdg(zi(jprnm-1+(numnoe_m-1)*nbec+1),cmp_index_drz)) then
            numnoe_a = numnoe_m
            goto 30
        endif
    enddo
!
! - No node with DRZ: IMPOSSIBLE !!!
!
    ASSERT(.false.)
!
 30 continue
!
    call jenuno(jexnum(noma//'.NOMNOE', numnoe_a), nomnoe_a)
!
! - Loop on nodes
!
    do i_no = 1, nb_node
        numnoe_m = zi(jlino+i_no-1)
        call jenuno(jexnum(noma//'.NOMNOE', numnoe_m), nomnoe_m)
!
        if (numnoe_m .ne. numnoe_a) then
!
! --------- Distances: x = DX(A) - DX(M) and y = DY(A) - DY(M)
!
            x = zr(jcoor-1+3*(numnoe_m-1)+1) - zr(jcoor-1+3*(numnoe_a-1)+1)
            y = zr(jcoor-1+3*(numnoe_m-1)+2) - zr(jcoor-1+3*(numnoe_a-1)+2)
!
! --------- First relation: DX(M) - DX(A) + Y*DRZ(A) = 0
!
            nb_term = 3
            lisno(1) = nomnoe_m
            lisno(2) = nomnoe_a
            lisno(3) = nomnoe_a
            lisddl(1) = 'DX'
            lisddl(2) = 'DX'
            lisddl(3) = 'DRZ'
            coer(1) = un
            coer(2) = -un
            coer(3) = y
!
! --------- Compute linear relation
!
            call afrela(coer, coec, lisddl, lisno, dime,&
                        direct, nb_term, vale_real, vale_cplx, vale_fonc,&
                        type_coef, type_vale, type_lagr, 0.d0, lisrel)
!
! --------- Second relation: DY(M) - DY(A) - X*DRZ(A) = 0
!
            nb_term = 3
            lisno(1) = nomnoe_m
            lisno(2) = nomnoe_a
            lisno(3) = nomnoe_a
            lisddl(1) = 'DY'
            lisddl(2) = 'DY'
            lisddl(3) = 'DRZ'
            coer(1) = un
            coer(2) = -un
            coer(3) = -x
!
! --------- Compute linear relation
!
            call afrela(coer, coec, lisddl, lisno, dime,&
                        direct, nb_term, vale_real, vale_cplx, vale_fonc,&
                        type_coef, type_vale, type_lagr, 0.d0, lisrel)
!
! --------- Third relation: DRZ(M) - DRZ(A)  = 0
!
            if (exisdg(zi(jprnm-1+(numnoe_m-1)*nbec+1),cmp_index_drz)) then
                nb_term = 2
                lisno(1) = nomnoe_m
                lisno(2) = nomnoe_a
                lisddl(1) = 'DRZ'
                lisddl(2) = 'DRZ'
                coer(1) = un
                coer(2) = -un
!
! ------------- Compute linear relation
!
                call afrela(coer, coec, lisddl, lisno, dime,&
                            direct, nb_term, vale_real, vale_cplx, vale_fonc,&
                            type_coef, type_vale, type_lagr, 0.d0, lisrel)
            endif
        endif
    end do
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
