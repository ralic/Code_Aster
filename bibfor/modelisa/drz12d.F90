subroutine drz12d(noma, ligrmo, type_vale, nb_node, list_node,  &
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
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=8), intent(in)  :: noma
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
    integer :: i_no, ier
    integer :: jcoor, jliscc, jliscr, jlisdi, jlisdl
    integer :: jlisdm, jlisno, jprnm
    integer :: nbec
    integer :: jlino, numnoe_m, numnoe_a
    integer :: nb_maxi, nb_term
    real(kind=8) :: un, x, y
    real(kind=8) :: vale_real
    complex(kind=8) :: vale_cplx
    character(len=8) :: vale_fonc
    character(len=4) :: type_coef
    character(len=8) :: nomg, nomnoe_m, nomnoe_a, k8bid
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
    call dismoi('F', 'NB_EC', nomg, 'GRANDEUR', nbec,&
                k8bid, ier)
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
    call wkvect('&&DRZ12D.LISNO', 'V V K8', nb_maxi, jlisno)
    call wkvect('&&DRZ12D.LISDDL', 'V V K8', nb_maxi, jlisdl)
    call wkvect('&&DRZ12D.COER', 'V V R', nb_maxi, jliscr)
    call wkvect('&&DRZ12D.COEC', 'V V C', nb_maxi, jliscc)
    call wkvect('&&DRZ12D.DIRECT', 'V V R', 3*nb_maxi, jlisdi)
    call wkvect('&&DRZ12D.DIME', 'V V I', nb_maxi, jlisdm)
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
30  continue
!
    call jenuno(jexnum(noma//'.NOMNOE', numnoe_a), nomnoe_a)
!
! - Loop on nodes
!
    do i_no = 1,nb_node
        numnoe_m = zi(jlino+i_no-1)
        call jenuno(jexnum(noma//'.NOMNOE', numnoe_m), nomnoe_m)
!
        if (numnoe_m.ne.numnoe_a) then
!
! --------- Distances: x = DX(A) - DX(M) and y = DY(A) - DY(M)
!
            x = zr(jcoor-1+3*(numnoe_m-1)+1) - zr(jcoor-1+3*(numnoe_a-1)+1)
            y = zr(jcoor-1+3*(numnoe_m-1)+2) - zr(jcoor-1+3*(numnoe_a-1)+2)
!
! --------- First relation: DX(M) - DX(A) + Y*DRZ(A) = 0
!
            nb_term = 3
            zk8(jlisno+1-1) = nomnoe_m
            zk8(jlisno+2-1) = nomnoe_a
            zk8(jlisno+3-1) = nomnoe_a
            zk8(jlisdl+1-1) = 'DX'
            zk8(jlisdl+2-1) = 'DX'
            zk8(jlisdl+3-1) = 'DRZ'
            zr(jliscr+1-1) = un
            zr(jliscr+2-1) = -un
            zr(jliscr+3-1) = y
!
! --------- Compute linear relation
!
            call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                        zr(jlisdi), nb_term, vale_real, vale_cplx, vale_fonc,&
                        type_coef, type_vale, type_lagr, 0.d0, lisrel)
!
! --------- Second relation: DY(M) - DY(A) - X*DRZ(A) = 0
!
            nb_term = 3
            zk8(jlisno+1-1) = nomnoe_m
            zk8(jlisno+2-1) = nomnoe_a
            zk8(jlisno+3-1) = nomnoe_a
            zk8(jlisdl+1-1) = 'DY'
            zk8(jlisdl+2-1) = 'DY'
            zk8(jlisdl+3-1) = 'DRZ'
            zr(jliscr+1-1) = un
            zr(jliscr+2-1) = -un
            zr(jliscr+3-1) = -x
!
! --------- Compute linear relation
!
            call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                        zr(jlisdi), nb_term, vale_real, vale_cplx, vale_fonc,&
                        type_coef, type_vale, type_lagr, 0.d0, lisrel)
!
! --------- Third relation: DRZ(M) - DRZ(A)  = 0
!
            if (exisdg(zi(jprnm-1+(numnoe_m-1)*nbec+1),cmp_index_drz)) then
                nb_term = 2
                zk8(jlisno+1-1) = nomnoe_m
                zk8(jlisno+2-1) = nomnoe_a
                zk8(jlisdl+1-1) = 'DRZ'
                zk8(jlisdl+2-1) = 'DRZ'
                zr(jliscr+1-1) = un
                zr(jliscr+2-1) = -un
!
! ------------- Compute linear relation
!
                call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                            zr(jlisdi), nb_term, vale_real, vale_cplx, vale_fonc,&
                            type_coef, type_vale, type_lagr, 0.d0, lisrel)
            endif
        endif
    end do
!
    call jedetr('&&DRZ12D.LISNO')
    call jedetr('&&DRZ12D.LISDDL')
    call jedetr('&&DRZ12D.COER')
    call jedetr('&&DRZ12D.COEC')
    call jedetr('&&DRZ12D.DIRECT')
    call jedetr('&&DRZ12D.DIME')
!
    call jedema()
end subroutine
