subroutine drz13d(noma, ligrmo, type_vale, nb_node, list_node, &
                  cmp_index_dx, cmp_index_dy, cmp_index_dz, cmp_index_drx, cmp_index_dry,&
                  cmp_index_drz, type_lagr, lisrel)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/afrela.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=8), intent(in)  :: noma
    character(len=19), intent(in) :: ligrmo
    character(len=4), intent(in) :: type_vale
    integer, intent(in) :: nb_node
    character(len=24), intent(in) :: list_node
    character(len=2), intent(in) :: type_lagr
    integer, intent(in) :: cmp_index_dx
    integer, intent(in) :: cmp_index_dy
    integer, intent(in) :: cmp_index_dz
    integer, intent(in) :: cmp_index_drx
    integer, intent(in) :: cmp_index_dry
    integer, intent(in) :: cmp_index_drz
    character(len=19), intent(in) :: lisrel
!
! --------------------------------------------------------------------------------------------------
!
! Loads - Affectation
!
! Apply transformation - 3D with at least one node has DRX, DRY and DRZ dof
!
! --------------------------------------------------------------------------------------------------
!
! In  noma          : mesh
! In  ligrmo        : <LIGREL> of model
! In  type_vale     : type of affected value
! In  nb_node       : number of nodes  applying translation
! In  list_node     : list of nodes applying translation
! In  tran          : vector defining translation
! In  cmp_index_dx  : index in DEPL_R <GRANDEUR> for DX
! In  cmp_index_dy  : index in DEPL_R <GRANDEUR> for DY
! In  cmp_index_dz  : index in DEPL_R <GRANDEUR> for DZ
! In  cmp_index_drx : index in DEPL_R <GRANDEUR> for DRX
! In  cmp_index_dry : index in DEPL_R <GRANDEUR> for DRY
! In  cmp_index_drz : index in DEPL_R <GRANDEUR> for DRZ
! In  type_lagr     : choosing lagrange multipliers position
! In  lisrel        : list of relations
!
! --------------------------------------------------------------------------------------------------
!
!
    integer :: i_no, ier
    integer :: jcoor, jliscc, jliscr, jlisdi, jlisdl
    integer :: jlisdm, jlisno, jprnm
    integer :: nbec
    integer :: jlino, numnoe_m, numnoe_a
    integer :: nb_maxi, nb_term
    real(kind=8) :: un, x, y, z
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
    nb_maxi = 4
    call wkvect('&&DRZ13D.LISNO', 'V V K8', nb_maxi, jlisno)
    call wkvect('&&DRZ13D.LISDDL', 'V V K8', nb_maxi, jlisdl)
    call wkvect('&&DRZ13D.COER', 'V V R', nb_maxi, jliscr)
    call wkvect('&&DRZ13D.COEC', 'V V C', nb_maxi, jliscc)
    call wkvect('&&DRZ13D.DIRECT', 'V V R', 3*nb_maxi, jlisdi)
    call wkvect('&&DRZ13D.DIME', 'V V I', nb_maxi, jlisdm)
!
! - First node with DRX/DRY/DRZ node (reference)
!
    do i_no = 1, nb_node
        numnoe_m = zi(jlino+i_no-1)
        if (exisdg(zi(jprnm-1+(numnoe_m-1)*nbec+1),cmp_index_drx).and.&
            exisdg(zi(jprnm-1+(numnoe_m-1)*nbec+1),cmp_index_dry).and.&
            exisdg(zi(jprnm-1+(numnoe_m-1)*nbec+1),cmp_index_drz)) then
            numnoe_a = numnoe_m
            goto 30
        endif
    enddo
!
! - No node with DRX/DRY/DRZ: IMPOSSIBLE !
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
! --------- Distances: x = DX(A) - DX(M) and y = DY(A) - DY(M) and z = DZ(A) - DZ(M)
!
            x = zr(jcoor-1+3*(numnoe_m-1)+1) - zr(jcoor-1+3*(numnoe_a-1)+1)
            y = zr(jcoor-1+3*(numnoe_m-1)+2) - zr(jcoor-1+3*(numnoe_a-1)+2)
            z = zr(jcoor-1+3*(numnoe_m-1)+3) - zr(jcoor-1+3*(numnoe_a-1)+3)
!
! --------- Linear relations for translation dof
!
            if (exisdg(zi(jprnm-1+(numnoe_m-1)*nbec+1),cmp_index_dx).and.&
                exisdg(zi(jprnm-1+(numnoe_m-1)*nbec+1),cmp_index_dy).and.&
                exisdg(zi(jprnm-1+(numnoe_m-1)*nbec+1),cmp_index_dz)) then
!
                nb_term = 4
                zk8(jlisno+1-1) = nomnoe_m
                zk8(jlisno+2-1) = nomnoe_a
                zk8(jlisno+3-1) = nomnoe_a
                zk8(jlisno+4-1) = nomnoe_a
!
! ------------- First relation: DX(M) - DX(A) - Z*DRY(A) + Y*DRZ(A) =0
!
                zk8(jlisdl+1-1) = 'DX'
                zk8(jlisdl+2-1) = 'DX'
                zk8(jlisdl+3-1) = 'DRY'
                zk8(jlisdl+4-1) = 'DRZ'
                zr(jliscr+1-1) = un
                zr(jliscr+2-1) = -un
                zr(jliscr+3-1) = -z
                zr(jliscr+4-1) = y
!
! ------------- Compute linear relation
!
                call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                            zr(jlisdi), nb_term, vale_real, vale_cplx, vale_fonc,&
                            type_coef, type_vale, type_lagr, 0.d0, lisrel)
!
! ------------- Second relation: DY(M) - DY(A) - X*DRZ(A) + Z*DRX(A) =0
!
                zk8(jlisdl+1-1) = 'DY'
                zk8(jlisdl+2-1) = 'DY'
                zk8(jlisdl+3-1) = 'DRZ'
                zk8(jlisdl+4-1) = 'DRX'
                zr(jliscr+1-1) = un
                zr(jliscr+2-1) = -un
                zr(jliscr+3-1) = -x
                zr(jliscr+4-1) = z
!
! ------------- Compute linear relation
!
                call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                            zr(jlisdi), nb_term, vale_real, vale_cplx, vale_fonc,&
                            type_coef, type_vale, type_lagr, 0.d0, lisrel)
!
! ------------- Third relation: DZ(M) - DZ(A) - Y*DRX(A) + X*DRY(A) =0
!
                zk8(jlisdl+1-1) = 'DZ'
                zk8(jlisdl+2-1) = 'DZ'
                zk8(jlisdl+3-1) = 'DRX'
                zk8(jlisdl+4-1) = 'DRY'
                zr(jliscr+1-1) = un
                zr(jliscr+2-1) = -un
                zr(jliscr+3-1) = -y
                zr(jliscr+4-1) = x
!
! ------------- Compute linear relation
!
                call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                            zr(jlisdi), nb_term, vale_real, vale_cplx, vale_fonc,&
                            type_coef, type_vale, type_lagr, 0.d0, lisrel)
            endif
!
! --------- Linear relations for rotation dof
!
            if (exisdg(zi(jprnm-1+(numnoe_m-1)*nbec+1),cmp_index_drx).and.&
                exisdg(zi(jprnm-1+(numnoe_m-1)*nbec+1),cmp_index_dry).and.&
                exisdg(zi(jprnm-1+(numnoe_m-1)*nbec+1),cmp_index_drz)) then
!
                nb_term = 2
                zk8(jlisno+1-1) = nomnoe_m
                zk8(jlisno+2-1) = nomnoe_a
!
! ------------- Fourth relation: DRX(M) - DRX(A)  = 0
!
                zk8(jlisdl+1-1) = 'DRX'
                zk8(jlisdl+2-1) = 'DRX'
                zr(jliscr+1-1) = un
                zr(jliscr+2-1) = -un
!
! ------------- Compute linear relation
!
                call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                            zr(jlisdi), nb_term, vale_real, vale_cplx, vale_fonc,&
                            type_coef, type_vale, type_lagr, 0.d0, lisrel)

!
! ------------- Fifth relation: DRY(M) - DRY(A)  = 0
!
                zk8(jlisdl+1-1) = 'DRY'
                zk8(jlisdl+2-1) = 'DRY'
                zr(jliscr+1-1) = un
                zr(jliscr+2-1) = -un
!
! ------------- Compute linear relation
!
                call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                            zr(jlisdi), nb_term, vale_real, vale_cplx, vale_fonc,&
                            type_coef, type_vale, type_lagr, 0.d0, lisrel)
!
! ------------- Sixth relation: DRZ(M) - DRZ(A)  = 0
!
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
    call jedetr('&&DRZ13D.LISNO')
    call jedetr('&&DRZ13D.LISDDL')
    call jedetr('&&DRZ13D.COER')
    call jedetr('&&DRZ13D.COEC')
    call jedetr('&&DRZ13D.DIRECT')
    call jedetr('&&DRZ13D.DIME')
!
    call jedema()
end subroutine
