subroutine drzrot(noma, ligrmo, nb_node, list_node, type_lagr,&
                  tran, lisrel)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/afrela.h"
#include "asterfort/assert.h"
#include "asterfort/calirg.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=8), intent(in)  :: noma
    character(len=19), intent(in) :: ligrmo
    integer, intent(in) :: nb_node
    character(len=24), intent(in) :: list_node
    character(len=2), intent(in) :: type_lagr
    real(kind=8), intent(in) :: tran(3)
    character(len=19), intent(in) :: lisrel
!
! --------------------------------------------------------------------------------------------------
!
! Loads - Affectation
!
! Apply translation
!
! --------------------------------------------------------------------------------------------------
!
! In  noma      : mesh
! In  ligrmo    : <LIGREL> of model
! In  nb_node   : number of nodes  applying translation
! In  list_node : list of nodes applying translation
! In  tran      : vector defining translation
! In  type_lagr : choosing lagrange multipliers position
! In  lisrel    : list of relations
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: nomnoe_m, k8bid, nomg
    character(len=8) :: cmp_name, nomcmp(3)
    character(len=24) :: geom2
    logical :: lrota
    complex(kind=8) :: c16bid
    real(kind=8) :: vale_real, r8bid, mrota(3, 3)
    complex(kind=8) :: coef_cplx_unit
    real(kind=8) :: coef_real_unit
    integer :: jlino, jcoor, jprnm, jgeom2, jnom
    integer :: i_no, i_cmp, i
    integer :: ier, cmp_index
    integer :: nbcmp, nbec
    integer :: numnoe_m
    real(kind=8) :: cent(3)
    logical :: l_angl_naut, l_tran
    real(kind=8) :: angl_naut(3)
!
    data nomcmp /'DX', 'DY', 'DZ' /
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    coef_cplx_unit = (1.d0,0.d0)
    coef_real_unit = 1.d0
    l_tran = .true.
    l_angl_naut = .false.
    do i = 1, 3
        cent(i) = 0.d0
    end do
!
! - Information about <GRANDEUR>
!
    nomg = 'DEPL_R'
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomg), 'L', jnom)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomg), 'LONMAX', nbcmp)
    call dismoi('F', 'NB_EC', nomg, 'GRANDEUR', nbec,&
                k8bid, ier)
    ASSERT(nbec.le.10)
    call jeveuo(ligrmo//'.PRNM', 'L', jprnm)
!
! - Access to geometry and list of nodes
!
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
    call jeveuo(list_node, 'L', jlino)
    geom2 = '&&DRZROT.GEOM_TRANSF'
!
! - Apply transformation
!
    call calirg(noma, nb_node, list_node, tran, cent, &
                l_angl_naut, angl_naut, geom2, lrota, mrota)
    ASSERT(.not.lrota)
    call jeveuo(geom2, 'L', jgeom2)
!
! - Loop on nodes
!
    do i_no = 1,nb_node
        numnoe_m = zi(jlino+i_no-1)
        call jenuno(jexnum(noma//'.NOMNOE', numnoe_m), nomnoe_m)
!
! ----- Relation DU(M) = 0
!
        do i_cmp = 1, 3
            cmp_name  = nomcmp(i_cmp)
            cmp_index = indik8(zk8(jnom), cmp_name, 1, nbcmp)
            ASSERT(cmp_index.gt.0)
            if (exisdg(zi(jprnm-1+(numnoe_m-1)*nbec+1),cmp_index)) then
                vale_real = zr(jgeom2-1+3*(i_no-1)+i_cmp) - &
                            zr(jcoor -1+3*(i_no-1)+i_cmp)
                call afrela(coef_real_unit, coef_cplx_unit, cmp_name, nomnoe_m, 0,&
                            r8bid, 1, vale_real, c16bid, ' ',&
                            'REEL', 'REEL', type_lagr, 0.d0, lisrel)
            endif
        end do
    end do
!
    call jedetr(geom2)
    call jedema()
end subroutine
