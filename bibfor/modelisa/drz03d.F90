subroutine drz03d(noma, type_vale, dist_mini, nb_node, list_node,&
                  type_lagr, lisrel, nom_noeuds, type_transf)
!
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/afrela.h"
#include "asterfort/assert.h"
#include "asterfort/char_soli_mat1.h"
#include "asterfort/char_soli_mat2.h"
#include "asterfort/char_soli_mat3.h"
#include "asterfort/drz03d_tria.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/provec.h"
#include "asterfort/utmess.h"
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
! =====================================================================
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
! Apply transformation - 3D without any rotation DRX, DRY and DRZ dof
!
! --------------------------------------------------------------------------------------------------
!
! In  noma          : mesh
! In  type_vale     : type of affected value
! In  dist_mini     : minimum distance to detect nodes in same place
! In  nb_node       : number of nodes applying transformation
! In  list_node     : list of nodes applying transformation
! In  tran          : vector defining translation
! In  type_lagr     : choosing lagrange multipliers position
! In  lisrel        : list of relations
! Out nom_noeuds    : nom des noeuds "maitres" pour la relation
! Out type_transf   : type de la transformation
!
! --------------------------------------------------------------------------------------------------
!
    integer :: jlino
    integer :: nb_maxi, nb_term
    real(kind=8) :: un, zero
    character(len=8) :: nomnoe_a, nomnoe_b, nomnoe_c, nomnoe_m
    integer :: numnoe_a, numnoe_b, numnoe_c, numnoe_m
    integer :: i_no, i, j
    real(kind=8) :: matr_inve_1(3, 3), matr_2(3, 12), matr_3(3, 3), matr_4(3, 12)
    real(kind=8) :: matr_5(3, 12), matr_6(3, 9), matr_7(3, 9)
    real(kind=8) :: ab(3), ac(3), am(3)
    real(kind=8) :: norm_ab(3), tang_ab_2(3)
    real(kind=8) :: abm(3), coek, lab, labm
    real(kind=8) :: vale_real
    complex(kind=8) :: vale_cplx
    character(len=8) :: vale_fonc
    character(len=4) :: type_coef
    aster_logical :: l_trian, l_same
    complex(kind=8), pointer :: coec(:) => null()
    real(kind=8), pointer :: coer(:) => null()
    integer, pointer :: dime(:) => null()
    real(kind=8), pointer :: direct(:) => null()
    character(len=8), pointer :: lisddl(:) => null()
    character(len=8), pointer :: lisno(:) => null()
    real(kind=8), pointer :: vale(:) => null()
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
    type_coef = 'REEL'
    ASSERT(type_vale.ne.'COMP')
    ASSERT(dist_mini .gt. 0.d0)
    un = 1.d0
    zero = 0.d0
    l_same = .false.
    l_trian = .false.
    numnoe_a = 0
    numnoe_b = 0
    numnoe_c = 0
!
    do i = 1, 3
        do j = 1, 3
            matr_inve_1(i,j) = zero
            matr_3(i,j) = zero
        enddo
        do j = 1, 9
            matr_6(i,j) = zero
            matr_7(i,j) = zero
        enddo
        do j = 1, 12
            matr_2(i,j) = zero
            matr_4(i,j) = zero
            matr_5(i,j) = zero
        enddo
    end do
!
! - Matrix [matr_4] - Constant
!
    matr_4(1,1) = -un
    matr_4(2,2) = -un
    matr_4(3,3) = -un
    matr_4(1,10) = un
    matr_4(2,11) = un
    matr_4(3,12) = un
!
! - Nodes coordinates
!
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=vale)
!
! - List of nodes to apply linear relation
!
    call jeveuo(list_node, 'L', jlino)
!
! - Working vectors
!
    nb_maxi = 12
    AS_ALLOCATE(vk8=lisno, size=nb_maxi)
    AS_ALLOCATE(vk8=lisddl, size=nb_maxi)
    AS_ALLOCATE(vr=coer, size=nb_maxi)
    AS_ALLOCATE(vc=coec, size=nb_maxi)
    AS_ALLOCATE(vr=direct, size=3*nb_maxi)
    AS_ALLOCATE(vi=dime, size=nb_maxi)
!
! - Reference node A: first node
!
    numnoe_a = zi(jlino+1-1)
!
! - Find a triangle (three nodes) with non-zero surface
!
    call drz03d_tria(dist_mini, nb_node, zi(jlino), vale, numnoe_a,&
                     numnoe_b, numnoe_c, ab, ac, l_trian)
!
! - Name of nodes
!
    if (numnoe_a .ne. 0) then
        call jenuno(jexnum(noma//'.NOMNOE', numnoe_a), nomnoe_a)
        nom_noeuds(1) = nomnoe_a
    endif
    if (numnoe_b .ne. 0) then
        call jenuno(jexnum(noma//'.NOMNOE', numnoe_b), nomnoe_b)
        nom_noeuds(2) = nomnoe_b
    endif
    if (numnoe_c .ne. 0) then
        call jenuno(jexnum(noma//'.NOMNOE', numnoe_c), nomnoe_c)
        nom_noeuds(3) = nomnoe_c
    endif
!
! - Zero_surface triangle - Degenerate case: are all nodes have the same coordinates ?
!
    if (.not.l_trian) then
        l_same = .true.
        do i_no = 2, nb_node
            numnoe_m = zi(jlino+i_no-1)
            am(1) = vale(3*(numnoe_m-1)+1) - vale(3*(numnoe_a-1) +1)
            am(2) = vale(3*(numnoe_m-1)+2) - vale(3*(numnoe_a-1) +2)
            am(3) = vale(3*(numnoe_m-1)+3) - vale(3*(numnoe_a-1) +3)
            if ((abs(am(1)).gt.dist_mini) .or. (abs(am(2)).gt.dist_mini) .or.&
                (abs(am(3)).gt.dist_mini)) then
                l_same = .false.
                goto 80
            endif
        enddo
    endif
!
 80 continue
!
    if (l_trian) then
!
        type_transf = '1'
        ASSERT(.not.l_same)
!
! ----- Three nodes build non-zero-triangle
!
        nb_term = 6
        lisddl(1) = 'DX'
        lisddl(2) = 'DX'
        lisddl(3) = 'DY'
        lisddl(4) = 'DY'
        lisddl(5) = 'DZ'
        lisddl(6) = 'DZ'
!
! ----- First relation (nodes A and B): (DU(B)-DU(A)).AB = 0
!
        lisno(1) = nomnoe_b
        lisno(2) = nomnoe_a
        lisno(3) = nomnoe_b
        lisno(4) = nomnoe_a
        lisno(5) = nomnoe_b
        lisno(6) = nomnoe_a
        coer(1) = ab(1)
        coer(2) = -ab(1)
        coer(3) = ab(2)
        coer(4) = -ab(2)
        coer(5) = ab(3)
        coer(6) = -ab(3)
        call afrela(coer, coec, lisddl, lisno, dime,&
                    direct, nb_term, vale_real, vale_cplx, vale_fonc,&
                    type_coef, type_vale, type_lagr, 0.d0, lisrel)
!
! ----- Second relation (nodes A and C): (DU(C)-DU(A)).AC = 0
!
        lisno(1) = nomnoe_c
        lisno(2) = nomnoe_a
        lisno(3) = nomnoe_c
        lisno(4) = nomnoe_a
        lisno(5) = nomnoe_c
        lisno(6) = nomnoe_a
        coer(1) = ac(1)
        coer(2) = -ac(1)
        coer(3) = ac(2)
        coer(4) = -ac(2)
        coer(5) = ac(3)
        coer(6) = -ac(3)
        call afrela(coer, coec, lisddl, lisno, dime,&
                    direct, nb_term, vale_real, vale_cplx, vale_fonc,&
                    type_coef, type_vale, type_lagr, 0.d0, lisrel)
!
! ----- Third relation (nodes C and C): (DU(B)-DU(A)).AC + (DU(C)-DU(A)).AB = 0
!
        nb_term = 9
        lisno(1) = nomnoe_a
        lisno(2) = nomnoe_b
        lisno(3) = nomnoe_c
        lisno(4) = nomnoe_a
        lisno(5) = nomnoe_b
        lisno(6) = nomnoe_c
        lisno(7) = nomnoe_a
        lisno(8) = nomnoe_b
        lisno(9) = nomnoe_c
        lisddl(1) = 'DX'
        lisddl(2) = 'DX'
        lisddl(3) = 'DX'
        lisddl(4) = 'DY'
        lisddl(5) = 'DY'
        lisddl(6) = 'DY'
        lisddl(7) = 'DZ'
        lisddl(8) = 'DZ'
        lisddl(9) = 'DZ'
        coer(1) = -ab(1) - ac(1)
        coer(2) = ac(1)
        coer(3) = ab(1)
        coer(4) = -ab(2) - ac(2)
        coer(5) = ac(2)
        coer(6) = ab(2)
        coer(7) = -ab(3) - ac(3)
        coer(8) = ac(3)
        coer(9) = ab(3)
        call afrela(coer, coec, lisddl, lisno, dime,&
                    direct, nb_term, vale_real, vale_cplx, vale_fonc,&
                    type_coef, type_vale, type_lagr, 0.d0, lisrel)
!
! ----- Computation of matrix for solid movement in 3D - First part
!
        call char_soli_mat1(ab, ac, matr_inve_1, matr_2)
!
        do i_no = 2, nb_node
            numnoe_m = zi(jlino+i_no-1)
            call jenuno(jexnum(noma//'.NOMNOE', numnoe_m), nomnoe_m)
            if ((numnoe_m .ne. numnoe_b) .and. (numnoe_m .ne. numnoe_c)) then
!
                am(1) = vale(3*(numnoe_m-1)+1) - vale(3*(numnoe_a-1) +1)
                am(2) = vale(3*(numnoe_m-1)+2) - vale(3*(numnoe_a-1) +2)
                am(3) = vale(3*(numnoe_m-1)+3) - vale(3*(numnoe_a-1) +3)
!
! ------------- Computation of matrix for solid movement in 3D - Second part
!
                call char_soli_mat2(am, matr_inve_1, matr_2, matr_4, matr_3,&
                                    matr_5)
!
! ------------- Relation : [M5].DU(AM,BM,CM) = 0
!
                nb_term = 12
                do i = 1, 3
                    lisno(i) = nomnoe_a
                    lisno(1+3+i-1) = nomnoe_b
                    lisno(1+6+i-1) = nomnoe_c
                    lisno(1+9+i-1) = nomnoe_m
                enddo
                do i = 1, 4
                    lisddl(1+3*(i-1)+1-1) = 'DX'
                    lisddl(1+3*(i-1)+2-1) = 'DY'
                    lisddl(1+3*(i-1)+3-1) = 'DZ'
                enddo
!
! ------------- Apply linear relation
!
                do i = 1, 3
                    do j = 1, 12
                        coer(j) = matr_5(i,j)
                    enddo
                    call afrela(coer, coec, lisddl, lisno, dime,&
                                direct, nb_term, vale_real, vale_cplx, vale_fonc,&
                                type_coef, type_vale, type_lagr, 0.d0, lisrel)
                enddo
            endif
        enddo
    else
!
! ----- Three nodes are building zero triangle
!
        if (l_same) then
!
! --------- Zero triangle/degenerate case: all nodes have same coordinates
!
            type_transf = '2'
            nb_term = 2
            lisno(1) = nomnoe_a
            coer(1) = un
!
! --------- Relation: DX(M) - DX(A) = 0
!
            lisddl(1) = 'DX'
            do i_no = 2, nb_node
                numnoe_m = zi(jlino+i_no-1)
                call jenuno(jexnum(noma//'.NOMNOE', numnoe_m), nomnoe_m)
                lisno(2) = nomnoe_m
                lisddl(2) = 'DX'
                coer(2) = -un
                call afrela(coer, coec, lisddl, lisno, dime,&
                            direct, nb_term, vale_real, vale_cplx, vale_fonc,&
                            type_coef, type_vale, type_lagr, 0.d0, lisrel)
            enddo
!
! --------- Relation: DY(M) - DY(A) = 0
!
            lisddl(1) = 'DY'
            do i_no = 2, nb_node
                numnoe_m = zi(jlino+i_no-1)
                call jenuno(jexnum(noma//'.NOMNOE', numnoe_m), nomnoe_m)
                lisno(2) = nomnoe_m
                lisddl(2) = 'DY'
                coer(2) = -un
                call afrela(coer, coec, lisddl, lisno, dime,&
                            direct, nb_term, vale_real, vale_cplx, vale_fonc,&
                            type_coef, type_vale, type_lagr, 0.d0, lisrel)
            enddo
!
! --------- Relation: DZ(M) - DZ(A) = 0
!
            lisddl(1) = 'DZ'
            do i_no = 2, nb_node
                numnoe_m = zi(jlino+i_no-1)
                call jenuno(jexnum(noma//'.NOMNOE', numnoe_m), nomnoe_m)
                lisno(2) = nomnoe_m
                lisddl(2) = 'DZ'
                coer(2) = -un
                call afrela(coer, coec, lisddl, lisno, dime,&
                            direct, nb_term, vale_real, vale_cplx, vale_fonc,&
                            type_coef, type_vale, type_lagr, 0.d0, lisrel)
            enddo
        else
!
! --------- Zero triangle/degenerate case: other degenerate cases
!
            type_transf = '3'
            if (nb_node .eq. 2) then
!
! ------------- Zero triangle/degenerate case: only two nodes in model
!
                nb_term = 6
!
! ------------- One relation: (DU(B)-DU(A)).AB = 0
!
                lisno(1) = nomnoe_b
                lisno(2) = nomnoe_a
                lisno(3) = nomnoe_b
                lisno(4) = nomnoe_a
                lisno(5) = nomnoe_b
                lisno(6) = nomnoe_a
                lisddl(1) = 'DX'
                lisddl(2) = 'DX'
                lisddl(3) = 'DY'
                lisddl(4) = 'DY'
                lisddl(5) = 'DZ'
                lisddl(6) = 'DZ'
                coer(1) = ab(1)
                coer(2) = -ab(1)
                coer(3) = ab(2)
                coer(4) = -ab(2)
                coer(5) = ab(3)
                coer(6) = -ab(3)
                call afrela(coer, coec, lisddl, lisno, dime,&
                            direct, nb_term, vale_real, vale_cplx, vale_fonc,&
                            type_coef, type_vale, type_lagr, 0.d0, lisrel)
            else
!
! ------------- Zero triangle/degenerate case: three nodes are aligned
!
                do i_no = 2, nb_node
                    numnoe_b = zi(jlino+i_no-1)
                    call jenuno(jexnum(noma//'.NOMNOE', numnoe_b ), nomnoe_b)
                    ab(1) = vale(3*(numnoe_b-1)+1) - vale(3*(numnoe_a-1)+1)
                    ab(2) = vale(3*(numnoe_b-1)+2) - vale(3*(numnoe_a-1)+2)
                    ab(3) = vale(3*(numnoe_b-1)+3) - vale(3*(numnoe_a-1)+3)
                    lab = sqrt(ab(1)*ab(1)+ab(2)*ab(2)+ab(3)*ab(3))
                    if (lab .gt. dist_mini) goto 260
                enddo
!
260             continue
!
! ------------- Try to define normal to AB
!
                norm_ab(1) = zero
                norm_ab(2) = -ab(3)
                norm_ab(3) = ab(2)
                lab = sqrt(ab(2)*ab(2)+ab(3)*ab(3))
                if (lab .le. dist_mini) then
                    norm_ab(1) = ab(3)
                    norm_ab(2) = zero
                    norm_ab(3) = -ab(1)
                    lab = sqrt(ab(1)*ab(1)+ab(3)*ab(3))
                    if (lab .le. dist_mini) then
                        call utmess('F', 'CHARGES2_46')
                    endif
                endif
!
! ------------- Defining second tangent to AB
!
                tang_ab_2(1) = ab(2)*norm_ab(3) - ab(3)*norm_ab(2)
                tang_ab_2(2) = ab(3)*norm_ab(1) - ab(1)*norm_ab(3)
                tang_ab_2(3) = ab(1)*norm_ab(2) - ab(2)*norm_ab(1)
!
! ------------- Are nodes are really aligned ?
!
                do i_no = 2, nb_node
                    numnoe_m = zi(jlino+i_no-1)
                    am(1) = vale(3*(numnoe_m-1)+1) - vale(3*(numnoe_a-1) +1)
                    am(2) = vale(3*(numnoe_m-1)+2) - vale(3*(numnoe_a-1) +2)
                    am(3) = vale(3*(numnoe_m-1)+3) - vale(3*(numnoe_a-1) +3)
                    abm(1) = ab(2)*am(3) - ab(3)*am(2)
                    abm(2) = ab(3)*am(1) - ab(1)*am(3)
                    abm(3) = ab(1)*am(2) - ab(2)*am(1)
                    labm = sqrt(abm(1)*abm(1)+abm(2)*abm(2)+abm(3)*abm(3))
                    if (labm .gt. dist_mini) then
                        ASSERT(.false.)
                    endif
                enddo
!
! ------------- First relation (nodes A and B): (DU(B)-DU(A)).AB = 0
!
                nb_term = 6
                lisddl(1) = 'DX'
                lisddl(2) = 'DX'
                lisddl(3) = 'DY'
                lisddl(4) = 'DY'
                lisddl(5) = 'DZ'
                lisddl(6) = 'DZ'
                lisno(1) = nomnoe_b
                lisno(2) = nomnoe_a
                lisno(3) = nomnoe_b
                lisno(4) = nomnoe_a
                lisno(5) = nomnoe_b
                lisno(6) = nomnoe_a
                coer(1) = ab(1)
                coer(2) = -ab(1)
                coer(3) = ab(2)
                coer(4) = -ab(2)
                coer(5) = ab(3)
                coer(6) = -ab(3)
                call afrela(coer, coec, lisddl, lisno, dime,&
                            direct, nb_term, vale_real, vale_cplx, vale_fonc,&
                            type_coef, type_vale, type_lagr, 0.d0, lisrel)
!
! ------------- Matrix matr_6
!
                matr_6(1,2) = tang_ab_2(2)*norm_ab(1) - norm_ab(2)*tang_ab_2(1)
                matr_6(1,3) = tang_ab_2(3)*norm_ab(1) - norm_ab(3)*tang_ab_2(1)
                matr_6(2,1) = tang_ab_2(1)*norm_ab(2) - norm_ab(1)*tang_ab_2(2)
                matr_6(2,3) = tang_ab_2(3)*norm_ab(2) - norm_ab(3)*tang_ab_2(2)
                matr_6(3,1) = tang_ab_2(1)*norm_ab(3) - norm_ab(1)*tang_ab_2(3)
                matr_6(3,2) = tang_ab_2(2)*norm_ab(3) - norm_ab(2)*tang_ab_2(3)
                matr_6(1,5) = -matr_6(1,2)
                matr_6(1,6) = -matr_6(1,3)
                matr_6(2,4) = -matr_6(2,1)
                matr_6(2,6) = -matr_6(2,3)
                matr_6(3,4) = -matr_6(3,1)
                matr_6(3,5) = -matr_6(3,2)
!
! ------------- Coefficient
!
                coek = (&
                       norm_ab(1)*norm_ab(1) + norm_ab(2)*norm_ab(2) + norm_ab(3)*norm_ab(3)) * (&
                       &ab(1)*ab( 1)+ab(2)*ab(2)+ab(3)*ab(3)&
                       )
                coek = 1.d0/coek
!
                do i_no = 2, nb_node
                    numnoe_m = zi(jlino+i_no-1)
                    call jenuno(jexnum(noma//'.NOMNOE', numnoe_m), nomnoe_m)
                    if (numnoe_m .ne. numnoe_b) then
!
                        am(1) = vale(3*(numnoe_m-1)+1) - vale(3*(numnoe_a-1)+1)
                        am(2) = vale(3*(numnoe_m-1)+2) - vale(3*(numnoe_a-1)+2)
                        am(3) = vale(3*(numnoe_m-1)+3) - vale(3*(numnoe_a-1)+3)
!
! --------------------- Matrix matr_8
!
                        call char_soli_mat3(am, coek, matr_6, matr_7)
!
! --------------------- Relation : [M8].DU(AM,BM) = 0
!
                        nb_term = 9
                        do i = 1, 3
                            lisno(i) = nomnoe_a
                            lisno(1+3+i-1) = nomnoe_b
                            lisno(1+6+i-1) = nomnoe_m
                            lisddl(1+3*(i-1)+1-1) = 'DX'
                            lisddl(1+3*(i-1)+2-1) = 'DY'
                            lisddl(1+3*(i-1)+3-1) = 'DZ'
                        enddo
!
! --------------------- Apply linear relation
!
                        do i = 1, 3
                            do j = 1, 9
                                coer(j) = matr_7(i,j)
                            enddo
                            call afrela(coer, coec, lisddl, lisno, dime,&
                                        direct, nb_term, vale_real, vale_cplx, vale_fonc,&
                                        type_coef, type_vale, type_lagr, 0.d0, lisrel)
                        enddo
                    endif
                enddo
            endif
        endif
    endif
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
