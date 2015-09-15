subroutine solide_tran(type_geo , noma  , type_vale, dist_mini, nb_node, list_node,&
                       type_lagr, lisrel, nom_noeuds, dim)
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
#include "asterfort/getcara_lisno.h"
#include "asterfort/coor_bary.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "asterfort/codent.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=2), intent(in)  :: type_geo
    character(len=8), intent(in)  :: noma
    character(len=4), intent(in)  :: type_vale
    real(kind=8), intent(in)      :: dist_mini
    integer, intent(in)           :: nb_node
    character(len=24), intent(in) :: list_node
    character(len=2), intent(in)  :: type_lagr
    character(len=19), intent(in) :: lisrel
    character(len=8), intent(out) :: nom_noeuds(:)
    integer, intent(out)          :: dim
!
! --------------------------------------------------------------------------------------------------
!
! Loads - Affectation
!
! Apply transformation - 3D with without nodes with DRZ dof
!
! --------------------------------------------------------------------------------------------------
!
! In  type_geo      : '2D' / '3D'
! In  noma          : mesh
! In  type_vale     : type of affected value
! In  dist_mini     : minimum distance to detect nodes in same place
! In  nb_node       : number of nodes  applying translation
! In  list_node     : list of nodes applying translation
! In  type_lagr     : choosing lagrange multipliers position
! In  lisrel        : list of relations
! Out nom_noeuds    : nom des (dim+1) noeuds "maitres"
! Out dim           : "dimension" du solide : 0/1/2/3
!
! --------------------------------------------------------------------------------------------------
!
    integer :: k, km, ka, kb
    integer :: numnoe_m, numnoe_a, numnoe_b
    character(len=8) :: nomnoe_m, nomnoe_a, nomnoe_b
    integer ::    jlino

    integer :: nb_maxi, nb_term, linocara(4),nbnot
    real(kind=8) :: un, cobary(4)
    real(kind=8) :: xa,ya,xb,yb,za,zb
    real(kind=8) :: vale_real
    complex(kind=8) :: vale_cplx
    character(len=8) :: vale_fonc
    real(kind=8) :: xm(3)
    character(len=4) :: type_coef
    aster_logical :: l3d

    complex(kind=8), pointer :: coec(:) => null()
    real(kind=8), pointer :: coer(:) => null()
    integer, pointer :: dime(:) => null()
    real(kind=8), pointer :: direct(:) => null()
    character(len=8), pointer :: lisddl(:) => null()
    character(len=8), pointer :: lisno(:) => null()
    real(kind=8), pointer :: coor(:) => null()
    integer, pointer :: nunocara(:) => null()
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
    ASSERT(type_geo.eq.'2D' .or. type_geo.eq.'3D')
    l3d= type_geo.eq.'3D'
!
! - Nodes coordinates
!
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=coor)
    nbnot=size(coor)/3
!
! - List of nodes to apply linear relation
!
    call jeveuo(list_node, 'L', jlino)
!
! - Working vectors
!
    nb_maxi = 10
    AS_ALLOCATE(vk8=lisno, size=nb_maxi)
    AS_ALLOCATE(vk8=lisddl, size=nb_maxi)
    AS_ALLOCATE(vr=coer, size=nb_maxi)
    AS_ALLOCATE(vc=coec, size=nb_maxi)
    AS_ALLOCATE(vr=direct, size=3*nb_maxi)
    AS_ALLOCATE(vi=dime, size=nb_maxi)


!   -- Quelle est la situation geometrique ?
!   -----------------------------------------
    call getcara_lisno(noma,nb_node,zi(jlino),dist_mini,dim,linocara)
    ASSERT(dim.le.3)


!   -- 1) Les relations potentiellement non-lineaires sont celles traduisant
!         l'indeformabilite des dim+1 noeuds de linocara
!   ----------------------------------------------------------------------


!   -- boucle sur les couples de points A, B de linocara :
!   ---------------------------------------------------------
    do ka=1,dim
        numnoe_a=linocara(ka)
        call jenuno(jexnum(noma//'.NOMNOE', numnoe_a), nomnoe_a)
        xa = coor(3*(numnoe_a-1)+1)
        ya = coor(3*(numnoe_a-1)+2)
        if (l3d) za = coor(3*(numnoe_a-1)+3)

        do kb=ka+1,dim+1
            numnoe_b=linocara(kb)
            call jenuno(jexnum(noma//'.NOMNOE', numnoe_b), nomnoe_b)
            xb = coor(3*(numnoe_b-1)+1)
            yb = coor(3*(numnoe_b-1)+2)
            if (l3d) zb = coor(3*(numnoe_b-1)+3)

            if (l3d) then
                nb_term = 6
            else
                nb_term = 4
            endif

!           -- Relation: AB^2 = cste

!           -- Ordre : A,   B,     A,   B      A,   B
!                     'DX','DX',  'DY','DY',  'DZ','DZ'

            lisno(1) = nomnoe_a
            lisno(2) = nomnoe_b
            lisno(3) = nomnoe_a
            lisno(4) = nomnoe_b
            if (l3d) then
                lisno(5) = nomnoe_a
                lisno(6) = nomnoe_b
            endif

            lisddl(1) = 'DX'
            lisddl(2) = 'DX'
            lisddl(3) = 'DY'
            lisddl(4) = 'DY'
            if (l3d) then
                lisddl(5) = 'DZ'
                lisddl(6) = 'DZ'
            endif

            coer(1) =  -2*(xb-xa)
            coer(2) =   2*(xb-xa)
            coer(3) =  -2*(yb-ya)
            coer(4) =   2*(yb-ya)
            if (l3d) then
                coer(5) = -2*(zb-za)
                coer(6) =  2*(zb-za)
            endif

            call afrela(coer, coec, lisddl, lisno, dime,&
                        direct, nb_term, vale_real, vale_cplx, vale_fonc,&
                        type_coef, type_vale, type_lagr, 0.d0, lisrel)
        enddo

    enddo
    if (nb_node.eq.dim+1) goto 999



!   -- 2) Les relations restantes sont toujours lineaires.
!   ----------------------------------------------------------------------


!   -- boucle sur les noeuds M n'appartenant pas a linocara :
!      On exprime que M est relie aux dim+1 noeuds de linocara
!   ---------------------------------------------------------
    nb_term = dim+2

    AS_ALLOCATE(vi=nunocara, size=nbnot)
    nunocara=0
    do k=1,dim+1
        numnoe_a=linocara(k)
        call jenuno(jexnum(noma//'.NOMNOE', numnoe_a), nomnoe_a)
        lisno(1+k) = nomnoe_a
        nunocara(numnoe_a)=1
    enddo

    do km=1,nb_node
        numnoe_m=zi(jlino-1+km)
        if (nunocara(numnoe_m).eq.1) cycle

        xm(1:3) = coor(3*(numnoe_m-1)+1:3*(numnoe_m-1)+3)

        call coor_bary(coor,xm,dim,linocara,cobary)

        call jenuno(jexnum(noma//'.NOMNOE', numnoe_m), nomnoe_m)
        lisno(1) = nomnoe_m

        coer(1)=-1.d0
        do k=1,dim+1
            coer(1+k)=cobary(k)
        enddo

!       -- relation pour DX :
        lisddl(1:dim+2)='DX'
        call afrela(coer, coec, lisddl, lisno, dime,&
                        direct, nb_term, vale_real, vale_cplx, vale_fonc,&
                        type_coef, type_vale, type_lagr, 0.d0, lisrel)

!       -- relation pour DY :
        lisddl(1:dim+2)='DY'
        call afrela(coer, coec, lisddl, lisno, dime,&
                        direct, nb_term, vale_real, vale_cplx, vale_fonc,&
                        type_coef, type_vale, type_lagr, 0.d0, lisrel)

!       -- relation pour DZ :
        if (l3d) then
            lisddl(1:dim+2)='DZ'
            call afrela(coer, coec, lisddl, lisno, dime,&
                        direct, nb_term, vale_real, vale_cplx, vale_fonc,&
                        type_coef, type_vale, type_lagr, 0.d0, lisrel)
        endif

    enddo


999 continue

!   -- remplissage de nom_noeuds :
!   ----------------------------------------------
    ASSERT(size(nom_noeuds).ge.dim+1)
    do k=1,dim+1
        call jenuno(jexnum(noma//'.NOMNOE', linocara(k)), nomnoe_a)
        nom_noeuds(k)=nomnoe_a
    enddo



    AS_DEALLOCATE(vi=nunocara)
    AS_DEALLOCATE(vk8=lisno)
    AS_DEALLOCATE(vk8=lisddl)
    AS_DEALLOCATE(vr=coer)
    AS_DEALLOCATE(vc=coec)
    AS_DEALLOCATE(vr=direct)
    AS_DEALLOCATE(vi=dime)

    call jedema()
end subroutine
