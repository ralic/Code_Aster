subroutine profchno_crsd(prof_chnoz , base      , nb_equa     , meshz      , nb_ligrz,&
                         nb_ecz     , gran_namez, prno_lengthz, l_coll_const)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jecreo.h"
#include "asterfort/jeecra.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
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
    character(len=*), intent(in) :: prof_chnoz
    character(len=1), intent(in) :: base
    integer, intent(in) :: nb_equa
    character(len=*), optional, intent(in) :: meshz
    character(len=*), optional, intent(in) :: gran_namez
    integer, optional, intent(in) :: nb_ecz
    integer, optional, intent(in) :: nb_ligrz
    integer, optional, intent(in) :: prno_lengthz
    logical, optional, intent(in) :: l_coll_const
!
! --------------------------------------------------------------------------------------------------
!
! PROF_CHNO 
!
! Create object
!
! --------------------------------------------------------------------------------------------------
!
! In  prof_chno    : name of PROF_CHNO
! In  base         : JEVEUX base to create PROF_CHNO
! In  nb_equa      : number of equations
! In  nb_ligr      : number of LIGREL in .LILI object 
!                   if not present => only mesh (nb_ligr=1)
! In  mesh         : name of mesh
! In  gran_name    : name of GRANDEUR
! In  nb_ec        : number of coding integers for GRANDEUR
!                   if not present => get from gran_name
! In  prno_length  : length of first PRNO object (on mesh)
! In  l_coll_const : .true. if PRNO colelction is CONSTANT (not variable) for cnscno
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: prof_chno
    integer :: i_equa, i_ligr_mesh, nb_node_mesh, nb_ec, nb_ligr, prno_length
    integer, pointer :: prchno_nueq(:) => null()
    integer, pointer :: prchno_deeq(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    prno_length = 0
    nb_ec       = 0
    prof_chno = prof_chnoz
    call detrsd('PROF_CHNO',prof_chno)
    if (present(nb_ligrz)) then
        nb_ligr = nb_ligrz
    else
        nb_ligr = 1
    endif
    if (present(nb_ecz)) then
        nb_ec = nb_ecz
    else
        ASSERT(present(gran_namez).or.present(prno_lengthz))
        if (present(gran_namez)) then
            call dismoi('NB_EC', gran_namez, 'GRANDEUR', repi=nb_ec)
        endif
    endif
    if (present(meshz)) then
        ASSERT(.not.present(prno_lengthz))
        call dismoi('NB_NO_MAILLA', meshz, 'MAILLAGE', repi=nb_node_mesh)
    endif
!
! - Create object NUEQ
!
    call wkvect(prof_chno//'.NUEQ', base//' V I', nb_equa, vi = prchno_nueq)
!
! - Set to identity
!
    do i_equa = 1, nb_equa
        prchno_nueq(i_equa) = i_equa
    end do
!
! - Create object DEEQ
!
    call wkvect(prof_chno//'.DEEQ', base//' V I', 2*nb_equa, vi = prchno_deeq)
!
! - Create object LILI (name repertory)
!
    call jecreo(prof_chno//'.LILI', base//' N K24')
    call jeecra(prof_chno//'.LILI', 'NOMMAX', nb_ligr)
!
! - Create &MAILLA object in LILI
!
    call jecroc(jexnom(prof_chno(1:19)//'.LILI', '&MAILLA'))
    call jenonu(jexnom(prof_chno//'.LILI', '&MAILLA'), i_ligr_mesh)
    ASSERT(i_ligr_mesh.eq.1)
!
! - Length of first PRNO object (on mesh)
!
    if (present(meshz)) then
        prno_length = (2+nb_ec)*nb_node_mesh
    else
        prno_length = prno_lengthz
    endif   
!
! - Create object PRNO (collection)
!
    if (present(l_coll_const)) then
        call jecrec(prof_chno//'.PRNO', base//' V I', 'NU', 'CONTIG', 'CONSTANT', nb_ligr)
    else
        call jecrec(prof_chno//'.PRNO', base//' V I', 'NU', 'CONTIG', 'VARIABLE', nb_ligr)
    endif
!
! - Length of &MAILLA object in PRNO
!
    call jeecra(jexnum(prof_chno//'.PRNO', i_ligr_mesh), 'LONMAX', prno_length)
!
end subroutine
