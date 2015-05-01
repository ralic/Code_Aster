subroutine profgene_crsd(prof_genez , base      , nb_equa, nb_sstr, nb_link,&
                         model_genez, gran_namez)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=*), intent(in) :: prof_genez
    character(len=1), intent(in) :: base
    integer, intent(in) :: nb_equa
    integer, intent(in) :: nb_sstr
    integer, intent(in) :: nb_link
    character(len=*), optional, intent(in) :: model_genez
    character(len=*), optional, intent(in) :: gran_namez
!
! --------------------------------------------------------------------------------------------------
!
! PROF_GENE 
!
! Create object
!
! --------------------------------------------------------------------------------------------------
!
! In  prof_gene   : name of PROF_GENE
! In  base        : JEVEUX base to create PROF_GENE
! In  nb_equa     : number of equations
! In  nb_sstr     : number of sub_structures
! In  nb_link     : number of links
! In  model_gene  : name of model 
! In  gran_name   : name of GRANDEUR 
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: prof_gene
    integer :: i_equa, i_ligr_sstr, i_ligr_link, nb_ligr
    logical :: l_ligr_sstr, l_ligr_link
    character(len=24) :: model_gene, gran_name
    integer, pointer :: prgene_nueq(:) => null()
    integer, pointer :: prgene_deeq(:) => null()
    integer, pointer :: prgene_delg(:) => null()
    character(len=24), pointer :: prgene_refn(:) => null()
    integer, pointer :: prgene_desc(:) => null()
    integer, pointer :: prgene_nequ(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    prof_gene  = prof_genez
    model_gene = ' '
    if (present(model_genez)) then
        model_gene = model_genez
    endif
    gran_name  = ' '
    if (present(gran_namez)) then
        gran_name = gran_namez
    endif
!
    call detrsd('PROF_GENE',prof_gene)
!
! - Create object NUEQ
!
    call wkvect(prof_gene//'.NUEQ', base//' V I', nb_equa, vi = prgene_nueq)
!
! - Set to identity
!
    do i_equa = 1, nb_equa
        prgene_nueq(i_equa) = i_equa
    end do
!
! - Create object DEEQ
!
    call wkvect(prof_gene//'.DEEQ', base//' V I', 2*nb_equa, vi = prgene_deeq)
!
! - Set
!
    do i_equa = 1, nb_equa
        prgene_deeq(2*(i_equa-1)+1) = i_equa
        prgene_deeq(2*(i_equa-1)+2) = 1
    end do
!
! - Number of LIGREL
!
    l_ligr_sstr = nb_sstr.gt.0
    l_ligr_link = nb_link.gt.0
    nb_ligr = 0
    if (l_ligr_sstr) then
        nb_ligr = nb_ligr+1
    endif
    if (l_ligr_link) then
        nb_ligr = nb_ligr+1
    endif
    ASSERT(nb_ligr.le.2)
!
! - Create object LILI
! 
    call jecreo(prof_gene//'.LILI', base//' N K8')
    call jeecra(prof_gene//'.LILI', 'NOMMAX', nb_ligr)
    if (l_ligr_sstr) then
        call jecroc(jexnom(prof_gene//'.LILI', '&SOUSSTR'))
        call jenonu(jexnom(prof_gene//'.LILI', '&SOUSSTR'), i_ligr_sstr)
        ASSERT(i_ligr_sstr.eq.1)
    endif
    if (l_ligr_link) then
        call jecroc(jexnom(prof_gene//'.LILI', 'LIAISONS'))
        call jenonu(jexnom(prof_gene//'.LILI', 'LIAISONS'), i_ligr_link)
    endif
!
! - Create object PRNO
!
    call jecrec(prof_gene//'.PRNO', base//' V I', 'NU', 'DISPERSE', 'VARIABLE', nb_ligr)
    if (l_ligr_sstr) then
        call jeecra(jexnum(prof_gene//'.PRNO', i_ligr_sstr), 'LONMAX', 2*nb_sstr)
    endif
    if (l_ligr_link) then
        call jeecra(jexnum(prof_gene//'.PRNO', i_ligr_link), 'LONMAX', 2*nb_link)
    endif
!
! - Create object ORIG
!
    call jecrec(prof_gene//'.ORIG', base//' V I', 'NU', 'DISPERSE', 'VARIABLE', 2)
    if (l_ligr_sstr) then
        call jeecra(jexnum(prof_gene//'.ORIG', i_ligr_sstr), 'LONMAX', nb_sstr)
    endif
    if (l_ligr_link) then
        call jeecra(jexnum(prof_gene//'.ORIG', i_ligr_link), 'LONMAX', nb_link)
    endif
!
! - Create object NEQU
!
    call wkvect(prof_gene//'.NEQU', base//' V I', 1, vi = prgene_nequ)
    prgene_nequ(1) = nb_equa
!
! - Create object DESC
!
    call wkvect(prof_gene//'.DESC', base//' V I', 1, vi = prgene_desc)
    prgene_desc(1) = 2 
!
! - Create object REFN
! 
    call wkvect(prof_gene//'.REFN', base//' V K24', 4, vk24 = prgene_refn)
    prgene_refn(1) = model_gene
    prgene_refn(2) = gran_name
!
! - Create object DELG
!
    call wkvect(prof_gene//'.DELG', base//' V I', nb_equa, vi = prgene_delg)

end subroutine
