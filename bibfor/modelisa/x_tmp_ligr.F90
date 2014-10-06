subroutine x_tmp_ligr(mesh, ligrel, list_cells, n_list_cells)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/adalig.h"
#include "asterfort/assert.h"
#include "asterfort/cormgi.h"
#include "asterfort/dismoi.h"
#include "asterfort/initel.h"
#include "asterfort/jeecra.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utflm2.h"
#include "asterfort/wkvect.h"
    character(len=8), intent(in) :: mesh
    character(len=19), intent(inout) :: ligrel
    character(len=19), optional, intent(in) :: list_cells
    integer, optional, intent(in) :: n_list_cells
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
! person_in_charge: sam.cuvilliez at edf.fr
! ----------------------------------------------------------------------
!
! DEFI_FISS_XFEM / PROPA_FISS :
! -----------------------------
! - creer un ligrel temporaire (dans la base volatile) defini sur une
!   liste de mailles principales du maillage "mesh"
! - si les arguments optionnels "list_cells" et "n_list_cells" sont
!   absents, on prend toutes les mailles principales de "mesh"
!
! Version simplifiee de ce qui est fait dans op0018 (AFFE_MODELE)
!
! ----------------------------------------------------------------------
!
!    in     : mesh   -> nom du maillage
!
!    in/out : ligrel -> ligrel cree uniquement a partir des mailles 
!                       principales de "mesh"
!
!    optionnel, in : list_cells   -> nom d'un vecteur jeveux contenant 
!                                    liste de numero de mailles 
!
!    optionnel, in : n_list_cells -> longueur du vecteur "list_cells"
!
! ----------------------------------------------------------------------
!
! Attention : "list_cells" doit etre une sous-liste de la liste
! de toutes les mailles principales de "mesh"
!
! ----------------------------------------------------------------------
!
    integer :: nu_ma, nu_typ_ma, dim_ma, nu_typ_el1, nu_typ_el2
    integer :: i, nbma, ndim, nmaprin, idx_modeli, nb_grel, lont_liel
    integer :: ncells, nume_grel, long_grel, idx_in_grel
    integer, pointer :: p_ligrel_nbno(:) => null()
    integer, pointer :: lmatout(:) => null()
    integer, pointer :: lmatmp(:) => null()
    integer, pointer :: lmaprin(:) => null()
    integer, pointer :: p_cata_typ_el(:) => null()
    integer, pointer :: p_mesh_type_geom(:) => null()
    integer, pointer :: p_dim_topo(:) => null()
    integer, pointer :: p_liel(:) => null()
    integer, pointer :: p_list_cells(:) => null()
    character(len=8), pointer :: p_ligrel_lgrf(:) => null()
    character(len=16) :: phenom, modeli(3)
    character(len=24) :: liel, lgrf, nbno
    aster_logical :: all_cells
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! - Check presence and coherence of optional arguments
!
    all_cells = .true.
!
    if (present(list_cells)) then
        ASSERT(present(n_list_cells))
        all_cells = .false.
    endif
    if (present(n_list_cells)) then
        ASSERT(present(list_cells))
    endif
!
! - Give a name for temporary LIGREL used to compute GRAD_NEUT_R option
!
    liel   = ligrel//'.LIEL'
    lgrf   = ligrel//'.LGRF'
    nbno   = ligrel//'.NBNO'
!
! - Common definition for ligrel SD
!
    call wkvect(lgrf, 'V V K8', 2, vk8 = p_ligrel_lgrf)
    call wkvect(nbno, 'V V I' , 1, vi  = p_ligrel_nbno)
    call jeecra(lgrf, 'DOCU', cval='MECA')
    p_ligrel_lgrf(1) = mesh
!   no sd_model => no sd_partition to get for this ligrel :
!     -> options will be computed sequentially
    p_ligrel_lgrf(2) = ''
    p_ligrel_nbno(1) = 0
!
! - Get mesh dimension "ndim" (2 or 3)
!
    call dismoi('DIM_GEOM', mesh, 'MAILLAGE', repi=ndim)
    ASSERT(ndim .eq. 2 .or. ndim .eq. 3)
!
! - Get list of cells to treat
!
    if (all_cells) then
!
! ----- Get list of all cells of dimension "ndim" that belong to mesh
!
        call dismoi('NB_MA_MAILLA', mesh, 'MAILLAGE', repi=nbma)
        ASSERT(nbma.gt.0)
        AS_ALLOCATE(vi=lmatout, size=nbma)
        AS_ALLOCATE(vi=lmatmp, size=nbma)
        do i=1,nbma
            lmatout(i) = i
            lmatmp(i)  = 0
        enddo
!
        call utflm2(mesh, lmatout, nbma, ndim, ' ', nmaprin, lmatmp)
        ASSERT(nmaprin .gt. 0)
        AS_ALLOCATE(vi=lmaprin, size=nmaprin)
        do i=1,nmaprin
            lmaprin(i) = lmatmp(i)
        enddo
        AS_DEALLOCATE(vi=lmatout)
        AS_DEALLOCATE(vi=lmatmp)
!
    else
!
! ----- Get list of all cells "p_list_cells" (optional argument)
!
        call jeveuo(list_cells, 'L', vi=p_list_cells)
!
    endif
!
! - Get number of cells to treat
!
    if (all_cells) then
        ncells = nmaprin
    else
        ncells = n_list_cells
    endif
    ASSERT(ncells .gt. 0)
!
! - Set modelisation type acording to dimension "ndim"
!                
    phenom    = 'PRESENTATION    '
    modeli(1) = '                '
    modeli(2) = '2D_GEOM         '
    modeli(3) = '3D_GEOM         '
!
! - Get cell type ids in mesh
!
    call jeveuo(mesh//'.TYPMAIL', 'L', vi=p_mesh_type_geom)
!
! - Get element type ids available in CATA for the selected modelisation
!
    call jenonu(jexnom('&CATA.'//phenom(1:13)//'.MODL', modeli(ndim)), idx_modeli)
    call jeveuo(jexnum('&CATA.'//phenom, idx_modeli), 'L', vi=p_cata_typ_el)
!
! - Count number of GREL - Elements
!
    nb_grel    = 0
    nu_typ_el2 = 0
    do i=1,ncells
!
        if (all_cells) then
            nu_ma = lmaprin(i)
        else
            nu_ma = p_list_cells(i)
        endif
!
        nu_typ_ma = p_mesh_type_geom(nu_ma)
!       current cell dimension != mesh dimension => fatal error
        call jeveuo(jexnum('&CATA.TM.TMDIM', nu_typ_ma), 'L', vi=p_dim_topo)
        dim_ma = p_dim_topo(1)
        ASSERT(dim_ma .eq. ndim)
!       affectation of an element on current cell not possible => fatal error
        nu_typ_el1 = p_cata_typ_el(nu_typ_ma)
        ASSERT(nu_typ_el1 .gt. 0)
        if (nu_typ_el1 .ne. nu_typ_el2) then
            nu_typ_el2 = nu_typ_el1
            nb_grel    = nb_grel+1
        endif 
    enddo
!
! - Create LIEL
!
    lont_liel = nb_grel+ncells
    call jecrec(liel, 'V V I', 'NU', 'CONTIG', 'VARIABLE', nb_grel)
    call jeecra(liel, 'LONT', lont_liel)
    call jeveuo(liel, 'E', vi=p_liel)
!
! - Store GREL in LIEL - Elements
!
    nu_typ_el2  = 0
    nume_grel   = 0
    long_grel   = 0
    idx_in_grel = 0
    do i=1,ncells
!
        if (all_cells) then
            nu_ma = lmaprin(i)
        else
            nu_ma = p_list_cells(i)
        endif
!
        nu_typ_ma = p_mesh_type_geom(nu_ma)
        nu_typ_el1 = p_cata_typ_el(nu_typ_ma)
!
! ----- Create new GREL
!
        if (nu_typ_el1 .ne. nu_typ_el2 .and. nu_typ_el2 .ne. 0) then
            nume_grel   = nume_grel+1
            long_grel   = long_grel+1
            idx_in_grel = idx_in_grel+1
            p_liel(idx_in_grel) = nu_typ_el2
            call jecroc(jexnum(liel, nume_grel))
            call jeecra(jexnum(liel, nume_grel), 'LONMAX', long_grel)
            long_grel = 0
        endif
!
! ------ Add element in GREL
!
        long_grel   = long_grel+1
        idx_in_grel = idx_in_grel+1
        p_liel(idx_in_grel) = nu_ma
        nu_typ_el2 = nu_typ_el1
!
! ----- Last element
!
        if (i .eq. ncells) then
            nume_grel   = nume_grel+1
            long_grel   = long_grel+1
            idx_in_grel = idx_in_grel+1
            p_liel(idx_in_grel) = nu_typ_el2
            call jecroc(jexnum(liel, nume_grel))
            call jeecra(jexnum(liel, nume_grel), 'LONMAX', long_grel)
        endif
    end do
!
! - Automatic GREL size adaptation
!
    call adalig(ligrel)
!
! - Set element/(IGREL,IM) object
!
    call cormgi('V', ligrel)
!
! - Init elements for this LIGREL
!
    call initel(ligrel)
!
! - De-allocation of tmp arrays
!
    if (all_cells) then
        AS_DEALLOCATE(vi=lmaprin)
    endif
!
    call jedema()
!
end subroutine
