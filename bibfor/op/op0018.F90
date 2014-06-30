subroutine op0018()
!
    implicit none
!
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/adalig.h"
#include "asterfort/ajlipa.h"
#include "asterfort/assert.h"
#include "asterfort/cetucr.h"
#include "asterfort/cormgi.h"
#include "asterfort/crevge.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getnode.h"
#include "asterfort/getelem.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/initel.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/ssafmo.h"
#include "asterfort/utmess.h"
#include "asterfort/model_check.h"
#include "asterfort/model_print.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!
!
! --------------------------------------------------------------------------------------------------
!
! COMMAND:  AFFE_MODELE
!
! --------------------------------------------------------------------------------------------------
!
! REMARQUES ET RESTRICTIONS D UTILISATION:
!       LES SEULES VERIFICATIONS FAITES ( FAUX=EXIT ), PORTENT SUR:
!       - L AFFECTATION D ELEMENTS FINIS A TOUTES LES MAILLES DEMANDEES
!       - L AFFECTATION D ELEMENTS FINIS A TOUS LES NOEUDS DEMANDES
!       - L AFFECTATION D ELEMENTS FINIS SUR UNE MAILLE AU MOINS
!
! --------------------------------------------------------------------------------------------------
!
    integer :: dim_topo_curr, dim_topo_init
    integer :: ifm, niv
    character(len=8) :: mesh, model
    character(len=8) :: name_elem, name_node
    character(len=16) :: k16dummy, name_type_geom, repk, valk(2)
    character(len=16) :: phenom, modeli, list_modelisa(10), keywordfact
    character(len=19) :: ligrel
    character(len=24) :: mesh_name_elem, mesh_name_node
    character(len=32) :: phemod
    character(len=24) :: list_elem
    integer, pointer :: p_list_elem(:) => null()
    integer :: nb_elem
    character(len=24) :: list_node
    integer, pointer :: p_list_node(:) => null()
    integer :: nb_node
    logical(kind=1) :: l_elem, l_node, l_grandeur_cara
    logical(kind=1) :: l_calc_rigi, l_veri_elem, l_volu_fini, l_need_neigh
    integer :: inode, ielem, iaffe
    integer :: vali(4),  ico, imodel, idx_modelisa
    integer, pointer          :: p_cata_dim(:) => null()
    integer, pointer          :: p_cata_model(:) => null()
    character(len=24)         :: mesh_type_geom
    integer, pointer          :: p_mesh_type_geom(:) => null()
    integer, pointer          :: p_wk_mail1(:) => null()
    integer, pointer          :: p_wk_mail2(:) => null()
    integer, pointer          :: p_wk_mail3(:) => null()
    integer, pointer          :: p_wk_node(:) => null()
    character(len=24)         :: model_liel
    integer, pointer          :: p_model_liel(:) => null()
    character(len=24)         :: model_nema
    integer, pointer          :: p_model_nema(:) => null()
    character(len=24)         :: model_maille
    integer, pointer          :: p_model_maille(:) => null()
    character(len=24)         :: model_noeud
    integer, pointer          :: p_model_noeud(:) => null()
    character(len=8), pointer :: p_model_lgrf(:) => null()
    integer, pointer          :: p_model_nbno(:) => null()
    integer :: lont_liel, lont_nema, nb_grel, nb_elem_affe, nb_mesh_elem
    integer :: nb_elem_naffe, nb_node_affe, nb_mesh_node, nb_node_naffe
    integer :: nb_affe, nb_affe_ss, nbocc
    integer :: long_grel, nb_modelisa, nume_type_poi1, nume_grel
    integer :: nume_elem, nume_node, numsup, idx_in_grel, nume_type_model, nume_type_geom
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
! - Initializations
!
    l_elem = .false.
    l_node = .false.
    list_elem = '&&OP0018.LIST_ELEM'
    list_node = '&&OP0018.LIST_NODE'
!
! - Get command parameters
!
    call getres(model, k16dummy, k16dummy)
    ligrel = model//'.MODELE'
!
! - Get mesh (or grid)
!
    call getvid(' ', 'MAILLAGE', scal=mesh, nbret=nbocc)
    if (nbocc .eq. 0) then
        call getvid(' ', 'GRILLE', scal=mesh)
    endif
!
! - Check jacobians
!
    call getvtx(' ', 'VERI_JACOBIEN', scal=repk)
    l_veri_elem = repk.eq.'OUI'
!
! - Grandeurs caracteristiques
!
    keywordfact = 'GRANDEUR_CARA'
    call getfac(keywordfact, nbocc)
    l_grandeur_cara = nbocc.gt.0
!
! - AFFE_SOUS_STRUC
!
    keywordfact = 'AFFE_SOUS_STRUC'
    call getfac(keywordfact, nb_affe_ss)
!
! - AFFE
!
    keywordfact = 'AFFE'
    call getfac(keywordfact, nb_affe)
!
! - Access to catalog
!
    call jeveuo('&CATA.TM.TMDIM', 'L', vi = p_cata_dim)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'POI1'), nume_type_poi1)
!
! - Common definition for model SD
!
    call wkvect(model//'.MODELE    .LGRF', 'G V K8', 2, vk8 = p_model_lgrf)
    call wkvect(model//'.MODELE    .NBNO', 'G V I' , 1, vi  = p_model_nbno)
    p_model_lgrf(1) = mesh
    p_model_lgrf(2) = model
    p_model_nbno(1) = 0
!
! - Get phenomenon
!
    if (nb_affe .gt. 0) then
        call getvtx('AFFE', 'PHENOMENE', iocc=1, scal=phenom)
    else if (nb_affe_ss.gt.0) then
        call getvtx('AFFE_SOUS_STRUC', 'PHENOMENE', iocc=1, scal=phenom)
    endif
    call jeecra(model//'.MODELE    .LGRF', 'DOCU', cval=phenom(1:4))
!
    if (nb_affe .ne. 0) then
!
        keywordfact = 'AFFE'
!
! ----- Access to mesh objects
!
        mesh_name_elem = mesh//'.NOMMAI'
        mesh_name_node = mesh//'.NOMNOE'
        mesh_type_geom = mesh//'.TYPMAIL'
        call jelira(mesh_name_elem, 'NOMMAX', nb_mesh_elem)
        call jelira(mesh_name_node, 'NOMMAX', nb_mesh_node)
        call jeveuo(mesh_type_geom, 'L', vi = p_mesh_type_geom)
!
! ----- Name of objects for model
!
        model_maille = model//'.MAILLE'
        model_noeud  = model//'.NOEUD'
        model_liel   = model//'.MODELE    .LIEL'
        model_nema   = model//'.MODELE    .NEMA'
!
! ----- Create main objects for model
!
        call wkvect(model_maille, 'G V I', nb_mesh_elem, vi = p_model_maille)
        call wkvect(model_noeud , 'G V I', nb_mesh_node, vi = p_model_noeud)
!
! ----- Working objects
!
        AS_ALLOCATE(vi = p_wk_mail1, size = nb_mesh_elem)
        AS_ALLOCATE(vi = p_wk_mail2, size = nb_mesh_elem)
        AS_ALLOCATE(vi = p_wk_mail3, size = nb_mesh_elem)
        AS_ALLOCATE(vi = p_wk_node , size = nb_mesh_node)
!
! ----- Loop on AFFE keyword
!
        do iaffe = 1, nb_affe
!
            dim_topo_init=-99
            p_wk_mail2(1:nb_mesh_elem) = 0
            p_wk_mail3(1:nb_mesh_elem) = 0
!
! --------- Get phenomene/modelisation
!
            call getvtx(keywordfact, 'PHENOMENE'   , iocc=iaffe, scal=phenom)
            call getvtx(keywordfact, 'MODELISATION', iocc=iaffe, nbval=10, vect=list_modelisa,&
                        nbret=nb_modelisa)
            ASSERT(nb_modelisa.gt.0)
            ASSERT(nb_modelisa.le.10)
!
! --------- Get elements
!
            call jedetr(list_elem)
            call getelem(mesh   , keywordfact, iaffe , ' ', list_elem,&
                         nb_elem)
!
! --------- Get nodes
!
            call jedetr(list_node)
            call getnode(mesh   , keywordfact, iaffe , ' ', list_node,&
                         nb_node, elem_excl = .true._1)
!
! --------- Loop on modelisations
!
            do imodel = 1, nb_modelisa
!
! ------------- Current modelisation
!
                modeli = list_modelisa(imodel)
                call jenonu(jexnom('&CATA.'//phenom(1:13)//'.MODL', modeli), idx_modelisa)
                call jeveuo(jexnum('&CATA.'//phenom, idx_modelisa), 'L', vi = p_cata_model)
                phemod = phenom//modeli
!
! ------------- Topological(kind=1) dimensions
!
                call dismoi('DIM_TOPO', phemod, 'PHEN_MODE', repi=dim_topo_curr)
                if (dim_topo_init .eq. -99) then
                    dim_topo_init = dim_topo_curr
                else
                    if (dim_topo_init .ne. dim_topo_curr) then
                        call utmess('F', 'MODELE1_1')
                    endif
                endif
!
! ------------- Loop on elements 
!
                if (nb_elem .ne. 0) then
                    ASSERT(nb_node.eq.0)
                    l_elem = .true.
                    call jeveuo(list_elem, 'L', vi = p_list_elem)
                    do ielem = 1, nb_elem
                        nume_elem      = p_list_elem(ielem)
                        nume_type_geom = p_mesh_type_geom(nume_elem)
                        if (p_cata_model(nume_type_geom) .gt. 0) then
                            p_model_maille(nume_elem) = p_cata_model(nume_type_geom)
                            p_wk_mail3(nume_elem)     = p_cata_model(nume_type_geom)
                        endif
                        p_wk_mail1(nume_elem) = 1
                        if (p_cata_dim(nume_type_geom) .eq. dim_topo_init) then
                            p_wk_mail2(nume_elem) = 1
                        endif
                    end do
                endif
!
! ------------- Loop on nodes 
!
                if (nb_node .ne. 0) then
                    ASSERT(nb_elem.eq.0)
                    l_node = .true.
                    call jeveuo(list_node, 'L', vi = p_list_node)
                    do inode = 1, nb_node
                        nume_node      = p_list_node(inode)
                        if (p_cata_model(nume_type_poi1) .gt. 0) then
                            p_model_noeud(nume_node)= p_cata_model(nume_type_poi1)
                        endif
                        p_wk_node(nume_node) = 1
                    end do
                endif
!
! ------------- ON VERIFIE QU'A CHAQUE OCCURENCE DE AFFE, LES MAILLES
! ------------- "PRINCIPALES" ONT BIEN ETE AFFECTEES PAR DES ELEMENTS
! ------------- (PB DES MODELISATIONS A "TROUS") :
!
                ico=0
                do nume_elem = 1, nb_mesh_elem
                    if ((p_wk_mail2(nume_elem).eq.1) .and. (p_wk_mail3(nume_elem).eq.0)) then
                        ico=ico+1
                    endif
                end do
                if (ico .gt. 0) then
                    vali(1) = iaffe
                    vali(2) = ico
                    vali(3) = dim_topo_init
                    call utmess('A', 'MODELE1_70', ni=3, vali=vali)
                endif
            end do
!
! --------- Check if user elements have been affected
!
            nb_elem_naffe = 0
            do ielem = 1, nb_mesh_elem
                nume_elem = ielem
                if (p_wk_mail1(nume_elem) .eq. 1) then
                    if (p_model_maille(nume_elem) .eq. 0) then
                        nb_elem_naffe  = nb_elem_naffe+1
                        call jenuno(jexnum(mesh_name_elem, nume_elem), name_elem)
                        nume_type_geom = p_mesh_type_geom(nume_elem)
                        call jenuno(jexnum('&CATA.TM.NOMTM', nume_type_geom), name_type_geom)
                        if (niv .eq. 2) then
                            valk(1) = name_elem
                            valk(2) = name_type_geom
                            call utmess('I','MODELE1_2', nk=2, valk=valk)
                        endif
                    endif
                endif
            end do
!
! --------- Check if user nodes have been affected
!
            nb_node_naffe = 0
            do inode = 1, nb_mesh_node
                nume_node = inode
                if (p_wk_node(nume_node) .eq. 1) then
                    if (p_model_noeud(nume_node) .eq. 0) then
                        nb_node_naffe = nb_node_naffe+1
                        call jenuno(jexnum(mesh_name_node, nume_node), name_node)
                        if (niv .eq. 2) then
                            call utmess('I','MODELE1_3', sk = name_node)
                        endif
                    endif
                endif
            end do
        end do
!
! ----- Count number of GREL
!
        nb_grel         = 0
!
! ----- Count number of GREL - Elements
!
        nb_elem_affe    = 0
        nume_type_model = 0
        do ielem = 1, nb_mesh_elem
            nume_elem = ielem
            if (p_model_maille(nume_elem) .ne. 0) then
                nb_elem_affe = nb_elem_affe+1
                if (p_model_maille(nume_elem) .ne. nume_type_model) then
                    nume_type_model = p_model_maille(nume_elem)
                    nb_grel         = nb_grel+1
                endif
            endif
        end do
!
! ----- Count number of GREL - Nodes
!
        nb_node_affe    = 0
        nume_type_model = 0
        do inode = 1, nb_mesh_node
            nume_node = inode
            if (p_model_noeud(nume_node) .ne. 0) then
                nb_node_affe = nb_node_affe+1
                if (p_model_noeud(nume_node) .ne. nume_type_model) then
                    nume_type_model = p_model_noeud(nume_node)
                    nb_grel         = nb_grel+1
                endif
            endif
        end do
!
! ----- Printing informations
!
        if (l_elem) then
            vali(1) = nb_mesh_elem
            vali(2) = nb_elem_affe+nb_elem_naffe
            vali(3) = nb_elem_affe
            call utmess('I','MODELE1_4', sk=mesh, ni = 3, vali = vali)
        endif
        if (l_node) then
            vali(1) = nb_mesh_node
            vali(2) = nb_node_affe+nb_node_naffe
            vali(3) = nb_node_affe
            call utmess('I','MODELE1_5', sk=mesh, ni = 3, vali = vali)
        endif
        if (nb_elem_affe .eq. 0) then
            call utmess('F', 'MODELE1_6', sk=mesh)
        endif          
!
! ----- Create LIEL
!
        lont_liel = nb_grel+nb_elem_affe+nb_node_affe
        call jecrec(model_liel, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                    nb_grel)
        call jeecra(model_liel, 'LONT', lont_liel)
        call jeveuo(model_liel, 'E', vi = p_model_liel)
!
! ----- Store GREL in LIEL - Elements
!
        nume_type_model = 0
        nume_grel       = 0
        long_grel       = 0
        idx_in_grel     = 0
        do nume_elem = 1, nb_mesh_elem
            if (p_model_maille(nume_elem) .ne. 0) then
!
! ------------- Create new GREL
!
                if (p_model_maille(nume_elem) .ne. nume_type_model .and. &
                    nume_type_model .ne. 0) then
                    nume_grel   = nume_grel+1
                    long_grel   = long_grel+1
                    idx_in_grel = idx_in_grel+1
                    p_model_liel(idx_in_grel) = nume_type_model
                    call jecroc(jexnum(model_liel, nume_grel))
                    call jeecra(jexnum(model_liel, nume_grel), 'LONMAX', long_grel)
                    long_grel   = 0
                endif
!
! -------------- Add element in GREL
!
                long_grel   = long_grel+1
                idx_in_grel = idx_in_grel+1
                p_model_liel(idx_in_grel) = nume_elem
                nume_type_model           = p_model_maille(nume_elem)
            endif
!
! --------- Last element
!
            if (nume_elem .eq. nb_mesh_elem .and. long_grel .ne. 0) then
                nume_grel   = nume_grel+1
                long_grel   = long_grel+1
                idx_in_grel = idx_in_grel+1
                p_model_liel(idx_in_grel) = nume_type_model
                call jecroc(jexnum(model_liel, nume_grel))
                call jeecra(jexnum(model_liel, nume_grel), 'LONMAX', long_grel)
            endif
        end do
!
! ----- Store GREL in LIEL - Nodes
!
        nume_type_model=0
        numsup    = 0
        long_grel = 0
        do nume_node = 1, nb_mesh_node
            if (p_model_noeud(nume_node) .ne. 0) then
!
! ------------- Create new GREL
!
                if (p_model_noeud(nume_node) .ne. nume_type_model .and. &
                    nume_type_model .ne. 0) then
                    nume_grel   = nume_grel+1
                    long_grel   = long_grel+1
                    idx_in_grel = idx_in_grel+1
                    p_model_liel(idx_in_grel) = nume_type_model
                    call jecroc(jexnum(model_liel, nume_grel))
                    call jeecra(jexnum(model_liel, nume_grel), 'LONMAX', long_grel)
                    long_grel   = 0
                endif
!
! ------------- Add node in GREL
!
                long_grel   = long_grel+1
                idx_in_grel = idx_in_grel+1
                numsup      = numsup+1
                p_model_liel(idx_in_grel) = -numsup
                nume_type_model           = p_model_noeud(nume_node)
            endif
!
! --------- Last node
!
            if (nume_node .eq. nb_mesh_node .and. long_grel .ne. 0) then
                nume_grel   = nume_grel+1
                long_grel   = long_grel+1
                idx_in_grel = idx_in_grel+1
                p_model_liel(idx_in_grel) = nume_type_model
                call jecroc(jexnum(model_liel, nume_grel))
                call jeecra(jexnum(model_liel, nume_grel), 'LONMAX', long_grel)
            endif
        end do
!
! ----- Create NEMA
!
        lont_nema = nb_node_affe*2
        if (nb_node_affe .ne. 0) then
            call jecrec(model_nema, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                        nb_node_affe)
            call jeecra(model_nema, 'LONT', lont_nema)
            call jeveuo(model_nema, 'E', vi = p_model_nema)
        endif
!
! ----- Store nodes in NEMA
!
        if (nb_node_affe .ne. 0) then
            idx_in_grel = 0
            numsup      = 0
            do nume_node = 1, nb_mesh_node
                if (p_model_noeud(nume_node) .ne. 0) then
                    p_model_nema(idx_in_grel+1) = nume_node
                    p_model_nema(idx_in_grel+2) = nume_type_poi1
                    idx_in_grel = idx_in_grel+2
                    numsup      = numsup+1
                    call jecroc(jexnum(model_nema, numsup))
                    call jeecra(jexnum(model_nema, numsup), 'LONMAX', 2)
                endif
            end do
        endif
!
        AS_DEALLOCATE(vi = p_wk_mail1)
        AS_DEALLOCATE(vi = p_wk_mail2)
        AS_DEALLOCATE(vi = p_wk_mail3)
        AS_DEALLOCATE(vi = p_wk_node )
    endif
!
! - AFFE_SOUS_STRUCT
!
    if (nb_affe_ss.gt.0) then
        call ssafmo(model)
    endif
!
! - Automatic GREL size adaptation
!
    call adalig(ligrel)
!
! - Set element/(IGREL,IM) object
!
    call cormgi('G', ligrel)
!
! - Init elements for this LIGREL
!
    call initel(ligrel, l_calc_rigi)
    if ((.not.l_calc_rigi).and.(nb_affe.ne.0)) then
        call utmess('A', 'MODELE1_64', sk=model)
    endif
!
! - Check model
!
    call model_check(model, l_veri_elem)
!
! - Print model information
!
    if (nb_affe .gt. 0) then
        call model_print(model)
    endif
!
! - SD_PARTITION
!
    call ajlipa(model, 'G')
!
! - Create grandeurs caracteristiques
!
    if (l_grandeur_cara) then
        keywordfact = 'GRANDEUR_CARA'
        call cetucr(keywordfact, model)
    endif
!
! - Finite volumes ?
!
    call dismoi('EXI_VF', ligrel, 'LIGREL', repk=repk)
    l_volu_fini = repk.eq.'OUI'
!
! - Need neighbours ?
!
    call dismoi('BESOIN_VOISIN', ligrel, 'LIGREL', repk=repk)
    l_need_neigh = repk.eq.'OUI'
!
! - Create SD_VOISINAGE if necessary
!
    if (l_volu_fini .or. l_need_neigh) then
        call crevge(ligrel, 'G')
    endif
!
    call jedema()
end subroutine
