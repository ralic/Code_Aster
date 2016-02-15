subroutine postkutil(nomres, nomfis, repmat, repmod)
    implicit none
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/cesexi.h"
#include "asterfort/cncinv.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/lteatt.h"
#include "asterfort/utmess.h"
#include "asterfort/xtmafi.h"
    character(len=*), intent(in) :: nomres
    character(len=*), intent(in) :: nomfis
    character(len=*), intent(out) :: repmat
    character(len=*), intent(out) :: repmod
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: sam.cuvilliez at edf.fr
! ----------------------------------------------------------------------
!
! in:
!   resu   : nom d'une sd_resultat
!   nomfis : nom d'une sd_fond_fiss ou d'une sd_fiss_xfem
! out:
!   repmat : nom d'une sd_mater
!   repmod : "nom" d'une modelisation parmi '3D', 'AXIS', 'D_PLAN', 'C_PLAN'
!
! ------------------------------------------------------------------
!
! but : recuperer un nom de modélisation (parmi '3D', 'AXIS', 'D_PLAN', 
! ---   'C_PLAN') et le nom d'une sd_mater dans la sd_resultat resu,
!       connaissant nomfis (sd_fond_fiss en fem ou sd_fiss_xfem en xfem)
!
!       On recupere la liste des mailles "voisines" du fond de fissure
!       (pour fem les mailles connectees aux noeuds du fond de fissure, 
!       et pour xfem les mailles CTIP). On s'assure que la meme sd_mater 
!       et la meme modelisation ont ete affectees sur ces mailles.
!
!       On renvoie le nom de cette sd_mater et le nom de cette modelisation.
!       Cette routine est utilisee depuis le corps de la macro POST_K1_K2_K3
!
! ----------------------------------------------------------------------
!
    integer :: nbma, ier, nfiss, iret, ima, nutyel, iad, i
    integer :: imodeli, ndim, jcesl, jcesd, nbcmp, icmp, vin_modeli(4)
    integer :: ier1, ier2, ier3, nbnof, ino, inoeu
    integer :: ideb, ifin, nmanoe, jmanoe, imanoe, itypma, ndime
    integer :: cpt_ma_fon, cpt_ma_noe, imaf, j, ima1, ima2, cpt_dbl
    integer :: nbma_tmp, nbma_fon
    character(len=8) :: nommod, nomchm, noma, vk8_typmod(4)
    character(len=8) :: vk8_modeli(4), vk8_mater(5)
    character(len=8) :: k8typmo, k8model, k8mater, k8noeu, k8typma
    character(len=16) :: ktyel
    character(len=19) :: chmat, cesmat, cnxinv
    character(len=24) :: mesmai, limafo
    aster_logical :: l_xfem
    integer, pointer :: vmatmp(:) => null()
    integer, pointer :: vmafon(:) => null()
    integer, pointer :: vtyele(:) => null()
    integer, pointer :: vtypma(:) => null()
    character(len=8), pointer :: vcesv(:) => null()
    character(len=8), pointer :: vcesk(:) => null()
    character(len=8), pointer :: v8fiss(:) => null()
    character(len=8), pointer :: vnofon(:) => null()
    character(len=8), pointer :: vnofo_inf(:) => null()
    character(len=8), pointer :: vnofo_sup(:) => null()
!
    data vk8_typmod/ 'COMP3D',   'AXIS', 'D_PLAN', 'C_PLAN'/
    data vk8_modeli/     '3D',   'AXIS', 'D_PLAN', 'C_PLAN'/
    data vin_modeli/        3,        2,        2,        2/
!
!   --------------------------------------------------------------------
!   debut
!   --------------------------------------------------------------------
!
    call jemarq()
!
!   --------------------------------------------------------------------
!   prealables
!   --------------------------------------------------------------------
!
!   recup du modele et du vecteur .MAILLE
    call dismoi('NOM_MODELE', nomres, 'RESULTAT', repk=nommod)
    ASSERT( (nommod .ne. '#AUCUN') .or. (nommod .ne. '#PLUSIEURS') )
    call jeveuo(nommod//'.MAILLE', 'E', vi=vtyele)
!
!   recup du maillage de definition du modele
    call dismoi('NOM_MAILLA', nommod, 'MODELE', repk=noma)
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
!
!   recup de la dimension du probleme
    call dismoi('DIM_GEOM', nommod, 'MODELE', repi=ndim)
    ASSERT( (ndim .eq. 2) .or. (ndim .eq. 3) )
!
!   recup de la carte cham_mater//'.CHAMP_MAT' sous forme de cham_elem_s
    call dismoi('CHAM_MATER', nomres, 'RESULTAT', repk=nomchm)
    ASSERT( (nomchm .ne. '#AUCUN') .or. (nomchm .ne. '#PLUSIEURS') )
!
    chmat = nomchm//'.CHAMP_MAT'
    cesmat = '&&POSTKUTIL.CESMAT'
    call carces(chmat, 'ELEM', ' ', 'V', cesmat, 'A', iret)
    ASSERT( iret .eq. 0 )
!
    call jeveuo(cesmat//'.CESV', 'L', vk8=vcesv)
    call jeveuo(cesmat//'.CESL', 'L', jcesl)
    call jeveuo(cesmat//'.CESD', 'L', jcesd)
    call jeveuo(cesmat//'.CESK', 'L', vk8=vcesk)
    nbcmp = zi(jcesd-1+2)
!
!   verifs sur le cham_mater
    ASSERT( vcesk(1) .eq. noma )
    ASSERT( vcesk(3) .eq. 'ELEM' )
    ASSERT( zi(jcesd-1+1) .eq. nbma )
    ASSERT( (nbcmp .eq. 1) .or. (nbcmp .eq. 5) )
    ASSERT( zi(jcesd-1+3) .eq. 1 )
    ASSERT( zi(jcesd-1+4) .eq. 1 )
!
!   s'agit-il d'un modele fem ou x-fem
    l_xfem = .false.
    call jeexin(nommod//'.FISS', ier)
    if (ier .ne. 0) then
        l_xfem = .true.
    endif
!
!   --------------------------------------------------------------------
!   recup de la liste vmafon des mailles principales CTIP dans le cas xfem
!   --------------------------------------------------------------------
!
    if (l_xfem) then
!
!       verif sur la sd_fiss_xfem in
        call dismoi('NB_FISS_XFEM', nommod, 'MODELE', repi=nfiss)
        call jeveuo(nommod//'.FISS', 'L', vk8=v8fiss)
        ASSERT( indik8(v8fiss, nomfis, 1, nfiss) .gt. 0 )
!
!       recup de la liste des mailles CTIP pour la fissure nomfis
        limafo = '&&XMOT2M.NUM_MAILLES'
        mesmai = '&&XMOT2M.MES_MAILLES'
        call xtmafi(ndim, [nomfis], 1, limafo, mesmai, nbma_fon, model=nommod, typ_enr='CTIP')
        call jeveuo(limafo, 'L', vi=vmafon)
!
!       menage
        call jedetr(mesmai)
!
!   --------------------------------------------------------------------
!   recup de la liste vmafon des mailles principales connectees aux noeuds 
!   du fond dans le cas fem
!   --------------------------------------------------------------------
!
    else
!
!       creation de la connectivite inverse
        cnxinv = '&&POSTKUTIL.CNXINV'
        call cncinv(noma, [0], 0, 'V', cnxinv)
!
!       recup du vecteur .TYPMAIL
        call jeveuo(noma//'.TYPMAIL', 'L', vi=vtypma)
!
!       recup de la liste des noeuds du fond
        call jeexin(nomfis//'.FOND.NOEU', ier1)
        call jeexin(nomfis//'.FOND_INF.NOEU', ier2)
        call jeexin(nomfis//'.FOND_SUP.NOEU', ier3)
        ASSERT( (ier1 .gt. 0) .or. ((ier2 .gt. 0) .and. (ier3 .gt. 0)) )
        if ( ier1 .ne. 0 ) then
            call jeveuo(nomfis//'.FOND.NOEU', 'L', vk8=vnofon)
        endif
        if ( ier2 .ne. 0 ) then
            call jeveuo(nomfis//'.FOND_INF.NOEU', 'L', vk8=vnofo_inf)
            call jeveuo(nomfis//'.FOND_SUP.NOEU', 'L', vk8=vnofo_sup)
            AS_ALLOCATE(vk8=vnofon, size=size(vnofo_inf)+size(vnofo_sup))
            ideb = 1
            ifin = size(vnofo_inf)
            vnofon(ideb:ifin) = vnofo_inf(:)
            ideb = size(vnofo_inf) + 1
            ifin = size(vnofo_inf) + size(vnofo_sup)
            vnofon(ideb:ifin) = vnofo_sup(:)
        endif
        nbnof = size(vnofon)
!
!       1ere boucle sur les noeuds du fond : dimensionnement de la liste
!       brute des mailles principales connectees aux noeud du fond
        cpt_ma_fon = 0
        do ino = 1, nbnof
!
!           recup des mailles connectees au noeud courant
            k8noeu = vnofon(ino)
            call jenonu(jexnom(noma//'.NOMNOE', k8noeu), inoeu)
            call jelira(jexnum(cnxinv, inoeu), 'LONMAX', nmanoe)
            call jeveuo(jexnum(cnxinv, inoeu), 'L', jmanoe)
!           on s'assure que le noeud n'est pas orphelin
            ASSERT( zi(jmanoe-1+1) .ne. 0 )
!
!           boucle sur les mailles connectees au noeud courant
            cpt_ma_noe = 0
            do ima = 1, nmanoe
!
!               on ne garde que les mailles principales
                imanoe = zi(jmanoe-1+ima)
                itypma = vtypma(imanoe)
                call jenuno(jexnum('&CATA.TM.NOMTM', itypma), k8typma)
                call dismoi('DIM_TOPO', k8typma, 'TYPE_MAILLE', repi=ndime)
                if (ndime .eq. ndim) then
                    cpt_ma_noe = cpt_ma_noe + 1
                endif
!
            enddo
            ASSERT( cpt_ma_noe .gt. 0 )
            cpt_ma_fon = cpt_ma_fon + cpt_ma_noe
!
        enddo
        nbma_tmp = cpt_ma_fon
        AS_ALLOCATE(vi=vmatmp, size=nbma_tmp)
!
!       2eme boucle sur les noeuds du fond : remplissage de la liste
!       brute des mailles principales connectees aux noeud du fond
        cpt_ma_fon = 0
        do ino = 1, nbnof
!
!           recup des mailles connectees au noeud courant
            k8noeu = vnofon(ino)
            call jenonu(jexnom(noma//'.NOMNOE', k8noeu), inoeu)
            call jelira(jexnum(cnxinv, inoeu), 'LONMAX', nmanoe)
            call jeveuo(jexnum(cnxinv, inoeu), 'L', jmanoe)
!           on s'assure que le noeud n'est pas orphelin
            ASSERT( zi(jmanoe-1+1) .ne. 0 )
!
!           boucle sur les mailles connectees au noeud courant
            do ima = 1, nmanoe
!
!               on ne garde que les mailles principales
                imanoe = zi(jmanoe-1+ima)
                ASSERT( imanoe .gt. 0 )
                itypma = vtypma(imanoe)
                call jenuno(jexnum('&CATA.TM.NOMTM', itypma), k8typma)
                call dismoi('DIM_TOPO', k8typma, 'TYPE_MAILLE', repi=ndime)
                if (ndime .eq. ndim) then
                    cpt_ma_fon = cpt_ma_fon + 1
                    vmatmp(cpt_ma_fon) = imanoe
                endif
!
            enddo
!
        enddo
!
!       suppression des doublons dans la liste brute et allocation
!       de liste sans doublons
        cpt_dbl = 0
        do i = 1, nbma_tmp
            ima1 = vmatmp(i)
            if ( ima1 .ne. 0 ) then
                do j = i+1, nbma_tmp
                    ima2 = vmatmp(j)
                    if ( (ima1 .eq. ima2) .and. (ima2 .ne. 0) ) then
                        vmatmp(j) = 0
                        cpt_dbl = cpt_dbl + 1
                    endif
                enddo
            endif
        enddo
        nbma_fon = nbma_tmp - cpt_dbl
        ASSERT( nbma_fon .gt. 0 )
        AS_ALLOCATE(vi=vmafon, size=nbma_fon)
!
!       remplissage de la liste sans doublons
        cpt_ma_fon = 0
        do i = 1, nbma_tmp
            if ( vmatmp(i) .ne. 0 ) then
                cpt_ma_fon = cpt_ma_fon + 1
                vmafon(cpt_ma_fon) = vmatmp(i)
            endif
        enddo
!
!       menage
        call jedetr(cnxinv)
        if ( ier2 .ne. 0 ) then
            AS_DEALLOCATE(vk8=vnofon)
        endif
        AS_DEALLOCATE(vi=vmatmp)
!
    endif
!
!   --------------------------------------------------------------------
!   recup du nom du materiau et de la modelisation sur les mailles 
!   principales situees "au voisinage du fond de la fissure" (fem ou xfem)
!   --------------------------------------------------------------------
!
!   recup de la modelisation sur la 1ere maille de vmafon
    imodeli = 0
    imaf = vmafon(1)
    nutyel = vtyele(imaf)
    call jenuno(jexnum('&CATA.TE.NOMTE', nutyel), ktyel)
    do i = 1, 4
        if (lteatt('TYPMOD', vk8_typmod(i), ktyel)) then
            imodeli = i
            exit
        endif
    enddo
    ASSERT( imodeli .gt. 0 )
    k8typmo = vk8_typmod(imodeli)
    k8model = vk8_modeli(imodeli)
    ASSERT( ndim .eq. vin_modeli(imodeli) )
!
!   recup du materiau sur la 1ere maille de vmafon
    vk8_mater(:) = ''
    imaf = vmafon(1)
    do icmp = 1, nbcmp
        call cesexi('S', jcesd, jcesl, imaf, 1, 1, icmp, iad)
        ASSERT( iad .gt. 0 )
        vk8_mater(icmp) = vcesv(iad)
    enddo
    if ( nbcmp.gt.1 ) then
        ASSERT( vk8_mater(2) .eq. 'TREF=>' )
    endif
    k8mater = vk8_mater(1)
!
!   boucle sur les mailles de vmafon
    do ima = 1, nbma_fon
!
        imaf = vmafon(ima)
!
!       verif que toutes les mailles de vmafon sont affectees avec
!       la meme modelisation
        nutyel = vtyele(imaf)
        call jenuno(jexnum('&CATA.TE.NOMTE', nutyel), ktyel)
        ASSERT( lteatt('TYPMOD', k8typmo, ktyel) )
!
!       verif que toutes les mailles de vmafon sont affectees avec
!       le meme materiau
        do icmp = 1, nbcmp
            call cesexi('S', jcesd, jcesl, imaf, 1, 1, icmp, iad)
            ASSERT( iad .gt. 0 )
            ASSERT( vk8_mater(icmp) .eq. vcesv(iad) )
        enddo
!
    enddo
!
!   --------------------------------------------------------------------
!   variables out
!   --------------------------------------------------------------------
!
    repmat = k8mater
    repmod = k8model
!
!   --------------------------------------------------------------------
!   menage
!   --------------------------------------------------------------------
!
    if (l_xfem) then
        call jedetr(limafo)
    else
        AS_DEALLOCATE(vi=vmafon)
    endif
!
    call detrsd('CHAM_ELEM_S', cesmat)
!
!   --------------------------------------------------------------------
!   fin
!   --------------------------------------------------------------------
!
    call jedema()
!
end subroutine
