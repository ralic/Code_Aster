subroutine cnonua(nb_dim, chnoz, list_nodez, nuagez)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/crenua.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbec.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "asterfort/nueq_chck.h"
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
!
    integer, intent(in) :: nb_dim
    character(len=*), intent(in) :: chnoz
    character(len=*), intent(in) :: list_nodez
    character(len=*), intent(in) :: nuagez
!
! --------------------------------------------------------------------------------------------------
!
! Convert CHAM_NO to NUAGE
!
! --------------------------------------------------------------------------------------------------
!
! In  nuage       : name of NUAGE datastructure
! In  chno        : name of CHAM_NO (nodal field) datastructure
! In  nb_dim      : dimension of model
! In  list_node   : list of nodes
!
! --------------------------------------------------------------------------------------------------
!
    integer :: idx_gd, num, ncmpmx, nb_ec
    integer :: nb_point, kcoor, kvale, itype
    integer :: nb_cmp_max, i_ec, ianueq, iaprno, nume_pt, ncmp, icompt
    integer :: i_cmp, i_pt, i_dim, i_cmp_mx
    integer :: jnuav, ival, k, ieq, i_ligr_mesh
    character(len=4) :: type_scal
    character(len=8) :: mesh, gran_name
    character(len=19) :: chno, list_node, nuage, profchno
    aster_logical :: l_crea_nual, prem
    integer, pointer :: ent_cod(:) => null()
    integer, pointer :: cmp_name(:) => null()
    integer, pointer :: p_nuai(:) => null()
    real(kind=8), pointer :: p_nuax(:) => null()
    aster_logical, pointer :: p_nual(:) => null()
    integer, pointer :: p_desc(:) => null()
    integer, pointer :: p_list_node(:) => null()
    character(len=24), pointer :: p_refe(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    chno        = chnoz
    list_node   = list_nodez
    nuage       = nuagez
    l_crea_nual = .false.
!
    call jeveuo(chno//'.DESC', 'L', vi = p_desc)
    idx_gd = p_desc(1)
    num    = p_desc(2)
    call jelira(jexnum('&CATA.GD.NOMCMP', idx_gd), 'LONMAX', ncmpmx)
    call jenuno(jexnum('&CATA.GD.NOMGD', idx_gd), gran_name)
    nb_ec = nbec(idx_gd)
    AS_ALLOCATE(vi=cmp_name, size=ncmpmx)
    AS_ALLOCATE(vi=ent_cod, size=nb_ec)
!
    call jeveuo(chno//'.REFE', 'L', vk24 = p_refe)
    mesh     = p_refe(1)(1:8)
    profchno = p_refe(2)(1:19)
!
    call nueq_chck(profchno, l_error= .true.)
    call dismoi('NB_NO_MAILLA', mesh, 'MAILLAGE', repi=nb_point)
    call jeveuo(mesh//'.COORDO    .VALE', 'L', kcoor)
!
    if (list_node .ne. ' ') then
        call jelira(list_node, 'LONUTI', nb_point)
        call jeveuo(list_node, 'L', vi = p_list_node)
    else
        call wkvect('&&CNONUA.NOEUD', 'V V I', nb_point, vi = p_list_node)
        do i_pt = 1, nb_point
            p_list_node(i_pt) = i_pt
        end do
    endif
!
    call jelira(chno//'.VALE', 'TYPE', cval=type_scal)
    call jeveuo(chno//'.VALE', 'L', kvale)
    if (type_scal .eq. 'R') then
        itype = 1
    else if (type_scal .eq. 'C') then
        itype = 2
    else
        ASSERT(.false.)
    endif
!
!
!     --SI LE CHAMP EST A REPRESENTATION CONSTANTE ---
!     ---------------------------------------------------
    if (num .lt. 0) then
        nb_cmp_max = -num
        do i_ec = 1, nb_ec
            ent_cod(i_ec) = p_desc(2+i_ec)
        end do
        do i_cmp_mx = 1, ncmpmx
            if (exisdg(ent_cod, i_cmp_mx)) then
                cmp_name(i_cmp_mx) = i_cmp_mx
            endif
        end do
    else
!
!
!     --- SI LE CHAMP EST DECRIT PAR 1 "PRNO" ---
!     ---------------------------------------------------
        prem = .true.
        call jeveuo(profchno//'.NUEQ', 'L', ianueq)
        call jenonu(jexnom(profchno//'.LILI', '&MAILLA'), i_ligr_mesh)
        call jeveuo(jexnum(profchno//'.PRNO', i_ligr_mesh), 'L', iaprno)
        do i_pt = 1, nb_point
            nume_pt = p_list_node(i_pt)
            ncmp    = zi(iaprno-1+ (nume_pt-1)*(nb_ec+2)+2 )
            if (ncmp .ne. 0) then
                do i_ec = 1, nb_ec
                    ent_cod(i_ec) = zi(iaprno-1+ (nume_pt-1)*(nb_ec+2)+2+i_ec )
                end do
                icompt = 0
                do i_cmp = 1, ncmpmx
                    if (exisdg(ent_cod, i_cmp )) then
                        icompt = icompt + 1
                        cmp_name(i_cmp) = i_cmp
                    endif
                end do
                if (prem) then
                    nb_cmp_max = icompt
                    prem = .false.
                else
                    if (nb_cmp_max .ne. icompt) then
                        nb_cmp_max = max( nb_cmp_max , icompt )
                        l_crea_nual = .true.
                    endif
                endif
            endif
        end do
    endif
!
! - Create NUAGE datastructure
!
    call crenua(nuagez     , gran_name, nb_point, nb_dim, nb_cmp_max,&
                l_crea_nual)
!
! - Set .NUAI
!
    call jeveuo(nuage//'.NUAI', 'E', vi = p_nuai)
    p_nuai(1) = nb_point
    p_nuai(2) = nb_dim
    p_nuai(3) = nb_cmp_max
    p_nuai(4) = idx_gd
    p_nuai(5) = itype
    i_cmp = 0
    do i_cmp_mx = 1, ncmpmx
        if (cmp_name(i_cmp_mx) .ne. 0) then
            i_cmp = i_cmp + 1
            p_nuai(5+i_cmp) = cmp_name(i_cmp_mx)
        endif
    end do
!
! - Set .NUAX
!
    call jeveuo(nuage//'.NUAX', 'E', vr = p_nuax)
    do i_pt = 1, nb_point
        do i_dim = 1, nb_dim
            p_nuax(nb_dim*(i_pt-1)+i_dim) = zr(kcoor-1+3*(i_pt-1)+i_dim)
        end do
    end do
!
! - Set .NUAV and .NUAL
!
    call jeveuo(nuage//'.NUAV', 'E', jnuav)
    if (l_crea_nual) then
        call jeveuo(nuage//'.NUAL', 'E', vl = p_nual)
    endif
!
!     --SI LE CHAMP EST A REPRESENTATION CONSTANTE ---
!
    if (num .lt. 0) then
        ncmp = -num
        do i_pt = 1, nb_point
            nume_pt = p_list_node(i_pt)
            ival = ncmp * ( nume_pt - 1 )
            icompt = 0
            do i_cmp = 1, ncmpmx
                if (exisdg(ent_cod, i_cmp )) then
                    icompt = icompt + 1
                    k = nb_cmp_max*(i_pt-1) + icompt
                    if (itype .eq. 1) then
                        zr(jnuav+k-1) = zr(kvale-1+ival+i_cmp)
                    else
                        zc(jnuav+k-1) = zc(kvale-1+ival+i_cmp)
                    endif
                endif
            end do
        end do
    else
!
!     --- SI LE CHAMP EST DECRIT PAR 1 "PRNO" ---
!
        call jeveuo(profchno//'.NUEQ', 'L', ianueq)
        call jenonu(jexnom(profchno//'.LILI', '&MAILLA'), i_ligr_mesh)
        call jeveuo(jexnum(profchno//'.PRNO', i_ligr_mesh), 'L', iaprno)
        do i_pt = 1, nb_point
            nume_pt = p_list_node(i_pt)
            ival    = zi(iaprno-1+ (nume_pt-1)*(nb_ec+2)+1 )
            ncmp    = zi(iaprno-1+ (nume_pt-1)*(nb_ec+2)+2 )
            if (ncmp .ne. 0) then
                do i_ec = 1, nb_ec
                    ent_cod(i_ec) = zi(iaprno-1+ (nume_pt-1)*(nb_ec+2)+2+i_ec)
                end do
                icompt = 0
                do i_cmp_mx = 1, ncmpmx
                    if (exisdg(ent_cod, i_cmp_mx)) then
                        icompt = icompt + 1
                        ieq = zi(ianueq-1+ival-1+icompt)
                        k = nb_cmp_max*(i_pt-1) + icompt
                        if (l_crea_nual) then
                            p_nual(k) = .true.
                        endif
                        if (itype .eq. 1) then
                            zr(jnuav+k-1) = zr(kvale-1+ieq)
                        else
                            zc(jnuav+k-1) = zc(kvale-1+ieq)
                        endif
                    endif
                end do
            endif
        end do
    endif
!
    AS_DEALLOCATE(vi=cmp_name)
    AS_DEALLOCATE(vi=ent_cod)
    call jedetr('&&CNONUA.NOEUD')
    call jedema()
end subroutine
