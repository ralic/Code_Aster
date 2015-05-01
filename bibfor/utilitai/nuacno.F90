subroutine nuacno(nuagez, list_nodez, chnoz)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
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
    character(len=*), intent(in) :: chnoz
    character(len=*), intent(in) :: list_nodez
    character(len=*), intent(in) :: nuagez
!
! --------------------------------------------------------------------------------------------------
!
! Convert NUAGE to CHAM_NO
!
! --------------------------------------------------------------------------------------------------
!
! In  nuagez       : name of NUAGE datastructure
! In  chnoz        : name of CHAM_NO (nodal field) datastructure
! In  list_node   : list of nodes
!
! --------------------------------------------------------------------------------------------------
!
    integer :: idx_gd
    character(len=4) :: type_scal
    character(len=8) :: mesh, gran_name
    character(len=19) :: chno, list_node, nuage, profchno
    integer :: iaprno, icmp
    integer :: icompt, i_ec, ieq, nume_pt, itype, ival, i_pt
    integer :: j, jnuai, jnuav, k, i_ligr_mesh
    integer :: kcomp, kvale, nc, ncmp, ncmpmx, nb_ec, nb_point
    integer :: num
    integer, pointer :: p_desc(:) => null()
    integer, pointer :: ent_cod(:) => null()
    integer, pointer :: nueq(:) => null()
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
!
    call jeveuo(chno//'.DESC', 'L', vi = p_desc)
    idx_gd = p_desc(1)
    num    = p_desc(2)
    call jelira(jexnum('&CATA.GD.NOMCMP', idx_gd), 'LONMAX', ncmpmx)
    call jenuno(jexnum('&CATA.GD.NOMGD', idx_gd), gran_name)
    nb_ec = nbec(idx_gd)
    call wkvect('&&NUACNO.NOMCMP', 'V V I', ncmpmx, kcomp)
    AS_ALLOCATE(vi=ent_cod, size=nb_ec)
!
    call jeveuo(chno//'.REFE', 'L', vk24 = p_refe)
    mesh     = p_refe(1)(1:8)
    profchno = p_refe(2)(1:19)
    call nueq_chck(profchno, l_error= .true.)
    call dismoi('NB_NO_MAILLA', mesh, 'MAILLAGE', repi=nb_point)
!
    if (list_node .ne. ' ') then
        call jelira(list_node, 'LONUTI', nb_point)
        call jeveuo(list_node, 'L', vi = p_list_node)
    else
        call wkvect('&&NUACNO.NOEUD', 'V V I', nb_point, vi = p_list_node)
        do i_pt = 1, nb_point
            p_list_node(i_pt) = i_pt
        end do
    endif
!
    call jelira(chno//'.VALE', 'TYPE', cval=type_scal)
    call jeveuo(chno//'.VALE', 'E', kvale)
    if (type_scal(1:1) .eq. 'R') then
        itype = 1
    else if (type_scal(1:1) .eq. 'C') then
        itype = 2
    else
        ASSERT(.false.)
    endif
!
    call jeveuo(nuage//'.NUAV', 'L', jnuav)
    call jeveuo(nuage//'.NUAI', 'L', jnuai)
    nc = zi(jnuai+2)
!
!     --SI LE CHAMP EST A REPRESENTATION CONSTANTE ---
!
    if (num .lt. 0) then
        ncmp = -num
        do i_ec = 1, nb_ec
            ent_cod(i_ec) = p_desc(2+i_ec)
        end do
        do j = 1, nb_point
            nume_pt = p_list_node(j)
            ival = ncmp * ( nume_pt - 1 )
            icompt = 0
            do icmp = 1, ncmpmx
                if (exisdg(ent_cod, icmp )) then
                    icompt = icompt + 1
                    k = nc*(j-1) + icompt
                    if (itype .eq. 1) then
                        zr(kvale-1+ival+icmp) = zr(jnuav+k-1)
                    else
                        zc(kvale-1+ival+icmp) = zc(jnuav+k-1)
                    endif
                endif
            end do
        end do
    else
!
!     --- SI LE CHAMP EST DECRIT PAR 1 "PRNO" ---
!
        call jeveuo(profchno//'.NUEQ', 'L',  vi=nueq)
        call jenonu(jexnom(profchno//'.LILI', '&MAILLA'), i_ligr_mesh)
        call jeveuo(jexnum(profchno//'.PRNO', i_ligr_mesh), 'L', iaprno)
        do j = 1, nb_point
            nume_pt = p_list_node(j)
            ival = zi(iaprno-1+ (nume_pt-1)*(nb_ec+2)+1 )
            ncmp = zi(iaprno-1+ (nume_pt-1)*(nb_ec+2)+2 )
            if (ncmp .eq. 0) goto 210
            do i_ec = 1, nb_ec
                ent_cod(i_ec) = zi(iaprno-1+ (nume_pt-1)*(nb_ec+2)+2+i_ec )
            end do
            icompt = 0
            do icmp = 1, ncmpmx
                if (exisdg(ent_cod, icmp )) then
                    icompt = icompt + 1
                    ieq = nueq(ival-1+icompt)
                    k = nc*(j-1) + icompt
                    if (itype .eq. 1) then
                        zr(kvale-1+ieq) = zr(jnuav+k-1)
                    else
                        zc(kvale-1+ieq) = zc(jnuav+k-1)
                    endif
                endif
            end do
210         continue
        end do
    endif
!
    call jedetr('&&NUACNO.NOMCMP')
    AS_DEALLOCATE(vi=ent_cod)
    call jedetr('&&NUACNO.NOEUD')
    call jedema()
end subroutine
