subroutine cafaci(char, noma, ligrmo, fonree)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/afddli.h"
#include "asterfort/aflrch.h"
#include "asterfort/afrela.h"
#include "asterfort/assert.h"
#include "asterfort/canort.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
#include "asterfort/xddlim.h"
#include "asterfort/char_excl_keyw.h"
#include "asterfort/char_read_val.h"
#include "asterfort/char_read_keyw.h"
#include "asterfort/char_read_mesh.h"
#include "asterfort/char_xfem.h"
!
    character(len=4), intent(in)  :: fonree
    character(len=8), intent(in)  :: char, noma
    character(len=19), intent(in) :: ligrmo
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
!     BUT: CREER LES CARTES CHAR.CHME.CMULT ET CHAR.CHME.CIMPO
!          ET REMPLIR LIGRCH POUR FACE_IMPO
!
! ARGUMENTS D'ENTREE:
!      FONREE  : TYPE DE LA VALEUR IMPOSEE :
!                REEL OU FONC OU COMP
!      CHAR  : NOM UTILISATEUR DU RESULTAT DE CHARGE
!
!
    integer :: n_max_keyword
    parameter (n_max_keyword=300)
    integer :: ddlimp(n_max_keyword)
    real(kind=8) :: valimr(n_max_keyword)
    complex(kind=8) :: valimc(n_max_keyword)
    character(len=8) :: valimf(n_max_keyword)
    character(len=16) :: keywordlist(n_max_keyword)
!
    integer :: i
    integer :: nbnoeu, jval, jdirec, nbno
    integer :: idim, in, jnorm, jtang, jnono, nfaci
    integer :: ibid,  ier, ndim, jcompt
    integer :: ino, jprnm, nbec
    integer :: nbma, nbcmp, inom
    real(kind=8) :: coef, direct(3)
    complex(kind=8) :: coefc
    integer :: i_keyword, n_keyword
    character(len=24) :: list_node, list_elem
    integer :: jlino, jlima
    character(len=2) :: typlag
    character(len=4) :: typcoe
    character(len=8) :: nomo, nomg
    character(len=8) :: nomnoe, ddl, k8bid
    character(len=16) :: keywordfact, keyword
    character(len=19) :: lisrel
    character(len=19) :: connex_inv
    character(len=19) :: ch_xfem_stat, ch_xfem_node, ch_xfem_lnno, ch_xfem_ltno
    integer :: jnoxfl, jnoxfv
    logical :: lxfem, l_ocmp
    logical :: l_dtan, l_dnor
    integer :: val_nb_dnor, val_nb_dtan
    real(kind=8) :: val_r_dnor, val_r_dtan
    character(len=8) :: val_f_dnor, val_f_dtan
    complex(kind=8):: val_c_dnor, val_c_dtan
    character(len=16) :: val_t_dnor, val_t_dtan
    character(len=24) :: keywordexcl
    integer :: n_keyexcl
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    keywordfact = 'FACE_IMPO'
    call getfac(keywordfact, nfaci)
    if (nfaci .eq. 0) goto 999
!
! - Initializations
!
    lisrel = '&&CAFACI.RLLISTE'
    typlag = '12'
    coefc = (1.0d0,0.0d0)
    coef  = 1.0d0
    ddl   = 'DEPL'
!
    typcoe = 'REEL'
    if (fonree .eq. 'COMP') ASSERT(.false.)
    nomo = ligrmo(1:8)
!
! - Create list of excluded keywords for using in char_read_keyw
!
    keywordexcl = '&&CAFACI.KEYWORDEXCL'
    call char_excl_keyw(keywordfact, keywordexcl, n_keyexcl)
!
! - Information about <GRANDEUR>
! 
    nomg = 'DEPL_R'
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomg), 'L', inom)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomg), 'LONMAX', nbcmp, k8bid)
    call dismoi('F', 'NB_EC', nomg, 'GRANDEUR', nbec,&
                k8bid, ier)
    ASSERT(nbec.le.10)
    call jeveuo(ligrmo//'.PRNM', 'L', jprnm)
!
    call dismoi('F', 'DIM_GEOM', nomo, 'MODELE', ndim,&
                k8bid, ier)
!
! - Xfem fields
! 
    call char_xfem(noma, nomo, lxfem, connex_inv, ch_xfem_stat, &
                    ch_xfem_node, ch_xfem_lnno, ch_xfem_ltno)    
    if (lxfem) then
        call jeveuo(ch_xfem_node//'.CNSL', 'L', jnoxfl)
        call jeveuo(ch_xfem_node//'.CNSV', 'L', jnoxfv)
    endif
!
    do i = 1, nfaci
!
! ----- Read mesh affectation
!
        list_node = '&&CAFACI.LIST_NODE'
        list_elem = '&&CAFACI.LIST_ELEM'
        call char_read_mesh(noma, keywordfact, i ,list_node, nbno,&
                            list_elem, nbma)
        call jeveuo(list_node,'L',jlino)
        call jeveuo(list_elem,'L',jlima)
!
! ----- Read affected components and their values
!
        call char_read_keyw(keywordfact, i , fonree, n_keyexcl, keywordexcl,  &
                            n_max_keyword, n_keyword  ,keywordlist, ddlimp, valimr, &
                            valimf, valimc)
!
! ----- Detection of DNOR, DTAN and others
!
        call char_read_val(keywordfact, i, 'DNOR', fonree, val_nb_dnor, &
                           val_r_dnor, val_f_dnor, val_c_dnor, val_t_dnor)
        l_dnor = val_nb_dnor.gt.0
        call char_read_val(keywordfact, i, 'DTAN', fonree, val_nb_dtan, &
                           val_r_dtan, val_f_dtan, val_c_dtan, val_t_dtan)
        l_dtan = val_nb_dtan.gt.0
        l_ocmp = n_keyword.gt.0
!
! ----- Some verifications
!  
        if (ndim .eq. 3 .and. l_dtan) call u2mess('F', 'CHARGES2_63')

        if (fonree .eq. 'FONC') then
            if (l_dnor .or. l_dtan) then
                if (.not.(ndim.eq.2.or.ndim.eq.3)) call u2mess('F', 'CHARGES2_6')
            endif
        endif
!
! ----- Normals and/or tangents
!
        if (l_dnor) then
            call canort(noma, nbma, zi(jlima), ndim,&
                        nbno, zi(jlino), 1)
            call jeveuo('&&CANORT.NORMALE', 'L', jnorm)
        endif
        if (l_dtan) then
            call canort(noma, nbma, zi(jlima), ndim,&
                        nbno, zi(jlino), 2)
            call jeveuo('&&CANORT.TANGENT', 'L', jtang)
        endif
!
! ----- If DNOR exists
!
        if (l_dnor) then
            do ino = 1, nbno
                in = zi(jlino+ino-1)
                call jenuno(jexnum(noma//'.NOMNOE', in), nomnoe)
                do idim = 1, ndim
                    direct(idim) = zr(jnorm-1+ndim*(ino-1)+idim)
                end do
!
                if (lxfem) then
                    if (zl(jnoxfl-1+2*in)) then
                        call xddlim(nomo, ddl, nomnoe, in, val_r_dnor,&
                                    val_c_dnor, val_f_dnor, fonree, ibid, lisrel,&
                                    ndim, direct, jnoxfv, ch_xfem_stat, ch_xfem_lnno,&
                                    ch_xfem_ltno, connex_inv)
                        goto 105
                    endif
                endif
!
                call afrela(coef, coefc, ddl, nomnoe, ndim,&
                            direct, val_nb_dnor, val_r_dnor, val_c_dnor, val_f_dnor,&
                            typcoe, fonree, typlag, 0.d0, lisrel)
!
105             continue
            enddo
        endif
!
! ----- If DTAN exists
!
        if (l_dtan) then
            do ino = 1, nbno
                in = zi(jlino+ino-1)
                call jenuno(jexnum(noma//'.NOMNOE', in), nomnoe)
                do idim = 1, ndim
                    direct(idim) = zr(jtang-1+ndim* (ino-1)+idim)
                end do
!
                if (lxfem) then
                    if (zl(jnoxfl-1+2*in)) then
                        call xddlim(nomo, ddl, nomnoe, in, val_r_dtan,&
                                    val_c_dtan, val_f_dtan, fonree, ibid, lisrel,&
                                    ndim, direct, jnoxfv, ch_xfem_stat, ch_xfem_lnno,&
                                    ch_xfem_ltno, connex_inv)
                        goto 115
                    endif
                endif
!
                call afrela(coef, coefc, ddl, nomnoe, ndim,&
                            direct, val_nb_dtan, val_r_dtan, val_c_dtan, val_f_dtan,&
                            typcoe, fonree, typlag, 0.d0, lisrel)

115             continue
            enddo
        endif
!
! ----- If other components exist
!
        if (l_ocmp) then
!
! -------- Overload preparation
!
            call jelira(noma//'.NOMNOE', 'NOMMAX', nbnoeu, k8bid)
            call wkvect('&&CAFACI.NOMS_NOEUDS', 'V V K8', nbnoeu, jnono)
            if (fonree .eq. 'REEL') then
                call wkvect('&&CAFACI.VALDDL', 'V V R', n_keyword*nbnoeu, jval)
            else
                call wkvect('&&CAFACI.VALDDL', 'V V K8', n_keyword*nbnoeu, jval)
            endif
            call wkvect('&&CAFACI.DIRECT', 'V V R', 3*nbnoeu, jdirec)
            call wkvect('&&CAFACI.ICOMPT', 'V V I', n_keyword, jcompt)
!
! --------- Linear relation
!
            do ino = 1, nbno
                in = zi(jlino-1+ino)
                call jenuno(jexnum(noma//'.NOMNOE', in), nomnoe)
                call afddli(zr(jval), zk8(jval), zc(jval), zi(jprnm-1+ (in- 1)*nbec+1), n_keyword,&
                            fonree, nomnoe, in, ddlimp, valimr,&
                            valimf, valimc, keywordlist, zr(jdirec+3* (in-1)), 0,&
                            nomo, lisrel, zk8( inom), nbcmp, zi(jcompt),&
                            lxfem, jnoxfl, jnoxfv, ch_xfem_stat, ch_xfem_lnno,&
                            ch_xfem_ltno, connex_inv)
            enddo
!
! --------- Components doesn't exist on all nodes
!
            do i_keyword = 1,n_keyword
                keyword = keywordlist(i_keyword)
                if (zi(jcompt-1+i_keyword) .eq. 0) then
                    call u2mesk('F', 'CHARGES2_45', 1, keyword)
                endif
            enddo
!
            call jedetr('&&CAFACI.ICOMPT')
            call jedetr('&&CAFACI.NOMS_NOEUDS')
            call jedetr('&&CAFACI.VALDDL')
            call jedetr('&&CAFACI.DIRECT')
!
        endif
!
        call jedetr(list_node)
        call jedetr(list_elem)
!
    end do
!
! - Final linear relation affectation
!
    call aflrch(lisrel, char)
!
    call jedetr('&&CANORT.NORMALE')
    call jedetr('&&CANORT.TANGENT')
    call jedetr(keywordexcl)
    if (lxfem) then
        call jedetr(connex_inv)
        call detrsd('CHAM_NO_S', ch_xfem_node)
        call detrsd('CHAM_ELEM_S', ch_xfem_stat)
        call detrsd('CHAM_ELEM_S', ch_xfem_lnno)
        call detrsd('CHAM_ELEM_S', ch_xfem_ltno)
    endif
!
999 continue
    call jedema()
end subroutine
