subroutine caddli(keywordfact, char, noma, ligrmo, fonree)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getmjm.h"
#include "asterc/getvc8.h"
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/indik8.h"
#include "asterfort/afddli.h"
#include "asterfort/aflrch.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/char_excl_keyw.h"
#include "asterfort/char_impo_liai.h"
#include "asterfort/char_read_val.h"
#include "asterfort/char_read_keyw.h"
#include "asterfort/char_read_mesh.h"
#include "asterfort/char_xfem.h"
#include "asterfort/cncinv.h"
#include "asterfort/cnocns.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/reliem.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
!
    character(len=16), intent(in) :: keywordfact
    character(len=8), intent(in)  :: char
    character(len=8), intent(in)  :: noma
    character(len=19), intent(in) :: ligrmo
    character(len=4), intent(in)  :: fonree
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
!          ET REMPLIR LIGRCH, EN SE SERVANT DE L'OBJET .PRNM
!          POUR AFFECTER LE BON NOMBRE DE DEGRES DE LIBERTE A CHAQUE NOE
!
! ARGUMENTS D'ENTREE:
!
!      NOMCMD  : NOM DE LA COMMANDE
!      MOTFAC  : DDL_IMPO OU TEMP_IMPO OU PRES_IMPO
!      FONREE  : TYPE DE LA VALEUR IMPOSEE :
!                REEL OU FONC OU COMP
!      CHAR    : NOM UTILISATEUR DU RESULTAT DE CHARGE
!

    integer :: n_max_keyword
    parameter (n_max_keyword=300)
    integer :: ddlimp(n_max_keyword)
    real(kind=8) :: valimr(n_max_keyword)
    complex(kind=8) :: valimc(n_max_keyword)
    character(len=8) :: valimf(n_max_keyword)
    character(len=16) :: keywordlist(n_max_keyword)
!
    integer :: i, ino, icmp, ier, numnoe, i_keyword
    integer :: jdirec, jdimen, jval, jprnm, jnom, jcompt
    integer :: n_keyword, nbcmp, nbec, nbnoeu, nddli
!
    character(len=8) :: nomo, k8bid, nomg
    character(len=19) :: lisrel
    character(len=24) :: nomnoe
    complex(kind=8):: coef_cplx
    real(kind=8):: coef_real
    character(len=2) :: type_lagr
    character(len=19) :: connex_inv
    character(len=19) :: ch_xfem_stat, ch_xfem_node, ch_xfem_lnno, ch_xfem_ltno
    integer :: jnoxfl, jnoxfv
    logical :: lxfem
    character(len=24) :: list_node, list_elem
    integer :: jlino
    integer :: nbno, nbma
    character(len=24) :: keywordexcl
    integer :: n_keyexcl
    logical :: l_liai, l_ocmp
    integer :: val_nb_liai
    real(kind=8) :: val_r_liai
    character(len=8) :: val_f_liai
    character(len=16) :: val_t_liai
    complex(kind=8):: val_c_liai
    character(len=8) :: liai_cmp_name(6)
    integer :: liai_cmp_index(6)
    integer :: liai_cmp_nb
    real(kind=8) :: liai_vale_real
    character(len=8) :: liai_vale_fonc
    complex(kind=8):: liai_vale_cplx
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call getfac(keywordfact, nddli)
    if (nddli .eq. 0) goto 999
!
! - Initializations
!
    lisrel = '&&CADDLI.RLLISTE'
    type_lagr = '12'
    coef_cplx = (1.d0,0.d0)
    coef_real = 1.d0
    nomo  = ligrmo(1:8)
!
! - Create list of excluded keywords for using in char_read_keyw
!
    keywordexcl = '&&CADDLI.KEYWORDEXCL'
    call char_excl_keyw(keywordfact, keywordexcl, n_keyexcl)
!
! - Information about <GRANDEUR>
! 
    if (keywordfact.eq. 'DDL_IMPO') then
        nomg = 'DEPL_R'
    else if (keywordfact.eq. 'TEMP_IMPO') then
        nomg = 'TEMP_R'
    else if (keywordfact.eq. 'PRES_IMPO') then
        nomg = 'PRES_C'
    else
        call assert(.false.)
    endif
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomg), 'L', jnom)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomg), 'LONMAX', nbcmp, k8bid)
    call dismoi('F', 'NB_EC', nomg, 'GRANDEUR', nbec,&
                k8bid, ier)
    call assert(nbec.le.10)
    call jeveuo(ligrmo//'.PRNM', 'L', jprnm)
!
    call jelira(noma//'.NOMNOE', 'NOMMAX', nbnoeu, k8bid)
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
! - Loop on factor keyword
!
    do i = 1, nddli
!
! ----- Read mesh affectation
!
        list_node = '&&CADDLI.LIST_NODE'
        list_elem = '&&CADDLI.LIST_ELEM'
        call char_read_mesh(noma, keywordfact, i ,list_node, nbno,&
                            list_elem, nbma)
!
! ----- No nodes (empty groups)
!
        if (nbno.eq.0) goto 60
        call jeveuo(list_node,'L',jlino)
!
! ----- Detection of LIAISON
!
        call char_read_val(keywordfact, i, 'LIAISON', 'TEXT' , val_nb_liai, &
                           val_r_liai, val_f_liai, val_c_liai, val_t_liai)
        l_liai = val_nb_liai.gt.0
!
! ----- Loop on nodes: LIAISON case
!
        if (l_liai) then
!
! -------- Overload preparation
!
            n_keyword = 6
            call wkvect('&&CADDLI.ICOMPT', 'V V I', n_keyword, jcompt)
            if (fonree .eq. 'REEL') then
                call wkvect('&&CADDLI.VALDDL', 'V V R', n_keyword*nbnoeu, jval)
            else if (fonree.eq.'COMP') then
                call wkvect('&&CADDLI.VALDDL', 'V V C', n_keyword*nbnoeu, jval)
            else if (fonree.eq.'FONC') then
                call wkvect('&&CADDLI.VALDDL', 'V V K8', n_keyword*nbnoeu, jval)
            else
                call assert(.false.)
            endif
            call wkvect('&&CADDLI.DIRECT', 'V V R', 3*nbnoeu, jdirec)
            call wkvect('&&CADDLI.DIMENSION', 'V V I', nbnoeu, jdimen)
            call assert(val_nb_liai.eq.1)
            call char_impo_liai(nomg, val_t_liai, liai_cmp_nb, liai_cmp_name, liai_cmp_index, &
                                liai_vale_real, liai_vale_cplx, liai_vale_fonc)
            do ino = 1, nbno
                numnoe = zi(jlino+ino-1)
                call jenuno(jexnum(noma//'.NOMNOE', numnoe), nomnoe)
                n_keyword = 0
                ddlimp = 1
                do icmp = 1, liai_cmp_nb
                    if (exisdg(zi(jprnm-1+(numnoe-1)*nbec+1),liai_cmp_index(icmp))) then
                        n_keyword = n_keyword + 1
                        valimr(n_keyword) = liai_vale_real
                        valimc(n_keyword) = liai_vale_cplx
                        valimf(n_keyword) = liai_vale_fonc
                        keywordlist(n_keyword) = liai_cmp_name(icmp)
                    endif
                enddo
                call afddli(zr(jval), zk8(jval), zc(jval), zi(jprnm-1+(numnoe-1)*nbec+1),n_keyword,&
                            fonree, nomnoe, numnoe, ddlimp, valimr,&
                            valimf, valimc, keywordlist, zr(jdirec+3*(numnoe-1)), 0,&
                            nomo, lisrel, zk8(jnom), nbcmp, zi(jcompt),&
                            lxfem, jnoxfl, jnoxfv, ch_xfem_stat, ch_xfem_lnno,&
                            ch_xfem_ltno, connex_inv)
            enddo
!
        call jedetr('&&CADDLI.ICOMPT')
        call jedetr('&&CADDLI.VALDDL')
        call jedetr('&&CADDLI.DIRECT')
        call jedetr('&&CADDLI.DIMENSION')
        call jedetr('&&CADDLI.NUNOTMP')

        endif
!
! ----- Read affected components and their values
!
        call char_read_keyw(keywordfact, i , fonree, n_keyexcl, keywordexcl,  &
                            n_max_keyword, n_keyword  ,keywordlist, ddlimp, valimr, &
                            valimf, valimc)
        l_ocmp = n_keyword.gt.0
!
! ----- Loop on nodes: other cases
!
        if (l_ocmp) then 
!
! -------- Overload preparation
!
            call wkvect('&&CADDLI.ICOMPT', 'V V I', n_keyword, jcompt)
            if (fonree .eq. 'REEL') then
                call wkvect('&&CADDLI.VALDDL', 'V V R', n_keyword*nbnoeu, jval)
            else if (fonree.eq.'COMP') then
                call wkvect('&&CADDLI.VALDDL', 'V V C', n_keyword*nbnoeu, jval)
            else if (fonree.eq.'FONC') then
                call wkvect('&&CADDLI.VALDDL', 'V V K8', n_keyword*nbnoeu, jval)
            else
                call assert(.false.)
            endif
            call wkvect('&&CADDLI.DIRECT', 'V V R', 3*nbnoeu, jdirec)
            call wkvect('&&CADDLI.DIMENSION', 'V V I', nbnoeu, jdimen)
            do ino = 1, nbno
                numnoe = zi(jlino+ino-1)
                call jenuno(jexnum(noma//'.NOMNOE', numnoe), nomnoe)
                call afddli(zr(jval), zk8(jval), zc(jval), zi(jprnm-1+(numnoe-1)*nbec+1),n_keyword,&
                            fonree, nomnoe, numnoe, ddlimp, valimr,&
                            valimf, valimc, keywordlist, zr(jdirec+3*(numnoe-1)), 0,&
                            nomo, lisrel, zk8(jnom), nbcmp, zi(jcompt),&
                            lxfem, jnoxfl, jnoxfv, ch_xfem_stat, ch_xfem_lnno,&
                            ch_xfem_ltno, connex_inv)
            enddo
            do i_keyword=1,n_keyword
                if (zi(jcompt-1+i_keyword) .eq. 0) call u2mesk('F', 'CHARGES2_45', 1,&
                                                               keywordlist(i_keyword))
            enddo
        endif
!
60      continue
!
        call jedetr('&&CADDLI.ICOMPT')
        call jedetr('&&CADDLI.VALDDL')
        call jedetr('&&CADDLI.DIRECT')
        call jedetr('&&CADDLI.DIMENSION')
        call jedetr('&&CADDLI.NUNOTMP')
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
