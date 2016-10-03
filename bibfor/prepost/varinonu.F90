subroutine varinonu(model    , compor_  , sdresu_,&
                    nb_elem  , list_elem, nb_vari, name_vari,&
                    nume_vari)
!
implicit none
!
#include "jeveux.h"
#include "asterc/lccree.h"
#include "asterc/lcinfo.h"
#include "asterc/lcdiscard.h"
#include "asterc/lcvari.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/cesred.h"
#include "asterfort/cesexi.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/detrsd.h"
#include "asterfort/etenca.h"
#include "asterfort/jeexin.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/indk16.h"
#include "asterfort/comp_meca_pvar.h"
#include "asterfort/utmess.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=*), intent(in) :: model
    character(len=*), intent(in) :: compor_
    character(len=*), intent(in) :: sdresu_
    integer, intent(in) :: nb_elem
    integer, intent(in) :: list_elem(nb_elem)
    integer, intent(in) :: nb_vari
    character(len=16), intent(in) :: name_vari(nb_vari)
    character(len=8), intent(out) ::  nume_vari(nb_elem, nb_vari)
!
! -------------------------------------------------------------------
! But : Etablir la correspondance entre les noms "mecaniques" de variables internes
! et leurs "numeros"  : 'V1', 'V7', ...
!
!  entrees:
!    compor_: nom de la carte de comportement (ou ' ')
!    sdresu_: nom d'une sd_resultat (ou ' ')
!       il faut fournir un et un seul des 2 arguments compor_ et sdresu_
!    nb_elem   : longueur de la liste list_elem
!    list_elem   : liste des numeros de mailles concernees
!    nb_vari : longueur des listes name_vari et nume_vari
!    name_vari : liste des noms "mecaniques" des variables internes
! sorties:
!    nume_vari : liste des "numeros" des variables internes
! -------------------------------------------------------------------
!
    integer :: i_vari,inum
    integer :: iret, i_elem, i_zone, jv_vari
    integer :: nume_elem, nb_vari_zone
    aster_logical :: dbg=.false.
    character(len=19) :: compor, ligrmo
    character(len=19) :: compor_info
    integer, pointer :: v_compor_ptma(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    compor_info = '&&NMDOCC.INFO'
!
! - Get COMPOR field
!
    if (compor_ .ne.' ') then
        ASSERT(sdresu_ .eq. ' ')
        compor=compor_
    else
        ASSERT(sdresu_ .ne. ' ')
        call dismoi('COMPOR_1', sdresu_, 'RESULTAT', repk=compor)
        ASSERT(compor.ne.' ')
    endif
!
! - Access to COMPOR field
!
    ligrmo = model(1:8)//'.MODELE'
    call etenca(compor, ligrmo, iret)
    call jeveuo(compor//'.PTMA', 'L', vi = v_compor_ptma)
!
! - Prepare informations about internal variables
!
    call jeexin(compor_info(1:19)//'.ZONE', iret)
    if (iret .eq. 0) then
        call comp_meca_pvar(model_ = model, compor_cart_ = compor, compor_info = compor_info)
    endif
!
! - Access to informations
!
    do i_elem = 1, nb_elem
        nume_elem = list_elem(i_elem)
        i_zone    = v_compor_ptma(nume_elem)
        call jelira(jexnum(compor_info(1:19)//'.VARI', i_zone), 'LONMAX', nb_vari_zone)
        call jeveuo(jexnum(compor_info(1:19)//'.VARI', i_zone), 'L', jv_vari)
        do i_vari = 1, nb_vari
            inum = indk16(zk16(jv_vari), name_vari(i_vari), 1, nb_vari_zone)
            if (inum .eq. 0) then
                call utmess('F','EXTRACTION_22',sk=name_vari(i_vari))
            endif
            nume_vari(i_elem, i_vari) = 'V'
            call codent(inum, 'G', nume_vari(i_elem, i_vari)(2:8))
            if (dbg) write(6,*) 'varinonu nnuvari=',nume_vari(i_elem, i_vari)
        enddo
    end do
!
    call jedema()
end subroutine
