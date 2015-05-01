subroutine paligi(modelisa, ligrch, idx_grel, idx_nema, list_elem)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/ini002.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
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
    character(len=24), intent(in) :: modelisa
    character(len=*), intent(in) :: ligrch
    integer, intent(in) :: idx_grel
    integer, intent(inout) :: idx_nema
    integer, intent(in) :: list_elem(*)
!
! --------------------------------------------------------------------------------------------------
!
! Loads affectation - ECHANGE_PAROI
!
! --------------------------------------------------------------------------------------------------
!
! In  ligrch       : name of <LIGREL> for load
! In  modelisa     : modelisation
! In  idx_grel     : index for GREL
! IO  idx_nema     : index for last "last" element in ligrch
! In  list_elem    : TABLEAU DECRIVANT LES VIS A VIS
!                  (1) = ITYPM : NUM. DU TYPE_MAIL
!                  (2) = nb_elem  : NBRE DE VIS A VIS
!                  (3) = nb_node :NBRE DE NOEUDS DES MAILLES EN VIS A VIS
!          POUR IC = 1,nb_elem
! V(3+(IC-1)*(2+2*NBNTOT)+1)= NUMA1 NUM. DE LA 1ERE MAILLE DU COUPLE
! V(3+(IC-1)*(2+2*NBNTOT)+2)= NUMA2 NUM. DE LA 2EME MAILLE DU COUPLE
!                        EN VIS A VIS AVEC NUMA1
!        POUR INO = 1,nb_node
! V(3+(IC-1)*(2+2*NBTOT)+2+2*(INO-1)+1) = N1(INO)NUM.DU NO. INO DE NUMA1
! V(3+(IC-1)*(2+2*NBTOT)+2+2*(INO-1)+1) = N2(N1(INO)) NUM.DU NOEUD DE
!                   NUMA2 EN VIS A VIS AVEC LE NOEUD N1(INO)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nmaxob
    parameter (nmaxob=30)
!
    character(len=16) :: type_elem
    character(len=24) :: liel
    integer, pointer :: p_liel(:) => null()
    character(len=24) :: nema
    integer, pointer :: p_nema(:) => null()
    integer :: ielem, ino, type_nume, jma, nb_elem
    integer :: nb_node
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    nb_elem = list_elem(2)
    nb_node = list_elem(3)
    liel    = ligrch(1:19)//'.LIEL'
    nema    = ligrch(1:19)//'.NEMA'

    if (modelisa(1:2) .eq. 'PL') then
        if (nb_node .eq. 2) then
            type_elem = 'THPLSE22'
        else if (nb_node.eq.3) then
            type_elem = 'THPLSE33'
        else
            call utmess('F', 'CHARGES6_1')
        endif
    else if (modelisa(1:2).eq.'AX') then
        type_elem(3:6) = 'PLSE'
        if (nb_node .eq. 2) then
            type_elem = 'THPLSE22'
        else if (nb_node.eq.3) then
            type_elem = 'THPLSE33'
        else
            call utmess('F', 'CHARGES6_1')
        endif
    else if (modelisa(1:2).eq.'3D') then
        type_elem(5:9) = '_FACE'
        if (nb_node .eq. 3) then
            type_elem = 'THER_FACE33'
        else if (nb_node.eq.4) then
            type_elem = 'THER_FACE44'
        else if (nb_node.eq.6) then
            type_elem = 'THER_FACE66'
        else if (nb_node.eq.8) then
            type_elem = 'THER_FACE88'
        else if (nb_node.eq.9) then
            type_elem = 'THER_FACE99'
        else
            call utmess('F', 'CHARGES6_1')
        endif
        call ini002(type_elem, nmaxob)
    else
        call utmess('F', 'CHARGES6_2')
    endif
    call jenonu(jexnom('&CATA.TE.NOMTE', type_elem), type_nume)
    ASSERT(type_nume.ne.0)
!
! - Create GREL
!
    call jecroc(jexnum(liel, idx_grel))
    call jeecra(jexnum(liel, idx_grel), 'LONMAX', nb_elem+1)
    call jeveuo(jexnum(liel, idx_grel), 'E', vi = p_liel)
    do ielem = 1, nb_elem
        idx_nema = idx_nema + 1
        jma = 3 + (ielem-1)*2*(nb_node+1)
        p_liel(ielem) = -idx_nema
        call jecroc(jexnum(nema, idx_nema))
        call jeecra(jexnum(nema, idx_nema), 'LONMAX', 2*nb_node+1)
        call jeveuo(jexnum(nema, idx_nema), 'E', vi = p_nema)
        do ino = 1, nb_node
            p_nema(ino)         = list_elem(jma+2+2*ino-1)
            p_nema(nb_node+ino) = list_elem(jma+2+2*ino)
        end do
        p_nema(2*nb_node+1) = list_elem(1)
    end do
    p_liel(nb_elem+1) = type_nume
!
    call jedema()
end subroutine
