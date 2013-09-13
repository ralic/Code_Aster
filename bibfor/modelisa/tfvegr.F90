subroutine tfvegr(nommcf, ocgril)
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
    implicit none
!-----------------------------------------------------------------------
!     APPELANT : TFVERI, OP0143 , OPERATEUR DEFI_FLUI_STRU
!     VERIFICATIONS DE PREMIER NIVEAU : MOT-CLE FACTEUR FAISCEAU_AXIAL,
!     OPERANDES CARACTERISTIQUES DES GRILLES
!-----------------------------------------------------------------------
!  IN   : NOMMCF : NOM DU MOT-CLE FACTEUR UTILISE (FAISCEAU_AXIAL)
!  IN   : OCGRIL : OCCURENCE DU MOT-CLE FACTEUR POUR LAQUELLE ON
!                  VERIFIE LES ARGUMENTS FOURNIS SOUS LES OPERANDES
!-----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
!
    character(len=16) :: nommcf
    integer :: ocgril, ntypg
!
!    ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ibid, igril, iveci, nbgtot, ntot2, ntypg2, ntypg3
    integer :: ntypg4, ntypg5, ntypg6
!-----------------------------------------------------------------------
    call jemarq()
!
    call getvr8(nommcf, 'LONG_TYPG', iocc=ocgril, nbval=0, nbret=ntypg)
    ntypg = abs(ntypg)
    call getvr8(nommcf, 'COOR_GRILLE', iocc=ocgril, nbval=0, nbret=nbgtot)
    nbgtot = abs(nbgtot)
    if (nbgtot .lt. ntypg) then
        call u2mess('E', 'MODELISA7_15')
    endif
    call getvis(nommcf, 'TYPE_GRILLE', iocc=ocgril, nbval=0, nbret=ntot2)
    if (abs(ntot2) .ne. nbgtot) then
        call u2mess('E', 'MODELISA7_16')
    endif
    call wkvect('&&TFVEGR.TEMP.VECI', 'V V I', nbgtot, iveci)
    call getvis(nommcf, 'TYPE_GRILLE', iocc=ocgril, nbval=nbgtot, vect=zi(iveci),&
                nbret=ibid)
    do 100 igril = 1, nbgtot
        if ((zi(iveci+igril-1).lt.1) .or. (zi(iveci+igril-1).gt.ntypg)) then
            call u2mess('E', 'MODELISA7_17')
        endif
100  continue
    call getvr8(nommcf, 'LARG_TYPG', iocc=ocgril, nbval=0, nbret=ntypg2)
    call getvr8(nommcf, 'EPAI_TYPG', iocc=ocgril, nbval=0, nbret=ntypg3)
    call getvr8(nommcf, 'RUGO_TYPG', iocc=ocgril, nbval=0, nbret=ntypg4)
    call getvr8(nommcf, 'COEF_TRAI_TYPG', iocc=ocgril, nbval=0, nbret=ntypg5)
    call getvr8(nommcf, 'COEF_DPOR_TYPG', iocc=ocgril, nbval=0, nbret=ntypg6)
    if ((abs(ntypg2).ne.ntypg) .or. (abs(ntypg3).ne.ntypg) .or. (abs(ntypg4).ne.ntypg) .or.&
        (abs(ntypg5).ne.ntypg) .or. (abs(ntypg6).ne.ntypg)) then
        call u2mess('E', 'MODELISA7_18')
    endif
!
!
    call jedetr('&&TFVEGR.TEMP.VECI')
    call jedema()
end subroutine
