subroutine ss2mm2(mo, vecel, nomcas)
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
! INSPI  SS2MME
    implicit none
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
!
    character(len=8) :: mo, nomcas
    character(len=19) :: vecel
! ----------------------------------------------------------------------
!     BUT: TRAITER LE MOT-CLEF CAS_CHARGE DE LA COMMANDE
!          MACR_ELEM_STAT (POUR LES MACR_ELEM DU NIVEAU INFERIEUR)
!
!
!     IN:     MO : NOM DU MODELE
!          VECEL : NOM DU VECT_ELEM
!          NOMCAS: NOM DU CAS_CHARGE
!
!     OUT: VECEL EST  ENRICHI (EVENTUELLEMENT) DE L'OBJET .LISTE_CHAR
!
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
    character(len=8) :: ma, nosma, nomacr
!
!
!
!
!-----------------------------------------------------------------------
    integer :: i, ialsch
    integer :: iret, nbsma, nbssa
    character(len=24), pointer :: rerr(:) => null()
    character(len=8), pointer :: vnomacr(:) => null()
    integer, pointer :: sssa(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    call dismoi('NOM_MAILLA', mo, 'MODELE', repk=ma)
    call dismoi('NB_SS_ACTI', mo, 'MODELE', repi=nbssa)
    call dismoi('NB_SM_MAILLA', mo, 'MODELE', repi=nbsma)
!
    if (nbssa .eq. 0) goto 999
!
    call jeveuo(mo//'.MODELE    .SSSA', 'L', vi=sssa)
    call jeveuo(ma//'.NOMACR', 'L', vk8=vnomacr)
!
    call jeveuo(vecel//'.RERR', 'E', vk24=rerr)
    rerr(3)(1:3)='OUI'
!
    call jecrec(vecel//'.RELC', 'V V I', 'NO', 'CONTIG', 'CONSTANT',&
                1)
    call jeecra(vecel//'.RELC', 'LONMAX', nbsma)
    call jecroc(jexnom(vecel//'.RELC', nomcas))
    call jeveuo(jexnom(vecel//'.RELC', nomcas), 'E', ialsch)
!
!
!     -- REMPLISSAGE DE .RELC:
!     ------------------------------
!
!     -- ON VERIFIE QUE LES VECTEURS ELEMENTAIRES SONT CALCULES:
!     ----------------------------------------------------------
    do i = 1, nbsma
        if (sssa(i) .eq. 0) goto 3
        call jenuno(jexnum(ma//'.SUPMAIL', i), nosma)
        nomacr= vnomacr(i)
        call jeexin(jexnom(nomacr//'.LICA', nomcas), iret)
        if (iret .gt. 0) then
            zi(ialsch-1+i)=1
        else
            zi(ialsch-1+i)=0
        endif
  3     continue
    end do
!
!
!
!
!
999 continue
    call jedema()
end subroutine
