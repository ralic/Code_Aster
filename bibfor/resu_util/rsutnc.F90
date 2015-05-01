subroutine rsutnc(nomsd, nomsy, nbvale, tabnom, tabord,&
                  nbtrou)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
!
    character(len=*), intent(in) :: nomsd
    character(len=*), intent(in) :: nomsy
    integer, intent(in) :: nbvale
    character(len=*), intent(out) :: tabnom(*)
    integer, intent(out) :: tabord(*)
    integer, intent(out) :: nbtrou
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
! person_in_charge: jacques.pellet at edf.fr
!      RECUPERATION DES CHAMPS NOTES ET DE LEURS NUMEROS D'ORDRE DANS
!      UNE STRUCTURE DE DONNEES NOMSD ET DE NOM SYMBOLIQUE NOMSY.
! ----------------------------------------------------------------------
! IN  : NOMSD  : NOM DE LA STRUCTURE "RESULTAT"
! IN  : NOMSY  : NOM SYMBOLIQUE DES CHAMPS A RECHERCHER
! IN  : NBVALE : DIMENSION DES TABLEAUX
!                = 0 , ON REND LE NOMBRE DE CHAMPS TROUVES (-NBTROU)
! OUT : TABNOM : TABLEAU DES NOMS DE CHAMPS
! OUT : TABORD : TABLEAU DES NUMEROS D'ORDRE DES CHAMPS TROUVES
! OUT : NBTROU : NOMBRE DE CHAMPS TROUVES
!                SI NBTROU > NBVALE, ALORS NBTROU = -NBTROU.
! ----------------------------------------------------------------------
    character(len=16) :: noms2
    character(len=19) :: nomd2
    character(len=24) :: chextr
! ----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ibid, itrou,  jtach, nbordr
    integer, pointer :: ordr(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    nbtrou = 0
    noms2 = nomsy
    nomd2 = nomsd
    if (nbvale .lt. 0) goto 20
    if (noms2 .eq. ' ') goto 20
!
    call jelira(nomd2//'.ORDR', 'LONUTI', nbordr)
    call jeveuo(nomd2//'.ORDR', 'L', vi=ordr)
    call jenonu(jexnom(nomd2//'.DESC', noms2), ibid)
    call jeveuo(jexnum(nomd2//'.TACH', ibid), 'L', jtach)
    itrou = 0
    do i = 0, nbordr - 1
        chextr = zk24(jtach+i)
        if (chextr .ne. ' ') then
            nbtrou = nbtrou + 1
            if (nbvale .eq. 0) goto 10
            itrou = itrou + 1
            if (itrou .le. nbvale) then
                tabord(itrou) = ordr(1+i)
                tabnom(itrou) = chextr
            endif
        endif
 10     continue
    end do
    if (nbtrou .gt. nbvale) nbtrou = -nbtrou
!
 20 continue
    call jedema()
end subroutine
