subroutine lislic(nomo, prefob, indxch, ligcal)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit     none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lisdef.h"
    character(len=8) :: nomo
    character(len=13) :: prefob
    integer :: indxch
    character(len=19) :: ligcal
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! RETOURNE LE LIGREL SUR LEQUEL ON FAIT LE CALCUL
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE
! IN  PREFOB : PREFIXE DE L'OBJET DE LA CHARGE
! IN  INDXCH : INDICE DU TYPE DE CHARGE
! OUT LIGCAL : NOM DU LIGREL SUR LEQUEL ON FAIT LE CALCUL
!
!
!
!
    character(len=6) :: typlig
    integer :: ibid(2)
    character(len=19) :: ligrmo
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call lisdef('LIGC', ' ', indxch, typlig, ibid)
    ligrmo = nomo(1:8)//'.MODELE'
    if (typlig .eq. 'LIGRMO') then
        ligcal = ligrmo
    else if (typlig.eq.'LIGRCH') then
        ligcal = prefob//'.LIGRE'
    else
        ASSERT(.false.)
    endif
!
!
    call jedema()
end subroutine
