subroutine liscva(prefob, chamno)
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
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=13) :: prefob
    character(len=19) :: chamno
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! DONNE LE NOM DU CHAM_NO
!
! ----------------------------------------------------------------------
!
! CETTE ROUTINE EST OBLIGATOIRE TANT QUE L'ON A DES VRAIS VECT_ASSE
! ET DES FAUX
! VRAIS VECT_ASSE: DANS LES OPERATEURS DE DYNAMIQUE
! FAUX  VECT_ASSE: LE CHAMP EST STOCKE DANS UN OBJET CREE DANS
!                  AFFE_CHAR_MECA
!
! IN  PREFOB : PREFIXE DE L'OBJET DE LA CHARGE
! OUT CHAMNO : NOM DU CHAMP
!
!
!
!
    character(len=8) :: charge
    character(len=24) :: nomobj
    integer :: jobje, iret
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    chamno = ' '
    charge = prefob(1:8)
!
! --- ON IDENTIFIE
!
    call exisd('CHAM_NO', charge, iret)
    if (iret .eq. 1) then
        chamno = charge
    else
        nomobj = prefob(1:13)//'.VEASS'
        call jeexin(nomobj, iret)
        call assert(iret.ne.0)
        call jeveuo(nomobj, 'L', jobje)
        chamno = zk8(jobje)
        call exisd('CHAM_NO', chamno, iret)
        call assert(iret.gt.0)
    endif
!
    call jedema()
end subroutine
