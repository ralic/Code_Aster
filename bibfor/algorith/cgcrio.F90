subroutine cgcrio(resu, vecord)
    implicit none
!
#include "asterfort/assert.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/rsutnu.h"
    character(len=8) :: resu
    character(len=19) :: vecord
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
!     SOUS-ROUTINE DE L'OPERATEUR CALC_G
!
!     BUT : VERIFICATION DE LA COMPATIBILITE ENTRE EXCIT ET RESU
!
!  IN :
!     RESU   : MOT-CLE RESULTAT
!  OUT :
!     VECORD : VECTEUR DES NUME_ORDRE DU RESU
! ======================================================================
!
    integer :: ier, nbord
    real(kind=8) :: prec
    character(len=8) :: crit
!
    call jemarq()
!
    call getvr8(' ', 'PRECISION', scal=prec, nbret=ier)
    call getvtx(' ', 'CRITERE', scal=crit, nbret=ier)
!
    call rsutnu(resu, ' ', 0, vecord, nbord,&
                prec, crit, ier)
    ASSERT(ier.eq.0)
!
    call jedema()
!
end subroutine
