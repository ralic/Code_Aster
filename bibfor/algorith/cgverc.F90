subroutine cgverc(resu, nexci)
    implicit none
!
#include "asterc/gettco.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
    integer :: nexci
    character(len=8) :: resu
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
!     NEXCI  : NOMBRE D'OCCURENCES DU MOT-CLE FACTEUR EXCIT
!  OUT :
! ======================================================================
!
    character(len=16) :: typsd
!
    call jemarq()
!
    call gettco(resu, typsd)
!
    if (typsd .eq. 'DYNA_TRANS') then
!
!       LES RESULTATS DE TYPE DYNA_TRANS NE CONTIENNENT PAS DE CHARGES
!       LE MOT-CLE EXCIT EST DONC OBLIGATOIRE
        if (nexci .eq. 0) then
            call utmess('F', 'RUPTURE0_9')
        endif
!
    else
!
!       POUR LES AUTRES TYPE DE RESULTAT, EXCIT N'EST PAS CONSEILLE
!       (SAUF SI LE RESU PROVIENT DE CREA_RESU, VOIR TEXTE ALARME)
        if (nexci .ne. 0) then
            call utmess('A', 'RUPTURE0_55')
        endif
!
    endif
!
    call jedema()
!
end subroutine
