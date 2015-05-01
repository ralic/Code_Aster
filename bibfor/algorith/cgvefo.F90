subroutine cgvefo(option, typfis, nomfis, typdis)
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
!
    character(len=8) :: typfis, nomfis
    character(len=16) :: option, typdis
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
!     BUT : VERIFICATION DE LA COMPATIBILITE ENTRE
!           OPTION ET TYPE DE FISSURE
!
!  IN :
!    OPTION : OPTION DE CALC_G
!    TYPFIS : TYPE DE LA SD DECRIVANT LE FOND DE FISSURE
!             ('THETA' OU 'FONDIFSS' OU 'FISSURE')
!    NOMFIS : NOM DE LA SD DECRIVANT LE FOND DE FISSURE
! ======================================================================
!
    integer :: ier
    character(len=8) :: conf
!
    call jemarq()
!
!     LE CAS DES FONDS DOUBLES N'EST PAS TRAITE DANS CALC_G
    if (typfis .eq. 'FONDFISS') then
        call jeexin(nomfis//'.FOND.NOEU', ier)
        if (ier .eq. 0) then
            call utmess('F', 'RUPTURE1_4')
        endif
    endif
!
!     COMPATIBILITE ENTRE OPTION ET "ENTAILLE"
!     ON NE SAIT DEFINIR K QUE DANS LE CAS D'UNE FISSURE AVEC LEVRES
!     INITIALLEMENT COLLEES (PAS D'ENTAILLE), DONC
!     CERTAINES OPTIONS EN MODELISATION FEM NE TRAITENT PAS LES
!     FISSURES EN CONFIGURATION DECOLLEE
!     (SI X-FEM OU THETA, LA CONFIG ET TOUJOURS COLLEE)
    if (typfis .eq. 'FONDFISS') then
!
        call dismoi('CONFIG_INIT', nomfis, 'FOND_FISS', repk=conf)
!
        if ((&
            option .eq. 'CALC_K_G' .or. option .eq. 'K_G_MODA' .or. option .eq.&
            'CALC_K_MAX'&
            )&
            .and. (conf.eq.'DECOLLEE')) then
            call utmess('F', 'RUPTURE0_29', sk=option)
        endif
!
    endif
!
!   SI FISSURE TYPE 'COHESIF', LA SEULE OPTION EST CALC_K_G
    if(typdis.eq.'COHESIF'.and.option.ne.'CALC_K_G') then
        call utmess('F','RUPTURE2_5')
    endif
!
!     CERTAINES OPTIONS NE SONT PAS ENCORE PROGRAMMEES POUR X-FEM
    if (option .eq. 'G_MAX' .or. option .eq. 'G_BILI') then
        if (typfis .eq. 'FISSURE') then
            call utmess('F', 'RUPTURE0_48', sk=option)
        endif
    endif
!
!
    call jedema()
!
end subroutine
