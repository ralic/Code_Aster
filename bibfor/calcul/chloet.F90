subroutine chloet(iparg, etendu, jceld)
use module_calcul, only : ca_iachii_, ca_iachik_, ca_iachoi_, ca_iachok_, &
    ca_iawloc_, ca_nparin_
implicit none
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
! person_in_charge: jacques.pellet at edf.fr
! -----------------------------------------------------------------
!     BUT : DETERMINER SI LE CHAMP LOCAL ASSOCIE A IPARG
!           EST "ETENDU"
!     UN CHAMP LOCAL ETENDU N'A PAS LA MEME LONGUEUR POUR TOUS SES
!     ELEMENTS
! -----------------------------------------------------------------
#include "asterf_types.h"
#include "jeveux.h"
    aster_logical :: etendu
    integer :: iparg, jceld
! -----------------------------------------------------------------
!     ENTREES:
!     IPARG  : NUMERO DU PARAMETRE (DANS LE CATALOGUE DE L'OPTION)
!
!     SORTIES:
!     ETENDU : .TRUE. : LE CHAMP LOCAL EST ETENDU
!              .FALSE.: LE CHAMP LOCAL N'EST PAS ETENDU
!     JCELD : SI LE CHAMP LOCAL EST ETENDU, JCELD EST L'ADRESSE
!             DANS ZI DE L'OBJET CHAMP_GLOBAL.CELD
! -----------------------------------------------------------------
    integer ::  iachlo
    integer ::      ich
    character(len=8) :: tych
!
!
!
!     -- LE CHAMP LOCAL EST-IL UN CHAM_ELEM ETENDU ?
!     OUI  SI CHAMP GLOBAL ASSOCIE EST 1 CHAM_ELEM ETENDU
!     --------------------------------------------------
    iachlo=zi(ca_iawloc_-1+3*(iparg-1)+1)
    if ((iachlo.eq.-1) .or. (iachlo.eq.-2)) goto 20
    ich=zi(ca_iawloc_-1+3*(iparg-1)+3)
    if (ich .eq. 0) goto 20
    if (iparg .le. ca_nparin_) then
        tych = zk8(ca_iachik_-1+2* (ich-1)+1)
        if (tych .ne. 'CHML') goto 20
        jceld = zi(ca_iachii_-1+11* (ich-1)+4)
    else
        tych = zk8(ca_iachok_-1+2* (ich-1)+1)
        if (tych .ne. 'CHML') goto 20
        jceld = zi(ca_iachoi_-1+3* (ich-1)+1)
    endif
    if ((zi(jceld-1+4).eq.0) .and. (zi(jceld-1+3).le.1)) then
        goto 20
    else
        goto 10
    endif
!
!
!     LE CHAMP LOCAL EST ETENDU:
!     --------------------------
 10 continue
    etendu = .true.
    goto 30
!
!
!     LE CHAMP LOCAL N'EST PAS ETENDU:
!     --------------------------------
 20 continue
    etendu = .false.
    goto 30
!
!
 30 continue
!
end subroutine
