subroutine chloet(iparg, etendu, jceld)
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
#include "jeveux.h"
    logical(kind=1) :: etendu
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
    integer :: iadsgd, iamloc, iaopds, iaopmo, iaopno, iaoppa
    integer :: ilmloc, ilopmo, iaoptt
    integer :: ilopno, lgco, npario, iachlo
    integer :: nparin, iachii, iachik, iachix, iachoi, iachok, ich
!
    common /caii02/iaoptt,lgco,iaopmo,ilopmo,iaopno,ilopno,iaopds,&
     &       iaoppa,npario,nparin,iamloc,ilmloc,iadsgd
    integer :: nbgr, igr, nbelgr, jcteat, lcteat, iawloc, iawlo2, iawtyp
    common /caii06/nbgr,igr,nbelgr,jcteat,lcteat,iawloc,iawlo2,iawtyp
    common /caii04/iachii,iachik,iachix
    common /caii07/iachoi,iachok
!
    character(len=8) :: tych
!
!
!
!     -- LE CHAMP LOCAL EST-IL UN CHAM_ELEM ETENDU ?
!     OUI  SI CHAMP GLOBAL ASSOCIE EST 1 CHAM_ELEM ETENDU
!     --------------------------------------------------
    iachlo=zi(iawloc-1+3*(iparg-1)+1)
    if ((iachlo.eq.-1) .or. (iachlo.eq.-2)) goto 20
    ich=zi(iawloc-1+3*(iparg-1)+3)
    if (ich .eq. 0) goto 20
    if (iparg .le. nparin) then
        tych = zk8(iachik-1+2* (ich-1)+1)
        if (tych .ne. 'CHML') goto 20
        jceld = zi(iachii-1+11* (ich-1)+4)
    else
        tych = zk8(iachok-1+2* (ich-1)+1)
        if (tych .ne. 'CHML') goto 20
        jceld = zi(iachoi-1+3* (ich-1)+1)
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
10  continue
    etendu = .true.
    goto 30
!
!
!     LE CHAMP LOCAL N'EST PAS ETENDU:
!     --------------------------------
20  continue
    etendu = .false.
    goto 30
!
!
30  continue
!
end subroutine
