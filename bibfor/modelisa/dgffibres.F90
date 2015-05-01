subroutine dgffibres(nboccfib, iinbgf, tousgroupesnom, tousgroupesnbf, maxmailgrp, &
                     ulnbnoeuds, ulnbmailles, nbfibres1, maxfibre1, ncarfi1)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! --------------------------------------------------------------------------------------------------
!
!                O P E R A T E U R    DEFI_GEOM_FIBRE
!
!   Pré-traitement du mot clef FIBRE
!
! --------------------------------------------------------------------------------------------------
!
! person_in_charge: jean-luc.flejou at edf.fr
!
    implicit none
!
    integer :: nboccfib, iinbgf, maxmailgrp, ulnbnoeuds, ulnbmailles, nbfibres1, maxfibre1, ncarfi1
    integer           :: tousgroupesnbf(*)
    character(len=24) :: tousgroupesnom(*)
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/codent.h"
#include "asterfort/getvtx.h"
#include "asterfort/getvr8.h"
#include "asterfort/utmess.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer           :: ioc, nbvfibre
    character(len=7)  :: k7bid
!
    integer           :: vali(3)
    character(len=24) :: valk(3)
!
    character(len=16) :: limcls(3), ltymcl(3)
    data limcls/'MAILLE_SECT','GROUP_MA_SECT','TOUT_SECT'/
    data ltymcl/'MAILLE','GROUP_MA','TOUT'/
!
! --------------------------------------------------------------------------------------------------
!
    maxfibre1 = 10
    do ioc = 1, nboccfib
        iinbgf = iinbgf + 1
        call getvtx('FIBRE', 'GROUP_FIBRE', iocc=ioc, scal=tousgroupesnom(iinbgf))
!
        call getvr8('FIBRE', 'VALE', iocc=ioc, nbval=0, nbret=nbvfibre)
        nbvfibre = -nbvfibre
        maxfibre1 = max(maxfibre1,nbvfibre)
!       Vérification multiple de 'ncarfi1' pour 'vale' dans 'fibre'
        if ( modulo(nbvfibre,ncarfi1).ne.0 ) then
            call codent(nbvfibre, 'G', k7bid)
            valk(1)=tousgroupesnom(iinbgf)
            valk(1)='VALE'
            vali(1)=nbvfibre
            vali(2)=ncarfi1
            call utmess('F', 'MODELISA6_26', nk=2, valk=valk, ni=2, vali=vali)
        endif
        ulnbmailles = ulnbmailles + nbvfibre/ncarfi1
        nbfibres1   = nbfibres1   + nbvfibre/ncarfi1
        ulnbnoeuds  = ulnbnoeuds  + nbvfibre/ncarfi1
        maxmailgrp  = max(maxmailgrp,nbvfibre/ncarfi1)
        tousgroupesnbf(iinbgf) = tousgroupesnbf(iinbgf) + nbvfibre/ncarfi1
    enddo

end
