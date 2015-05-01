subroutine rsexc1(nomsd, nomsy, iordr, chextr)
    implicit none
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterfort/detrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
    integer :: iordr
    character(len=*) :: nomsd, nomsy, chextr
    character(len=16) :: nomcmd, option, tysd
    character(len=8) :: concep
    character(len=16) :: typcon
    character(len=24) :: valk(2)
    integer :: vali
! ----------------------------------------------------------------------
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
!      RECUPERATION DU NOM DU CHAMP-GD  CORRESPONDANT A:
!          NOMSD(IORDR,NOMSY).
!      IL S'AGIT D'UN APPEL A RSEXCH COMPLETE PAR DES VERIFICATIONS
!      NOTAMMENT SI L'OPTION A DEJA ETE CALCULEE
!      DANS CE CAS, LES OBJETS JEVEUX PRECEDENTS SONT DETRUITS
!      AFIN DE POUVOIR REFAIRE LE CALCUL DE L'OPTION
!      CE MODULE EST DESTINE A ETRE APPELE PAR DES OPERATEURS
!      PAR EXEMPLE OP0058 AFIN DE PREPARER LES NOMS DE CHAMP-GD
!      AVANT L'APPEL A MECALC
! ----------------------------------------------------------------------
! IN  : NOMSD  : NOM DE LA STRUCTURE "RESULTAT"
! IN  : NOMSY  : NOM SYMBOLIQUE DU CHAMP A CHERCHER.
! IN  : IORDR  : NUMERO D'ORDRE DU CHAMP A CHERCHER.
! OUT : CHEXTR : NOM DU CHAMP EXTRAIT.
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: icode
!-----------------------------------------------------------------------
    option=nomsy
    call rsexch(' ', nomsd, nomsy, iordr, chextr,&
                icode)
! --- SI L'OPTION A DEJA ETE CALCULEE, ON LA RECALCULE EN
!     EMETTANT UN MESSAGE D'ALARME
    if (icode .eq. 0) then
        call getres(concep, typcon, nomcmd)
        valk (1) = option
        vali = iordr
        call utmess('A', 'UTILITAI8_31', sk=valk(1), si=vali)
        call detrsd('CHAM_ELEM', chextr(1:19))
    else if (icode.gt.100) then
        call getres(concep, typcon, nomcmd)
        call gettco(nomsd, tysd)
        valk(1) = tysd
        valk(2) = option
        call utmess('F', 'CALCULEL3_27', nk=2, valk=valk)
    endif
end subroutine
