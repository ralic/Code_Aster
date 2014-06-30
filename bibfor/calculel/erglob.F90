subroutine erglob(cheler, yathm, perman, option, iord,&
                  resuco, resuc1)
    implicit none
#include "jeveux.h"
#include "asterfort/celver.h"
#include "asterfort/digdel.h"
#include "asterfort/erglhm.h"
#include "asterfort/erglme.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nbgrel.h"
#include "asterfort/utmess.h"
!
    integer :: iord
    character(len=*) :: resuco
    character(len=19) :: resuc1
    character(len=*) :: cheler, option
    logical(kind=1) :: yathm, perman
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
! =====================================================================
! ERREUR GLOBALE AU MAILLAGE
! **     ****
! =====================================================================
!     BUT:
!         CALCULER LES ESTIMATEURS GLOBAUX A PARTIR DES ESTIMATEURS
!         LOCAUX CONTENUS DANS CHELER.
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   CHELER : NOM DU CHAM_ELEM ERREUR
! IN   YATHM  : MODELISATION THM ?
! IN   PERMAN : MODELISATION THM PERMANENTE ?
! IN   OPTION :    'ERZ1_ELEM' OU 'ERZ2_ELEM'
!               OU 'QIZ1_ELEM' OU 'QIZ2_ELEM'
!               OU 'ERME_ELEM' OU 'QIRE_ELEM'
! IN   IORD   : NUMERO D'ORDRE
! IN   RESUCO : NOM DU CONCEPT ENTRANT
! IN   RESUC1 : NOM DU CONCEPT RESULTAT DE LA COMMANDE CALC_ERREUR
!
!      SORTIE :
!-------------
!
! ......................................................................
!
!
!
    integer :: ibid, longt, long2, mode, j, iavale, icoef, nbgr, jceld
    character(len=4) :: docu
    character(len=19) :: chele2, ligrel
    logical(kind=1) :: first
    character(len=24), pointer :: celk(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
!=================
! 1. VERIFICATIONS
!=================
    chele2 = cheler(1:19)
!
! 1.1. ON VERIFIE QUE CHELE2 A UN NOMBRE DE COMPOSANTES CONSTANT
!    PAR ELEMENT
!
    call celver(chele2, 'NBVARI_CST', 'STOP', ibid)
!
! 1.2. ON VERIFIE QUE LE CHAM_ELEM N'A QU'UN SOUS-POINT
!
    call celver(chele2, 'NBSPT_1', 'STOP', ibid)
!
    call jelira(chele2//'.CELD', 'DOCU', cval=docu)
    if (docu .ne. 'CHML') then
        call utmess('F', 'CALCULEL5_44')
    endif
!
! 1.3. ON RETROUVE LE NOM DU LIGREL
!
    call jeveuo(chele2//'.CELK', 'L', vk24=celk)
    ligrel = celk(1)(1:19)
    call jeveuo(chele2//'.CELD', 'L', jceld)
!
! 1.4. ON VERIFIE LA LONGUEUR DES CHAMPS LOCAUX POUR L'OPTION
!
    first = .true.
    nbgr = nbgrel(ligrel)
!
    do 10 ,j = 1,nbgr
    mode=zi(jceld-1+zi(jceld-1+4+j)+2)
    if (mode .eq. 0) goto 10
    long2 = digdel(mode)
    icoef=max(1,zi(jceld-1+4))
    long2 = long2 * icoef
    if (first) then
        longt = long2
        first = .false.
    else
        if (longt .ne. long2) then
            call utmess('F', 'CALCULEL3_54')
        endif
    endif
    10 end do
!
!==================================
! 2. CALCUL DES INDICATEURS GLOBAUX
!==================================
!
    call jeveuo(chele2//'.CELV', 'E', iavale)
    if (yathm) then
        call erglhm(perman, jceld, iavale, iord, ligrel,&
                    longt, nbgr, resuc1)
    else
        call erglme(jceld, iavale, option, iord, ligrel,&
                    longt, nbgr, resuco, resuc1)
    endif
!
    call jedema()
!
end subroutine
