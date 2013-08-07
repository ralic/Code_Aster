subroutine xthpos(resulz, modele)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "jeveux.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/xthpoc.h"
    character(len=24) :: resulz, modele
! ----------------------------------------------------------------------
!
! THER_LINEAIRE + XFEM : CALCUL ET STOCKAGE DE L'OPTION 'TEMP_ELGA'
!
! IN  RESULZ  : NOM DU RESULTAT
! IN  MODELE  : NOM DU MODELE
!
! ----------------------------------------------------------------------
    integer :: jord, nbordr, ior, iord, iret
    character(len=8) :: result
    character(len=19) :: ligrmo, chtn, chtpg, celtmp
    character(len=24) :: ordr
! ----------------------------------------------------------------------
!
    call jemarq()
!
    result=resulz(1:8)
    ligrmo=modele(1:8)//'.MODELE'
    celtmp='&&XTELGA.CELTMP'
!
    ordr=result//'           .ORDR'
    call jeveuo(ordr, 'L', jord)
    call jelira(ordr, 'LONUTI', nbordr)
!
!     ------------------------------------------------------------------
!     - BOUCLE SUR LES NBORDR NUMEROS D'ORDRE
!     ------------------------------------------------------------------
!
    do 1000 ior = 1, nbordr
!
        iord=zi(jord-1+ior)
!
!       SI LE CHAMP 'TEMP' N'EXISTE PAS DANS RESULT, ON PASSE...
        call rsexch(' ', result, 'TEMP', iord, chtn,&
                    iret)
        if (iret .gt. 0) goto 1000
!
!       ALLOCATION DU CHAM_ELEM TEMPORAIRE : CELTMP
!       RQ : LIGRMO CONTIENT TOUS LES EF DU MODELE, MAIS SEULS LES EF
!       ---  X-FEM SAVENT CALCULER L'OPTION 'TEMP_ELGA' -> CELTMP N'EST
!            DONC DEFINI QUE SUR L'ENSEMBLE DES EF X-FEM
        call alchml(ligrmo, 'TEMP_ELGA', 'PTEMPPG', 'V', celtmp,&
                    iret, ' ')
        ASSERT(iret.eq.0)
!
!       CALCUL DE L'OPTION 'TEMP_ELGA' ET ECRITURE DANS CELTMP
        call xthpoc(modele, chtn, celtmp)
!
!       RECUPERATION DU NOM DU CHAMP A ECRIRE : CHTPG
        call rsexch(' ', result, 'TEMP_ELGA', iord, chtpg,&
                    iret)
        ASSERT(iret.eq.100)
!       COPIE : CELTMP (BASE 'V') -> CHTPG (BASE 'G')
        call copisd('CHAMP', 'G', celtmp, chtpg)
!       STOCKAGE DANS LA SD RESULTAT
        call rsnoch(result, 'TEMP_ELGA', iord)
!
!       DESTRUCTION DE CELTMP
        call detrsd('CHAM_ELEM', celtmp)
!
1000  end do
!
    call jedema()
!
end subroutine
