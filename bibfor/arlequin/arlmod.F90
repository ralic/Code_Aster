subroutine arlmod(nomo,mailar,modarl,tabcor)

! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================

    implicit none

#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/detrsd.h"
#include "asterfort/arlmol.h"
#include "asterfort/arlmom.h"
#include "asterfort/jedema.h"

!     ARGUMENTS:
!     ----------
    character(len=8) :: mailar,modarl,nomo
    character(len=24) :: tabcor

! ----------------------------------------------------------------------

! ROUTINE ARLEQUIN

! CREATION DU PSEUDO-MODELE

! ----------------------------------------------------------------------


! IN  MAIL   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  MAILAR : NOM DU PSEUDO-MAILLAGE
! IN  TABCOR : TABLEAU DE CORRESPONDANCE
!            POUR CHAQUE NOUVEAU NUMERO ABSOLU DANS MAILAR
!             -> ANCIEN NUMERO ABSOLU DANS MAIL
!             -> SI NEGATIF, LA NOUVELLE MAILLE EST ISSUE D'UNE
!                DECOUPE DE LA MAILLE DE NUMERO ABSOLU ABS(NUM) DANS
!                MAIL
! IN  MODARL : NOM DU PSEUDO-MODELE

! ----------------------------------------------------------------------

    call jemarq()

! --- DESTRUCTION DU PSEUDO-MODELE S'IL EXISTE

    call detrsd('MODELE',modarl)

! --- NOM SD TEMPORAIRE

    modarl = '&&ARL.MO'

! --- CREATION DU LIGREL DU PSEUDO-MODELE

    call arlmol(nomo,mailar,modarl,tabcor)

! --- CREATION DU PSEUDO-MODELE

    call arlmom(mailar,modarl)

    call jedema()

end subroutine
