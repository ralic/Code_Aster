subroutine nmarcc(result, numarc, typchz, nomchz)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assde2.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
    character(len=8) :: result
    character(len=*) :: typchz, nomchz
    integer :: numarc
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (ALGORITHME - ARCHIVAGE)
!
! ARCHIVAGE D'UN CHAMP
!
! ----------------------------------------------------------------------
!
!
! IN  RESULT : NOM UTILISATEUR DU CONCEPT RESULTAT
! IN  NOMCHA : NOM DU CHAMP
! IN  TYPCHA : NOM DU CHAMP DANS SD RESULTAT
! IN  NUMARC : NUMERO D'ARCHIVAGE
!
!
!
!
    integer :: iret
    character(len=19) :: champ, nomcha
    character(len=24) :: typcha
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    typcha = typchz
    nomcha = nomchz
    champ = ' '
!
! --- ACCES AU CHAMP DANS SD RESULTAT
!
    call rsexch(' ', result, typcha, numarc, champ,&
                iret)
    if (iret .gt. 100) ASSERT(.false.)
!
! --- COPIE DU CHAMP DANS SD RESULTAT
!
    call assde2(nomcha)
    call copisd('CHAMP_GD', 'G', nomcha, champ)
    call rsnoch(result, typcha, numarc)
!
    call jedema()
end subroutine
