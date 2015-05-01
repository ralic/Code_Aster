subroutine nmttch(result, inst, nume)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/rslipa.h"
#include "asterfort/utacli.h"
#include "asterfort/utmess.h"
    real(kind=8) :: inst
    integer :: nume
    character(len=8) :: result
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (UTILITAIRE - SELEC. INST.)
!
! RECHERCHE DE L'INDICE DANS LA SD RESULTAT JUSTE AVANT L'INSTANT
! DONNE
!
! ----------------------------------------------------------------------
!
!
! IN  RESULT : NOM DE LA SD RESULTAT
! IN  INST   : INSTANT A RECHERCHER
! OUT NUME   : INDICE A ECRASER
!
!
!
!
    character(len=24) :: nomobj
    integer :: jtemps
    integer :: nbinst, i, nbintv
    real(kind=8) :: tole
    real(kind=8) :: dtmin, ins, dt
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD_DISC
!
    tole = r8prem()
    nomobj = '&&NMTTCH.LISTE'
    call rslipa(result, 'INST', nomobj, jtemps, nbinst)
!
! --- RECHERCHE INSTANT
!
    call utacli(inst, zr(jtemps), nbinst, tole, nume)
    nbintv = nbinst - 1
!
! --- SI INST NON PRESENT DANS LA LISTE D INSTANT
! --- ON CHERCHE L INSTANT LE PLUS PROCHE AVANT L'INSTANT CHERCHE
!
    if (nume .lt. 0) then
        dtmin = inst-zr(jtemps)
        ins = zr(jtemps)
        do 40 i = 1, nbintv
            dt = inst-zr(jtemps+i)
            if (dt .le. 0.d0) then
                goto 45
            endif
            if (dt .lt. dtmin) then
                dtmin = dt
                ins = zr(jtemps+i)
            endif
40      continue
45      continue
        inst = ins
        call utacli(inst, zr(jtemps), nbinst, tole, nume)
        nume = nume + 1
    endif
!
    if (nume .lt. 0) then
        call utmess('F', 'DISCRETISATION_89')
    endif
!
    call jedetr(nomobj)
!
    call jedema()
!
end subroutine
