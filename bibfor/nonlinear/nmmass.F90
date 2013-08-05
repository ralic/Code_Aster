subroutine nmmass(fonact, lischa, sddyna, solveu, numedd,&
                  numfix, meelem, masse)
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
! person_in_charge: ludovic.idoux at edf.fr
    implicit none
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mtdscr.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmassm.h"
    integer :: fonact(*)
    character(len=19) :: lischa, sddyna, solveu
    character(len=24) :: numedd, numfix
    character(len=19) :: meelem(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! PRE-CALCUL DES MATRICES ASSEMBLEES CONSTANTES AU COURS DU CALCUL
!
! ----------------------------------------------------------------------
!
!
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  LISCHA : LISTE DES CHARGEMENTS
! IN  SDDYNA : SD DYNAMIQUE
! IN  SOLVEU : SOLVEUR
! IN  NUMEDD : NUME_DDL (VARIABLE AU COURS DU CALCUL)
! IN  NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
! IN  MEELEM : MATRICES ELEMENTAIRES
! OUT MASSE  : MATRICE MASSE POUR LE CALCUL DES ENERGIES
!
! ----------------------------------------------------------------------
!
    logical :: ldyna, lexpl, limpl
    character(len=16) :: optass
    character(len=19) :: masse
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    lexpl = ndynlo(sddyna,'EXPLICITE')
    limpl = ndynlo(sddyna,'IMPLICITE')
!
! --- ASSEMBLAGE DE LA MATRICE MASSE
!
    if (ldyna) then
        if (limpl) then
            optass = ' '
        else if (lexpl) then
            optass = 'AVEC_DIRICHLET'
        else
            ASSERT(.false.)
        endif
        masse = '&&NMMASS.MASSENER'
        call nmassm(fonact, lischa, solveu, numedd, numfix,&
                    'MEMASS', optass, meelem, masse)
        call mtdscr(masse)
    endif
!
    call jedema()
end subroutine
