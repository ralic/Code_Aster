subroutine nminma(fonact, lischa, sddyna, solveu, numedd,&
                  numfix, meelem, measse)
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
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmassm.h"
#include "asterfort/nmchex.h"
    integer :: fonact(*)
    character(len=19) :: lischa, sddyna, solveu
    character(len=24) :: numedd, numfix
    character(len=19) :: meelem(*), measse(*)
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
! OUT MEASSE : MATRICES ASSEMBLEES
!
! ----------------------------------------------------------------------
!
    logical :: ldyna, lexpl, limpl, lamor, lktan
    integer :: ifm, niv
    character(len=16) :: optass
    character(len=19) :: masse, amort
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> PRECALCUL DES MATR_ASSE CONSTANTES'
    endif
!
! --- INITIALISATIONS
!
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    lexpl = ndynlo(sddyna,'EXPLICITE')
    limpl = ndynlo(sddyna,'IMPLICITE')
    lamor = ndynlo(sddyna,'MAT_AMORT')
    lktan = ndynlo(sddyna,'RAYLEIGH_KTAN')
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(measse, 'MEASSE', 'MEMASS', masse)
    call nmchex(measse, 'MEASSE', 'MEAMOR', amort)
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
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE> ... MATR_ASSE DE MASSE'
        endif
        call nmassm(fonact, lischa, solveu, numedd, numfix,&
                    'MEMASS', optass, meelem, masse)
    endif
!
! --- ASSEMBLAGE DE LA MATRICE AMORTISSEMENT
!
    if (lamor .and. .not.lktan) then
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE> ... MATR_ASSE AMORTISSEMENT'
        endif
        optass = ' '
        call nmassm(fonact, lischa, solveu, numedd, numfix,&
                    'MEAMOR', optass, meelem, amort)
    endif
!
    call jedema()
end subroutine
