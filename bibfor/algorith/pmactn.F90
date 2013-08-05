subroutine pmactn(sddisc, parcri, iterat, numins, itemax,&
                  sderro, liccvg, actite, action)
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
!
    implicit     none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmacto.h"
#include "asterfort/nmevac.h"
#include "asterfort/u2mess.h"
    character(len=19) :: sddisc
    character(len=24) :: sderro
    real(kind=8) :: parcri(*)
    integer :: liccvg(5)
    logical :: itemax
    integer :: action, actite
    integer :: iterat, numins
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! GESTION DES ACTIONS SUITE A UN EVENEMENT DANS NEWTON
!
! PASSAGE DE NEWTON -> BOUCLE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION
! IN  PARCRI : CRITERES DE CONVERGENCE
! IN  ITERAT : NUMERO D'ITERATION DE NEWTON
! IN  NUMINS : NUMERO D'INSTANT
! I/O ITEMAX : .TRUE. SI ITERATION MAXIMUM ATTEINTE
! IN  ACTITE : BOUCLE NEWTON -> ACTION POUR LA SUITE
!     0 - NEWTON OK   - BOUCLE DE CONTACT SUIVANTE
!     1 - NEWTON NOOK - IL FAUT FAIRE QUELQUE CHOSE
!     2 - NEWTON NCVG - ON CONTINUE NEWTON
!     3 - NEWTON STOP - TEMPS/USR1
! OUT ACTION : CODE RETOUR ACTION
!               0 ARRET DU CALCUL
!               1 NOUVEAU PAS DE TEMPS
!               2 ON FAIT DES ITERATIONS DE NEWTON EN PLUS
!               3 ON FINIT LE PAS DE TEMPS NORMALEMENT
!
!
!
!
    integer :: retact, ievdac
    logical :: arret
    integer :: ldccvg, faccvg
    character(len=24) :: sdimpr, k24bla
    character(len=19) :: solveu
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    ldccvg = liccvg(2)
    faccvg = liccvg(5)
    arret = (nint(parcri(4)).eq.0)
    action = 0
    sdimpr = '&&PMACTN.SDIMPR'
    k24bla = ' '
    solveu = '&&OP0033'
!
! --- CONTINUER LA BOUCLE DE NEWTON EST IMPOSSIBLE ICI
!
    if (actite .eq. 2) ASSERT(.false.)
!
! --- AFFICHAGE
!
    if (ldccvg .eq. 1) then
        call u2mess('I', 'COMPOR1_9')
    else if (faccvg.eq.1) then
        call u2mess('I', 'COMPOR2_4')
    endif
!
! --- BOUCLE TEMPS SUIVANTE
!
    if (actite .eq. 0) then
        action = 3
        goto 999
    endif
!
! --- SORTIE DE BOUCLE (PROBLEME)
!
    if (actite .eq. 3) then
        action = 0
        goto 999
    endif
!
! --- ECHEC DE NEWTON: IL FAUT FAIRE QUELQUE CHOSE
!
    ASSERT(actite.eq.1)
!
! --- RECHERCHE DES EVENEMENTS ACTIVES
!
    call nmacto(sddisc, ievdac)
!
! --- ACTIONS SUITE A UN EVENEMENT
!
    if (ievdac .eq. 0) then
        retact = 0
    else
        call nmevac(sdimpr, sddisc, sderro, k24bla, k24bla,&
                    solveu, ievdac, numins, iterat, retact)
    endif
!
! --- TRAITEMENT DE L'ACTION
!
    if (retact .eq. 1) then
!
! ----- LA SUBDIVISION S'EST BIEN PASSEE: ON REFAIT LE PAS
!
        action = 1
!
    else if (retact.eq.2) then
!
! ----- AUTORISATION DE FAIRE DES ITERATIONS DE NEWTON EN PLUS
!
        action = 2
!
    else if (retact.eq.3) then
!
! ----- ON ARRETE TOUT
!
        action = 0
        call u2mess('F', 'MECANONLINE9_7')
!
    else if ((retact.eq.4).and.(.not.arret).and.itemax) then
!
! ----- CONVERGENCE FORCEE: ON VA AU PAS DE TEMPS SUIVANT
!
        call u2mess('A', 'MECANONLINE2_37')
        action = 3
!
    else if ((retact.eq.4).and.(.not.arret)) then
!
! ----- CONVERGENCE FORCEE: ON VA AU PAS DE TEMPS SUIVANT
!
        call u2mess('A', 'MECANONLINE2_37')
        action = 3
!
    else
!
! ----- ARRET DU CALCUL
!
        action = 0
        call u2mess('A', 'MECANONLINE9_7')
    endif
!
999  continue
!
    itemax = .false.
!
    call jedema()
end subroutine
