subroutine nmitsp(sdimpr, sddisc, iterat, retsup)
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
    implicit     none
#include "jeveux.h"
#include "asterfort/affich.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmacex.h"
#include "asterfort/nmimpx.h"
#include "asterfort/nmlerr.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mess.h"
    character(len=24) :: sdimpr
    character(len=19) :: sddisc
    integer :: iterat, retsup
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - GESTION DES EVENEMENTS)
!
! GESTION DE L'ACTION ITER_SUPPL
!
! ----------------------------------------------------------------------
!
!
! IN  SDIMPR : SD AFFICHAGE
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  ITERAT : NUMERO D'ITERATION DE NEWTON
! OUT RETSUP : CODE RETOUR
!               0 ON NE PEUT AJOUTER D'ITERATIONS
!               1 ON FAIT DONC DES ITERATIONS EN PLUS
!
! ----------------------------------------------------------------------
!
    logical :: lextra
    real(kind=8) :: valext(4), ciblen
    integer :: itesup, nbitaj, vali(2), nbiter, mniter
    real(kind=8) :: r8bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    retsup = 1
    nbitaj = 0
    ciblen = 0.d0
    lextra = .false.
!
! --- LECTURE DES INFOS SUR LES CONVERGENCES
!
    call nmlerr(sddisc, 'L', 'MNITER', r8bid, mniter)
    call nmlerr(sddisc, 'L', 'NBITER', r8bid, nbiter)
!
! --- AFFICHAGE
!
    if (iterat .ge. nbiter) then
        retsup = 0
        call u2mesi('I', 'ITERSUPP_2', 1, nbiter)
        goto 999
    endif
!
! --- EXTRAPOLATION LINEAIRE DES RESIDUS
!
    call nmacex(sddisc, iterat, lextra, valext)
!
! --- CALCUL DE LA CIBLE (NOMBRE D'ITERATIONS)
!
    if (lextra) then
        ciblen = (valext(1) + valext(2)*log(valext(4)) )/valext(3)
        call u2mess('I', 'EXTRAPOLATION_11')
    else
        ciblen = 0.d0
        call u2mess('I', 'EXTRAPOLATION_10')
        retsup = 0
        goto 999
    endif
!
! --- NOMBRE D'ITERATIONS SUPPLEMENTAIRES
!
    nbitaj = nint(ciblen)
!
! --- AFFICHAGE
!
    call u2mesi('I', 'ITERSUPP_3', 1, nbitaj)
!
! --- L'EXTRAPOLATION DONNE UN NOMBRE D'ITERATION < LIMITE ITERATION
!
    if ((ciblen*1.20d0) .lt. mniter) then
        retsup = 0
        call u2mess('I', 'ITERSUPP_4')
        goto 999
    endif
!
! --- L'EXTRAPOLATION DONNE UN NOMBRE D'ITERATION > LIMITE ITERATION
!
    if (nbitaj .ge. nbiter) then
        retsup = 0
        vali(1) = nbitaj
        vali(2) = nbiter
        call u2mesi('I', 'ITERSUPP_5', 2, vali)
    endif
!
999  continue
!
    if (retsup .eq. 1) then
        call u2mess('I', 'ITERSUPP_7')
        call affich('MESSAGE', ' ')
        call nmimpx(sdimpr)
        itesup = 1
    else if (retsup.eq.0) then
        call u2mess('I', 'ITERSUPP_6')
        itesup = 0
    else
        ASSERT(.false.)
    endif
    call nmlerr(sddisc, 'E', 'ITERSUP', r8bid, itesup)
!
    call jedema()
end subroutine
