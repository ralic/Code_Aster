subroutine nmrech(fm, f, fopt, fcvg, rhomin,&
                  rhomax, rhoexm, rhoexp, rhom, rho,&
                  rhoopt, ldcopt, ldccvg, opt, act,&
                  stite)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterc/r8prem.h"
#include "asterfort/zbinte.h"
    real(kind=8) :: rhomin, rhomax, rhoexm, rhoexp
    real(kind=8) :: rhom, rho, rhoopt
    real(kind=8) :: fm, f, fopt, fcvg
    logical :: stite
    integer :: ldcopt, ldccvg
    integer :: opt, act
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (RECH. LINE. - METHODE CORDE)
!
! RECHERCHE LINEAIRE AVEC LA METHODE CORDE
!
! ----------------------------------------------------------------------
!
!
! I/O FM     : VALEUR PRECEDENTE DE LA FONCTIONNELLE
! IN  F      : VALEUR COURANTE DE LA FONCTIONNELLE
! I/O FOPT   : VALEUR OPTIMALE DE LA FONCTIONNELLE
! IN  FCVG   : VALEUR DONNANT LA VALEUR DE LA FONCTIONNELLE POUR QUE
!              L'ALGO CONVERGE
! I/O RHOM   : VALEUR PRECEDENTE DU COEF. RECH. LINE.
! IN  RHO    : VALEUR COURANTE DU COEF. RECH. LINE.
! I/O RHOOPT : VALEUR OPTIMALE DU COEF. RECH. LINE.
! IN  LDCCVG : CODE RETOUR INTEGRATION COMPORTEMENT
! OUT LDCOPT : CODE RETOUR INTEGRATION COMPORTEMENT QUAND COEF. RECH.
!              LINE. OPTIMAL
! I/O ACT    : INDICE DE LA SOLUTION (DEUX QUAND PILOTAGE)
! OUT OPT    : INDICE DE LA SOLUTION QUAND COEF. RECH.
!              LINE. OPTIMAL
! OUT STITE  : .TRUE. SI ALGO. A CONVERGE
! IN  RHOMIN : BORNE INFERIEURE DE RECHERCHE
! IN  RHOMAX : BORNE SUPERIEURE DE RECHERCHE
! IN  RHOEXM : INTERVALLE [RHOEXM,RHOEXP] POUR EXCLUSION
! IN  RHOEXP : INTERVALLE [RHOEXM,RHOEXP] POUR EXCLUSION
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: rhotmp
!
! ----------------------------------------------------------------------
!
    stite = .false.
!
! --- PRISE EN COMPTE D'UN RESIDU OPTIMAL SI NECESSAIRE
!
    if (abs(f) .lt. fopt) then
        rhoopt = rho
        ldcopt = ldccvg
        fopt = abs(f)
        opt = act
        act = 3 - act
        if (abs(f) .lt. fcvg) then
            stite = .true.
            goto 100
        endif
    endif
!
! --- CALCUL DE RHO(N+1) PAR METHODE DE SECANTE AVEC BORNES
!
    rhotmp = rho
    if (abs(f-fm) .gt. r8prem()) then
        rho = (f*rhom-fm*rho)/(f-fm)
        call zbinte(rho, rhomin, rhomax, rhoexm, rhoexp)
    else if (f*(rho-rhom)*(f-fm) .le. 0.d0) then
        rho = rhomax
    else
        rho = rhomin
    endif
    rhom = rhotmp
    fm = f
!
100  continue
!
end subroutine
