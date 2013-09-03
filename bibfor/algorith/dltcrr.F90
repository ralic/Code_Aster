subroutine dltcrr(result, neq, nbordr, iarchi, texte,&
                  ifm, t0, lcrea, typres, masse,&
                  rigid, amort, dep0, vit0, acc0,&
                  fexte, famor, fliai, numedd, nume,&
                  nbtyar, typear)
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
! ----------------------------------------------------------------------
!
!       DYNAMIQUE LINEAIRE TRANSITOIRE - CREATION DES RESULTATS
!       -         -        -             --           -
!
! ----------------------------------------------------------------------
!  IN  : NEQ       : NOMBRE D'EQUATIONS
!  IN  : IARCHI    : PILOTAGE DE L'ARCHIVAGE DES RESULTATS
!  IN  : TEXTE     : COMMENTAIRE A IMPRIMER
!  IN  : T0        : INSTANT DE CALCUL INITIAL
!  IN  : LCREA     : LOGIQUE INDIQUANT SI IL Y A REPRISE
!  IN  : TYPRES    : TYPE DE RESULTAT
!  IN  : MASSE     : MATRICE DE MASSE
!  IN  : RIGID     : MATRICE DE RIGIDITE
!  IN  : AMORT     : MATRICE D'AMORTISSEMENT
!  VAR : DEP0      : TABLEAU DES DEPLACEMENTS A L'INSTANT N
!  VAR : VIT0      : TABLEAU DES VITESSES A L'INSTANT N
!  VAR : ACC0      : TABLEAU DES ACCELERATIONS A L'INSTANT N
!  IN  : NUMEDD    : NUME_DDL DE LA MATR_ASSE RIGID
!  IN  : NUME      : NUMERO D'ORDRE DE REPRISE
!
!
! aslint: disable=W1504
    implicit none
#include "jeveux.h"
#include "asterfort/dlarch.h"
#include "asterfort/jelibe.h"
#include "asterfort/refdaj.h"
#include "asterfort/rsagsd.h"
#include "asterfort/rscrsd.h"
#include "asterfort/titre.h"
#include "asterfort/wkvect.h"
    integer :: neq, nbordr, iarchi, ifm, ir
    integer :: nume, nbtyar
!
    real(kind=8) :: dep0(neq), vit0(neq), acc0(neq), t0
    real(kind=8) :: fexte(2*neq), famor(2*neq), fliai(2*neq)
!
    character(len=8) :: masse, rigid, amort
    character(len=8) :: result
    character(len=16) :: typres
    character(len=16) :: typear(nbtyar)
    character(len=24) :: numedd, matric(3)
    character(len=*) :: texte
!
    logical :: lcrea
!
!
!
    integer :: istoc

!
!
!====
! 2. CREATION DE LA STRUCTURE DE DONNEE RESULTAT
!====
!
    if (lcrea) then
!
! 2.1. ==> CREATION DE LA STRUCTURE DE DONNEE RESULTAT
!
        call rscrsd('G', result, typres, nbordr)
        matric (1) = rigid
        matric (2) = masse
        matric (3) = amort
        call refdaj ('F', result, nbordr, numedd, 'DYNAMIQUE', matric, ir)
!
! 2.2. ==> ARCHIVAGE INITIAL
!
        iarchi = -1
        istoc = 0
!
        call dlarch(result, neq, istoc, iarchi, texte,&
                    1, ifm, t0, nbtyar, typear,&
                    masse, dep0, vit0, acc0, fexte(neq+1),&
                    famor( neq+1), fliai(neq+1))
!
        iarchi = 0
!
!====
! 3. RECUPERATION
!====
    else
        nbordr = nbordr + nume
        call rsagsd(result, nbordr)
    endif
!
!====
! 4. TITRE
!====
!
    call titre()
!
end subroutine
