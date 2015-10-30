subroutine dylach(nomo, mate, carele, lischa, numedd,&
                  vediri, veneum, vevoch, vassec)
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
!
    implicit      none
#include "jeveux.h"
#include "asterfort/asvepr.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/veassc.h"
#include "asterfort/vechms.h"
#include "asterfort/vedimd.h"
    character(len=8) :: nomo
    character(len=24) :: mate, carele
    character(len=19) :: lischa
    character(len=*) :: numedd
    character(len=19) :: vediri, veneum, vevoch, vassec
!
! ----------------------------------------------------------------------
!
! DYNA_VIBRA//HARM/GENE
!
! CALCUL ET PRE-ASSEMBLAGE DU CHARGEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  NOMO   : NOM DU MODELE
! IN  LISCHA : SD LISTE DES CHARGES
! IN  CARELE : CARACTERISTIQUES DES POUTRES ET COQUES
! IN  MATE   : MATERIAU CODE
! IN  NUMEDD : NOM DU NUME_DDL
! OUT VEDIRI : VECT_ELEM DE L'ASSEMBLAGE DES ELEMENTS DE LAGRANGE
! OUT VENEUM : VECT_ELEM DE L'ASSEMBLAGE DES CHARGEMENTS DE NEUMANN
! OUT VEVOCH : VECT_ELEM DE L'ASSEMBLAGE DES CHARGEMENTS EVOL_CHAR
! OUT VASSEC : VECT_ELEM DE L'ASSEMBLAGE DES CHARGEMENTS VECT_ASSE_CHAR
!
!
!
!
    real(kind=8) :: partps(3), instan
    character(len=19) :: k19bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    instan = 0.d0
    partps(1) = instan
    partps(2) = 0.d0
    partps(3) = 0.d0
!
! --- CALCUL DES VECTEURS ELEMENTAIRES
!
    call vedimd(nomo, lischa, instan, vediri)
    call vechms(nomo, mate, carele, k19bid, lischa,&
                partps, veneum)
    call veassc(lischa, vassec)
!
! --- PREPARATION DE L'ASSEMBLAGE
!
    call asvepr(lischa, vediri, 'C', numedd)
    call asvepr(lischa, veneum, 'C', numedd)
    call asvepr(lischa, vevoch, 'C', numedd)
    call asvepr(lischa, vassec, 'C', numedd)
!
    call jedema()
!
end subroutine
