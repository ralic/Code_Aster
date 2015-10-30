subroutine dldif0(result, force1, neq, istoc, iarchi,&
                  lamort, imat, masse, rigid, amort,&
                  dep0, vit0, acc0, depl1, vite1,&
                  acce1, vite2, fexte, famor, fliai,&
                  nchar, nveca, liad, lifo, modele,&
                  ener, solveu, mate, carele, charge,&
                  infoch, fomult, numedd, dt, temps,&
                  tabwk0, tabwk1, archiv, nbtyar, typear,&
                  numrep)
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
!     ------------------------------------------------------------------
!     CALCUL MECANIQUE TRANSITOIRE PAR INTEGRATION DIRECTE
!     AVEC  METHODE EXPLICITE :  DIFFERENCES CENTREES
!     ------------------------------------------------------------------
!  IN  : NEQ       : NOMBRE D'EQUATIONS
!  IN  : ISTOC     : PILOTAGE DU STOCKAGE DES RESULTATS
!  IN  : IARCHI    : PILOTAGE DE L'ARCHIVAGE DES RESULTATS
!  IN  : LAMORT    : LOGIQUE INDIQUANT SI IL Y A AMORTISSEMENT
!  IN  : IMAT      : TABLEAU D'ADRESSES POUR LES MATRICES
!  IN  : MASSE     : MATRICE DE MASSE
!  IN  : NCHAR     : NOMBRE D'OCCURENCES DU MOT CLE CHARGE
!  IN  : NVECA     : NOMBRE D'OCCURENCES DU MOT CLE VECT_ASSE
!  IN  : LIAD      : LISTE DES ADRESSES DES VECTEURS CHARGEMENT (NVECT)
!  IN  : LIFO      : LISTE DES NOMS DES FONCTIONS EVOLUTION (NVECT)
!  IN  : MODELE    : NOM DU MODELE
!  IN  : MATE      : NOM DU CHAMP DE MATERIAU
!  IN  : CARELE    : CARACTERISTIQUES DES POUTRES ET COQUES
!  IN  : CHARGE    : LISTE DES CHARGES
!  IN  : INFOCH    : INFO SUR LES CHARGES
!  IN  : FOMULT    : LISTE DES FONC_MULT ASSOCIES A DES CHARGES
!  IN  : NUMEDD    : NUME_DDL DE LA MATR_ASSE RIGID
!  VAR : DEP0      : TABLEAU DES DEPLACEMENTS A L'INSTANT N
!  VAR : VIT0      : TABLEAU DES VITESSES A L'INSTANT N
!  VAR : ACC0      : TABLEAU DES ACCELERATIONS A L'INSTANT N
!  IN  : TEMPS     : INSTANT COURANT
! IN  NUMREP : NUMERO DE REUSE POUR LA TABLE PARA_CALC
!
! CORPS DU PROGRAMME
! aslint: disable=W1504
    implicit none
! DECLARATION PARAMETRES D'APPELS
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/dlarch.h"
#include "asterfort/dlfdyn.h"
#include "asterfort/dlfext.h"
#include "asterfort/enerca.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmarpc.h"
#include "asterfort/wkvect.h"
#include "blas/dcopy.h"
    integer :: neq, istoc, iarchi, ivit0r
    integer :: ifnobi, ifcibi
    integer :: archiv, nbtyar
    integer :: imat(3)
    integer :: nchar, nveca, liad(*)
!
    real(kind=8) :: dep0(*), vit0(*), acc0(*)
    real(kind=8) :: depl1(neq), vite1(neq), acce1(neq)
    real(kind=8) :: vite2(neq)
    real(kind=8) :: fexte(*), famor(*), fliai(*)
    real(kind=8) :: tabwk0(neq), tabwk1(neq)
    real(kind=8) :: dt, temps
!
    character(len=8) :: masse, rigid, amort
    character(len=16) :: typear(nbtyar)
    character(len=24) :: modele, mate, carele, charge, infoch, fomult, numedd
    character(len=24) :: lifo(*)
    character(len=19) :: solveu, sdener
    character(len=8) :: result
    character(len=19) :: force1
    integer :: numrep
!
    aster_logical :: lamort, ener
!
!
!
!
!
    integer :: iforc1, ieq, alarm
    real(kind=8) :: r8bid
    character(len=19) :: masse1, amort1, rigid1, k19bid
!
! --- CALCUL DES DEPLACEMENTS ET VITESSES
!
    do ieq = 1 , neq
    vite1(ieq) = vit0(ieq) + dt*acc0(ieq)
    depl1(ieq) = dep0(ieq) + dt*vite1(ieq)
    end do
!
!====
! 3. CALCUL DU SECOND MEMBRE F*
!====
!
    call jeveuo(force1(1:19)//'.VALE', 'E', iforc1)
!
    call dlfext(nveca, nchar, temps, neq, liad,&
                lifo, charge, infoch, fomult, modele,&
                mate, carele, numedd, zr(iforc1))
!
    if (ener) then
        do 433 ieq = 1, neq
            fexte(ieq)=fexte(ieq+neq)
            fexte(ieq+neq)=zr(iforc1+ieq-1)
433     continue
    endif
!
    call dlfdyn(imat(1), imat(3), lamort, neq, depl1,&
                vite1, zr(iforc1), tabwk0)
!
!====
! 4.  RESOLUTION DU PROBLEME M . A = F ET DE LA VITESSE STOCKEE
!           --- RESOLUTION AVEC FORCE1 COMME SECOND MEMBRE ---
!====
!
    r8bid = dt/2.d0
!
    do ieq = 1 , neq
!
        acce1(ieq) = tabwk1(ieq)*zr(iforc1+ieq-1)
!
!        --- VITESSE AUX INSTANTS 'TEMPS + DT' ---
        vite2(ieq) = vite1(ieq) + r8bid*acce1(ieq)
!
    end do
!
!
!====
! 5.  CALCUL DES ENERGIES
!
!====
!
    sdener=solveu(1:8)//'.ENER      '
    if (ener) then
        masse1=masse//'           '
        amort1=amort//'           '
        rigid1=rigid//'           '
        call wkvect('FNODABID', 'V V R', 2*neq, ifnobi)
        call wkvect('FCINEBID', 'V V R', 2*neq, ifcibi)
! ON CALCULE LA VITESSE A T N-1
        call wkvect('VIT0_TR', 'V V R', neq, ivit0r)
        do 50 ieq = 1, neq
            zr(ivit0r-1+ieq)=vit0(ieq)+r8bid*acc0(ieq)
 50     continue
        call enerca(k19bid, dep0, zr(ivit0r), depl1, vite2,&
                    masse1, amort1, rigid1, fexte, famor,&
                    fliai, zr(ifnobi), zr(ifcibi), lamort, .true._1,&
                    .false._1, sdener, '&&DLDIFF')
        call jedetr('FNODABID')
        call jedetr('FCINEBID')
        call jedetr('VIT0_TR')
    endif
!====
! 5. TRANSFERT DES NOUVELLES VALEURS DANS LES ANCIENNES
!====
!
    call dcopy(neq, depl1, 1, dep0, 1)
    call dcopy(neq, vite1, 1, vit0, 1)
    call dcopy(neq, acce1, 1, acc0, 1)
!
!====
! 7. ARCHIVAGE EVENTUEL DANS L'OBJET SOLUTION
!====
!
    if (archiv .eq. 1) then
!
        istoc = 0
        alarm = 1
!
        call dlarch(result, neq, istoc, iarchi, ' ',&
                    alarm, temps, nbtyar, typear, masse,&
                    depl1, vite2, acce1, fexte(neq+ 1), famor(neq+1),&
                    fliai(neq+1))
!
    endif
!===
! 8. ARCHIVAGE DES PARAMETRES
!===
    call nmarpc(result, sdener, numrep, temps)
!
end subroutine
