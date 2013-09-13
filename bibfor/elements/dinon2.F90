subroutine dinon2(neq, ul, dul, utl, nno,&
                  nbcomp, varimo, raide, nbpar, param,&
                  okdire, varipl, dt)
! ----------------------------------------------------------------------
    implicit none
#include "asterc/r8miem.h"
#include "asterfort/utmess.h"
    integer :: neq, nbcomp, nno, nbpar
    real(kind=8) :: ul(neq), dul(neq), utl(neq), dt
    real(kind=8) :: varimo(nbcomp*2), varipl(nbcomp*2)
    real(kind=8) :: raide(nbcomp), param(6, nbpar)
    logical :: okdire(6)
! ----------------------------------------------------------------------
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
! ======================================================================
!
!     RELATION DE COMPORTEMENT "VISQUEUX" (DISCRET NON LINEAIRE).
!
!     F = C.|V|^a
!        V    : vitesse
!        C, a : caracteristiques de l'amortisseur
!
! ======================================================================
!
! IN  :
!       NEQ    : NOMBRE DE DDL DE L'ELEMENT
!       UL     : DEPLACEMENT PRECEDENT REPERE LOCAL (DIM NEQ)
!       DUL    : INCREMENT DE DEPLACEMENT REPERE LOCAL (DIM NEQ)
!       UTL    : DEPLACEMENT COURANT REPERE LOCAL (DIM NEQ)
!       NNO    : NOMBRE DE NOEUDS
!       NBCOMP : NOMBRE DE COMPOSANTES
!       VARIMO : VARIABLES INTERNES A T- (2 PAR COMPOSANTES)
!       RAIDE  : RAIDEUR ELASTIQUE DES DISCRETS
!       NBPAR  : NOMBRE MAXIMAL DE PARAMETRE DE LA LOI
!       PARAM  : PARAMETRES DE LA LOI
!       OKDIRE : VRAI SI LE COMPORTEMENT AFFECTE CETTE DIRECTION
!       DT     : INCREMENT DE TEMPS
!
! OUT :
!       RAIDE  : RAIDEUR QUASI-TANGENTE AU COMPORTEMENT DES DISCRETS
!       VARIPL : VARIABLES INTERNES INTERNES A T+ (2 PAR COMPOSANTE)
!
!***************** DECLARATION DES VARIABLES LOCALES *******************
!
    integer :: ii, iener, iforc
    real(kind=8) :: dulel, zero, r8min
!
    real(kind=8) :: vitess, fplus, fmoin, puiss, kr
!
!************ FIN DES DECLARATIONS DES VARIABLES LOCALES ***************
!
! ----------------------------------------------------------------------
    r8min = r8miem()
    zero = 0.0d0
!
    if (dt .lt. r8min) then
        call utmess('F', 'DISCRETS_4', sr=dt)
    endif
!
    do 20, ii=1,nbcomp
!        INDEX DES VARIABLES INTERNES
    iforc = 2*(ii-1)+1
    iener = 2*(ii-1)+2
!        PAR DEFAUT LES VARIABLES N'EVOLUENT PAS
    varipl(iforc) = varimo(iforc)
    varipl(iener) = varimo(iener)
!        SI LE COMPORTEMENT EST VISQUEUX
    if (okdire(ii)) then
        puiss = param(ii,1)
        if (nno .eq. 1) then
            dulel = dul(ii)
!               ULEL  = UL(II)
!               UTLEL = UTL(II)
        else
            dulel = dul(ii+nbcomp) - dul(ii)
!               ULEL  = UL(II+NBCOMP)  - UL(II)
!               UTLEL = UTL(II+NBCOMP) - UTL(II)
        endif
        kr = param(ii,2)
!           CALCUL DE LA FORCE A T+
        vitess = abs(dulel/dt)
        if (vitess .gt. r8min) then
            if (dulel .gt. zero) then
                fplus = -kr*((vitess)**puiss)
            else
                fplus = kr*((vitess)**puiss)
            endif
        else
            fplus = zero
        endif
!           RECUPERATION DE LA FORCE A T-
        fmoin = varimo(iforc)
!           ACTUALISATION DE LA VARIABLE INTERNE
        varipl(iforc) = fplus
!           RAIDEUR QUASI-TANGENTE AU COMPORTEMENT
        if (abs(dulel) .gt. r8min) then
            raide(ii) = abs((fplus - fmoin))/dulel
        endif
!           CALCUL DE L'ENERGIE DISSIPEE
        varipl(iener) = varimo(iener) - fplus*dulel
    endif
    20 end do
!
end subroutine
