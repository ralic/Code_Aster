subroutine lcloca(coeft,nmat, nbcomm,&
                  nphas, sigi, vini, iphas, granb,&
                  loca, sigg)
!

    implicit none
#include "asterc/r8miem.h"
#include "asterfort/lcdevi.h"
#include "asterfort/lcnrts.h"
#include "asterfort/utmess.h"
    integer :: nphas, nmat, nbcomm(nmat, 3), iphas
    real(kind=8) :: vini(*), coeft(nmat)
    real(kind=8) :: sigi(6), alpha, sigg(6)
    character(len=16) :: loca
    real(kind=8) :: mu, dev(6), norme, evpcum, granb(6)
    integer :: ievpg, i
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jean-michel.proix at edf.fr
! ======================================================================
!       in
!           coeft    :  coef materiau
!           nmat     :  nombre  maxi de coef materiau
!           nbcomm   :  nombre de coef materiau par famille
!           nphas    :  nombre de phases
!           sigi     :  contraintes a l'instant courant
!           vini     :  variables internes a t
!           iphas    :  phase courante
!           granb    :  variables internes pour la règle en Beta
!           loca     :  nom de la règle de localisation
!     out:
!           sigg    : tenseur des contraintes pour la phase iphas
! integration des lois polycristallines par une methode de runge kutta
!
!     cette routine permet d'appliquer la methode de localisation
!
!     7 variables : tenseur EVP + Norme(EVP)
!    description des variables internes :
!    pour chaque phase
!        6 variables : beta ou epsilonp par phase
!    pour chaque phase
!        pour chaque systeme de glissement
!              3 variables Alpha, Gamma, P
!    1 variable : indic
! ======================================================================
!
    mu=coeft(nbcomm((nphas+2),1)+0)
!
! --  METHODE LOCALISATION
    if (loca .eq. 'BZ') then
        call lcdevi(sigi, dev)
        norme = lcnrts( dev )
        evpcum=vini(7)
        if (norme .gt. r8miem()) then
            alpha=norme/(norme+1.5d0*mu*evpcum)
        else
            alpha=0.d0
        endif
!        EVP - EVPG(IPHAS)
        ievpg=7+6*(iphas-1)
        do 1 i = 1, 6
            sigg(i)=sigi(i)+alpha*mu*(vini(i)-vini(ievpg+i))
 1      continue
!
    else if (loca.eq.'BETA') then
!        EVP - EVPG(IPHAS)
        ievpg=7+6*(iphas-1)
        do 2 i = 1, 6
            sigg(i)=sigi(i)+mu*(granb(i)-vini(ievpg+i))
 2      continue
!
!
    else
        call utmess('F', 'ALGORITH4_63', sk=loca)
    endif
end subroutine
