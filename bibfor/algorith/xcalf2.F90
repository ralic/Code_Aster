subroutine xcalf2(he, lsng, lstg, baslog, fe,&
                  dgdgl, iret)
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
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
!
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/xdeffe.h"
#include "asterfort/xderfe.h"
    real(kind=8) :: he, lsng, lstg, baslog(6), fe(4), dgdgl(4, 2)
    integer :: iret
!
!
!
!
!
!     BUT:  CALCUL DES FONCTIONS D'ENRICHISSEMENT EN UN POINT DE GAUSS
!
! IN  XYZ     : COORDONNEES DU POINT DE GAUSS CONSIDERE
! IN  HE      : VALEUR DE LA FONCTION HEAVYSIDE CSTE LE SS-ELT
! IN  LSNG    : VALEUR DE LA LEVEL SET NORMALE AU POINT DE GAUSS
! IN  LSTG    : VALEUR DE LA LEVEL SET TANGENTE AU POINT DE GAUSS
! IN  BASLOG  : BASE LOCALE AU FOND DE FISSURE AU POINT DE GAUSS
!
! OUT FE      : VALEURS DES FONCTIONS D'ENRICHISSEMENT
! OUT DGDGL   : DERIVEES DES FONCTIONS D'ENRICHISSEMENT
! OUT IRET    : CODE RETOUR VALANT 0 SI ON SE TROUVE SUR LE FOND DE
!               FISSURE (RG=0).
!               LES DERIVEES DES FONCTIONS SINGULIERES (DGDGL)
!               NE SONT ALORS PAS CALCULEES (CAR EN 1/SQRT(RG)).
!
!----------------------------------------------------------------
!
    integer :: i, j, k
    real(kind=8) :: e1(2), e2(2), p(2, 2), invp(2, 2), norme
    real(kind=8) :: rg, tg, dgdpo(4, 2), dgdlo(4, 2), det
!
!     RECUPERATION DE LA BASE LOCALE ASSOCIEE AU PT
!     (E1=GRLT,E2=GRLN)
    do 123 i = 1, 2
        e1(i) = baslog(i+2)
        e2(i) = baslog(i+4)
123  end do
!     NORMALISATION DE LA BASE
    norme = e1(1)*e1(1)+e1(2)*e1(2)
    if (norme .ne. 0.0d0) then
        norme = sqrt(norme)
        e1(1) = e1(1)/norme
        e1(2) = e1(2)/norme
    endif
    norme = e2(1)*e2(1)+e2(2)*e2(2)
    if (norme .ne. 0.0d0) then
        norme = sqrt(norme)
        e2(1) = e2(1)/norme
        e2(2) = e2(2)/norme
    endif
!
!     CALCUL DE LA MATRICE DE PASSAGE P TQ 'GLOBAL' = P * 'LOCAL'
    do 124 i = 1, 2
        p(i,1)=e1(i)
        p(i,2)=e2(i)
124  end do
!
!     VERIFICATION QUE LE DETERMINANT DE P VAUT BIEN 1
    det = p(1,1)*p(2,2) - p(2,1)*p(1,2)
!
!     LA BASE LOCALE SEMBLE FAUSSE
    ASSERT((abs(det)-1.d0).le.1.d-5)
!
!     CALCUL DE L'INVERSE DE LA MATRICE DE PASSAGE : INV=TRANSPOSE(P)
    do 125 i = 1, 2
        do 126 j = 1, 2
            invp(i,j)=p(j,i)
126      continue
125  end do
!
!
!     COORDONNÉES POLAIRES DU POINT
    rg=sqrt(lsng**2+lstg**2)
!
    if (rg .gt. r8prem()) then
!       LE POINT N'EST PAS SUR LE FOND DE FISSURE
        tg = he * abs(atan2(lsng,lstg))
        iret=1
    else
!       LE POINT EST SUR LE FOND DE FISSURE :
!       L'ANGLE N'EST PAS DÉFINI, ON LE MET À ZÉRO
!       ON NE FERA PAS LE CALCUL DES DÉRIVÉES
        tg=0.d0
        iret=0
    endif
!
!     FONCTIONS D'ENRICHISSEMENT DANS LA BASE POLAIRE -> FE
    call xdeffe(rg, tg, fe)
!
!     CALCUL DES DERIVEES
!     -------------------
!
!     DERIVEES DES FONCTIONS D'ENRICHISSEMENT DANS LA BASE POLAIRE
    call xderfe(rg, tg, dgdpo)
!
!     DERIVEES DES FONCTIONS D'ENRICHISSEMENT DANS LA BASE LOCALE
    do 131 i = 1, 4
        dgdlo(i,1)=dgdpo(i,1)*cos(tg)-dgdpo(i,2)*sin(tg)/rg
        dgdlo(i,2)=dgdpo(i,1)*sin(tg)+dgdpo(i,2)*cos(tg)/rg
131  end do
!
!     DERIVEES DES FONCTIONS D'ENRICHISSEMENT DANS LA BASE GLOBALE
    do 132 i = 1, 4
        do 133 j = 1, 2
            dgdgl(i,j)=0.d0
            do 134 k = 1, 2
                dgdgl(i,j)=dgdgl(i,j)+dgdlo(i,k)*invp(k,j)
134          continue
133      continue
132  end do
!
end subroutine
