subroutine dxdmul(lcalct, icou, iniv, t1ve, t2ui,&
                  h, d1i, d2i, x3i, epi)
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
    implicit none
#include "jeveux.h"
!
#include "asterfort/codent.h"
#include "asterfort/jevech.h"
#include "asterfort/matini.h"
#include "asterfort/rcvala.h"
#include "asterfort/utbtab.h"
    logical :: lcalct
    integer :: icou
    integer :: iniv
    real(kind=8) :: t1ve(3, 3)
    real(kind=8) :: t2ui(2, 2)
    real(kind=8) :: h(3, 3)
    real(kind=8) :: d1i(2, 2), d2i(2, 4)
    real(kind=8) :: x3i, epi
!     ------------------------------------------------------------------
!     MATRICES D1I ET D2I POUR LE CALCUL DES CONTRAINTES EN MULTICOUCHE
!     (ELLES SONT FOURNIES DANS LE REPERE DE L'ELEMENT) CF BATOZ-DHATT
!     ------------------------------------------------------------------
!     IN  ICOU   : NUMERO DE LA COUCHE
!     IN  INIV   : NIVEAU DANS LA COUCHE (-1:INF , 0:MOY , 1:SUP)
!     IN  T1VE   : MATRICE DE CHANGEMENT DE REPERE D'UNE MATRICE (3,3)
!     IN  T2UI   : MATRICE DE CHANGEMENT DE REPERE D'UNE MATRICE (2,2)
!     OUT H      : MATRICE D'ELASTICITE DE LA COUCHE, REPERE INTRINSEQUE
!     OUT D1I    : MATRICE D1I REPERE INTRINSEQUE
!     OUT D2I    : MATRICE D2I REPERE INTRINSEQUE
!     OUT X3I    : Z DE CALCUL DE LA CONTRAINTE
!      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
    integer :: i, k, l, jcaco, jcou, jmate, icodre(27)
    character(len=2) :: val
    character(len=3) :: num
    character(len=8) :: nomres(27)
    real(kind=8) :: valres(27), r8bid
    real(kind=8) :: dx1i(2, 2), dx2i(2, 4)
    real(kind=8) :: da1i(2, 2), da2i(2, 4)
    real(kind=8) :: ordi, ai(3, 3)
    real(kind=8) :: xab1(3, 3), excen
!
    call jevech('PCACOQU', 'L', jcaco)
    excen = zr(jcaco+5-1)
!
    call jevech('PMATERC', 'L', jmate)
!     ----- RAPPEL DES CARACTERISTIQUES DU MONOCOUCHE ------------------
    call codent(icou, 'G', num)
    do 10 i = 1, 27
        call codent(i, 'G', val)
        nomres(i) = 'C'//num//'_V'//val
10  end do
    call rcvala(zi(jmate), ' ', 'ELAS_COQMU', 0, ' ',&
                [r8bid], 9, nomres, valres, icodre, 1)
    epi = valres(1)
    ordi = valres(3)
    h(1,1) = valres(4)
    h(1,2) = valres(5)
    h(1,3) = valres(6)
    h(2,2) = valres(7)
    h(2,3) = valres(8)
    h(3,3) = valres(9)
    h(2,1) = h(1,2)
    h(3,1) = h(1,3)
    h(3,2) = h(2,3)
!     ----- CALCUL DE Z ------------------------------------------------
    x3i = ordi + excen
    if (iniv .lt. 0) then
        x3i = x3i - epi/2.d0
    else if (iniv.gt.0) then
        x3i = x3i + epi/2.d0
    endif
!     ----- CALCUL DE D1I ET D2I ---------------------------------------
!
    call matini(2, 2, 0.d0, dx1i)
    call matini(2, 4, 0.d0, dx2i)
!
    if (.not.lcalct) then
        goto 999
    endif
!
    do 110 jcou = 1, icou
        call codent(jcou, 'G', num)
        do 60 i = 1, 27
            call codent(i, 'G', val)
            nomres(i) = 'C'//num//'_V'//val
60      continue
        call rcvala(zi(jmate), ' ', 'ELAS_COQMU', 0, ' ',&
                    [r8bid], 1, nomres, valres, icodre, 1)
        epi = valres(1)
        call rcvala(zi(jmate), ' ', 'ELAS_COQMU', 0, ' ',&
                    [r8bid], 1, nomres(3), valres(3), icodre(3), 1)
        ordi = valres(3)
        call rcvala(zi(jmate), ' ', 'ELAS_COQMU', 0, ' ',&
                    [r8bid], 12, nomres(16), valres(16), icodre(16), 1)
!
!      RECUP MATRICE AI = H(Z).HF-1
!
        ai(1,1) = valres(16)
        ai(2,1) = valres(17)
        ai(3,1) = valres(18)
        ai(1,2) = valres(19)
        ai(2,2) = valres(20)
        ai(3,2) = valres(21)
        ai(1,3) = valres(22)
        ai(2,3) = valres(23)
        ai(3,3) = valres(24)
!
!      PASSAGE DANS LE REPERE INTRINSEQUE A L'ELEMENT
!
        call utbtab('ZERO', 3, 3, ai, t1ve,&
                    xab1, ai)
!
!         TERMES DE LA MATRICE INTERVENANT DANS D1(Z)
!      CF. DHAT-BATOZ VOL 2 PAGE 243
!      DAI1 MATRICE (2,2) CONSTANTE PAR COUCHE
!      TERME 1,1 : A11+A33 TERME 1,2 : A13+A32
!      TERME 2,1 : A31+A23 TERME 2,2 : A22+A33
        da1i(1,1) = ai(1,1) + ai(3,3)
        da1i(1,2) = ai(1,3) + ai(3,2)
        da1i(2,1) = ai(3,1) + ai(2,3)
        da1i(2,2) = ai(2,2) + ai(3,3)
!         TERMES DE LA MATRICE INTERVENANT DANS D2(Z)
!      CF. DHAT-BATOZ VOL 2 PAGE 243
!      DAI2 MATRICE (2,4) CONSTANTE PAR COUCHE
        da2i(1,1) = ai(1,1) - ai(3,3)
        da2i(1,2) = ai(1,3) - ai(3,2)
        da2i(1,3) = ai(1,2)*2.d0
        da2i(1,4) = ai(3,1)*2.d0
        da2i(2,1) = ai(3,1) - ai(2,3)
        da2i(2,2) = ai(3,3) - ai(2,2)
        da2i(2,3) = ai(3,2)*2.d0
        da2i(2,4) = ai(2,1)*2.d0
!
!
!      D1(Z)=SOMME(-T,Z)(-Z/2*DA1I DZ)
!      TOUS CALCULS FAITS : K INDICE MAX TEL QUE ZK < X3I
!      D1=SOMME(I=1,K-1)(-1/2*DA1I*EPI*ORDI)+DAIK(ZK**2-X3I**2)
!
        do 80 k = 1, 2
            do 70 l = 1, 2
                d1i(k,l) = dx1i(k,l) + ((ordi-epi/2.d0)**2-x3i*x3i)* da1i(k,l)/4.d0
                dx1i(k,l) = dx1i(k,l) - epi*ordi*da1i(k,l)/2.d0
70          continue
80      continue
!
!      D2(Z)=SOMME(-T,Z)(-Z/2*DA2I DZ)
!      TOUS CALCULS FAITS : K INDICE MAX TEL QUE ZK < X3I
!      D2=SOMME(I=1,K-1)(-1/2*DA2I*EPI*ORDI)+DA2K(ZK**2-X3I**2)
!
        do 100 k = 1, 2
            do 90 l = 1, 4
                d2i(k,l) = dx2i(k,l) + ((ordi-epi/2.d0)**2-x3i*x3i)* da2i(k,l)/4.d0
                dx2i(k,l) = dx2i(k,l) - epi*ordi*da2i(k,l)/2.d0
90          continue
100      continue
110  end do
!
999  continue
!
!     MATRICE H DANS LE REPERE INTRINSEQUE DE L'ELEMENT
!
    call utbtab('ZERO', 3, 3, h, t1ve,&
                xab1, h)
!
end subroutine
