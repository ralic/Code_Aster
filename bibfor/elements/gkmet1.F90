subroutine gkmet1(ndeg, nnoff, chfond, iadrgk, iadgks,&
                  iadgki, abscur)
    implicit none
!
#include "jeveux.h"
#include "asterfort/detrsd.h"
#include "asterfort/glegen.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    integer :: ndeg, nnoff, iadrgk, iadgks, iadgki
    character(len=24) :: chfond, abscur
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
! ......................................................................
!      METHODE THETA-LEGENDRE ET G-LEGENDRE POUR LE CALCUL DE G(S)
!      K1(S) K2(S) et K3(S) DANS LE CADRE X-FEM
!
! ENTREE
!
!   NDEG     --> NOMBRE+1 PREMIERS CHAMPS THETA CHOISIS
!   NNOFF    --> NOMBRE DE POINTS DU FOND DE FISSURE
!   CHFOND   --> COORDS DES POINTS DU FOND DE FISSURE
!   IADRGK    --> ADRESSE DE VALEURS DE GKTHI
!                 (G, K1, K2, K3 POUR LES CHAMPS THETAI)
!
! SORTIE
!
!   IADGKS      --> ADRESSE DE VALEURS DE GKS
!         (VALEUR DE G(S), K1(S), K2(S), K3(S), BETA(S), G_IRWIN(S))
!   IADGKI      --> ADRESSE DE VALEURS DE GKTHI
!                  (G, K1, K2, K3 POUR LES CHAMPS THETAI)
!   ABSCUR     --> VALEURS DES ABSCISSES CURVILIGNES S
! ......................................................................
!
    integer :: iadrt3, i, j, k, ifon, iadabs
    real(kind=8) :: xl, som(5), gkthi(5), legs, gir(3)
!
!
!
!
!-----------------------------------------------------------------------
    integer :: i1
!-----------------------------------------------------------------------
    call jemarq()
!
!    VALEURS DU POLYNOME DE LEGENDRE  POUR LES NOEUDS DU FOND DE FISSURE
!
    call wkvect('&&METHO1.THETA', 'V V R8', (ndeg+1)*nnoff, iadrt3)
    call jeveuo(chfond, 'L', ifon)
!
    call jeveuo(abscur, 'E', iadabs)
    do 10 i = 1, nnoff
        zr(iadabs-1+(i-1)+1)=zr(ifon-1+4*(i-1)+4)
10  end do
    xl=zr(iadabs-1+(nnoff-1)+1)
!
    call glegen(ndeg, nnoff, xl, abscur, zr(iadrt3))
!
!     VALEURS DE G(S), K1(S), K2(S), K3(S), BETA(S)
!
    do 20 i = 1, nnoff
        do 21 k = 1, 5
            som(k) = 0.d0
21      continue
        do 25 k = 1, 3
            gir(k) = 0.d0
25      continue
        do 22 j = 1, ndeg+1
            legs=zr(iadrt3-1+(j-1)*nnoff+i)
! VALEUR DE G(S)
            gkthi(1)=zr(iadrgk-1+(j-1)*8+1)
            som(1) = som(1) + gkthi(1)*legs
! VALEUR DE K1(S), K2(S), K3(S)
            do 23 k = 2, 4
                gkthi(k)=zr(iadrgk-1+(j-1)*8+k+3)
                som(k) = som(k) + gkthi(k)*legs
23          continue
! VALEUR DE G_IRWIN(S)
            gir(1)= gir(1)+zr(iadrgk-1+(j-1)*8+2)*legs
            gir(2)= gir(2)+zr(iadrgk-1+(j-1)*8+3)*legs
            gir(3)= gir(3)+zr(iadrgk-1+(j-1)*8+4)*legs
22      continue
        som(5) = gir(1)*gir(1) + gir(2)*gir(2) + gir(3)*gir(3)
        do 24 k = 1, 5
            zr(iadgks-1+(i-1)*6+k) = som(k)
24      continue
        if (zr(iadgks-1+(i-1)*6+3) .ne. 0.d0) zr(&
                                              iadgks-1+(i-1)*6+6)= 2.0d0*atan2(0.25d0*(zr(iadgks-&
                                              &1+(i-1)*6+2)/zr(iadgks-1+(i-1) *6+3)-sign(1.0d0, z&
                                              &r(iadgks-1+(i-1)*6+3))*sqrt((zr(iadgks-1+ (i-1)*6+&
                                              &2)/zr(iadgks-1+(i-1)*6+3))**2.0d0+8.0d0)),&
                                              1.0d0&
                                              )
!
20  end do
!
!     VALEURS DE GI, K1I, K2I, K3I (ON RECOPIE SIMPLEMENT GKTHI)
    do 30 i = 1, (ndeg+1)
        i1 = i-1
        zr(iadgki-1+i1*5+1) =zr(iadrgk-1+(i-1)*8+1)
        zr(iadgki-1+i1*5+1+1) =zr(iadrgk-1+(i-1)*8+5)
        zr(iadgki-1+i1*5+2+1) =zr(iadrgk-1+(i-1)*8+6)
        zr(iadgki-1+i1*5+3+1) =zr(iadrgk-1+(i-1)*8+7)
30  end do
!
    call jedetr('&&METHO1.THETA')
    call jedetr('&&GKMET1.TEMP     .ABSCU')
    call detrsd('CHAMP_GD', '&&GMETH1.G2        ')
!
    call jedema()
end subroutine
