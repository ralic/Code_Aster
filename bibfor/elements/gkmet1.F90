subroutine gkmet1(ndeg, nnoff, chfond, iadrgk, iadgks, &
                  iadgki, abscur)

implicit none

#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/detrsd.h"
#include "asterfort/glegen.h"
#include "asterfort/jedema.h"
#include "asterfort/gmatr1.h"
#include "asterfort/gsyste.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"

    integer           :: ndeg, nnoff, iadrgk, iadgks, iadgki
    character(len=24) :: chfond, abscur 

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
! ......................................................................
!      METHODE THETA-LEGENDRE ET G-LEGENDRE POUR LE CALCUL DE G(S)
!      K1(S) K2(S) et K3(S) DANS LE CADRE X-FEM
!
! ENTREE
!
!   NDEG     --> NOMBRE+1 PREMIERS CHAMPS THETA CHOISIS
!   NNOFF    --> NOMBRE DE POINTS DU FOND DE FISSURE
!   CHFOND   --> COORDS DES POINTS DU FOND DE FISSURE
!   ABSCUR   --> VALEURS DES ABSCISSES CURVILIGNES S
!   IADRGK   --> ADRESSE DE VALEURS DE GKTHI
!                (G, K1, K2, K3 POUR LES CHAMPS THETAI)

! SORTIE
!
!   IADGKS      --> ADRESSE DE VALEURS DE GKS
!                   (VALEUR DE G(S), K1(S), K2(S), K3(S), BETA(S), G_IRWIN(S))
!   IADGKI      --> ADRESSE DE VALEURS DE GKTHI
!                   (G, K1, K2, K3 POUR LES CHAMPS THETAI)
! ......................................................................
!
    integer                            :: iadrt3, i, i1,j, k, ifon, iadabs
    real(kind=8)                       :: xl, som(5), gir(3)
    character(len=24)                  :: matr
    real(kind=8),dimension(nnoff)      :: gthi, k1th, k2th, k3th,g1th,g2th,g3th
    real(kind=8),dimension(nnoff)      :: gi, k1i, k2i, k3i, g1i,g2i,g3i
    real(kind=8),dimension(nnoff)      :: gs, k1s, k2s, k3s,gis

!.......................................................................
    call jemarq()

    call jeveuo(abscur, 'E', iadabs)
    call jeveuo(chfond, 'L', ifon)

!   ABSCISSES CURVILIGNES LE LONG DU FRONT DE FISSURE
    do i = 1, nnoff
        zr(iadabs-1+(i-1)+1)=zr(ifon-1+4*(i-1)+4)
    end do
    
    xl=zr(iadabs-1+(nnoff-1)+1)

!   CALCUL DE LA MATRICE DU SYSTEME LINÉAIRE [A] {GS} = {GTHI}
    matr = '&&METHO1.MATRIC'
    call gmatr1(nnoff, ndeg, abscur, xl, matr)

    do  i = 1, ndeg+1
            gthi(i)=zr(iadrgk-1+(i-1)*8+1)
            g1th(i)=zr(iadrgk-1+(i-1)*8+2)
            g2th(i)=zr(iadrgk-1+(i-1)*8+3)
            g3th(i)=zr(iadrgk-1+(i-1)*8+4)
            k1th(i)=zr(iadrgk-1+(i-1)*8+5)
            k2th(i)=zr(iadrgk-1+(i-1)*8+6)
            k3th(i)=zr(iadrgk-1+(i-1)*8+7)
    end do

!   SYSTEME LINEAIRE:  MATR*GS = GTHI
    call gsyste(matr, ndeg+1, ndeg+1, gthi, gi)

!   SYSTEME LINEAIRE:  MATR*K1S = K1TH
    call gsyste(matr, ndeg+1, ndeg+1, k1th, k1i)

!   SYSTEME LINEAIRE:  MATR*K2S = K2TH
    call gsyste(matr, ndeg+1, ndeg+1, k2th, k2i)

!   SYSTEME LINEAIRE:  MATR*K3S = K3TH
    call gsyste(matr, ndeg+1, ndeg+1, k3th, k3i)

!   SYSTEMES LINEAIRES POUR G_IRWIN
    call gsyste(matr, ndeg+1, ndeg+1, g1th, g1i)
    call gsyste(matr, ndeg+1, ndeg+1, g2th, g2i)
    call gsyste(matr, ndeg+1, ndeg+1, g3th, g3i)


!   VALEURS DU MODULE DU CHAMP THETA POUR LES NOEUDS DU FOND DE FISS
    call wkvect('&&METHO1.THETA', 'V V R8', (ndeg+1)*nnoff, iadrt3)

    call glegen(ndeg, nnoff, xl, abscur, zr(iadrt3))

!   VALEURS DE G(S)
    do i = 1, nnoff

        do k = 1, 5
            som(k) = 0.d0
        enddo

        do k = 1, 3
            gir(k) = 0.d0
        enddo

        do j = 1, ndeg+1
            som(1) = som(1) + gi(j)*zr(iadrt3+(j-1)*nnoff+i-1)
            som(2) = som(2) + k1i(j)*zr(iadrt3+(j-1)*nnoff+i-1)
            som(3) = som(3) + k2i(j)*zr(iadrt3+(j-1)*nnoff+i-1)
            som(4) = som(4) + k3i(j)*zr(iadrt3+(j-1)*nnoff+i-1)
            gir(1) = gir(1) + g1i(j)*zr(iadrt3+(j-1)*nnoff+i-1)
            gir(2) = gir(2) + g2i(j)*zr(iadrt3+(j-1)*nnoff+i-1)
            gir(3) = gir(3) + g3i(j)*zr(iadrt3+(j-1)*nnoff+i-1)
        end do

        gs(i) = som(1)
        k1s(i) = som(2)
        k2s(i) = som(3)
        k3s(i) = som(4)
        som(5) = gir(1)*gir(1) + gir(2)*gir(2) + gir(3)*gir(3)
        gis(i) = som(5)

        do k = 1, 5
            zr(iadgks-1+(i-1)*6+k) = som(k)
        enddo

!       CALCUL DES ANGLES DE PROPAGATION DE FISSURE LOCAUX BETA
        if (abs(zr(iadgks-1+(i-1)*6+3)) .ge. 1.e-12 ) &
            zr(iadgks-1+(i-1)*6+6)= 2.0d0*atan2(0.25d0*(zr(iadgks-1+(i-1)*6+2)/zr(iadgks-1+ &
                                    (i-1) *6+3) - sign(1.0d0, zr(iadgks-1+(i-1)*6+3))* &
                                    sqrt((zr(iadgks-1+ (i-1)*6+2)/ zr(iadgks-1+(i-1)*6+3))**2.0d0 &
                                    +8.0d0)), 1.0d0)

    end do

!   VALEURS DE GI, K1I, K2I, K3I (ON RECOPIE SIMPLEMENT GKTHI)
    do i = 1, (ndeg+1)
        i1 = i-1
        zr(iadgki-1+i1*5+1)   = zr(iadrgk-1+(i-1)*8+1)
        zr(iadgki-1+i1*5+1+1) = zr(iadrgk-1+(i-1)*8+5)
        zr(iadgki-1+i1*5+2+1) = zr(iadrgk-1+(i-1)*8+6)
        zr(iadgki-1+i1*5+3+1) = zr(iadrgk-1+(i-1)*8+7)
    enddo


   call jedetr('&&METHO1.THETA')
   call detrsd('CHAMP_GD', '&&GMETH1.G2')
   call jedetr('&&METHO1.MATRIC')

   call jedema()

end subroutine
