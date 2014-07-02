subroutine gkmet4(nnoff, ndimte, chfond, pair, iadrgk,&
                  milieu, connex, iadgks, iadgki, abscur,&
                  num)
! aslint: disable=W1306
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/gsyste.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    integer :: nnoff, ndimte, iadrgk, iadgks, iadgki, num
    character(len=24) :: chfond, abscur
    aster_logical :: pair, milieu, connex
!
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
!      METHODE LAGRANGE_REGU POUR LE CALCUL DE G(S)
!      K1(S) K2(S) ET K3(S)
!
! ENTREE
!
!     NNOFF    --> NOMBRE DE NOEUDS DU FOND DE FISSURE
!     NDIMTE   --> NOMBRE de CHAMPS THETA CHOISIS
!     CHFOND   --> COORDONNEES ET ABSCISSES CURVILIGNES DES NOEUDS
!                  DU FOND DE FISSURE
!     IADRGK   --> ADRESSE DE VALEURS DE GKTHI
!                 (G, K1, K2, K3 POUR LES CHAMPS THETAI)
!     MILIEU   --> .TRUE.  : ELEMENT QUADRATIQUE
!                  .FALSE. : ELEMENT LINEAIRE
!     CONNEX   --> .TRUE.  : SI FOND FERME
!                  .FALSE. : SI FOND OUVERT
!
!  SORTIE
!
!   IADGKS     --> ADRESSE DE VALEURS DE GKS
!                   (VALEUR DE G(S), K1(S), K2(S), K3(S))
!   IADGKI     --> ADRESSE DE VALEURS DE GKTHI
!                  (G, K1, K2, K3 POUR LES CHAMPS THETAI)
!   ABSCUR     --> VALEURS DES ABSCISSES CURVILIGNES S
!      NUM     --> 5 (LAGRANGE_REGU)
!
    integer :: ifon, iadabs, imatr
    integer :: i, i1, kk, nump, nn, j
    real(kind=8) :: s1, s2, delta, s3
    real(kind=8) :: gthi(nnoff), k1th(nnoff), k2th(nnoff), k3th(nnoff)
    real(kind=8) :: gs(ndimte), k1s(ndimte), k2s(ndimte), k3s(ndimte)
    real(kind=8) :: betas(nnoff), gis(nnoff)
    real(kind=8) :: g1th(nnoff), g2th(nnoff), g3th(nnoff)
    real(kind=8) :: g1s(nnoff), g2s(nnoff), g3s(nnoff)
    character(len=24) :: matr
!
!
!
!
    call jemarq()
!
!
    call jeveuo(chfond, 'L', ifon)
    call jeveuo(abscur, 'E', iadabs)
    do 10 i = 1, nnoff
        zr(iadabs-1+(i-1)+1)=zr(ifon-1+4*(i-1)+4)
 10 end do
    do 11 i = 1, ndimte
        zr(iadabs-1+(i-1)+1)=zr(ifon-1+4*(i-1)+4)
        gthi(i)=zr(iadrgk-1+(i-1)*8+1)
        k1th(i)=zr(iadrgk-1+(i-1)*8+5)
        k2th(i)=zr(iadrgk-1+(i-1)*8+6)
        k3th(i)=zr(iadrgk-1+(i-1)*8+7)
        g1th(i)=zr(iadrgk-1+(i-1)*8+2)
        g2th(i)=zr(iadrgk-1+(i-1)*8+3)
        g3th(i)=zr(iadrgk-1+(i-1)*8+4)
 11 end do
!
    num = 5
!
    matr = '&&METHO3.MATRI'
!
    call wkvect(matr, 'V V R8', ndimte*ndimte, imatr)
!
    i1 = 2
    if (milieu) i1 = 4
!
    do 40 i = 1, ndimte-2
        nump = 2*i-1
        if (milieu) nump = 4*i-3
        s1 = zr(iadabs+nump-1)
        s2 = zr(iadabs+nump-1+i1)
        delta = (s2-s1)/6.d0
!
        kk = imatr+(i-1 )*ndimte+i-1
        zr(kk )= zr(kk) + 2.d0*delta
        zr(imatr+(i-1+1)*ndimte+i-1 )= 1.d0*delta
!
        zr(imatr+(i-1 )*ndimte+i-1+1)= 1.d0*delta
        zr(imatr+(i-1+1)*ndimte+i-1+1)= 2.d0*delta
!
 40 end do
    i = ndimte -1
    nump = 2*(i-1)
    if (pair) then
        s1 = zr(iadabs+nump-1)
        s2 = zr(iadabs+nump-1+i1/2)
        delta = (s2-s1)/6.d0
        kk = imatr+(i-1 )*ndimte+i-1
        zr(kk )= zr(kk) + 3.5d0*delta
        zr(imatr+(i-1+1)*ndimte+i-1 )= 1.d0*delta
        zr(imatr+(i-1 )*ndimte+i-1+1)= 1.d0*delta
        zr(imatr+(i-1+1)*ndimte+i-1+1)= 0.5d0*delta
    else
        s1 = zr(iadabs+nump)
        s2 = zr(iadabs+nump+i1)
        delta = (s2-s1)/6.d0
        kk = imatr+(i-1 )*ndimte+i-1
        zr(kk )= zr(kk) + 2.d0*delta
        zr(imatr+(i-1+1)*ndimte+i-1 )= 1.d0*delta
        zr(imatr+(i-1 )*ndimte+i-1+1)= 1.d0*delta
        zr(imatr+(i-1+1)*ndimte+i-1+1)= 2.d0*delta
    endif
!
    if (nnoff .eq. 2) then
        s1 = zr(iadabs+1-1)
        s2 = zr(iadabs+1-1+1)
        delta = (s2-s1)/6.d0
        zr(imatr + 0)= 3.5d0*delta
        zr(imatr + 1)= 1.d0*delta
        zr(imatr + 2)= 1.d0*delta
        zr(imatr + 3)= 0.5d0*delta
    endif
!
    if (connex) then
        zr(imatr) = 2.d0*zr(imatr)
        s1 = zr(iadabs-1+nump-i1+1)
        s2 = zr(iadabs-1+nump+1)
        delta = (s2-s1)/6.d0
        zr(imatr+(1-1)*ndimte+ndimte-1-1)= 1.d0*delta
        kk = imatr+(ndimte-1)*ndimte+ndimte-1
        zr(kk) = 2.d0*zr(kk)
        s1 = zr(iadabs-1+1)
        s2 = zr(iadabs-1+i1+1)
        delta = (s2-s1)/6.d0
        zr(imatr+(ndimte-1)*ndimte+2-1)= 1.d0*delta
    endif
!
!     SYSTEME LINEAIRE:  MATR*GS = GTHI
    call gsyste(matr, ndimte, ndimte, gthi, gs)
!
!     SYSTEME LINEAIRE:  MATR*K1S = K1TH
    call gsyste(matr, ndimte, ndimte, k1th, k1s)
!
!     SYSTEME LINEAIRE:  MATR*K2S = K2TH
    call gsyste(matr, ndimte, ndimte, k2th, k2s)
!
!     SYSTEME LINEAIRE:  MATR*K3S = K3TH
    call gsyste(matr, ndimte, ndimte, k3th, k3s)
!
!       SYSTEMES LINEAIRES POUR GIRWIN
    call gsyste(matr, ndimte, ndimte, g1th, g1s)
    call gsyste(matr, ndimte, ndimte, g2th, g2s)
    call gsyste(matr, ndimte, ndimte, g3th, g3s)
    do 50 i = 1, ndimte
        gis(i)=g1s(i)*g1s(i) + g2s(i)*g2s(i) +g3s(i)*g3s(i)
 50 end do
!
!     CALCUL DES ANGLES DE PROPAGATION DE FISSURE LOCAUX BETA
    do 80 i = 1, ndimte
        betas(i) = 0.0d0
        if (k2s(i) .ne. 0.d0) betas(i) = 2.0d0*atan2(&
                                         0.25d0*(&
                                         k1s(i)/k2s( i) -sign(1.0d0, k2s(i))*sqrt((k1s(i)/k2s(i))&
                                         &**2.0d0+8.0d0)&
                                         ),&
                                         1.0d0&
                                         )
 80 end do
!
    if (nnoff .eq. 2) then
        zr(iadgks-1+1)=gs(1)
        zr(iadgks-1+2)=k1s(1)
        zr(iadgks-1+3)=k2s(1)
        zr(iadgks-1+4)=k3s(1)
        zr(iadgks-1+5)=gis(1)
        zr(iadgks-1+6)=betas(1)
        zr(iadgks-1+(nnoff-1)*6+1)=gs(ndimte)
        zr(iadgks-1+(nnoff-1)*6+2)=k1s(ndimte)
        zr(iadgks-1+(nnoff-1)*6+3)=k2s(ndimte)
        zr(iadgks-1+(nnoff-1)*6+4)=k3s(ndimte)
        zr(iadgks-1+(nnoff-1)*6+5)=gis(ndimte)
        zr(iadgks-1+(nnoff-1)*6+6)=betas(ndimte)
!
    else
        do 60 i = 1, ndimte-1
            if (milieu) then
                nn = 4*i-3
                zr(iadgks-1+(nn-1)*6+1)=gs(i)
                zr(iadgks-1+(nn-1)*6+2)=k1s(i)
                zr(iadgks-1+(nn-1)*6+3)=k2s(i)
                zr(iadgks-1+(nn-1)*6+4)=k3s(i)
                zr(iadgks-1+(nn-1)*6+5)=gis(i)
                zr(iadgks-1+(nn-1)*6+6)=betas(i)
                s1 = zr(iadabs+nn-1)
                s3 = zr(iadabs+nn+4-1)
!
                zr(iadgks-1+(nn-1+1)*6+1)=gs(i)+(zr(iadabs+nn+1-1)-s1)&
                * (gs(i+1)-gs(i))/(s3-s1)
                zr(iadgks-1+(nn-1+2)*6+1)=gs(i)+(zr(iadabs+nn+2-1)-s1)&
                * (gs(i+1)-gs(i))/(s3-s1)
                zr(iadgks-1+(nn-1+3)*6+1)=gs(i)+(zr(iadabs+nn+3-1)-s1)&
                * (gs(i+1)-gs(i))/(s3-s1)
!
                zr(iadgks-1+(nn-1+1)*6+2)=k1s(i)+(zr(iadabs+nn+1-1)-&
                s1)* (k1s(i+1)-k1s(i))/(s3-s1)
                zr(iadgks-1+(nn-1+2)*6+2)=k1s(i)+(zr(iadabs+nn+2-1)-&
                s1)* (k1s(i+1)-k1s(i))/(s3-s1)
                zr(iadgks-1+(nn-1+3)*6+2)=k1s(i)+(zr(iadabs+nn+3-1)-&
                s1)* (k1s(i+1)-k1s(i))/(s3-s1)
!
!
                zr(iadgks-1+(nn-1+1)*6+3)=k2s(i)+(zr(iadabs+nn+1-1)-&
                s1)* (k2s(i+1)-k2s(i))/(s3-s1)
                zr(iadgks-1+(nn-1+2)*6+3)=k2s(i)+(zr(iadabs+nn+2-1)-&
                s1)* (k2s(i+1)-k2s(i))/(s3-s1)
                zr(iadgks-1+(nn-1+3)*6+3)=k2s(i)+(zr(iadabs+nn+3-1)-&
                s1)* (k2s(i+1)-k2s(i))/(s3-s1)
!
!
                zr(iadgks-1+(nn-1+1)*6+4)=k3s(i)+(zr(iadabs+nn+1-1)-&
                s1)* (k3s(i+1)-k3s(i))/(s3-s1)
                zr(iadgks-1+(nn-1+2)*6+4)=k3s(i)+(zr(iadabs+nn+2-1)-&
                s1)* (k3s(i+1)-k3s(i))/(s3-s1)
                zr(iadgks-1+(nn-1+3)*6+4)=k3s(i)+(zr(iadabs+nn+3-1)-&
                s1)* (k3s(i+1)-k3s(i))/(s3-s1)
!
!
                zr(iadgks-1+(nn-1+1)*6+5)=gis(i)+(zr(iadabs+nn+1-1)-&
                s1)* (gis(i+1)-gis(i))/(s3-s1)
                zr(iadgks-1+(nn-1+2)*6+5)=gis(i)+(zr(iadabs+nn+2-1)-&
                s1)* (gis(i+1)-gis(i))/(s3-s1)
                zr(iadgks-1+(nn-1+3)*6+5)=gis(i)+(zr(iadabs+nn+3-1)-&
                s1)* (gis(i+1)-gis(i))/(s3-s1)
!
!
                zr(iadgks-1+(nn-1+1)*6+6)=betas(i)+(zr(iadabs+nn+1-1)-&
                s1)* (betas(i+1)-betas(i))/(s3-s1)
                zr(iadgks-1+(nn-1+2)*6+6)=betas(i)+(zr(iadabs+nn+2-1)-&
                s1)* (betas(i+1)-betas(i))/(s3-s1)
                zr(iadgks-1+(nn-1+3)*6+6)=betas(i)+(zr(iadabs+nn+3-1)-&
                s1)* (betas(i+1)-betas(i))/(s3-s1)
            else
                nn = 2*i-1
                zr(iadgks-1+(nn-1)*6+1)=gs(i)
                zr(iadgks-1+(nn-1)*6+2)=k1s(i)
                zr(iadgks-1+(nn-1)*6+3)=k2s(i)
                zr(iadgks-1+(nn-1)*6+4)=k3s(i)
                zr(iadgks-1+(nn-1)*6+5)=gis(i)
                zr(iadgks-1+(nn-1)*6+6)=betas(i)
                s1 = zr(iadabs+nn-1)
                s2 = zr(iadabs+nn-1+1)
                s3 = zr(iadabs+nn-1+2)
!
                zr(iadgks-1+(nn-1+1)*6+1)=gs(i)+(s2-s1)* (gs(i+1)-gs(&
                i))/(s3-s1)
                zr(iadgks-1+(nn-1+1)*6+2)=k1s(i)+(s2-s1)* (k1s(i+1)-&
                k1s(i))/(s3-s1)
                zr(iadgks-1+(nn-1+1)*6+3)=k2s(i)+(s2-s1)* (k2s(i+1)-&
                k2s(i))/(s3-s1)
                zr(iadgks-1+(nn-1+1)*6+4)=k3s(i)+(s2-s1)* (k3s(i+1)-&
                k3s(i))/(s3-s1)
                zr(iadgks-1+(nn-1+1)*6+5)=gis(i)+(s2-s1)* (gis(i+1)-&
                gis(i))/(s3-s1)
                zr(iadgks-1+(nn-1+1)*6+6)=betas(i)+(s2-s1)* (betas(i+&
                1)-betas(i))/(s3-s1)
            endif
 60     continue
!
        if (pair) then
            nn=2*(ndimte-2)
            s1 = zr(iadabs+nn-1)
            s2 = zr(iadabs+nn-1+1)
            s3 = zr(iadabs+nn-1+2)
            zr(iadgks-1+(nnoff-1)*6+1)=zr(iadgks-1+(nnoff-2)*6+1)+ (&
            s3-s2)* (zr(iadgks-1+(nnoff-3)*6+1)-zr(iadgks-1+(nnoff-2)*&
            6+1))/(s1-s2)
            zr(iadgks-1+(nnoff-1)*6+2)=zr(iadgks-1+(nnoff-2)*6+2)+ (&
            s3-s2)* (zr(iadgks-1+(nnoff-3)*6+2)-zr(iadgks-1+(nnoff-2)*&
            6+2))/(s1-s2)
            zr(iadgks-1+(nnoff-1)*6+3)=zr(iadgks-1+(nnoff-2)*6+3)+ (&
            s3-s2)* (zr(iadgks-1+(nnoff-3)*6+3)-zr(iadgks-1+(nnoff-2)*&
            6+3))/(s1-s2)
            zr(iadgks-1+(nnoff-1)*6+4)=zr(iadgks-1+(nnoff-2)*6+4)+ (&
            s3-s2)* (zr(iadgks-1+(nnoff-3)*6+4)-zr(iadgks-1+(nnoff-2)*&
            6+4))/(s1-s2)
            zr(iadgks-1+(nnoff-1)*6+5)=zr(iadgks-1+(nnoff-2)*6+5)+ (&
            s3-s2)* (zr(iadgks-1+(nnoff-3)*6+5)-zr(iadgks-1+(nnoff-2)*&
            6+5))/(s1-s2)
            zr(iadgks-1+(nnoff-1)*6+6)=zr(iadgks-1+(nnoff-2)*6+6)+ (&
            s3-s2)* (zr(iadgks-1+(nnoff-3)*6+6)-zr(iadgks-1+(nnoff-2)*&
            6+6))/(s1-s2)
        else
            zr(iadgks-1+(nnoff-1)*6+1)=gs(ndimte)
            zr(iadgks-1+(nnoff-1)*6+2)=k1s(ndimte)
            zr(iadgks-1+(nnoff-1)*6+3)=k2s(ndimte)
            zr(iadgks-1+(nnoff-1)*6+4)=k3s(ndimte)
            zr(iadgks-1+(nnoff-1)*6+5)=gis(ndimte)
            zr(iadgks-1+(nnoff-1)*6+6)=betas(ndimte)
        endif
    endif
!
    do 70 i = 1, ndimte
        do 71 j = 1, 5
            zr(iadgki-1+5*(i-1)+j)=zr(iadrgk-1+8*(i-1)+j)
 71     continue
 70 end do
!
!
!
    call jedetr('&&METHO3.MATRI')
    call jedetr('&&METHO3.VECT')
!
    call jedema()
end subroutine
