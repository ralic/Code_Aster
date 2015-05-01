subroutine ptkg00(sf, a1, a2, xiz, xiz2,&
                  xiy, xiy2, xl, ey, ez,&
                  dsm)
    implicit none
    real(kind=8) :: a1, a2, xiz, xiz2, xiy, xiy2, xl, ey, ez
    real(kind=8) :: sf(*), dsm(*)
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     CALCUL DE LA MATRICE DE RAIDEUR GEOMETRIQUE  (POUTRE)
!     ------------------------------------------------------------------
! IN  SF     - (12) COMPOSANTES EFFORTS STATIQUES DANS LES ELEMENTS
! IN  A1         - AIRE DE LA SECTION DROITE INITIALE
! IN  A2         - AIRE DE LA SECTION DROITE FINALE
! IN  XIZ        - MOMENT D INERTIE / Z PRINCIPAL  SECTION INITIALE
! IN  XIZ2       - MOMENT D INERTIE / Z PRINCIPAL  SECTION FINALE
! IN  XIY        - MOMENT D INERTIE / Y PRINCIPAL  SECTION INITIALE
! IN  XIY2       - MOMENT D INERTIE / Y PRINCIPAL  SECTION FINALE
! IN  XL         - LONGUEUR DE L ELEMENT
! IN  EY         - COMPOSANTE SUIVANT Y PRINCIPAL DE GT.
! IN  EZ         - COMPOSANTE SUIVANT Z PRINCIPAL DE GT.
! OUT DSM    - (78) MATRICE DE RIGIDITE GEOMETRIQUE
!     ------------------------------------------------------------------
    real(kind=8) :: z1, un2, deux, six, quinze, trente
    real(kind=8) :: fx, a, xia1, xia2, xia, my1, my2, mz1, mz2
    integer :: ip(12)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    data             ip/ 0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66 /
!     ------------------------------------------------------------------
    z1 = 0.1d0
    un2 = 1.2d0
    deux = 2.d0
    six = 6.d0
    quinze = 15.d0
    trente = 30.d0
!
    my1=sf(5)
    mz1=sf(6)
    my2=sf(11)
    mz2=sf(12)
!
!     --- TRIANGULAIRE SUPERIEURE DE LA MATRICE ---
!
    fx = (sf(7)+sf(1))/deux
    a = a1+a2
    xia1 = (xiz+xiz2)/a
    xia2 = (xiy+xiy2)/a
    xia = xia1+xia2
    dsm(2+ip(2)) = un2*fx/xl
    dsm(2+ip(4)) = -sf(5)/xl
    dsm(2+ip(6)) = z1*fx
    dsm(2+ip(8)) = -dsm(2+ip(2))
    dsm(2+ip(10)) = sf(11)/xl
    dsm(2+ip(12)) = dsm(2+ip(6))
    dsm(3+ip(3)) = dsm(2+ip(2))
    dsm(3+ip(4)) = -sf(6)/xl
    dsm(3+ip(5)) = -dsm(2+ip(6))
    dsm(3+ip(9)) = -dsm(2+ip(2))
    dsm(3+ip(10)) = sf(12)/xl
    dsm(3+ip(11)) = dsm(3+ip(5))
    dsm(4+ip(4)) = xia*fx/xl
    dsm(4+ip(5)) = -xl*sf(8)/six
    dsm(4+ip(6)) = -xl*sf(9)/six
    dsm(4+ip(8)) = -dsm(2+ip(4))
    dsm(4+ip(9)) = -dsm(3+ip(4))
    dsm(4+ip(10)) = -dsm(4+ip(4))
    dsm(4+ip(11)) = -dsm(4+ip(5))
    dsm(4+ip(12)) = -dsm(4+ip(6))
    dsm(5+ip(5)) = deux*xl*fx/quinze
    dsm(5+ip(9)) = dsm(2+ip(6))
    dsm(5+ip(10)) = dsm(4+ip(11))
    dsm(5+ip(11)) = -xl*fx/trente
    dsm(6+ip(6)) = dsm(5+ip(5))
    dsm(6+ip(8)) = dsm(3+ip(5))
    dsm(6+ip(10)) = dsm(4+ip(12))
    dsm(6+ip(12)) = dsm(5+ip(11))
    dsm(8+ip(8)) = dsm(2+ip(2))
    dsm(8+ip(10)) = -dsm(2+ip(10))
    dsm(8+ip(12)) = dsm(3+ip(5))
    dsm(9+ip(9)) = dsm(2+ip(2))
    dsm(9+ip(10)) = -dsm(3+ip(10))
    dsm(9+ip(11)) = dsm(2+ip(6))
    dsm(10+ip(10)) = dsm(4+ip(4))
    dsm(10+ip(11)) = dsm(4+ip(5))
    dsm(10+ip(12)) = dsm(4+ip(6))
    dsm(11+ip(11)) = dsm(5+ip(5))
    dsm(12+ip(12)) = dsm(5+ip(5))
!
!     TERMES DUS AU ROTATIONS MODEREES
!
    dsm(4 +ip(5 )) = dsm(4 +ip(5 )) + mz1/deux
    dsm(10+ip(11)) = dsm(10+ip(11)) - mz2/deux
!
    dsm(4 +ip(6 )) = dsm(4 +ip(6 )) - my1/deux
    dsm(10+ip(12)) = dsm(10+ip(12)) + my2/deux
!
!     TERMES DUS A L'EXCENTRICITE DU CENTRE DE TORSION
!
    dsm(2 +ip(4 )) = dsm(2 +ip(4 )) + fx * ez / xl
    dsm(2 +ip(10)) = dsm(2 +ip(10)) - fx * ez / xl
    dsm(3 +ip(4 )) = dsm(3 +ip(4 )) - fx * ey / xl
    dsm(3 +ip(10)) = dsm(3 +ip(10)) + fx * ey / xl
    dsm(4 +ip(8 )) = dsm(4 +ip(8 )) - fx * ez / xl
    dsm(4 +ip(9 )) = dsm(4 +ip(9 )) + fx * ey / xl
    dsm(8 +ip(10)) = dsm(8 +ip(10)) + fx * ez / xl
    dsm(9 +ip(10)) = dsm(9 +ip(10)) - fx * ey / xl
!
    dsm(4 +ip(4 )) = dsm(4 +ip(4 ) )+deux*mz1*ey/xl+fx*ey*ey/xl -deux*my1*ez/xl+fx*ez*ez/xl
!    &                               -DEUX*MY2*EZ/XL+FX*EZ*EZ/XL
    dsm(4 +ip(10)) = dsm(4 +ip(10))-(mz1+mz2)*ey/xl-fx*ey*ey/xl +(my1+my2 )*ez/xl-fx*ez*ez/xl
    dsm(10+ip(10)) = dsm(10+ip(10) )+deux*mz2*ey/xl+fx*ey*ey/xl -deux*my2*ez/xl+fx*ez*ez/xl
!    &                               -DEUX*MY1*EZ/XL+FX*EZ*EZ/XL
!
!     IL FAUT BIEN REPASSER DANS LE REPERE G,X,Y,Z
!        TERME INDUIT PAR L'EXCENTRICITE
!
    dsm(ip(4)+4)=dsm(ip(4)+4)+ez*ez*dsm(ip(2)+2)+ey*ey*dsm(ip(3)+3)&
     &             -deux*ez*dsm(ip(4)+2) + deux*ey*dsm(ip(4)+3)
    dsm(ip(4)+2) = dsm(ip(4)+2) - ez*dsm(ip(2)+2)
    dsm(ip(4)+3) = dsm(ip(4)+3) + ey*dsm(ip(3)+3)
    dsm(ip(5)+4) = dsm(ip(5)+4) + ey*dsm(ip(5)+3)
    dsm(ip(6)+4) = dsm(ip(6)+4) - ez*dsm(ip(6)+2)
!
    dsm(ip(10)+10)=dsm(ip(10)+10)&
     &              +ez*ez*dsm(ip(8)+8)+ey*ey*dsm(ip(9)+9)&
     &             -deux*ez*dsm(ip(10)+8) + deux*ey*dsm(ip(10)+9)
    dsm(ip(10)+8) = dsm(ip(10)+8) - ez*dsm(ip(8)+8)
    dsm(ip(10)+9) = dsm(ip(10)+9) + ey*dsm(ip(9)+9)
    dsm(ip(11)+10) = dsm(ip(11)+10) + ey*dsm(ip(11)+9)
    dsm(ip(12)+10) = dsm(ip(12)+10) - ez*dsm(ip(12)+8)
!
    dsm(ip(10)+4)=dsm(ip(10)+4)&
     &              +ez*ez*dsm(ip(8)+2)+ey*ey*dsm(ip(9)+3)&
     &             - ez*dsm(ip(10)+2) + ey*dsm(ip(10)+3)&
     &             - ez*dsm(ip(8)+4)  + ey*dsm(ip(9)+4)
!
    dsm(ip(8 )+ 4) = dsm(ip(8 )+ 4) - ez*dsm(ip(8 )+2 )
    dsm(ip(9 )+ 4) = dsm(ip(9 )+ 4) + ey*dsm(ip(9 )+3 )
    dsm(ip(10)+ 2) = dsm(ip(10)+ 2) - ez*dsm(ip(8 )+2 )
    dsm(ip(10)+ 3) = dsm(ip(10)+ 3) + ey*dsm(ip(9 )+3 )
!
    dsm(ip(10)+ 5) = dsm(ip(10)+ 5) + ey*dsm(ip(9 )+5 )
    dsm(ip(10)+ 6) = dsm(ip(10)+ 6) - ez*dsm(ip(8 )+6 )
    dsm(ip(11)+ 4) = dsm(ip(11)+ 4) + ey*dsm(ip(11)+3 )
    dsm(ip(12)+ 4) = dsm(ip(12)+ 4) - ez*dsm(ip(12)+2 )
!
end subroutine
