subroutine lcicma(a, la, ca, lc, cc,&
                  xa, ya, b, lb, cb,&
                  xb, yb)
    implicit none
!       ----------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!       ----------------------------------------------------------------
!       INCLUSION D UNE SOUS MATRICE C(LC,CC) SE TROUVANT
!       AU POINT (XA,YA) DE LA MATRICE  A(LA,CA)
!       DANS UNE MATRICE B(LB,CB) AU POINT (XB,YB)
!
!       IN  LA  CA  :  NB LIGNES ET COLONNES  DE A
!       IN  LC  CC  :  NB LIGNES ET COLONNES  DE C
!       IN  XA  YA  :  LIGNE , COLONNE DU POINT INCLUSION DE SA DANS A
!       IN  LB  CB  :  NB LIGNES ET COLONNES  DE B
!       IN  XB  YB  :  LIGNE , COLONNE DU POINT D INCLUSION DE SA DANS B
!       IN  A       :  MATRICE EMETTEUR
!       OUT B       :  MATRICE RECEPTEUR
!       ----------------------------------------------------------------
    integer :: la, ca, lc, cc, xa, ya, lb, cb, xb, yb, i, j
    real(kind=8) :: a(la, ca), b(lb, cb)
!
    do 1 i = 1, lc
        do 1 j = 1, cc
            b ( xb+i-1 , yb+j-1 ) = a ( xa+i-1 , ya+j-1 )
 1      continue
end subroutine
