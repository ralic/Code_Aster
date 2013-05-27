subroutine utdtab(raz, na, nb, mb, md,&
                  a, b, d, xab, dtab)
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
    implicit none
    include 'asterfort/r8inir.h'
    integer :: na, mb, md
    character(len=*) :: raz
    real(kind=8) :: a(na, nb), b(nb, mb), d(na, md), xab(na, mb)
    real(kind=8) :: dtab(md, mb)
!     ------------------------------------------------------------------
!     PRODUIT DT . A . B  - A B ET D  RECTANGULAIRES
!     ------------------------------------------------------------------
!IN   K4  RAZ  'ZERO' : ON FAIT DTAB = 0    + DT*A.B
!              'CUMU' : ON FAIT DTAB = DTAB + DT*A.B
!IN   I   NA   NB DE LIGNES DE A
!IN   I   NB   NB DE COLONNES DE A
!IN   I   MB   NB DE COLONNES DE B
!IN   I   MD   NB DE COLONNES DE C
!IN   R   A    MATRICE A           (NA,NB)
!IN   R   B    MATRICE B           (NB,MB)
!IN   R   D    MATRICE D           (NA,MD)
!IN   R   XAB  ZONE DE TRAVAIL XAB (NA,MB)
!OUT  R   DTAB PRODUIT DT . A . B  (MD,MB)
!     ------------------------------------------------------------------
    character(len=4) :: raz2
! --DEB
!-----------------------------------------------------------------------
    integer :: i, j, k, nb
!-----------------------------------------------------------------------
    raz2=raz
!
    call r8inir(na*mb, 0.0d0, xab, 1)
    do 15 i = 1, na
        do 15 k = 1, nb
            do 15 j = 1, mb
                xab(i,j) = xab(i,j) + a(i,k) * b(k,j)
15          continue
!
    if (raz2 .eq. 'ZERO') call r8inir(md*mb, 0.0d0, dtab, 1)
!
    do 25 i = 1, md
        do 25 k = 1, na
            do 25 j = 1, mb
                dtab(i,j) = dtab(i,j) + d(k,i) * xab(k,j)
25          continue
end subroutine
