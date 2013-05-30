subroutine wp2biy(lm, lc, lk, s2, dsr,&
                  isi, yh, yb, zh, zb,&
                  lbloq, u1, u2, u3, u4,&
                  n)
    implicit none
    include 'asterfort/mrmult.h'
    real(kind=8) :: u1(*), u2(*), u3(*), u4(*), yh(*), yb(*), zh(*), zb(*)
    real(kind=8) :: s2, dsr, isi
    integer :: lm, lc, lk, n, lbloq(*)
!     ------------------------------------------------------------------
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
!                    T               T
!     CALCUL (ZH  ZB)   = B * (YH YB)
!     OU B EST L' OPERATEUR (REEL) DU PSEUDO PRODUIT SCALAIRE POUR
!     L' APPROCHE EN PARTIE IMAGINAIRE
!     ------------------------------------------------------------------
! IN  LM   : I : MATRICE DE MASSE
! IN  LC   : I : MATRICE D' AMORTISSEMENT
! IN  LK   : I : MATRICE DE RAIDEUR
! IN  DSR  : C : VALEUR DE 2*RE(SHIFT)
! IN  S2   : C : VALEUR DU CARRE DU MODULE DU SHIFT
! IN  ISI  : C : VALEUR DE 1/IM(SHIFT)
! IN  YH   : R : PARTIE SUPERIEUR DE Y
! IN  YB   : R : PARTIE INFERIEURE DE Y
! IN  N    : I : DIMENSION DES MATRICES
! IN  LBLOQ  : I : TYPE DES DDL (LBOLOQ(I) = 0 <=> DDL(I) = BLOQUE)
! OUT ZH   : R : PARTIE SUPERIEURE DU RESULTAT
! OUT ZB   : R : PARTIE INFERIEURE DU RESULTAT
! VAR U1   : R : VECTEUR DE TRAVAIL
! VAR U2   : R : VECTEUR DE TRAVAIL
! VAR U3   : R : VECTEUR DE TRAVAIL
! VAR U4   : R : VECTEUR DE TRAVAIL
!     ------------------------------------------------------------------
    integer :: i
    real(kind=8) :: zero
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    zero = 0.0d0
!
    call mrmult('ZERO', lk, yh, u1, 1,&
                .false.)
    call mrmult('ZERO', lc, yh, u2, 1,&
                .false.)
    call mrmult('ZERO', lm, yb, u3, 1,&
                .false.)
    call mrmult('ZERO', lm, yh, u4, 1,&
                .false.)
!
    if (dsr .ne. zero) then
!        --- PARTIE REELLE DU DECALLAGE NON NULLE ---
        do 10, i = 1, n, 1
        zh(i) = -dsr*u1(i) - s2*(u2(i) + u3(i))
        zb(i) = dsr*u3(i) + (- s2* u4(i) + u1(i))*lbloq(i)
10      continue
    else
!        --- PARTIE REELLE DU DECALLAGE NULLE ---
        do 11, i = 1, n, 1
        zh(i) = -s2*(u2(i) + u3(i))
        zb(i) = (-s2* u4(i) + u1(i))*lbloq(i)
11      continue
    endif
!
    call mrmult('CUMU', lk, yb, zh, 1,&
                .false.)
    call mrmult('CUMU', lc, yb, zb, 1,&
                .false.)
!
    do 20, i = 1, n, 1
    zh(i) = isi*zh(i)
    zb(i) = isi*zb(i)
    20 end do
!
end subroutine
