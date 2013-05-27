subroutine wpordc(type, shift, vp, x, m,&
                  neq)
    implicit none
    include 'asterfort/u2mess.h'
    integer :: type, neq, m
    complex(kind=8) :: x(neq, m), shift, vp(*)
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
!     TRI DES VALEURS (ET DES VECTEURS) PROPRES COMPLEXES
!     DEUX TYPE DE TRI :
!          - TRI DANS LE SPECTRE : SUIVANT ABS(SHIFT - VPQ)
!          - TRI DE PRESNTATION  : SUIVANT IM(VPQ) - IM(SHIFT)
!     ------------------------------------------------------------------
! IN  TYPE   : IS : TYPE DU TRI PAR ORDRE CROISSANT SUR LES VALEURS.
!                   * SI TYPE = 0  TRI DE PRESENTATION
!                   * SI TYPE = 1  TRI DANS LE SPECTRE
! IN  M      : IS : NOMBRE DE VALEUR PROPRE
! IN  SHIFT  : C8 : DECALAGE SPECTRAL
! VAR VP     : C8 : TABLEAU DES DES VALEURS PROPRES
! VAR X      : C8 : MATRICE DES VECTEURS PROPRES
! IN  NEQ    : IS : NOMBRE D'EQUATIONS
!                 SI NEQ < NBPRO ALORS ON NE TRIE PAS DE VECTEURS
!     ------------------------------------------------------------------
    integer :: i, j, k
    real(kind=8) :: p, om
    complex(kind=8) :: c, q
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    om = dimag(shift)
    if (type .eq. 0) then
        do 100, i = 1, m, 1
        k = i
        p = dimag(vp(i)) - om
        do 110, j = i+1, m
        if ((dimag(vp(j))-om) .lt. p) then
            p = dimag(vp(j)) - om
            k = j
        endif
110      continue
        if (k .ne. i) then
            q=vp(i)
            vp(i)=vp(k)
            vp(k)=q
            do 120, j = 1, neq, 1
            c = x(j,i)
            x(j,i) = x(j,k)
            x(j,k) = c
120          continue
        endif
100      continue
    else if (type .eq. 1) then
        do 200, i = 1, m, 1
        k = i
        p = abs(vp(i) - shift)
        do 210, j = i+1, m
        if ((abs(vp(j)-shift)) .lt. p) then
            p = abs(vp(j) - shift)
            k = j
        endif
210      continue
        if (k .ne. i) then
            q=vp(i)
            vp(i)=vp(k)
            vp(k)=q
            do 220, j = 1, neq, 1
            c = x(j,i)
            x(j,i) = x(j,k)
            x(j,k) = c
220          continue
        endif
200      continue
    else
        call u2mess('F', 'ALGELINE3_97')
    endif
end subroutine
