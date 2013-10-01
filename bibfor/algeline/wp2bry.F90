subroutine wp2bry(ldrf, lmasse, lamor, lraide, sr,&
                  si2, yh, yb, zh, zb,&
                  u1, u2, u3, u4, n,&
                  solveu)
    implicit none
#include "jeveux.h"
#include "asterfort/mrmult.h"
#include "asterfort/resoud.h"
!
    real(kind=8) :: u1(*), u2(*), u3(*), u4(*), yh(*), yb(*), zh(*), zb(*), sr
    real(kind=8) :: si2
    integer :: ldrf, lmasse, lamor, lraide, n
    character(len=19) :: solveu
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
!
!     OU B EST L' OPERATEUR (REEL) DU PSEUDO PRODUIT SCALAIRE POUR
!     L' APPROCHE EN PARTIE REELLE
!     ------------------------------------------------------------------
! IN  LDRF : I : FACTORISEE LDLT (DANS R) DE LA MATRICE DYNAMIQUE DE SR
! IN  LMASSE : I : MATRICE DE MASSE
! IN  LAMOR  : I : MATRICE D'AMORTISSEMENT
! IN  LRAIDE : I : MATRICE DE RAIDEUR
! IN  SR   : C : VALEUR DE LA PARTIE REELLE DU SHIFT
! IN  SI2  : C : VALEUR DU CARRE DE LA PARTIE IMAGINAIRE DU SHIFT
! IN  YH   : R : PARTIE SUPERIEUR DE Y
! IN  YB   : R : PARTIE INFERIEURE DE Y
! IN  N    : I : DIMENSION DES MATRICES
! OUT ZH   : R : PARTIE SUPERIEURE DU RESULTAT
! OUT ZB   : R : PARTIE INFERIEURE DU RESULTAT
! VAR U1   : R : VECTEUR DE TRAVAIL, EN SORTIE VAUT C*YH
! VAR U2   : R : VECTEUR DE TRAVAIL, EN SORTIE VAUT M*YB
! VAR U3   : R : VECTEUR DE TRAVAIL, EN SORTIE VAUT M*YH
! VAR U4   : R : VECTEUR DE TRAVAIL
! IN  SOLVEU : K19: SD SOLVEUR POUR PARAMETRER LE SOLVEUR LINEAIRE
!     ------------------------------------------------------------------
!
!
    integer :: i
    real(kind=8) :: zero
    complex(kind=8) :: cbid
    character(len=1) :: kbid
    character(len=19) :: k19bid, matass, chcine, criter
    integer :: iret
    cbid = dcmplx(0.d0, 0.d0)
!     -----------------------------------------------------------------
! INIT. OBJETS ASTER
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    matass=zk24(zi(ldrf+1))
    chcine=' '
    criter=' '
    k19bid=' '
    zero = 0.0d0
    if (si2 .eq. zero) then
!        --- DECALAGE REEL ---
        if (sr .eq. zero) then
!           --- DECALAGE NUL ---
            call mrmult('ZERO', lraide, yh, zh, 1,&
                        .false.)
            call mrmult('ZERO', lmasse, yb, zb, 1,&
                        .false.)
            do i = 1, n, 1
                zb(i) = -zb(i)
            end do
!
        else
!           --- DECALAGE NON NUL ---
            call mrmult('ZERO', lamor, yh, u1, 1,&
                        .false.)
            call mrmult('ZERO', lmasse, yb, u3, 1,&
                        .false.)
            call mrmult('ZERO', lmasse, yh, u2, 1,&
                        .false.)
            call mrmult('ZERO', lraide, yh, u4, 1,&
                        .false.)
            do i = 1, n, 1
                zh(i) = u4(i) + sr*(u1(i) + u3(i))
                zb(i) = -u3(i) + sr* u2(i)
            end do
        endif
!
    else
!        --- DECALAGE COMPLEXE ---
        call mrmult('ZERO', lamor, yh, u1, 1,&
                    .false.)
        call mrmult('ZERO', lmasse, yb, u2, 1,&
                    .false.)
        call mrmult('ZERO', lmasse, yh, u3, 1,&
                    .false.)
        do i = 1, n, 1
            u4(i) = u1(i) + sr*u3(i) + u2(i)
        end do
!
        call resoud(matass, k19bid, solveu, chcine, 1,&
                    k19bid, k19bid, kbid, u4, [cbid],&
                    criter, .false., 0, iret)
        call mrmult('ZERO', lamor, u4, zh, 1,&
                    .false.)
        call mrmult('ZERO', lmasse, u4, zb, 1,&
                    .false.)
!
        do i = 1, n, 1
            zh(i) = si2*(zh(i) - u3(i) + sr*zb(i)) + sr*(u1(i) + u2(i) )
            zb(i) = si2* zb(i) + sr*u3(i) - u2(i)
        end do
        call mrmult('CUMU', lraide, yh, zh, 1,&
                    .false.)
    endif
!
end subroutine
