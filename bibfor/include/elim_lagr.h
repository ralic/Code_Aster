#ifndef ELIM_LAGR_H
#define ELIM_LAGR_H
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!----------------------------------------------------------------
#include "asterf_petsc.h"
!
!----------------------------------------------------------------------
!  Common utilisé par la fonctionnalité ELIM_LAGR='OUI'
!
!     -- Le type mat_elim_lagr regroupe les matrices et vecteurs PETSC
!        nécessaires pour ELIM_LAGR='OUI' (pour une matrice Aster).
type mat_elim_lagr
sequence
Mat :: kproj
Mat :: ctrans
Mat :: tfinal
Mat :: rct
Mat :: matb
Vec :: vx0
Vec :: vecb
Vec :: vecc
integer*4, dimension(:), pointer :: indred
end type
!
!     -- on prévoit de pouvoir utiliser simultanément ELIM_LAGR='OUI'
!        avec 5 matrices Aster différentes.
!     MELIM(5) stocke les 5 (éventuels) mat_elim_lagr
!     NOMELIM(5,3) stocke les triplets de noms des matrices
!                  Aster liées aux mat_elem_lagr.
!     NOMELIM(i,1) : nom de la matrice Aster "complète" (avec Lagranges)
!     NOMELIM(i,2) : nom de la matrice Aster "réduite" (sans Lagranges)
!     NOMELIM(i,3) : nom de la matrice Aster "réduite" de rigidité
!                    (voir remarque ci-dessous)
!
!     KE est l'indice à utiliser dans MELIM et NOMELIM.
!        C'est la variable "sensible" que l'on doit "positionner" avec
!        beaucoup de soins.
!     Aujourd'hui, KE est positionné au début de preres.f et au début
!     de resoud.f via la routine elima0.F
!
!     Le "ménage" (désallocation des matrices PETSc) est fait en
!     appelant ELIMA0('EFFACE',...)
!
!
integer*4 :: ke
type (mat_elim_lagr) :: melim(5)
character*24 :: nomelim(5, 3)
!
common /elimlg/ nomelim,melim,ke
!----------------------------------------------------------------------
! Notations :
!-------------
! On peut résoudre le système dualisé suivant
! en "éliminant" les contraintes A*X=c.
!
!             ! B    A' !   (X)    (b)
!             !         ! *      =
!             ! A    0  !   (L)    (c)
!
!   1) on calcule T = noyau de A
!   2) on calcule X0 solution particulière de : A*X=c
!      X0 = A' * ( R'*R \ c )
!      où R est obtenu par :
!        Q,R = qr(A') ; R=R(1:n2,1:n2)
!        R est tel que :
!           A*A' = R'*R
!           R est triangulaire supérieure
!
!   3) On résoud le sytème "réduit" :
!       [T'*B*T]*Y = T'*(b - B*X0)
!   4) On peut alors calculer X=X0 + T*Y
!   5) On peut alors calculer L = (R'*R) \ A*(b - B*X)
!   6) La solution complète est : [X, L]
!
!   Dimensions des matrices et vecteurs :
!     n1 (= nphys) : nombre de ddls "physiques"
!     n2 (= nlag)  : nombre de contraintes de A
!                   (= nbre ddls "Lagrange 1" par exemple)
!         normalement :   n2 <= n1
!     n3 = n1-n2
!     B          : n1xn1
!     A          : n2xn1  (on suppose A de rang maximum)
!     T=ker(A)   : n1xn3  (si A est de rang maximum)
!     Kr=T'*B*T  : n3xn3
!     R          : n2xn2
!     X,b,X0     : n1
!     L,c        : n2
!     Fr=T'*(b - B*X0) : n3
!     Y          : n3
!----------------------------------------------------------------------
! Remarque très importante :
!  Certaines matrices (masse, amortissement, ...) ne contiennent pas A
!  Quand on veut les "réduire", il faut utiliser le A de la matrice de
!  rigidité associée.
!  Dans ce cas, on appelle elima1.F avec l'argument RIGI=MATRIG2
!  Dans apelim.F, on ne calcule pas les matrices Ctrans, Tfinal et RCt
!  mais on "pointe" sur celles de la matrice MATRIG2
!
!----------------------------------------------------------------------
! Remarque :
!  la factorisation R=qr(A') n'est utile que pour le calcul de x0 quand
!  c != 0 et pour le calcul des coefficents de Lagrange.
!  Dans le cadre des calculs modaux, cette factorisation est inutile.
!
!----------------------------------------------------------------------
! Correspondance entre les variables :
! ------------------------------------
! B      -> MatB
! A'     -> Ctrans
! T      -> Tfinal
! T'*B*T -> Kproj
! R      -> RCt
! X0     -> VX0
! b      -> VecB
! c      -> VecC
!
! Réalité :
! ---------
! * RCt est rectangle de dimensions (n1, n2)
!     RCt contient  R dans ses 1ères lignes.
!     RCt est prolongé par 0.
!----------------------------------------------------------------------
#endif
