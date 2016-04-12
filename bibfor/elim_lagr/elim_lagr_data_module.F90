module elim_lagr_data_module
!
!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: natacha.bereux at edf.fr
! aslint:disable=C1308
! 
!
use elim_lagr_context_class
use petsc_data_module 
use saddle_point_context_class
!
implicit none 
!
private 
#include "asterf.h"
#include "asterf_petsc.h"
#include "asterfort/apalmc.h"
#include "asterfort/apmamc.h"
#include "asterfort/apmain.h"
#include "asterfort/assert.h"
#include "asterfort/compress_sparse_pattern.h"
#include "asterfort/extract_nonzero_col.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/nullbasis.h"
#include "asterfort/utmess.h"
!
!----------------------------------------------------------------------
!  Données globales utilisées par la fonctionnalité ELIM_LAGR='OUI'
!
!     on prévoit de pouvoir utiliser simultanément ELIM_LAGR='OUI'
!     avec nmax_ctxt matrice(s) Aster différente(s).
integer, parameter :: nmax_ctxt = 5 
!     KE est l'indice à utiliser dans le tableau elg_ctxt(:)
!     C'est la variable "sensible" que l'on doit "positionner" avec
!     beaucoup de soins.
!     Aujourd'hui, KE est positionné au début de preres.f et au début
!     de resoud.f
integer(kind=4), public :: ke
!     Tableau d'objets de type elim_lagr_context_type. 
!     Chaque objet contient toutes les données nécessaires pour 
!     procéder à l'élimination des multiplicateurs de Lagrange. 
type(elim_lagr_context_type), public, dimension(nmax_ctxt), target :: elg_context
!

! Routine de gestion des données 
public :: elg_gest_data
!
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
contains 
!
!--------------------------------------------------------------
! BUT :
!   * Gestion des variables globales du module elim_lagr_data_module
!   * Positionner l'indice KE correspondant à une matrice Aster
!
! IN  : ACTION :
!        / 'NOTE' : pour "déclarer" une nouvelle matrice
!                   (appelée par exemple dans preres.f)
!        / 'CHERCHE' : pour positionner KE
!                   (appelée par exemple dans resoud.f)
!        / 'EFFACE' : pour effacer une matrice
!                   (appelée par exemple dans detrsd.f)
! IN  : MAT1 :   / nom de la SD_MATR_ASSE complète
!                / ' ' si action='EFFACE'
! IN  : MAT2  : nom de la SD_MATR_ASSE réduite
! IN  : RIGI1 : / nom de la SD_MATR_ASSE complète qui contient
!                 réellement les relations linéaires
!               / ' '
!               Cet argument ne sert que pour action='NOTE'
!---------------------------------------------------------------
subroutine elg_gest_data (action, mat1, mat2, rigi1)
!
!   Dummy arguments 
    character(len=*), intent(in)  :: action, mat1, mat2, rigi1
#ifdef _HAVE_PETSC 
!   Local variables 
    integer :: k, ktrou, iprem
!
    save iprem
    data iprem / 0 /
!----------------------------------------------------------------
    iprem=iprem+1
!
    ASSERT(action.eq.'NOTE' .or.action.eq.'CHERCHE'.or.action.eq.'EFFACE')
    if (action .ne. 'NOTE') then
        ASSERT(rigi1.eq.' ')
    endif
!
!
!     -- au 1er appel on initialise les données
!     -----------------------------------------
    if (iprem .eq. 1) then
        do k = 1, nmax_ctxt
            elg_context(k) = new_elim_lagr_context()
        enddo
        ke=0
    endif
!
    if (action .eq. 'NOTE') then
        ktrou=0
!       -- on cherche une place libre
        do k = 1, nmax_ctxt
            if (elg_context(k)%full_matas .eq. ' ') then
                ktrou=k
                goto 1
            endif
        enddo
  1     continue
        ASSERT(ktrou.gt.0)
        ke=ktrou
        elg_context(ke)%full_matas=mat1
        elg_context(ke)%reduced_matas=mat2
        elg_context(ke)%k_matas=rigi1
    endif
!
!
    if (action .eq. 'CHERCHE') then
        ktrou=0
        do k = 1, 5
            if (elg_context(k)%full_matas .eq. mat1) then
                ktrou=k
                goto 2
            endif
        enddo
        ASSERT(ktrou.gt.0)
        ASSERT(elg_context(ktrou)%reduced_matas.eq.mat2)
        ke=ktrou
  2     continue
    endif
!
!
    if (action .eq. 'EFFACE') then
        ASSERT(mat1.eq.' ')
        ktrou=0
        do k = 1, 5
            if (elg_context(k)%reduced_matas .eq. mat2) then
                ktrou=k
                goto 3
            endif
        enddo
  3     continue
        if (ktrou .eq. 0) goto 4
!
        call free_elim_lagr_context( elg_context(ktrou) )
  4     continue
    endif
!
#endif 
end subroutine elg_gest_data
!
!
end module elim_lagr_data_module
