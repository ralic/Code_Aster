subroutine chrgd(nbcmp, jcesd, jcesl, jcesv, imai,&
                 ipt, isp, type_gd, rc, p,&
                 permvec)
! ======================================================================
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
! ======================================================================
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/tpsivp.h" 
#include "asterfort/cesexi.h"
#include "blas/dgemv.h"
!
    integer, intent(in) :: nbcmp, jcesd, jcesl, jcesv, imai, ipt
    integer, intent(in) :: isp
    character(len=*), intent(in) :: type_gd
    character, intent(in) :: rc
    real(kind=8), dimension(:, :), intent(in) :: p
    integer, dimension(:), intent(in), optional :: permvec
! ----------------------------------------------------------------------
!
!     BUT : CHANGEMENT DE REPERE D'UNE GRANDEUR REELLE EN UN SOUS-POINT 
!           D'UN CHAM_ELEM SIMPLE 
! ----------------------------------------------------------------------
!     ARGUMENTS :
!     JCESD, JCESL IN I : ADRESSES DES OBJETS .CESD ET .CESL
!     IMAI     IN  I    : NUMERO DE LA MAILLE 
!     IPT, ISP IN  I    : NUMERO DU POINT ET DU SOUS-POINT
!     NBCMP    IN  I    : NOMBRE DE COMPOSANTES A TRAITER
!     TYPE_GD  IN  K16  : TYPE DU CHAMP :'TENS_3D' 'TENS2D' 'VECT_3D' 'VECT_2D'
!     RC       IN  K1   : REEL OU COMPLEXE 
!     P (3,3)  IN  R    : MATRICE DE PASSAGE 
!     PERMVEC  IN  I    : VECTEUR DE PERMUTATION (POUR DIM 2 & CHANGEMENT CYLINDRIQUE)
! ---------------------------------------------------------------------
!
    integer :: ii, iad
    real(kind=8), dimension(6) :: val1, val1r, val1i
    real(kind=8), dimension(6) :: val, valr, vali
    integer, dimension(6) :: permvec_loc
!
! Lecture des composantes du champ (vecteur ou tenseur) au sous-point courant
! Si tenseur réel ou vecteur réel : lecture dans val1
! Si tenseur complexe : lecture dans val1r et val1i 
! On utilise un vecteur de longueur fixe (6). Les composantes effectivement lues sont 
! complétées par 0. On ne distingue pas dim=2 et dim=3 
!
    val(:) = 0.d0
    valr(:)= 0.d0
    vali(:)= 0.d0
    val1(:) = 0.d0
    val1r(:)= 0.d0
    val1i(:)= 0.d0
    
! Vecteur (optionnel) permettant de permuter les composantes (du tenseur ou du vecteur)
! après application du changement de repère.  Cette possibilité est utilisée pour le 
! changement de repère vers un repère cylindrique dans les éléments de milieu continu. 
    if (present(permvec)) then
        permvec_loc(:)=permvec(:) 
    else
        permvec_loc(:)=(/(ii, ii=1,6)/)
    endif 
!
    do ii = 1, nbcmp
        call cesexi('C', jcesd, jcesl, imai, ipt,&
                    isp, ii, iad)
        if (iad .gt. 0) then
            select case (rc)
                case ('R') 
                val1(ii)=zr(jcesv-1+iad)
                case ('C')
                val1r(ii)=real(zc(jcesv-1+iad))
                val1i(ii)=imag(zc(jcesv-1+iad))
            case default
                ASSERT(.false.)
            end select 
        endif
    end do
! Changement de base
    select case( type_gd(1:7) )
    case ( 'TENS_3D', 'TENS_2D' ) 
!  Sigma <- P^T Sigma P
    select case (rc)
        case('R')
        val(:)=val1(:)
        call tpsivp(p, val)
        case('C')
        valr(:)=val1r(:)
        vali(:)=val1i(:)
        call tpsivp(p, valr)
        call tpsivp(p, vali)
    end select 
    case( 'VECT_3D', 'VECT_2D' ) 
! val = P^T val1 
    select case (rc)
        case('R')
        call dgemv(trans='T', m=3, n=3, alpha=1.d0, a=p,&
                   lda=3, x=val1, incx=1, beta=0.d0, y=val,&
                   incy=1)
        case('C')
        call dgemv(trans='T', m=3, n=3, alpha=1.d0, a=p,&
                   lda=3, x=val1r, incx=1, beta=0.d0, y=valr,&
                   incy=1)
        call dgemv(trans='T', m=3, n=3, alpha=1.d0, a=p,&
                   lda=3, x=val1i, incx=1, beta=0.d0, y=vali,&
                   incy=1)
    end select 
case default
    ASSERT(.false.)
end select 
! Copie des composantes modifiées dans le champ
    do ii = 1, nbcmp
        call cesexi('C', jcesd, jcesl, imai, ipt,&
                    isp, ii, iad)
        if (iad .gt. 0) then
            select case (rc)
                case('R')        
                    zr(jcesv-1+iad) = val(permvec_loc(ii))
                case('C')
                    zc(jcesv-1+iad) = cmplx( valr(permvec_loc(ii)), vali(permvec_loc(ii)))
            end select 
        endif
    end do
!
end subroutine chrgd
