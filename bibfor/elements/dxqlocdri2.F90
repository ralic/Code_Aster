subroutine dxqlocdri2(btgmemb, matloc)
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterf_types.h"
#include "jeveux.h"
    real(kind=8) :: btgmemb(*)
    real(kind=8) :: matloc(*)
!----------------------------------------------------------
!     IN  GMEMB  : MATRICE DE MEMBRANE (DRILLING) CARREE
!     OUT MATLOC : MATRICE DE RIGIDITE OU DE MASSE LOCALE
!                  REMPLISSAGE DE MATELEM LOCAL (300 TERMES) AVEC
!                  LES TERMES DE MEMBRANE DRILLING DRZ
!----------------------------------------------------------
!
!
    integer :: im(32), jm(32)
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: k
!-----------------------------------------------------------------------
!     ------------------------------------------------------------------

    data jm   /&
     &   16 ,  17 ,  27 ,  34 ,  67 ,  68 ,  73 ,  74 ,  84 ,  90 ,&
     &   97 , 103 , 154 , 155 , 160 , 161 , 166 , 167 , 177 , 183 ,&
     &  189 , 196 , 202 , 208 , 277 , 278 , 283 , 284 , 289 , 290 ,&
     &  295 , 296 /

    data im   /&
     &    1 ,   2 ,   3 ,   4 ,   9 ,  10 ,  11 ,  12 ,   5 ,  13 ,&
     &    6 ,  14 ,  17 ,  18 ,  19 ,  20 ,  21 ,  22 ,   7 ,  15 ,&
     &   23 ,   8 ,  16 ,  24 ,  25 ,  26 ,  27 ,  28 ,  29 ,  30 ,&
     &   31 ,  32 /


!     ------------------------------------------------------------------


!                       ---- TERMES DE MEMBRANE (DRILL OUT-OF-DIAGONAL)
    do k = 1, 32
        matloc(jm(k)) = matloc(jm(k)) + btgmemb(im(k))
    end do


end subroutine
