subroutine dxqlocdri1(gmemb, matloc)
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
#include "jeveux.h"
#include "asterf_types.h"
    real(kind=8) :: gmemb(*)
    real(kind=8) :: matloc(*)
!----------------------------------------------------------
!     IN  GMEMB  : MATRICE DE MEMBRANE (DRILLING) CARREE
!     OUT MATLOC : MATRICE DE RIGIDITE OU DE MASSE LOCALE
!                  REMPLISSAGE DE MATELEM LOCAL (300 TERMES) AVEC
!                  LES TERMES DE MEMBRANE DRILLING DRZ
!----------------------------------------------------------
!
    integer :: im(10), jm(10)
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: k
!-----------------------------------------------------------------------
!     ------------------------------------------------------------------
    data jm   /&
     &   21 ,  72 ,  78 , 159 , 165 , 171 , 282 , 288 , 294 , 300 /
    data im   /&
     &    1 ,   5 ,   6 ,   9 ,  10 ,  11 ,  13 ,  14 ,  15 ,  16 /

!     ------------------------------------------------------------------


!                          ---- TERMES DE MEMBRANE (DRILLING-DIAGONAL)

    do k = 1, 10
!        write (6,*) "gmemb", " indice ",jm(k) , " ", gmemb(im(k))
        matloc(jm(k)) = matloc(jm(k)) + gmemb(im(k))
    end do


end subroutine
