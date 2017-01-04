subroutine dkqnim(shp, shpr1, shpr2, shpmem1, shpmem2, gm1, gm2)
    implicit  none
    real(kind=8) :: shp(3,4), shpr1(3,4), shpr2(3,4)
    real(kind=8) :: shpmem1(8), shpmem2(8), gm1(4), gm2(4)
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
!     ------------------------------------------------------------------
!     MATRICES DES FONCTIONS DE BASE POUR LE COMPOSANTES u1 et u2 
!       DU DEPLACEMENT DE LA MEMBRANE AU POINT QSI ETA POUR ELEMENTS DKQ 
!     ------------------------------------------------------------------
!     person_in_charge: ayaovi-dzifa.kudawoo at edf.fr


    integer :: j, j1
    

        j1 = 0
        
        do j = 1, 4
        
         shpmem1(1+j1) = shp(3,j)
         shpmem1(2+j1) = 0.d0

         shpmem2(1+j1) = 0.d0
         shpmem2(2+j1) = shp(3,j)

         j1 = j1+2
         
         gm1(j) = shpr1(3,j)
         gm2(j) = shpr2(3,j)

        end do
   
!
end subroutine
