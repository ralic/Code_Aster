subroutine dxqloe_NV( coupmf, matloc, depl, ener)
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
!
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/utvtsv.h"
#include "asterfort/dxqloe.h"
    aster_logical :: coupmf
    real(kind=8) :: depl(*), ener(*)
    real(kind=8) :: matloc(*)
!----------------------------------------------------------
!     CALCUL DE L'ENERGIE DE DEFORMATION OU CINETIQUE
!            SUR UNE MAILLE TRIANGLE
!     IN  FLEX   : MATRICE DE FLEXION CARREE
!     IN  MEMB   : MATRICE DE MEMBRANE CARREE
!     IN  MEFL   : MATRICE MEMBRANE - FLEXION CARREE
!     IN  CTOR   : COEFF DE TORSION
!-------------------------------------------------------------
!     IN  GMEMB  : MATRICE DE MEMBRANE (DRILLING) CARREE
!     IN  BTGMEMB: MATRICE DE MEMBRANE (DRILLING) - MEMBRANE DEPLACEMENTS
!     IN  GMEFL  : MATRICE MEMBRANE (DRILLING) - FLEXION
!-------------------------------------------------------------
!
!     IN  DEPL   : DEPLACEMENT DANS LE REPERE LOCAL
!     OUT ENER   : 3 TERMES POUR ENER_POT (EPOT_ELEM) OU
!                           POUR ENER_CIN (ECIN_ELEM)
!----------------------------------------------------------
    integer :: jf(78)
    integer :: k
    integer :: jm4(78)


    integer :: km(12), kf(12)
    real(kind=8) :: deplm(12), deplf(12)
    real(kind=8) ::  matf(78), matm(78)



!      FLEXURE (12X12)-> 78 TERMS
    data jf/6,9,10,13,14,15,39,40,41,45,48,49,50,54,55,58,59,60,64,65,&
     &     66,108,109,110,114,115,116,120,123,124,125,129,130,131,135,&
     &     136,139,140,141,145,146,147,151,152,153,213,214,215,219,220,&
     &     221,225,226,227,231,234,235,236,240,241,242,246,247,248,252,&
     &     253,256,257,258,262,263,264,268,269,270,274,275,276/
!    data km/1,2,7,8,13,14,19,20/
    data km/1,2,6,7,8, 12,13,14,18,19,20,24/
    data kf/3,4,5,9,10,11,15,16,17,21,22,23/


!     same as for dri4
    data jm4/  1,   2,   3,  16,  17,  21,  22,  23,  27,  28,&
          &   29,  30,  34,  35,  36,  67,  68,  72,  73,  74,&
          &   78,  79,  80,  84,  85,  86,  90,  91,  92,  93,&
          &   97,  98,  99, 103, 104, 105, 154, 155, 159, 160,&
          &  161, 165, 166, 167, 171, 172, 173, 177, 178, 179,&
          &  183, 184, 185, 189, 190, 191, 192, 196, 197, 198,&
          &  202, 203, 204, 208, 209, 210, 277, 278, 282, 283,&
          &  284, 288, 289, 290, 294, 295, 296, 300/
          

!  MATRICES DIAGONALES DE FLEXION ET DE MEBRANE  -> use jmemb(78)
    do k = 1, 78
        matf(k) = matloc(jf(k))
        matm(k) = matloc(jm4(k))
    end do

! ------------------------------------------------------------------  


 !    ------------------------------------------------------------------
 !-> computes product Eu*u = 2*Energy
    call utvtsv('ZERO', 24, matloc, depl, ener(1)) 
    
    if (coupmf) then
    
        ener(2) = 0.d0
        ener(3) = 0.d0
        
    else
! - ENER EN MEMBRANE ----------
        do  k = 1, 12
            deplm(k) = depl(km(k))
      enddo
 ! -> computes product Em um*um (membrane part only)
        call utvtsv('ZERO', 12, matm, deplm, ener(2))
        
!  ENER EN FLEXION ----------
        do  k = 1, 12
            deplf(k) = depl(kf(k))
      enddo
 ! -> computes product Ef uf*uf (bending part only)
        call utvtsv('ZERO', 12, matf, deplf, ener(3))
        
    endif
    
    ener(1) = 0.5d0*ener(1)     
!! 1/2 Eu*u  (TOTAL ENERGY)
!    ener(1) = ener(1) + ener_dri(1)
!    ener(2) = ener(2) + ener_dri(2)
!    ener(3) = ener(3) + ener_dri(3)
    
    if (abs(ener(1)) .gt. 1.d-6) then
! ->  Em um*um / Eu*u      (Membrane - RELATIVE)
        ener(2) = 0.5d0*ener(2)/ener(1)    
! ->  Ef uf*uf / Eu*u      (Bending  - RELATIVE)
        ener(3) = 0.5d0*ener(3)/ener(1)    
    else
        ener(2) = 0.d0
        ener(3) = 0.d0
    endif
end subroutine

