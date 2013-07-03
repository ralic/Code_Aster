subroutine btdbmc(b, d, jacob, ndim, nno,&
                  nbsig, phenoz, btdb)
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
!.======================================================================
    implicit none
!
!       BTDBMC -- CALCUL DU PRODUIT BT*D*B DONNANT LA MATRICE
!                 DE RIGIDITE ELEMENTAIRE POUR TOUS LES
!                 ELEMENTS ISOPARAMETRIQUES
!
!   ARGUMENT        E/S  TYPE         ROLE
!    B(6,81)        IN     R        MATRICE (B) CALCULEE AU POINT
!                                   D'INTEGRATION COURANT ET RELIANT
!                                   LES DEFORMATIONS DU PREMIER ORDRE
!                                   AUX DEPLACEMENTS
!    D(6,6)         IN     R        MATRICE DE HOOKE DANS LE REPERE
!                                   GLOBAL
!    JACOB          IN     R        PRODUIT JACOBIEN*POIDS AU POINT
!                                   D'INTEGRATION COURANT
!    NDIM           IN     I        DIMENSION DE L'ELEMENT (2 OU 3)
!    NNO            IN     I        NOMBRE DE NOEUDS DE L'ELEMENT
!    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE A
!                                   L'ELEMENT
!    PHENOM         IN     K16      TYPE DU MATERIAU ('ELAS' OU
!                                   'ELAS_ORTH' OU 'ELAS_ISTR')
!    BTDB(81,81)    OUT    R        MATRICE ELEMENTAIRE DE RIGIDITE
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
#include "asterfort/btdbpr.h"
#include "asterfort/lteatt.h"
#include "asterfort/u2mesk.h"
    character(len=*) :: phenoz
    real(kind=8) :: b(nbsig, *), d(nbsig, *), jacob, btdb(81, 81)
! -----  VARIABLES LOCALES
    character(len=16) :: phenom
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! ---- INITIALISATIONS
!      ---------------
!-----------------------------------------------------------------------
    integer :: i1, i2, l1, l2, nbinco, nbsig, ndim
    integer :: nno
    real(kind=8) :: d1, d2, d3, r11, r12, r13, r21
    real(kind=8) :: r22, r23, r31, r32, r33, rs13, rs23
    real(kind=8) :: rs33, s33, sr31, sr32, sr33
!-----------------------------------------------------------------------
    phenom = phenoz
    nbinco = nno*ndim
!
!      ------------
! ---- CAS ISOTROPE
!      ------------
    if (phenom .eq. 'ELAS' .or. phenom .eq. 'ELAS_HYPER') then
!
! ----   CAS CONTRAINTES PLANES ET DEFORMATIONS PLANES
!        ---------------------------------------------
        if (lteatt(' ','C_PLAN','OUI') .or. lteatt(' ','D_PLAN','OUI')) then
!
            l1 = nbinco - 1
!
            d1 = d(1,1)*jacob
            d2 = d(1,2)*jacob
            d3 = d(4,4)*jacob
!
            do 10 i1 = 1, l1, 2
                do 10 i2 = 1, l1, 2
!
                    r11 = b(1,i1) *b(1,i2)
                    r21 = b(2,i1+1)*b(1,i2)
                    r12 = b(1,i1) *b(2,i2+1)
                    r22 = b(2,i1+1)*b(2,i2+1)
!
                    btdb(i1,i2) = btdb(i1,i2) + d1*r11 + d3*r22
                    btdb(i1+1,i2) = btdb(i1+1,i2) + d2*r21 + d3*r12
                    btdb(i1, i2+1) = btdb(i1, i2+1) + d2*r12 + d3*r21
                    btdb(i1+1,i2+1) = btdb(i1+1,i2+1) + d1*r22 + d3* r11
!
10              continue
!
! ----   CAS AXI
!        -------
            elseif (lteatt(' ','AXIS','OUI').and. (.not.lteatt(' ',&
        'FOURIER','OUI'))) then
!
            l1 = nbinco - 1
!
            d1 = d(1,1)*jacob
            d2 = d(1,2)*jacob
            d3 = d(4,4)*jacob
!
            do 20 i1 = 1, l1, 2
                do 20 i2 = 1, l1, 2
!
                    r11 = b(1,i1) *b(1,i2)
                    r21 = b(2,i1+1)*b(1,i2)
                    r31 = b(3,i1) *b(1,i2)
!
                    r12 = b(1,i1) *b(2,i2+1)
                    r22 = b(2,i1+1)*b(2,i2+1)
                    r32 = b(3,i1) *b(2,i2+1)
!
                    r13 = b(1,i1) *b(3,i2)
                    r23 = b(2,i1+1)*b(3,i2)
                    r33 = b(3,i1) *b(3,i2)
!
                    btdb(i1,i2) = btdb(i1,i2) + d1*(r11+r33) + d2*( r31+r13) + d3*r22
                    btdb(i1+1,i2) = btdb(i1+1,i2) + d2*(r21+r23) + d3*r12
                    btdb(i1, i2+1) = btdb(i1, i2+1) + d2*(r12+r32) + d3*r21
                    btdb(i1+1,i2+1) = btdb(i1+1,i2+1) + d1*r22 + d3* r11
!
20              continue
!
! ----   CAS 3D
!        ------
        else if (lteatt(' ','DIM_TOPO_MAILLE','3')) then
!
            l2 = nbinco - 2
!
            d1 = d(1,1)*jacob
            d2 = d(1,2)*jacob
            d3 = d(4,4)*jacob
!
            do 30 i1 = 1, l2, 3
                do 30 i2 = 1, l2, 3
!
                    r11 = b(1,i1)*b(1,i2)
                    r12 = b(1,i1)*b(2,i2+1)
                    r13 = b(1,i1)*b(3,i2+2)
!
                    r21 = b(2,i1+1)*b(1,i2)
                    r22 = b(2,i1+1)*b(2,i2+1)
                    r23 = b(2,i1+1)*b(3,i2+2)
!
                    r31 = b(3,i1+2)*b(1,i2)
                    r32 = b(3,i1+2)*b(2,i2+1)
                    r33 = b(3,i1+2)*b(3,i2+2)
!
                    btdb(i1,i2) = btdb(i1,i2) + d1*r11 + d3*(r22+r33)
                    btdb(i1+1,i2) = btdb(i1+1,i2) + d2*r21 + d3*r12
                    btdb(i1+2,i2) = btdb(i1+2,i2) + d2*r31 + d3*r13
!
                    btdb(i1, i2+1) = btdb(i1, i2+1) + d2*r12 + d3*r21
                    btdb(i1+1,i2+1) = btdb(i1+1,i2+1) + d1*r22 + d3*( r11+r33)
                    btdb(i1+2,i2+1) = btdb(i1+2,i2+1) + d2*r32 + d3* r23
!
                    btdb(i1, i2+2) = btdb(i1, i2+2) + d2*r13 + d3*r31
                    btdb(i1+1,i2+2) = btdb(i1+1,i2+2) + d2*r23 + d3* r32
                    btdb(i1+2,i2+2) = btdb(i1+2,i2+2) + d1*r33 + d3*( r11+r22)
!
30              continue
!
! ----   CAS FOURIER
!        -----------
        else if (lteatt(' ','FOURIER','OUI')) then
!
            l2 = nbinco - 2
!
            d1 = d(1,1)*jacob
            d2 = d(1,2)*jacob
            d3 = d(4,4)*jacob
!
            do 40 i1 = 1, l2, 3
                do 40 i2 = 1, l2, 3
!
                    r11 = b(1,i1)*b(1,i2)
                    r12 = b(1,i1)*b(2,i2+1)
                    r13 = b(1,i1)*b(3,i2+2)
!
                    r21 = b(2,i1+1)*b(1,i2)
                    r22 = b(2,i1+1)*b(2,i2+1)
                    r23 = b(2,i1+1)*b(3,i2+2)
!
                    r31 = b(3,i1+2)*b(1,i2)
                    r32 = b(3,i1+2)*b(2,i2+1)
                    r33 = b(3,i1+2)*b(3,i2+2)
!
                    rs13 = b(1,i1) *b(3,i2)
                    rs23 = b(2,i1+1)*b(3,i2)
                    sr31 = b(3,i1) *b(1,i2)
                    sr32 = b(3,i1) *b(2,i2+1)
                    sr33 = b(3,i1) *b(3,i2+2)
                    rs33 = b(3,i1+2)*b(3,i2)
!
                    s33 = b(3,i1) *b(3,i2)
!
                    btdb(i1,i2) = btdb(i1,i2) + d1*(r11+s33) + d2*( rs13+sr31) + d3*(r22+r33)
                    btdb(i1+1,i2) = btdb(i1+1,i2) + d2*(r21+rs23) + d3*r12
                    btdb(i1+2,i2) = btdb(i1+2,i2) + d1*rs33+ d2*r31 + d3*(-r13+sr33)
!
                    btdb(i1, i2+1) = btdb(i1, i2+1) + d2*(r12+sr32) + d3*r21
                    btdb(i1+1,i2+1) = btdb(i1+1,i2+1) + d1*r22 + d3*( r11+r33)
                    btdb(i1+2,i2+1) = btdb(i1+2,i2+1) + d2*r32 - d3* r23
!
                    btdb(i1, i2+2) = btdb(i1, i2+2) + d1*sr33 + d2* r13 + d3*(-r31+rs33)
                    btdb(i1+1,i2+2) = btdb(i1+1,i2+2) + d2*r23 - d3* r32
                    btdb(i1+2,i2+2) = btdb(i1+2,i2+2) + d1*r33 + d3*(r11+r22+s33-sr31-rs13)
!
40              continue
!
        endif
!      -------------------------------------
! ---- CAS ORTHOTROPE ET ISOTROPE TRANSVERSE
!      -------------------------------------
    else if (phenom.eq.'ELAS_ORTH'.or.phenom.eq.'ELAS_ISTR') then
!
        call btdbpr(b, d, jacob, nbsig, nbinco,&
                    btdb)
!
    else
        call u2mesk('F', 'ELEMENTS_15', 1, phenom)
    endif
!.============================ FIN DE LA ROUTINE ======================
end subroutine
