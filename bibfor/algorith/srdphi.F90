subroutine srdphi(nbmat, mater, de, seuilv, dfdsv, dphi)

!
! ===================================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG             
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
! ===================================================================================

!!!
!!! MODELE LKR : DERIVEE DE L'AMPLITUDE DES DEFORMATIONS IRREVERSIBLES DEPS
!!!

! ===================================================================================
! IN  : NBMAT  : NOMBRE DE PARAMETRES MATERIAU
!     : MATER  : COEFFICIENTS MATERIAU A T+DT
!                   MATER(*,1) = CARACTERISTIQUES ELASTIQUES
!                   MATER(*,2) = CARACTERISTIQUES PLASTIQUES
!     : DE     : MATRICE HYPOELASTIQUE
!     : SEUILV : SEUIL VISCOPLASTIQUE
!     : DFDSV  : DERIVEE DU CRITERE VISCOPLASTIQUE PAR RAPPORT A LA
!                CONTRAINTE
! OUT : DPHI   : AMPLITUDE DES DEFORMATIONS IRREVERSIBLES
!                    DPHI = A*NV/PA*(FVP/PA)**(NV-1)*DFVP/DSIGE*DE
! ===================================================================================

    implicit    none

#include "asterfort/lcprmv.h"
#include "asterfort/r8inir.h"

    !!!
    !!! Variables globales
    !!!
    
    integer :: nbmat
    real(kind=8) :: mater(nbmat,2), de(6,6)
    real(kind=8) :: dphi(6), dfdsv(6), seuilv
    
    !!!
    !!! Variables locales
    !!!
    
    integer :: i, ndi, ndt
    real(kind=8) :: pa, a, n, aa(6), a0, r, z
    real(kind=8) :: tpp, trr
    
    common /tdim/   ndt , ndi
    
    !!!
    !!! Recuperation des temperatures
    !!!
    
    tpp=mater(7,1)
    trr=mater(8,1)
    
    !!!
    !!! Para. du modele
    !!!
    
    !!! a T0
    pa=mater(1,2)
    a0=mater(16,2)
    n=mater(17,2)
    z=mater(27,2)
    r=8.3144621d0
    
    !!! a T
    if ((tpp.ge.trr).and.(trr.gt.0.d0)) then
        a=a0*exp(-z/r/tpp*(1.d0-tpp/trr))
    else
        a=a0
    endif
    
    !!!
    !!! Matrice intermediaire
    !!!
    
    call r8inir(6, 0.d0, aa, 1)
    call lcprmv(de, dfdsv, aa)
    
    !!!
    !!! Calcul de dphi/deps
    !!!
    
    do i=1, ndt
        if (seuilv.le.0.d0) then
            dphi(i)=0.d0
        else
            dphi(i)=a*n/pa*(seuilv/pa)**(n-1.d0)*aa(i)
        endif
    end do

end subroutine
