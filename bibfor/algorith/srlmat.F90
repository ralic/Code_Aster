subroutine srlmat(mod, imat, nbmat, tempd, tempf, tempr, materd,&
                  materf, matcst, ndt, ndi, nvi, indal)

!
! ===================================================================================
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.                      
! ===================================================================================

!!!
!!! MODELE LKR : RECUPERATION DES DONNEES MATERIAU
!!!

! ===================================================================================
! IN  : MOD    : TYPE DE MODELISATION
!     : IMAT   : ADRESSE DU MATERIAU CODE
!     : NBMAT  : NOMBRE DE PARAMETRES MATERIAU
!     : TEMPD  : TEMPERATURE A L'INSTANT -
!     : TEMPF  : TEMPERATURE A L'INSTANT +
!     : TEMPR  : TEMPERATURE DE REFERENCE
! OUT : MATERD : COEFFICIENTS MATERIAU A T
!     : MATERF : COEFFICIENTS MATERIAU A T + DT
!                     MATER(*,1) = CARACTERISTIQUES ELASTIQUES
!                     MATER(*,2) = CARACTERISTIQUES PLASTIQUES
!     : MATCST : 'OUI'
!     : NDT    : NOMBRE TOTAL DE COMPOSANTES DU TENSEUR
!     : NDI    : NOMBRE DE COMPOSANTES DIRECTES DU TENSEUR
!     : NVI    : NB DE VARIABLES INTERNES
!     : INDAL  : INDICATEUR SUR ALPHA
! ===================================================================================

    implicit none
    
#include "asterfort/srlnvi.h"
#include "asterfort/matini.h"
#include "asterfort/rcvala.h"

    !!!
    !!! Variables globales
    !!!
    
    integer :: ndt,ndi,nvi,imat,nbmat
    real(kind=8) :: materd(nbmat,2),materf(nbmat,2),tempd,tempf,tempr
    character(len=3) :: matcst
    character(len=8) :: mod
    
    !!!
    !!! Variables locales
    !!!
    
    integer :: ii,indal
    real(kind=8) :: e,nu,mu,k
    real(kind=8) :: dtempm,dtempp,dtemp
    integer :: cerr(31)
    character(len=13) :: nomc(31)
    
    !!!
    !!! Recuperation du nombre de composantes et de variables internes
    !!!

    call srlnvi(mod, ndt, ndi, nvi)

    !!!
    !!! Definition du nom des parametres materiau
    !!!
    
    nomc(1)  = 'E            '
    nomc(2)  = 'NU           '
    nomc(3)  = 'ALPHA        '
    nomc(4)  = 'PA           '
    nomc(5)  = 'NELAS        '
    nomc(6)  = 'SIGMA_C      '
    nomc(7)  = 'BETA         '
    nomc(8)  = 'GAMMA        '
    nomc(9)  = 'V_1          '
    nomc(10) = 'V_2          '
    nomc(11) = 'A_2          '
    nomc(12) = 'M_0          '
    nomc(13) = 'M_1          '
    nomc(14) = 'Q_I          '
    nomc(15) = 'XI_1         '
    nomc(16) = 'XI_2         '
    nomc(17) = 'XI_5         '
    nomc(18) = 'F_P          '
    nomc(19) = 'A            '
    nomc(20) = 'N            '
    nomc(21) = 'RHO_1        '
    nomc(22) = 'RHO_2        '
    nomc(23) = 'RHO_4        '
    nomc(24) = 'R_Q          '
    nomc(25) = 'R_M          '
    nomc(26) = 'R_S          '
    nomc(27) = 'R_X1         '
    nomc(28) = 'R_X2         '
    nomc(29) = 'R_X5         '
    nomc(30) = 'Z            '
    nomc(31) = 'COUPLAGE_P_VP'
    
    !!!
    !!! Recuperation des parametres materiau
    !!!
    
    call matini(nbmat,2,0.d0,materd)
    
    !!! parametres elastiques
    call rcvala(imat,' ','ELAS',3,'TEMP',[tempd,tempf,tempr],&
                3,nomc(1),materd(1,1),cerr(1),0)
    indal=1
    if (cerr(3).ne.0) indal=0
    
    !!! parametres lkr
    call rcvala(imat,' ','LKR',3,'TEMP',[tempd,tempf,tempr],&
                28,nomc(4),materd(1,2),cerr(4),0)
    
    !!!
    !!! Calcul des modules de cisaillement et de compressibilite et stockage
    !!!

    e=materd(1,1)
    nu=materd(2,1)
    mu=e/(2.d0*(1.d0+nu))
    k=e/(3.d0*(1.d0-2.d0*nu))
    
    materd(4,1)=mu
    materd(5,1)=k
    
    !!!
    !!! Stockage des temperatures et increments comme parametres materiau
    !!!
    
    materd(6,1)=tempd
    materd(7,1)=tempf
    materd(8,1)=tempr
    
    if ((tempf-tempr).ge.0.d0) then
        dtempp=tempf-tempr
    else
        dtempp = 0.d0
    endif
    
    if ((tempd-tempr).ge.0.d0) then
        dtempm=tempd-tempr
    else
        dtempm=0.d0
    endif
    
    dtemp=tempf-tempd
    
    materd(9,1)=dtempm
    materd(10,1)=dtempp
    materd(11,1)=dtemp
    
    !!!
    !!! Definition d'un materiau final
    !!!
    
    do ii=1,nbmat
        materf(ii,1)=materd(ii,1)
        materf(ii,2)=materd(ii,2)
    end do

    matcst='OUI'

end subroutine
