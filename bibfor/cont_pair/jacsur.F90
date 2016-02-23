subroutine jacsur(coorma, nbnma, typma, ndim, ksi1, ksi2,&
                  jac, ndir)
    
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/mmnorm.h"
#include "asterfort/mmtang.h"
#include "asterfort/mmdonf.h"
!
    integer :: nbnma, ndim
    real(kind=8) :: coorma(3,9), ksi1, ksi2, jac, ndir(3)
    character(len=8) :: typma
! ----------------------------------------------------------------------
!         Calcul du produit vectoriel du jacobien au point (KSI1,KSI2)
! ----------------------------------------------------------------------
! IN         COORMA       COORDONNEES DE LA MAILLE MAITRE
! IN         NBNMA        NOMBRE DE POINTS MAITRES
! IN         TYPMA        TYPE DE LA MAILLE MAITRE
! IN         NDIM
! IN         KSI1         
! IN         KSI2
! OUT        JAC          "DETERMINANT" DE LA JACOBIENNE SURFACIQUE
! OUT        NDIR         DIRECTION NORMALE 
! ----------------------------------------------------------------------
!
    integer :: ino, ind
    real(kind=8) :: dff(2,9), jcs1(3), jcs2(3)
    real(kind=8) :: tau1(3), tau2(3), dxdk, dydk, dzdk
    real(kind=8) :: norm, aux(3)
!
! ---- Initialisation

    ndir(1:3)=0.d0
    norm=0.d0
    do ind=1,3
        tau1(ind)=0.d0
        tau2(ind)=0.d0
    end do
!
! ---- Calcul des dérivées des fonctions de forme au point (ksi1,ksi2) - 
    call mmdonf(ndim,nbnma,typma,ksi1,ksi2,&
                dff)
!
! ---- Calcul de la normale au point réelle de coordonnée paramétrique -
! -----                      (Ksi1,ksi2)                       ---------
    call mmtang(ndim,nbnma,coorma,dff,tau1,tau2)
    call mmnorm(ndim, tau1, tau2, ndir, norm)
    ndir(1:3)=-ndir(1:3)
    if( (ndim-1) .eq. 2 ) then
! ---- Calcul de la jacobienne surfacique ------------------------------
        jcs1(1:3)=0.d0
        jcs2(1:3)=0.d0
        do ino = 1, nbnma
            jcs1(1)=jcs1(1)+dff(1,ino)*coorma(1,ino)
            jcs1(2)=jcs1(2)+dff(1,ino)*coorma(2,ino)
            jcs1(3)=jcs1(3)+dff(1,ino)*coorma(3,ino)
            jcs2(1)=jcs2(1)+dff(2,ino)*coorma(1,ino)
            jcs2(2)=jcs2(2)+dff(2,ino)*coorma(2,ino)
            jcs2(3)=jcs2(3)+dff(2,ino)*coorma(3,ino)
        end do
! ---- Norme du produit vectoriel -------------------------------
        aux(1)=jcs1(2)*jcs2(3)-jcs1(3)*jcs2(2)
        aux(2)=jcs1(3)*jcs2(1)-jcs1(1)*jcs2(3)
        aux(3)=jcs1(1)*jcs2(2)-jcs1(2)*jcs2(1)    
        jac=sqrt(aux(1)**2+aux(2)**2+aux(3)**2)
    elseif ((ndim-1) .eq. 1) then
        dxdk=0.d0
        dydk=0.d0
        dzdk=0.d0
        do ino = 1, nbnma
            dxdk = dxdk + coorma(1,ino)*dff(1,ino)
            dydk = dydk + coorma(2,ino)*dff(1,ino)
            if (ndim .eq. 3) then
                dzdk = dzdk + coorma(3,ino)*dff(1,ino)
            end if
        end do
!       JACOBIEN 1D == DERIVEE DE L'ABSCISSE CURVILIGNE
        jac = sqrt(dxdk**2+dydk**2+dzdk**2)
    endif

end subroutine
