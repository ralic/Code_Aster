subroutine lcodrm(cordin,nbpint,tole ,resu, ndim)
   

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
! aslint: disable=W1306
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/ordr8.h"
!
    integer :: nbpint
    integer, intent(in) :: ndim
    real(kind=8), intent(in) :: cordin(ndim-1,nbpint)
    real(kind=8), intent(in) :: tole
    real(kind=8), intent(out) :: resu(ndim-1,16)
! -------------------------------------------------------------------------------
!        Suppression des doublons et classement des noeuds d'intersections
! -------------------------------------------------------------------------------
! ----------------------------------------------------------------------
!
    real(kind=8) :: a(nbpint), b(2)
    real(kind=8) :: v(2), norm, auxp
    integer :: k, ord(nbpint), indsu(nbpint), nbptr
!
! --- Initialisation ---------------------------------------------------
!
    if ((ndim-1) .eq. 2) then
        b(1)=0.d0
        b(2)=0.d0
        v(1)=0.d0
        v(2)=0.d0
        nbptr=0
        auxp=real(nbpint)
!
! --- Coordonées du barycentre de l'intesection ------------------------
!      
        do k=1,nbpint
            b(1)=b(1)+cordin(1,k)/auxp
            b(2)=b(2)+cordin(2,k)/auxp
        end do
!   
! --- Liste des angles -------------------------------------------------
!
        do k=1, nbpint
            v(1)=cordin(1,k)-b(1)
            v(2)=cordin(2,k)-b(2)
            a(k)=atan2(v(1),v(2))
        end do
!
! --- On ordonne les points dans le sens trigonométrique ---------------
!
        call ordr8(a,nbpint,ord)  
! --- On supprime les doublons ------------------------------------------
!
! --- Vecteur indice suivant --------------------------------------------
        do k=2, nbpint
            indsu(k-1)=k
        end do
        indsu(nbpint)=1
! --- Initialisation
        nbptr=0
!
        do k=1, nbpint
            norm=sqrt((a(ord(k))-a(ord(indsu(k))))**2)
            if (norm.gt.10*tole) then
                nbptr=nbptr+1
                resu(1,nbptr) = cordin(1,ord(k))
                resu(2,nbptr) = cordin(2,ord(k))     
            endif
        end do
        nbpint=nbptr
    elseif ((ndim-1) .eq. 1) then
        resu(1,1) = cordin(1,1)
        resu(1,2) = cordin(1,1)
        do k=2, nbpint
            if (cordin(1,k) .le. resu(1,1) .and.  cordin(1,k) .ge. -1.d0) then
                resu(1,1) =  cordin(1,k)
            elseif (cordin(1,k) .ge. resu(1,2) .and.  cordin(1,k) .le. 1.d0) then
                resu(1,2) =  cordin(1,k)
            end if
        end do        
        nbpint = 2 
    end if
!
end subroutine
