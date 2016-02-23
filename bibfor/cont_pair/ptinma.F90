subroutine ptinma(coorma,nbnma,typma,xpt,ypt,tres,tole, ndim)
    
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

!
    integer :: nbnma, tres, ndim
    real(kind=8) :: coorma(ndim-1,nbnma), xpt, ypt, tole 
    character(len=8) :: typma
! -------------------------------------------------------------------------------------------------
!         TEST D'APPARTENANCE D'UN POINT A UNE MAILLE DANS UN ESPACE 2D ou 1D
! -------------------------------------------------------------------------------------------------
! IN         COORMA      COORDONNÉES DES POINTS DE LA MAILLE
! IN         NBNMA       NOMBRE DE POINT CONSTITUANT LA MAILLE
! IN         TYPMA       TYPE DE MAILLE
! IN         XPT         ABSCISSE DU POINT A TESTER 
! IN         YPT         ORDONNEE DU POINT A TESTER
! OUT        TRES        RESULTAT DU TEST
!                         =1 si (xpt,ypt) appartient à la maille
!                         =0 sinon
!                         =-1 erreur (points alignés)
! IN         TOLE        TOLERANCE
! IN         NDIM        DIMENSION
! -------------------------------------------------------------------------------------------------
!
    real(kind=8) :: v0(2), v1(2), v2(2),vaux(2), d00, d10, d11, m, u, v
    real(kind=8) :: t1, t2, d02, d12, xpmin, xpmax
!
! --- Vérification du type de l'élément
!
    ASSERT(typma .eq. 'TR3' .or. typma .eq. 'QU4' .or. typma .eq. 'SE2')
    if ((ndim-1) .eq. 2) then 
!
! ---Calcul de la base vectoriel de l'élément ----------------------
!
        v0(1)=coorma(1,2)-coorma(1,1)   
        v0(2)=coorma(2,2)-coorma(2,1)
        v1(1)=coorma(1,nbnma)-coorma(1,1)   
        v1(2)=coorma(2,nbnma)-coorma(2,1) 
!   
        d00=v0(1)*v0(1)+v0(2)*v0(2)
        d10=v0(1)*v1(1)+v0(2)*v1(2)
        d11=v1(1)*v1(1)+v1(2)*v1(2)
   
!
        m=(d00*d11-d10*d10) 
        
!------ Vecteur colinéaires ------------------------------
        if(abs(m).le.tole) then
            tres=-1
            go to 30
        end if
!
! --- Calcul des coordonnées tests (cas QUAD4) -------------------------
! 
        if (typma.eq.'QU4') then      
            vaux(1)=coorma(1,3)-coorma(1,1)
            vaux(2)=coorma(2,3)-coorma(2,1)
!
            d02=v0(1)*vaux(1)+v0(2)*vaux(2)
            d12=v1(1)*vaux(1)+v1(2)*vaux(2)
!
            t1=d02
            t2=d12
            d02=0.d0
            d12=0.d0
        endif
!
! --- Calcul des coordonnées du points dans le repère de la maille -----
!
        v2(1)=xpt-coorma(1,1)
        v2(2)=ypt-coorma(2,1)

!
        d02=v0(1)*v2(1)+v0(2)*v2(2)
        d12=v1(1)*v2(1)+v1(2)*v2(2)
        if (sqrt(v2(1)**2+v2(2)**2) .le. 0.d0+tole) then    
            tres = 1
            go to 30
        end if
!
! --- Test sur les coordonnées avec tolerance 
!
        if (typma.eq.'TR3') then
            u=1/m*(d11*d02-d10*d12)
            v=1/m*(d00*d12-d10*d02)
            if (u.ge.(0.d0-tole) .and. v.ge.(0.d0-tole) .and. (u+v).le.(1.d0+tole)) then
                tres=1
            else
                tres=0
            endif
        elseif (typma.eq.'QU4') then
            u=d02/t1
            v=d12/t2
            if (u.ge.(0.d0-tole) .and. v.ge.(0.d0-tole) .and. u.le.(1.d0+tole) .and.&
                v.le.(1.d0+tole)) then
                tres=1
            else
                tres=0
            endif
        endif
    elseif (ndim-1 .eq. 1) then
        xpmin = min(coorma(1,1), coorma(1,2))
        xpmax = max(coorma(1,1), coorma(1,2))
        if (xpt .ge. (xpmin-tole) .and. xpt .le. (xpmax+tole)) then
            tres=1
        else
            tres=0
        endif
    end if
    30 continue
end subroutine
