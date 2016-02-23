subroutine insema(coorma,nbnma,xp1,yp1,xp2,&
                  yp2,corres,nbpint,tole, itvois)
   
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
! aslint: disable=W1306
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
!
    integer :: nbnma, nbpint, itvois(4)
    real(kind=8) :: coorma(2,nbnma), xp1, yp1, xp2, yp2 
    real(kind=8) :: corres(3,3), tole
! ----------------------------------------------------------------------
!         TEST D'INTERSECTION ENTRE UN SEGMENT et UNE MAILLE
! ----------------------------------------------------------------------
! IN         COORMA     COORDONNÉE DES POINT DE LA MAILLE
! IN         NBNMA       NOMBRE DE POINT CONSTITUANT LA MAILLE
! IN         XP1         ABSCISSE DU POINT 1 TESTER 
! IN         YP1         ORDONNEE DU POINT 1 TESTER
! IN         XP2         ABSCISSE DU POINT 2 TESTER 
! IN         YP2         ORDONNEE DU POINT 2 TESTER
! OUT        CORRES      COORDONNEE DU/DES POINTS D'INTERSECTION
! OUT        NBPINT      NOMBRE DE POINTS D'INTERSECTION
! ----------------------------------------------------------------------
!
    real(kind=8) :: a, b, c, d, x1, y1, x2, y2
    real(kind=8) :: t1, t2, det, norm, aux(2)
    integer :: k, indsu(nbnma)
!
!
! --- Initialisation
!
    nbpint=0
!
! --- Equation paramétrique du segment----------------------------------
! 
    x1=xp1
    y1=yp1
!
    a=xp2-xp1
    b=yp2-yp1
!
! --- Vecteur indice suivant
!
    do k=2, nbnma
        indsu(k-1)=k
    end do
    indsu(nbnma)=1

!
! --- Boucle sur les arêtes de la maille -------------------------------
!     
    do k=1, nbnma
! ------- Equation paramétrique du segment courant de la maille --------

! 
        x2=coorma(1,k)
        y2=coorma(2,k)
!
        c=coorma(1,indsu(k))-coorma(1,k)
        d=coorma(2,indsu(k))-coorma(2,k)     
! ------- Résolution de l'intersection si les droites sont sécantes ----
        
        det=b*c-a*d
        if (sqrt(det**2).gt. tole) then
            t1=1/det*(d*(x1-x2)-c*(y1-y2))
            t2=1/det*(b*(x1-x2)-a*(y1-y2)) 
            aux(1)=(-t1*a-t2*c)-(x1-x2)
            aux(2)=( t1*b+t2*d)-(y1-y2)
            norm=sqrt(aux(1)**2+aux(2)**2)
        else
            t1=-1.d0
            t2=-1.d0    
        endif
       
! ------- Test sur l'intersection
        if (t1.lt. 1.d0+tole .and. t1.gt. 0.d0-tole .and. t2.lt. 1.d0+tole .and.&
            t2.gt. 0.d0-tole) then
            nbpint=nbpint+1
            !write(*,*)nbpint
            !write(*,*)     t2*c+x2, t2*d+y2
            ASSERT(nbpint.le.4)
! ------- On recopie le point d'intersection 
            corres(1,nbpint)=(t2*c+x2+t1*a+x1)/2.d0
            corres(2,nbpint)=(t2*d+y2+t1*b+y1)/2.d0
! ------- On renseigne le test d'intersection avec voisin
            itvois(k)=1
        endif
    end do
end subroutine

