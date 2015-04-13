subroutine xsolveurtria(coor_nod, x, y, z, D, indmax, solution )

   implicit none

#include "asterc/r8gaem.h"

    integer                           ::  indmax    
    real(kind=8),dimension(3,3)       ::  coor_nod
    real(kind=8)                      ::  D(:),x(:),y(:),z(:) ,solution
  
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: patrick.massin at edf.fr

    integer                           ::  i, j, Id(2)
    real(kind=8)                      ::  valphi(2), term(3), delta, n(2), p, detT
    real(kind=8),dimension(3,2)       ::  V
    real(kind=8),dimension(2,2)       ::  T, invT

    
    do i = 1 , 3

        j = mod(i,3) + 1 

        !   première colonne 
        V(1,1)  = coor_nod(1,i)-x(indmax)
        V(2,1)  = coor_nod(2,i)-y(indmax)
        V(3,1)  = coor_nod(3,i)-z(indmax)

        !   deuxième colonne    
        V(1,2)  = coor_nod(1,j)-x(indmax)
        V(2,2)  = coor_nod(2,j)-y(indmax)
        V(3,2)  = coor_nod(3,j)-z(indmax)            

        !   valeur de phi au noeud
        valphi(1) = D(i) 
        valphi(2) = D(j)

        !   vecteur unité
        Id(1) = 1
        Id(2) = 1


        !!!!!!!!!!!!!!!!!!!!!!!!!!!CALCUL DES TERMES!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        T= matmul(transpose(V),V)
        
        detT = T(1,1)*T(2,2)-T(1,2)*T(2,1)
        
        invT(1,1) = (1/detT)*T(2,2)
        invT(1,2) = (-1/detT)*T(1,2)

        invT(2,1) = (-1/detT)*T(2,1)
        invT(2,2) = (1/detT)*T(1,1)

                                            
        term(1)=dot_product(Id,matmul(invT,Id))
        term(2)=dot_product(Id,matmul(invT,valphi))
        term(3)=dot_product(valphi,matmul(invT,valphi))                
                                                                     
        !!!!!!!!!!!!!!!!!!!!CALCUL DE LA LEVEL SET AU POINT INCONNU!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!       Calcul du discriminant
        delta = (term(2))**2.d0-term(1)*(term(3)-1)
                              
        if ( delta < 0 ) cycle
        
        p = (1/term(1))*(term(2)+sqrt(delta)) 
        
!       Test sur la direction de propagation
        n = matmul(invT,valphi-p*Id)
        
        if (n(1) .lt. 0 .and. n(2) .lt. 0 ) then
            solution = min( p, solution)
        endif

    enddo

end subroutine    
