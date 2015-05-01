subroutine matrq9(mat)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: xavier.desroches at edf.fr
!
! CALCUL DE LA MATRICE DE PASSAGE GAUSS NOEUDS POUR LES MECQQU9
!
    real(kind=8) :: mat(9,9), a, b1, b2, c1, c2, d1, d2  
    integer :: i, j
!
    a = sqrt(3.d0)/3.d0
    b1 = 2.25d0*(a+1.d0)**2
    b2 = 2.25d0*(a-1.d0)**2
    c1 = -3.d0*(a+1.d0)
    c2 =  3.d0*(a-1.d0)
    d1 =  1.5d0*(1.d0+a)
    d2 =  1.5d0*(1.d0-a)
!
    do 1 i=1,9
        do 2 j=1,9    
            mat(i,j)=0.d0       
2       continue            
1   continue
!
    mat(1,1) = b1           
    mat(2,1) = 1.5d0        
    mat(3,1) = b2           
    mat(4,1) = 1.5d0        
!
    mat(1,2) = c1           
    mat(2,2) = c1           
    mat(3,2) = c2           
    mat(4,2) = c2           
    mat(5,2) = d1           
    mat(7,2) = d2
!
    mat(2,3) = b1           
    mat(1,3) = 1.5d0        
    mat(4,3) = b2           
    mat(3,3) = 1.5d0        
!
    mat(1,4) = c2           
    mat(2,4) = c1           
    mat(3,4) = c1           
    mat(4,4) = c2           
    mat(6,4) = d1           
    mat(8,4) = d2
!
    mat(1,5) = b2   
    mat(2,5) = 1.5d0        
    mat(3,5) = b1           
    mat(4,5) = 1.5d0        
!
    mat(1,6) = c2           
    mat(2,6) = c2           
    mat(3,6) = c1           
    mat(4,6) = c1           
    mat(5,6) = d2           
    mat(7,6) = d1
!
    mat(2,7) = b2           
    mat(1,7) = 1.5d0        
    mat(4,7) = b1           
    mat(3,7) = 1.5d0        
!
    mat(1,8) = c1           
    mat(2,8) = c2           
    mat(3,8) = c2           
    mat(4,8) = c1           
    mat(6,8) = d2           
    mat(8,8) = d1
!
    mat(1,9) = 4.d0         
    mat(2,9) = 4.d0         
    mat(3,9) = 4.d0         
    mat(4,9) = 4.d0         
    mat(5,9) = -2.d0        
    mat(6,9) = -2.d0        
    mat(7,9) = -2.d0        
    mat(8,9) = -2.d0        
    mat(9,9) = 1.d0
!
end subroutine
            