subroutine xdecoupe(elp, cnset, nse, nnose)
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"

    integer          :: cnset(:), nse, nnose
    character(len=8) :: elp
    
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
! person_in_charge: samuel.geniaut at edf.fr
!
!                      CONNECTIVITÉ DES ÉLÉMENTS TETRAS À PARTIR
!                               D'UN ÉLÉMENT PARENT X-FEM
!                          (VOIR BOOK III 19/04/04)
!
!     ENTREE
!       ELP     : TYPE DE MAILLE
!
!     SORTIE
!       CNSET   : CONNECTIVITÉ DES NOEUDS DE LA MAILLE
!       NSE     : NOMBRE DE SOUS-TÉTRAS (SOUS TRIA)
!       NNOSE   : NOMBRE DE NOEUDS DU SOUS TETRA (SOUS TRIA)
!     ------------------------------------------------------------------
!
    integer :: i, ino, ise, connec(144, 4), connec1(12,6)
! ----------------------------------------------------------------------

    call jemarq()
    if (elp .eq. 'HE8') then
!       Nombre de pentahèdre dans un hexahèdre    
!       1
        connec1(1,1)=4
        connec1(1,2)=1
        connec1(1,3)=2
        connec1(1,4)=3
        connec1(1,5)=5
        connec1(1,6)=6  
!       2                          
        connec1(2,1)=5
        connec1(2,2)=8
        connec1(2,3)=7
        connec1(2,4)=6
        connec1(2,5)=4
        connec1(2,6)=3 
!       3                      
        connec1(3,1)=7
        connec1(3,2)=6
        connec1(3,3)=5
        connec1(3,4)=8
        connec1(3,5)=2
        connec1(3,6)=1 
!       4              
        connec1(4,1)=2
        connec1(4,2)=3
        connec1(4,3)=4
        connec1(4,4)=1
        connec1(4,5)=7
        connec1(4,6)=8  
!       5                     
        connec1(5,1)=2
        connec1(5,2)=1
        connec1(5,3)=5
        connec1(5,4)=6
        connec1(5,5)=4
        connec1(5,6)=8  
!       6                  
        connec1(6,1)=4
        connec1(6,2)=3
        connec1(6,3)=7
        connec1(6,4)=8
        connec1(6,5)=2
        connec1(6,6)=6
!       7        
        connec1(7,1)=7
        connec1(7,2)=8
        connec1(7,3)=4
        connec1(7,4)=3
        connec1(7,5)=5
        connec1(7,6)=1
!       8        
        connec1(8,1)=5
        connec1(8,2)=6
        connec1(8,3)=2
        connec1(8,4)=1
        connec1(8,5)=7
        connec1(8,6)=3
!       9            
        connec1(9,1)=3
        connec1(9,2)=4
        connec1(9,3)=1
        connec1(9,4)=2
        connec1(9,5)=8
        connec1(9,6)=5  
!       10                      
        connec1(10,1)=8
        connec1(10,2)=7
        connec1(10,3)=6
        connec1(10,4)=5
        connec1(10,5)=3
        connec1(10,6)=2
!       11        
        connec1(11,1)=6
        connec1(11,2)=5
        connec1(11,3)=8
        connec1(11,4)=7
        connec1(11,5)=1
        connec1(11,6)=4 
!       12                       
        connec1(12,1)=1
        connec1(12,2)=2
        connec1(12,3)=3
        connec1(12,4)=4   
        connec1(12,5)=6
        connec1(12,6)=7
        
        nse = 12
        nnose=6

!       12 tétraèdres par pentahèdre                 
        do i=1 , nse 
            connec(((i-1)*12)+1,1)=connec1(i,1)
            connec(((i-1)*12)+1,2)=connec1(i,2)
            connec(((i-1)*12)+1,3)=connec1(i,4)
            connec(((i-1)*12)+1,4)=connec1(i,5)

            connec(((i-1)*12)+2,1)=connec1(i,1)
            connec(((i-1)*12)+2,2)=connec1(i,3)
            connec(((i-1)*12)+2,3)=connec1(i,4)
            connec(((i-1)*12)+2,4)=connec1(i,5)
                          
            connec(((i-1)*12)+3,1)=connec1(i,3)
            connec(((i-1)*12)+3,2)=connec1(i,4)
            connec(((i-1)*12)+3,3)=connec1(i,5)
            connec(((i-1)*12)+3,4)=connec1(i,6)
                  
            connec(((i-1)*12)+4,1)=connec1(i,2)
            connec(((i-1)*12)+4,2)=connec1(i,4)
            connec(((i-1)*12)+4,3)=connec1(i,5)
            connec(((i-1)*12)+4,4)=connec1(i,6)
                         
            connec(((i-1)*12)+5,1)=connec1(i,2)
            connec(((i-1)*12)+5,2)=connec1(i,3)
            connec(((i-1)*12)+5,3)=connec1(i,4)
            connec(((i-1)*12)+5,4)=connec1(i,6)
                      
            connec(((i-1)*12)+6,1)=connec1(i,1)
            connec(((i-1)*12)+6,2)=connec1(i,2)
            connec(((i-1)*12)+6,3)=connec1(i,3)
            connec(((i-1)*12)+6,4)=connec1(i,5)
            
            connec(((i-1)*12)+7,1)=connec1(i,1)
            connec(((i-1)*12)+7,2)=connec1(i,2)
            connec(((i-1)*12)+7,3)=connec1(i,5)
            connec(((i-1)*12)+7,4)=connec1(i,6)
            
            connec(((i-1)*12)+8,1)=connec1(i,1)
            connec(((i-1)*12)+8,2)=connec1(i,3)
            connec(((i-1)*12)+8,3)=connec1(i,5)
            connec(((i-1)*12)+8,4)=connec1(i,6)
            
            connec(((i-1)*12)+9,1)=connec1(i,1)
            connec(((i-1)*12)+9,2)=connec1(i,3)
            connec(((i-1)*12)+9,3)=connec1(i,4)
            connec(((i-1)*12)+9,4)=connec1(i,6)
                          
            connec(((i-1)*12)+10,1)=connec1(i,1)
            connec(((i-1)*12)+10,2)=connec1(i,2)
            connec(((i-1)*12)+10,3)=connec1(i,4)
            connec(((i-1)*12)+10,4)=connec1(i,6)
            
            connec(((i-1)*12)+11,1)=connec1(i,1)
            connec(((i-1)*12)+11,2)=connec1(i,2)
            connec(((i-1)*12)+11,3)=connec1(i,3)
            connec(((i-1)*12)+11,4)=connec1(i,6)
                           
            connec(((i-1)*12)+12,1)=connec1(i,2)
            connec(((i-1)*12)+12,2)=connec1(i,3)
            connec(((i-1)*12)+12,3)=connec1(i,4)
            connec(((i-1)*12)+12,4)=connec1(i,5)               
        enddo
                             
        nse = 144
        nnose = 4
        
    else if (elp.eq.'PE6') then
    
!           12 tétraèdres dans un pentahèdre    

            connec(1,1) = 1
            connec(1,2) = 2
            connec(1,3) = 4
            connec(1,4) = 5

            connec(2,1) = 1
            connec(2,2) = 3
            connec(2,3) = 4
            connec(2,4) = 5
                          
            connec(3,1) = 3
            connec(3,2) = 4
            connec(3,3) = 5
            connec(3,4) = 6
                  
            connec(4,1) = 2
            connec(4,2) = 4
            connec(4,3) = 5
            connec(4,4) = 6
                         
            connec(5,1) = 2
            connec(5,2) = 3
            connec(5,3) = 4
            connec(5,4) = 6
                      
            connec(6,1)  = 1
            connec(6,2) = 2
            connec(6,3) = 3
            connec(6,4) = 5
            
            connec(7,1) = 1
            connec(7,2) = 2
            connec(7,3) = 5
            connec(7,4) = 6
            
            connec(8,1) = 1
            connec(8,2) = 3
            connec(8,3) = 5
            connec(8,4) = 6
            
            connec(9,1) = 1
            connec(9,2) = 3
            connec(9,3) = 4
            connec(9,4) = 6
                          
            connec(10,1) = 1
            connec(10,2) = 2
            connec(10,3) = 4
            connec(10,4) = 6
            
            connec(11,1) = 1
            connec(11,2) = 2
            connec(11,3) = 3
            connec(11,4) = 6
                           
            connec(12,1) = 2
            connec(12,2) = 3
            connec(12,3) = 4
            connec(12,4) = 5
            
            nse=12
            nnose=4
    else if (elp.eq.'PY5') then
    
!       4 tétraèdres dasn une pyramide PY5

        connec(1,1) = 1
        connec(1,2) = 2
        connec(1,3) = 3
        connec(1,4) = 5
        
        connec(2,1) = 1
        connec(2,2) = 3
        connec(2,3) = 4
        connec(2,4) = 5 

        connec(3,1) = 1
        connec(3,2) = 2
        connec(3,3) = 4
        connec(3,4) = 5        

        connec(4,1) = 2
        connec(4,2) = 3
        connec(4,3) = 4
        connec(4,4) = 5        
        
        nse=4
        nnose=4
    else if (elp.eq.'TE4') then
        connec(1,1)=1
        connec(1,2)=2
        connec(1,3)=3
        connec(1,4)=4
        
        nse=1
        nnose=4
    else if (elp.eq.'QU4') then
!       4 triangles    
        connec(1,1)=1
        connec(1,2)=2
        connec(1,3)=4
        
        connec(2,1)=2
        connec(2,2)=3
        connec(2,3)=4
        
        connec(3,1)=1
        connec(3,2)=2
        connec(3,3)=3
        
        connec(4,1)=3
        connec(4,2)=1
        connec(4,3)=4
        
        nse=4
        nnose=3
    else if (elp.eq.'TR3') then
        connec(1,1)=1
        connec(1,2)=2
        connec(1,3)=3
        
        nse=1
        nnose=3
    else
!       TYPE D'ELEMENT FINI PAS TRAITE
        ASSERT(.false.)
    endif
!
    do  ise = 1, nse
        do ino = 1, nnose
            cnset(nnose*(ise-1)+ino)=connec(ise,ino)
        enddo    
    enddo
!
    call jedema()
end subroutine
