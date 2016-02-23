subroutine apdcma(typma, lissma, nbnsma, nbssma)
   
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
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/assert.h"


!
    character(len=8), intent(in) :: typma
    integer, intent(out) :: lissma(8,4)
    integer, intent(out) :: nbnsma(8)
    integer, intent(out) :: nbssma
!
! ----------------------------------------------------------------------
!     ROUTINE APPARIEMENT APPROXIMATION GEOMETRIQUE MAITRE
!     (DECOUPE EN SOUS-MAILLE LINEAIRE)
! ----------------------------------------------------------------------
!   IN        TYPMA      TYPE DE LA MAILLE MAITRE
!   IN        NBNMA      NOMBRE DE NOEUDS MAITRES
!   OUT       LISSMA     LISTE DES NOEUDS DES SOUS-MAILLES
!   OUT       NBNSMA     VECTEUR NOEUDS PAR SOUS-MAILLE
!   OUT       NBSSMA     NOMBRE DE SOUS-MAILLES LINEAIRE      
! ----------------------------------------------------------------------
!

! ----------------------------------------------------------------------
!
    if (typma .eq. 'SE2') then
        nbssma= 1
! ----- Sous maille 1 -------
        nbnsma(1) = 2
        lissma(1,1) = 1
        lissma(1,2) = 2 
    elseif (typma .eq. 'SE3') then
        nbssma= 2
! ----- Sous maille 1 -------
        nbnsma(1) = 2
        lissma(1,1) = 1
        lissma(1,2) = 3 
! ----- Sous maille 1 -------
        nbnsma(2) = 2
        lissma(2,1) = 3
        lissma(2,2) = 2 
    elseif (typma .eq. 'TR3') then
        nbssma = 1
! ----- Sous maille 1 -------
        nbnsma(1) = 3
        lissma(1,1) = 1
        lissma(1,2) = 2           
        lissma(1,3) = 3       
    else if (typma .eq. 'TR6') then
        nbssma=4
! ----- Sous maille 1 -------
        nbnsma(1) = 3
        lissma(1,1) = 1
        lissma(1,2) = 4           
        lissma(1,3) = 6
! ----- Sous maille 2 -------
        nbnsma(2) = 3
        lissma(2,1) = 4
        lissma(2,2) = 2           
        lissma(2,3) = 5  
! ----- Sous maille 3 -------
        nbnsma(3) = 3
        lissma(3,1) = 4
        lissma(3,2) = 5          
        lissma(3,3) = 6    
! ----- Sous maille 4 -------
        nbnsma(4) = 3
        lissma(4,1) = 5
        lissma(4,2) = 3          
        lissma(4,3) = 6 
    else if (typma .eq. 'QU4') then
        nbssma=2
! ----- Sous maille 1 -------
        nbnsma(1) = 3
        lissma(1,1) = 1
        lissma(1,2) = 2           
        lissma(1,3) = 3
! ----- Sous maille 2 -------
        nbnsma(2) = 3
        lissma(2,1) = 3
        lissma(2,2) = 4           
        lissma(2,3) = 1        
    else if (typma .eq. 'QU8') then
        nbssma=6
! ----- Sous maille 1 -------
        nbnsma(1) = 3
        lissma(1,1) = 1
        lissma(1,2) = 5          
        lissma(1,3) = 8
! ----- Sous maille 2 -------
        nbnsma(2) = 3
        lissma(2,1) = 5
        lissma(2,2) = 2           
        lissma(2,3) = 6
! ----- Sous maille 3 -------
        nbnsma(3) = 3
        lissma(3,1) = 6
        lissma(3,2) = 3           
        lissma(3,3) = 7
! ----- Sous maille 4 -------
        nbnsma(4) = 3
        lissma(4,1) = 7
        lissma(4,2) = 4           
        lissma(4,3) = 8
! ----- Sous maille 5 -------
        nbnsma(5) = 3
        lissma(5,1) = 5
        lissma(5,2) = 6           
        lissma(5,3) = 7
! ----- Sous maille 6 -------
        nbnsma(6) = 3
        lissma(6,1) = 7
        lissma(6,2) = 8           
        lissma(6,3) = 5
    else if (typma .eq. 'QU9') then
        nbssma=8
! ----- Sous maille 1 -------
        nbnsma(1) = 3
        lissma(1,1) = 1
        lissma(1,2) = 5           
        lissma(1,3) = 9

! ----- Sous maille 2 -------
        nbnsma(2) = 3
        lissma(2,1) = 5
        lissma(2,2) = 2           
        lissma(2,3) = 9

! ----- Sous maille 3 -------
        nbnsma(3) = 3
        lissma(3,1) = 2
        lissma(3,2) = 6         
        lissma(3,3) = 9
   
! ----- Sous maille 4 -------
        nbnsma(4) = 3
        lissma(4,1) = 6
        lissma(4,2) = 3          
        lissma(4,3) = 9

! ----- Sous maille 5 -------
        nbnsma(5) = 3
        lissma(5,1) = 3
        lissma(5,2) = 7           
        lissma(5,3) = 9

! ----- Sous maille 6 -------
        nbnsma(6) = 3
        lissma(6,1) = 7
        lissma(6,2) = 4           
        lissma(6,3) = 9
  
! ----- Sous maille 7 -------
        nbnsma(7) = 3
        lissma(7,1) = 4
        lissma(7,2) = 8         
        lissma(7,3) = 9
 
! ----- Sous maille 8 -------
        nbnsma(8) = 3
        lissma(8,1) = 8
        lissma(8,2) = 1          
        lissma(8,3) = 9
           
    else
        ASSERT(.false.)
    end if
!
!
end subroutine
    