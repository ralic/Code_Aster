subroutine dctest(typma, lissma, nbnsma, nbssma,typesm)
   
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
    character(len=8),intent(out) :: typesm
!
! ----------------------------------------------------------------------
!     ROUTINE APPROXIMATION GEOMETRIQUE DECOUPE TEST
! ----------------------------------------------------------------------
!   IN        TYPMA      TYPE DE LA MAILLE MAITRE
!   OUT       LISSMA     LISTE DES NOEUDS DES SOUS-MAILLES
!   OUT       NBNSMA     VECTEUR NOEUDS PAR SOUS-MAILLE
!   OUT       NBSSMA     NOMBRE DE SOUS-MAILLES LINEAIRE     
!   OUT       TYPESM     TYPE DES SOUS MAILLES DECOUPES 
! ----------------------------------------------------------------------
!

! ----------------------------------------------------------------------
!
    if (typma .eq. 'SE2') then
        typesm='SE2'
        nbssma= 1
! ----- Sous maille 1 -------
        nbnsma(1) = 2
        lissma(1,1) = 1
        lissma(1,2) = 2 
    elseif (typma .eq. 'SE3') then
        typesm='SE2'
        nbssma= 1
! ----- Sous maille 1 -------
        nbnsma(1) = 2
        lissma(1,1) = 1
        lissma(1,2) = 2 
    elseif (typma .eq. 'TR3') then
        typesm='TR3'
        nbssma = 1
! ----- Sous maille 1 -------
        nbnsma(1) = 3
        lissma(1,1) = 1
        lissma(1,2) = 2           
        lissma(1,3) = 3       
    else if (typma .eq. 'TR6') then
        typesm='TR3'
        nbssma=1
! ----- Sous maille 1 -------
        nbnsma(1) = 3
        lissma(1,1) = 1
        lissma(1,2) = 2           
        lissma(1,3) = 3
    else if (typma .eq. 'QU4') then
        typesm='TR3'
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
        typesm='TR3'
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
    else if (typma .eq. 'QU9') then
        typesm='TR3'
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
    else
        ASSERT(.false.)
    end if
!
!
end subroutine
    