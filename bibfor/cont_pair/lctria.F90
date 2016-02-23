subroutine lctria(nbpint,nbtri,resu)
    
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
#include "asterfort/assert.h"
!
    integer, intent(in) :: nbpint
    integer, intent(out) :: nbtri
    integer, intent(out) :: resu(6,3)
! ----------------------------------------------------------------------
!         TRIANGULATION D'UN POLYGONE CONVEXE DE 3 A 8 NOEUDS
! ----------------------------------------------------------------------
! IN         NBPINT       NOMBRE DE POINT CONSTITUANT L'INTERSECTION
! OUT        NBTRI        NOMBRE DE TRIANGLE DU DECOUPAGE
! OUT        RESU         NON L'OBJET JEVEUX RESULTAT (connectivité)
! ----------------------------------------------------------------------
!
! ----------------------------------------------------------------------
!  
! --- Nombre de triangle dans le découpage -----------------------------
!
    if (nbpint.ge.3) then
        nbtri=nbpint-2
    else
        ASSERT(.false.)
    endif
!
    ASSERT(nbpint.le.8)
! --- DECOUPAGE
    if (nbtri.eq.1) then    
        resu(1,1) =1
        resu(1,2) =2
        resu(1,3) =3
    else if (nbtri.eq.2) then
        resu(1,1) =1
        resu(1,2) =2
        resu(1,3) =3
    
        resu(2,1) =3
        resu(2,2) =4
        resu(2,3) =1
    else if (nbtri.eq.3) then
        resu(1,1) =1
        resu(1,2) =2
        resu(1,3) =3
    
        resu(2,1) =3
        resu(2,2) =4
        resu(2,3) =5

        resu(3,1) =5
        resu(3,2) =1
        resu(3,3) =3
    else if (nbtri.eq.4) then
        resu(1,1) =1
        resu(1,2) =2
        resu(1,3) =3
    
        resu(2,1) =3
        resu(2,2) =4
        resu(2,3) =5

        resu(3,1) =5
        resu(3,2) =6
        resu(3,3) =1
        
        resu(4,1) =1
        resu(4,2) =3
        resu(4,3) =5
    else if (nbtri.eq.5) then
        resu(1,1) =1
        resu(1,2) =2
        resu(1,3) =3
    
        resu(2,1) =3
        resu(2,2) =4
        resu(2,3) =5

        resu(3,1) =5
        resu(3,2) =6
        resu(3,3) =7
        
        resu(4,1) =7
        resu(4,2) =1
        resu(4,3) =3
    
        resu(5,1) =3
        resu(5,2) =5
        resu(5,3) =7
    else if (nbtri.eq.6) then
        resu(1,1) =1
        resu(1,2) =2
        resu(1,3) =3
    
        resu(2,1) =3
        resu(2,2) =4
        resu(2,3) =5

        resu(3,1) =5
        resu(3,2) =6
        resu(3,3) =7
        
        resu(4,1) =7
        resu(4,2) =8
        resu(4,3) =1
    
        resu(5,1) =1
        resu(5,2) =3
        resu(5,3) =5

        resu(6,1) =5
        resu(6,2) =7
        resu(6,3) =1
    end if

end subroutine
