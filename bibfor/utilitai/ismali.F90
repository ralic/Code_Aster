function ismali(typma)
    implicit none
#include "asterf_types.h"
#include "asterfort/assert.h"
    aster_logical :: ismali
    character(len=8) :: typma
! ----------------------------------------------------------------------
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
! person_in_charge: samuel.geniaut at edf.fr
! ----------------------------------------------------------------------
!
! FONCTION VALANT TRUE SI LE TYPE DE MAILLE EST LINEAIRE
!        (AUTANT DE NOEUDS QUE DE NOEUDS SOMMET)
!
! ----------------------------------------------------------------------
!
! IN   TYPMA  : TYPE DE MAILLE (TYPE_MAILLE)
! OUT  ISMALI : TRUE SI LE TYPE DE MAILLE EST LINEAIRE
!
    if (typma .eq. 'POI1' .or. typma .eq. 'SEG2' .or. typma .eq. 'TRIA3' .or. typma .eq.&
        'QUAD4' .or. typma .eq. 'TETRA4' .or. typma .eq. 'PYRAM5' .or. typma .eq. 'PENTA6'&
        .or. typma .eq. 'HEXA8') then
!
        ismali=.true.
!
        elseif (typma.eq.'SEG3'   .or.&
     &        typma.eq.'TRIA6'  .or.&
     &        typma.eq.'TRIA7'  .or.&
     &        typma.eq.'QUAD8'  .or.&
     &        typma.eq.'QUAD9'  .or.&
     &        typma.eq.'TETRA10'.or.&
     &        typma.eq.'PYRAM13'.or.&
     &        typma.eq.'PENTA15'.or.&
     &        typma.eq.'PENTA18'.or.&
     &        typma.eq.'HEXA20' .or.&
     &        typma.eq.'HEXA27' ) then
!
        ismali=.false.
!
    else
!
        ASSERT(.false.)
!
    endif
!
end function
