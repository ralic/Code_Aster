subroutine cgajpa(para, notype, nbpara, linopa, litypa, nxpara)
!
    implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
    integer :: nbpara, nxpara
    character(len=*) :: para, notype, litypa(nxpara), linopa(nxpara)

!
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
! person_in_charge: emricka.julan at edf.fr
!
!     SOUS-ROUTINE DE L'OPERATEUR CALC_G
!
!     BUT : AJOUT D'UN COUPLE (PARAMETRE,TYPE)
!           A UNE LISTE linopa/litypa
!
! ----------------------------------------------------------------------
!     IN  : PARA   : NOM DE PARAMETRE CORRESPONDANT A linopa
!     IN  : typ : NOM DU TYPE CORRESPONDANT A litypa
!     I/O : NBPARA : NOMBRE DE PARAMETRE
!     I/O : linopa : TABLEAU DES NOM DE PARAMETRE
!     I/O : litypa : TABLEAU DES NOMS DE TYPE
!     IN  : nxpara : NOMBRE MAXIMUM DE PARAMETRE
!
! ----------------------------------------------------------------------
!
    integer :: i
!
! ----------------------------------------------------------------------
!     

    ASSERT(para .ne. ' ')
      
!   1. RECHERCHE SI LE PARAMETRE EXISTE DEJA :
!   ------------------------------------------

    if (nbpara.ne.0) then 
        do 10 i = 1, nbpara
            if (linopa(i) .eq. para) goto 9999
10      enddo
    endif

!    2. IL S'AGIT D'UN NOUVEAU PARAMETRE ON L'AJOUTE :
!    -------------------------------------------------
     nbpara = nbpara+1  
     ASSERT(nbpara .le. nxpara)
     linopa(nbpara) = para
     litypa(nbpara) = notype

9999 continue
!
end subroutine
