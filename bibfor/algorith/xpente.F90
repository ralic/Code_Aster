subroutine xpente(pl, cnse, n)
    implicit none
!
#include "jeveux.h"
    integer :: pl, n(18), cnse(6, 10)
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
!
!                      DÉCOUPER LE SOUS-PENTA EN SOUS-TETRAS
!
!     ENTREE
!       PL                : PLACE DU 1ER SOUS-TETRA DANS CNSE
!       N1,N2,N3,N4,...N18 : NUMEROS DES NOEUDS DU PENTA
!
!     SORTIE
!       CNSE      : CONNECTIVITE NOMBRE DE SOUS-ÉLÉMENTS (TÉTRAS)
!     ------------------------------------------------------------------
!
! ----------------------------------------------------------------------
!
!
!     ON REMPLIT 3 SOUS TETRAS DE CNSE À PARTIR DE LA PLACE PL
!
    cnse(pl,1)=n(1)
    cnse(pl,2)=n(5)
    cnse(pl,3)=n(2)
    cnse(pl,4)=n(6)
    cnse(pl,5)=n(16)
    cnse(pl,6)=n(11)
    cnse(pl,7)=n(7)
    cnse(pl,8)=n(18)
    cnse(pl,9)=n(14)
    cnse(pl,10)=n(17)
!
    cnse(pl+1,1)=n(4)
    cnse(pl+1,2)=n(5)
    cnse(pl+1,3)=n(1)
    cnse(pl+1,4)=n(6)
    cnse(pl+1,5)=n(13)
    cnse(pl+1,6)=n(16)
    cnse(pl+1,7)=n(10)
    cnse(pl+1,8)=n(15)
    cnse(pl+1,9)=n(14)
    cnse(pl+1,10)=n(18)
!
    cnse(pl+2,1)=n(1)
    cnse(pl+2,2)=n(2)
    cnse(pl+2,3)=n(3)
    cnse(pl+2,4)=n(6)
    cnse(pl+2,5)=n(7)
    cnse(pl+2,6)=n(8)
    cnse(pl+2,7)=n(9)
    cnse(pl+2,8)=n(18)
    cnse(pl+2,9)=n(17)
    cnse(pl+2,10)=n(12)
!
end subroutine
