subroutine xpente(pl, cnse, n1, n2, n3,&
                  n4, n5, n6)
    implicit none
!
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    integer :: pl, n1, n2, n3, n4, n5, n6, cnse(6, 4)
!     ------------------------------------------------------------------
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
! person_in_charge: samuel.geniaut at edf.fr
!
!                      DÉCOUPER LE SOUS-PENTA EN SOUS-TETRAS
!
!     ENTREE
!       PL                : PLACE DU 1ER SOUS-TETRA DANS CNSE
!       N1,N2,N3,N4,N5,N6 : NUMEROS DES NOEUDS DU PENTA
!
!     SORTIE
!       CNSE      : CONNECTIVITE NOMBRE DE SOUS-ÉLÉMENTS (TÉTRAS)
!     ------------------------------------------------------------------
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
!     ON REMPLIT 3 SOUS TETRAS DE CNSE À PARTIR DE LA PLACE PL
    cnse(pl,1)=n1
    cnse(pl,2)=n5
    cnse(pl,3)=n2
    cnse(pl,4)=n6
!
    cnse(pl+1,1)=n4
    cnse(pl+1,2)=n5
    cnse(pl+1,3)=n1
    cnse(pl+1,4)=n6
!
    cnse(pl+2,1)=n1
    cnse(pl+2,2)=n2
    cnse(pl+2,3)=n3
    cnse(pl+2,4)=n6
!
    call jedema()
end subroutine
