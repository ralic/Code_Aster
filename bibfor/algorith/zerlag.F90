subroutine zerlag(nbddl, ideeq, vectr, vectz )
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!                             FONCTION
!    _________________________________________________________________
!   | ANNULER LES DDL DE LAGRANGE DANS UN VECTEUR (.VALE D'UN CHAMNO) |
!   |_________________________________________________________________|
!
!
!   EXEMPLES : call zerlag (neq, zi(jdeeq), zr(jvect))
!              call zerlag (neq, zi(jdeeq), vectz=zc(jvect))
!
!                     DESCRIPTIVE DES VARIABLES
!   ___________________________________________________________________
!  | IN > NBDDL  : NOMBRE DE DDL PHYSIQUES / TAILLE DU VECTEUR      [I]|
!  | IN > IDEEQ  : VECTEUR DES DESCRIPTEURS D'EQUATIONS DU NUME_DDL [I]|
!  | IN > VECTR  : VECTEUR REEL DE TAILLE NBDDL A TRAITER          [R8]|
!  |OUT <                      (SI COMPL.EQ.0 )                        |
!  | IN > VECTZ  : VECTEUR COMPLEXE DE TAILLE NBDDL A TRAITER     [C16]|
!  |OUT <                      (SI COMPL.EQ.1 )                        |
!   ___________________________________________________________________
!
#include "jeveux.h"
!   ___________________________________________________________________
!
!  - 0 - INITIALISATIONS DIVERSES
!   ___________________________________________________________________
!
!     0.1 - DECLARATION DES VARIABLES D'ENTREE/SORTIE
!
    integer :: nbddl, ideeq(2*nbddl)
    real(kind=8), intent(out), optional :: vectr(nbddl)
    complex(kind=8), intent(out), optional :: vectz(nbddl)
    character(len=1) :: typc
!
!     0.2 - DECLARATION DES VARIABLES LOCALES
!
    integer :: i, ityp
!  ____________________________________________________________________
!
!  - 1 - RECHERCHE DES DDL DE LAGRANGE ET REMPLISSAGE LES VALEURS DES
!        CORRESPONDANTS PAR DES ZEROS (REELS OU COMPLEXES SELON LE CAS)
!  ____________________________________________________________________
!
!     1.1 - CAS REEL
    if (present(vectr)) then
        do i = 1, nbddl
            ityp = ideeq(2*i)
            if (ityp .le. 0) vectr(i)=0.d0
        end do
    endif    
    if (present(vectz)) then
!     1.2 - CAS COMPLEXE
        do i = 1, nbddl
            ityp = ideeq(2*i)
            if (ityp .le. 0) vectz(i)=dcmplx(0.d0,0.d0)
        end do 
    endif
!  ____________________________________________________________________
!
end subroutine
