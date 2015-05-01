subroutine xcninv(nnotot, nse, nnop, nno, jcnset,&
                  cninv)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
    integer :: nnotot, nse, nnop, nno, jcnset
    integer :: cninv(nnotot, nse+1)
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
!
!     BUT:
!         CALCUL DE LA CONNECTIVITE INVERSE DES SOUS ELEMENTS
!         DE L'ELEMENT XFEM PARENT (EN 2D).
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   NNOTOT : NOMBRE TOTAL DE NOEUDS (POINTS D'INTERSECTION INCLUS)
! IN   NSE    : NOMBRE TOTAL DE SOUS ELEMENT DE L'ELEMENT PARENT
! IN   NNOP   : NOMBRE DE NOEUDS DE L'ELEMENT PARENT (POINTS)
!                D'INTERSECTION EXCLUS
! IN   NNO    : NOMBRE DE NOEUDS DU SOUS-ELEMENT DE REFERENCE
! IN   JCNSET : ADRESSE DANS ZI DE LA CONNECTIVITE DES SOUS-ELEMENTS
!
!      SORTIE :
!-------------
! OUT  CNINV  : TABLEAU DE LA CONNECTIVITE INVERSE
!
! ......................................................................
!
!
!
!
    integer :: ise, in, ino, jno
!
! --- RÉCUPÉRATION DE LA SUBDIVISION DE L'ÉLÉMENT PARENT
! --- EN NSE SIMPLEXES
!
! ------------------- BOUCLE SUR LES NSE SOUS-ÉLÉMENTS  ----------------
!
    do 110 ise = 1, nse
!
! ------------- BOUCLE SUR LES SOMMETS DU SOUS-ÉLÉMENTS  ---------------
!
        do 111 in = 1, nno
!
            ino=zi(jcnset-1+nno*(ise-1)+in)
! ------- NUMÉROTATION PROPRE A LA CONNECTIVITÉ INVERSE
            if (ino .lt. 1000) then
                jno=ino
            else
                jno=ino-1000+nnop
            endif
! ------- STOCKAGE
            cninv(jno,1)=cninv(jno,1)+1
            ASSERT(cninv(jno, 1).le.nse)
            cninv(jno,cninv(jno,1)+1)=ise
!
111      continue
!
110  end do
!
end subroutine
