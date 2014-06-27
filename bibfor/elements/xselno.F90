subroutine xselno(nno, nnop, nbsig, nse, ndim,&
                  jcnset, siseno, jout2)
    implicit none
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
!
!    - FONCTIONS REALISEES :  ROUTINE X-FEM
!
!          PASSAGE DES CONTRAINTES
!          DES POINTS DE GAUSS DES SOUS-ELEMENTS :
!            * AUX NOEUDS DES ELEMENTS PARENTS
!              (OPTION 'SIGM_ELNO') ;
!            * AUX SOMMETS (NOEUDS) DES SOUS-ELEMENTS
!              (OPTION 'SISE_ELNO') ;
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!
!
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: mxval
    parameter (mxval=32*10*6)
!     EN 2D :
!     MXVAL =  6 (NBSE MAX) * 3 (NBNOSE MAX) * 4 (NBCMP MAX)-> en lineaire
!     MXVAL =  6 (NBSE MAX) * 6 (NBNOSE MAX) * 4 (NBCMP MAX)-> en quadratique
!     EN 3D :
!     MXVAL = 32 (NBSE MAX) * 4 (NBNOSE MAX) * 6 (NBCMP MAX)-> en lineaire
!     MXVAL = 32 (NBSE MAX) * 10(NBNOSE MAX) * 6 (NBCMP MAX)-> en quadratique

    integer :: ndim, nnop, nno
    integer :: nbsig, nbseco(27)
    integer :: jcnset
    integer :: jout2
    integer :: i, j, nse, ise, in, ino, ic
!
    real(kind=8) :: tmp, somsig(27, 6)
!
    real(kind=8) :: siseno(mxval)
!
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
!     TABLEAUX DE LA SOMME DES CONTRAINTES
    do 444 i = 1, nnop
        do 445 j = 1, nbsig
            somsig(i,j)=0
445      continue
444  continue
!
!     TABLEAUX DU NOMBRE DE SOUS-ELEMENTS CONNECTES AUX NOEUDS
    do 446 i = 1, nnop
        nbseco(i)=0
446  continue
!
!       BOUCLE SUR LES NSE SOUS-ÉLÉMENTS
    do 210 ise = 1, nse
!
!       BOUCLE SUR LES 4/3 SOMMETS DU SOUS-TETRA/TRIA
        do 211 in = 1, nno
            ino=zi(jcnset-1+(ndim+1)*(ise-1)+in)
            if (ino .lt. 1000) then
                nbseco(ino)=nbseco(ino)+1
                do 212 ic = 1, nbsig
                    tmp = siseno(nbsig*nno*(ise-1)+nbsig*(in-1)+ic)
                    somsig(ino,ic)=somsig(ino,ic)+tmp
212              continue
            endif
211      continue
!
210  continue
!
!     MOYENNES DES CONTRAINTES AUX NOEUDS DE L'ELEMENT PARENT
    do 300 ino = 1, nnop
        ASSERT(nbseco(ino).gt.0)
        do 310 ic = 1, nbsig
            zr(jout2-1+nbsig*(ino-1)+ic) = somsig(ino,ic) / nbseco( ino)
310      continue
300  continue
!
    call jedema()
!
end subroutine
