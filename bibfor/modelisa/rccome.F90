subroutine rccome(nommat, pheno, phenom, icodre)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "jeveux.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mess.h"
    character(len=*), intent(in) :: nommat, pheno
    character(len=*), intent(out) :: phenom
    integer, intent(out) :: icodre
! ----------------------------------------------------------------------
!     OBTENTION DU COMPORTEMENT COMPLET D'UN MATERIAU DONNE A PARTIR
!     D'UN PREMISSE
!
!     ARGUMENTS D'ENTREE:
!        NOMMAT : NOM DU MATERIAU
!        PHENO  : NOM DU PHENOMENE INCOMPLET
!     ARGUMENTS DE SORTIE:
!        PHENOM: NOM DU PHENOMENE COMPLET
!     ICODRE : POUR CHAQUE RESULTAT, 0 SI ON A TROUVE, 1 SINON
!
!
!
! ----------------------------------------------------------------------
! DEB ------------------------------------------------------------------
    character(len=32) :: ncomp
!-----------------------------------------------------------------------
    integer :: i, icomp, nbcomp
!-----------------------------------------------------------------------
    icodre = 0
    ncomp = nommat//'.MATERIAU.NOMRC         '
    call jelira(ncomp, 'LONUTI', nbcomp)
    call jeveuo(ncomp, 'L', icomp)
    do 10 i = 1, nbcomp
        if (pheno .eq. zk16(icomp+i-1)(1:len(pheno))) then
            phenom=zk16(icomp+i-1)
            goto 999
        endif
10  end do
    icodre = 1
    call u2mess('A', 'ELEMENTS2_63')
    goto 999
!
999  continue
! FIN ------------------------------------------------------------------
end subroutine
