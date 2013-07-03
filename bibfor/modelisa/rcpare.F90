subroutine rcpare(nommat, pheno, para, icodre)
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
    character(len=*) :: nommat, pheno, para
    integer :: icodre
! ----------------------------------------------------------------------
!     VERIFICATION DE LA PRESENCE D'UNE CARACTERISTIQUE DANS UN
!     COMPORTEMENT DONNE
!
!     ARGUMENTS D'ENTREE:
!        NOMMAT  : NOM DU MATERIAU
!        PHENO   : NOM DE LA LOI DE COMPORTEMENT
!        PARA    : NOM DU PARAMETRE
!     ARGUMENTS DE SORTIE:
!     ICODRE : POUR CHAQUE RESULTAT, 0 SI ON A TROUVE, 1 SINON
!
!
!
! ----------------------------------------------------------------------
! DEB ------------------------------------------------------------------
    character(len=8) :: k8bid, nomma2
    character(len=10) :: pheno2
    character(len=32) :: ncomr, ncomc, ncomk
    integer :: nbpar, nbr, nbc, nbk
!
!-----------------------------------------------------------------------
    integer :: i, ipar
!-----------------------------------------------------------------------
    icodre = 0
    pheno2=pheno
    nomma2=nommat
!
    ncomr = nomma2//'.'//pheno2//'.VALR        '
    ncomc = nomma2//'.'//pheno2//'.VALC        '
    ncomk = nomma2//'.'//pheno2//'.VALK        '
    call jelira(ncomr, 'LONUTI', nbr, k8bid)
    call jelira(ncomc, 'LONUTI', nbc, k8bid)
    call jelira(ncomk, 'LONUTI', nbk, k8bid)
    call jeveuo(ncomk, 'L', ipar)
    nbpar = nbr + nbc + nbk/2
    do 10 i = 1, nbpar
        if (para .eq. zk8(ipar+i-1)(1:len(para))) then
            goto 999
        endif
10  end do
    icodre = 1
    goto 999
!
999  continue
! FIN ------------------------------------------------------------------
end subroutine
