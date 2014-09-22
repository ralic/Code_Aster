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
#include "asterfort/codent.h"
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
    character(len=6) ::  k6
    character(len=8) ::  nomma2
    character(len=32) :: pheno2
    character(len=32) :: ncomr, ncomc, ncomk, ncomp
    integer :: nbpar, nbr, nbc, nbk, nbcomp, icomp
!
!-----------------------------------------------------------------------
    integer :: i, j, ipar
!-----------------------------------------------------------------------
    icodre = 1
    pheno2=pheno
    nomma2=nommat
!    
    ncomp = nommat//'.MATERIAU.NOMRC         '
    call jelira(ncomp, 'LONUTI', nbcomp)
    call jeveuo(ncomp, 'L', icomp)
    do i = 1, nbcomp
       if (pheno2 .eq. zk32(icomp+i-1)) then            
          call codent(i, 'D0', k6)
          ncomr = nomma2//'.CPT.'//k6//'.VALR        '
          ncomc = nomma2//'.CPT.'//k6//'.VALC        '
          ncomk = nomma2//'.CPT.'//k6//'.VALK        '
          call jelira(ncomr, 'LONUTI', nbr)
          call jelira(ncomc, 'LONUTI', nbc)
          call jelira(ncomk, 'LONUTI', nbk)
          call jeveuo(ncomk, 'L', ipar)
          nbpar = nbr + nbc + nbk/2
          do j = 1, nbpar
             if (para .eq. zk16(ipar+j-1)) then
               icodre = 0
             endif
          end do
       endif
    end do  
! FIN ------------------------------------------------------------------
end subroutine
