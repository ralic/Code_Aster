subroutine mdfrev(nbmode, vitgen, fexgen, nbrevi, dplrev,&
                  fonrev, saurev, sarevi)
    implicit none
#include "asterfort/fointe.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! CALCUL DU SECOND MEMBRE POUR UNE NON-LINEARITE DE TYPE RELA_EFFO_VITE
! ----------------------------------------------------------------------
! IN  : NBMODE : NOMBRE DE MODES NORMAUX CONSIDERES
! IN  : VITGEN : VITESSES GENERALISES AU PAS COURANT
! VAR : FEXGEN : FORCES GENERALISEES AU PAS COURANT
! IN  : NBREVI : NOMBRE DE POINTS DECRIVANT LA NON-LINEARITE
! IN  : DPLREV : TABLEAU DES DEPLACEMENTS MODAUX AUX NOEUDS DE REV
! IN  : FONREV : FONCTIONS DE NON-LINEARITE
! ----------------------------------------------------------------------
#include "asterc/r8prem.h"
    integer :: ier, icomp, nbmode, nbrevi, sarevi(*)
    real(kind=8) :: force, vitess, vitgen(*), fexgen(*), saurev(*)
    real(kind=8) :: dplrev(nbrevi, nbmode, *)
    character(len=8) :: fonc, comp, fonrev(nbrevi, *)
    integer :: i, j
!-----------------------------------------------------------------------
!
!     --- BOUCLE SUR LES NOEUDS DE NON-LINEARITE ---
!
    do i = 1, nbrevi
!
        comp = fonrev(i,2)
        fonc = fonrev(i,3)
!
        if (comp(1:2) .eq. 'DX') icomp = 1
        if (comp(1:2) .eq. 'DY') icomp = 2
        if (comp(1:2) .eq. 'DZ') icomp = 3
        if (comp(1:3) .eq. 'DRX') icomp = 4
        if (comp(1:3) .eq. 'DRY') icomp = 5
        if (comp(1:3) .eq. 'DRZ') icomp = 6
!
        vitess = 0.d0
        do j = 1, nbmode
            vitess = vitess + dplrev(i,j,icomp)*vitgen(j)
        end do
!
        saurev(i) = vitess
        sarevi(i) = 1
!
        call fointe('F ', fonc, 1, [comp], [vitess],&
                    force, ier)
        if (abs(force) .le. r8prem()) sarevi(i) = 0
!
        do j = 1, nbmode
            fexgen(j)=fexgen(j)+dplrev(i,j,icomp)*force
        end do
!
    end do
!
end subroutine
