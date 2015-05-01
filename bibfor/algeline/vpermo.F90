subroutine vpermo(lmasse, lraide, nbprop, vecp, valp,&
                  excl, omecor, ernorm)
    implicit none
#include "jeveux.h"
#include "asterc/r8miem.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/mrmult.h"
#include "asterfort/wkvect.h"
#include "blas/daxpy.h"
#include "blas/dscal.h"
    integer :: lmasse, lraide, nbprop, excl(*)
    real(kind=8) :: vecp(*), valp(*), omecor, ernorm(*)
!     ------------------------------------------------------------------
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
!     CALCUL DE LA NORME D'ERREUR MODALE
!     ( IE NORME D'ERREUR SUR LES VALEURS ET VECTEURS PROPRES.)
!     ------------------------------------------------------------------
!     PROBLEME GENERALISE :  (LRAIDE)*VECP = VALP *(LMASSE)*VECP
!
!                   !! LRAIDE * VECP  - VALP * LMASSE * VECP !!
!       ERNORM   =     -------------------------------------
!                           !! LRAIDE * VECP !!
!     ------------------------------------------------------------------
!     REFERENCE: BATHE ET WILSON
!     ------------------------------------------------------------------
! IN  LMASSE : IS : DESCRIPTEUR MATRICE DE "MASSE"
! IN  LRAIDE : IS : DESCRIPTEUR MATRICE DE "RAIDEUR"
! IN  NBPROP : IS : NOMBRE DE VALEURS ET DE VECTEURS PROPRES
! IN  VECP   : R8 : TABLEAU DES VECTEURS PROPRES
! IN  VALP   : R8 : TABLEAU DES VALEURS PROPRES
! IN  EXCL   : IS : TABLEAU DES NON-EXCLUS
! IN  FCORIG : R8 : FREQUENCE DE CORPS RIGIDE
! OUT ERNORM : R8 : TABLEAU DES NORMES D'ERREUR
!     ------------------------------------------------------------------
    real(kind=8) :: anorm1, anorm2
!
!
!     --- SEUIL EN PULSATION POUR LES MODES DE CORPS RIGIDE ---
!-----------------------------------------------------------------------
    integer :: i, iaux1, iaux2, j, neq, ivec
    real(kind=8) :: xseuil, rmin, raux
!-----------------------------------------------------------------------
    call jemarq()
    xseuil = omecor
!
!     ------------------------------------------------------------------
!     ---------------------- DONNEES SUR LES MATRICES ------------------
!     ------------------------------------------------------------------
    neq = zi(lmasse+2)
!     ------------------------------------------------------------------
!     -------------- ALLOCATION DES ZONES DE TRAVAIL -------------------
!     ------------------------------------------------------------------
    call wkvect('&&VPERMO.TAMPON.PROV_1', 'V V R', neq, iaux1)
    call wkvect('&&VPERMO.TAMPON.PROV_2', 'V V R', neq, iaux2)
!     ------------------------------------------------------------------
!     ---------------------- CALCUL DES NORMES D'ERREUR ----------------
!     ------------------------------------------------------------------
    rmin=100.d0*r8miem()
!
!        --- NON PRISE EN COMPTE DES DDLS EXCLUS
    do 15 i = 1, neq
        raux=excl(i)
        call dscal(nbprop, raux, vecp(i), neq)
15  end do
!
    do 30 i = 1, nbprop
        ivec=(i-1)*neq+1
        call mrmult('ZERO', lraide, vecp(ivec), zr(iaux1), 1,&
                    .false._1)
        call mrmult('ZERO', lmasse, vecp(ivec), zr(iaux2), 1,&
                    .false._1)
        anorm1 = 0.d0
        do 20 j = 1, neq
            raux=zr(iaux1+j-1)
            anorm1 = anorm1+raux*raux*excl(j)
20      continue
        raux=-valp(i)
        call daxpy(neq, raux, zr(iaux2), 1, zr(iaux1),&
                   1)
        anorm2 = 0.d0
        do 25 j = 1, neq
            raux=zr(iaux1+j-1)
            anorm2 = anorm2+raux*raux*excl(j)
25      continue
!
!
        if (abs(valp(i)) .gt. xseuil) then
            if (anorm1 .ge. rmin) then
                ernorm(i)= sqrt(anorm2/anorm1)
            else
                ernorm(i)= 1.d+70
            endif
        else
            ernorm(i) = abs(valp(i)) * sqrt(anorm2)
        endif
30  end do
!
!     ----------------------------------------------------------------
!     -------------- DESALLOCATION DES ZONES DE TRAVAIL --------------
!     ----------------------------------------------------------------
!
    call jedetr('&&VPERMO.TAMPON.PROV_1')
    call jedetr('&&VPERMO.TAMPON.PROV_2')
!
    call jedema()
end subroutine
