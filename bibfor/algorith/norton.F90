subroutine norton(nvi, vini, coeft, nmat, sigi,&
                  dvin, iret)
    implicit none
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
! ==================================================================
!      MODELE VISCOPLASTIQUE DE NORTON
! ==================================================================
!     CETTE ROUTINE FOURNIT LA DERIVEE DE L ENSEMBLE DES VARIABLES
!     INTERNES DU MODELE
!       IN  NVI     :  NOMBRE DE VARIABLES INTERNES
!           VINI    :  VARIABLES INTERNES A T
!           COEFT   :  COEFFICIENTS MATERIAU INELASTIQUE A T
!           NMAT    :  DIMENSION MAXI DE COEFT
!           SIGP    :  CONTRAINTES A L'INSTANT COURANT, AVEC SQRT(2)
!           DEPS    :  INCREMENT DE DEFORMATIONS, AVEC SQRT(2)
!     OUT:
!           DVIN    :  DERIVEES DES VARIABLES INTERNES A T
!           IRET    :  CODE RETOUR =0 SI OK, =1 SI PB
!     ----------------------------------------------------------------
#include "asterc/r8miem.h"
#include "asterfort/lcdevi.h"
#include "asterfort/lcnrts.h"
#include "asterfort/r8inir.h"
    integer :: iret, itens, ndi, nmat, nvi, ndt
    real(kind=8) :: coeft(nmat), vini(nvi), dvin(nvi), smx(6), sigi(6)
    real(kind=8) :: dp, n, unsurk, grj2v, epsi
!     ----------------------------------------------------------------
    common /tdim/   ndt,    ndi
!     ----------------------------------------------------------------
!
    iret=0
!     INITIALISATION DES DERIVEES DES VARIABLES INTERNES A ZERO
    call r8inir(7, 0.d0, dvin, 1)
!
! --    COEFFICIENTS MATERIAU
    n = coeft(1)
    unsurk = coeft(2)
!
!     ZERO NUMERIQUE ABSOLU
    epsi=r8miem()
!
!------------ CALCUL DU TENSEUR DEVIATORIQUE DES CONTRAINTES ---
!
    call lcdevi(sigi, smx)
!
!------------CALCUL DU DEUXIEME INVARIANT DE CONTRAINTE  -------
!
    grj2v = lcnrts(smx )
!
!------ EQUATION DONNANT LA DERIVEE DE LA DEF VISCO PLAST
!
    if (grj2v .gt. epsi) then
!
        dp=(grj2v*unsurk)**n
!
!        INUTILE DE CALCULER DES DEFORMATIONS PLASTIQUES MINUSCULES
        if (dp .gt. 1.d-10) then
!
            do 12 itens = 1, ndt
                dvin(itens)=1.5d0*dp*smx(itens)/grj2v
12          continue
            dvin(7)=dp
!
        endif
!
    endif
end subroutine
