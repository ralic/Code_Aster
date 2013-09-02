subroutine lcdvin(fami, kpg, ksp, comp, mod,&
                  imat, matcst, nvi, nmat, vini,&
                  coeft, x, dtime, sigi, dvin,&
                  iret)
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
! ======================================================================
!     ----------------------------------------------------------------
!     ROUTINE D AIGUILLAGE
!     ----------------------------------------------------------------
!     INTEGRATION DE LOIS DE COMPORTEMENT ELASTO-VISCOPLASTIQUE
!     PAR UNE METHODE DE RUNGE KUTTA. ROUTINE D'AIGUILLAGE
!     ----------------------------------------------------------------
!     IN  FAMI    FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!         KPG,KSP NUMERO DU (SOUS)POINT DE GAUSS
!         COMP     :  NOM DU MODELE DE COMPORTEMENT
!         MOD     :  TYPE DE MODELISATION
!         IMAT    :  ADRESSE DU MATERIAU CODE
!         MATCST  :  NATURE DES PARAMETRES INELASTIQUES
!         NVI     :  NOMBRE DE VARIABLES INTERNES
!         NMAT    :  NOMBRE DE PARAMETRES MATERIAU INELASTIQUE
!         VINI    :  VARIABLES INTERNES A T
!         COEFT   :  COEFFICIENTS MATERIAU INELASTIQUE A T
!         X       :  INTERVALE DE TEMPS ADAPTATIF
!         DTIME   :  INTERVALE DE TEMPS
!         SIGI    :  CONTRAINTES A L'INSTANT COURANT
!     OUT DVIN    :  DERIVEES DES VARIABLES INTERNES A T
!         IRET    :  CODE RETOUR
!     ----------------------------------------------------------------
!
#include "asterfort/norton.h"
#include "asterfort/rkdcha.h"
#include "asterfort/rkdhay.h"
#include "asterfort/rkdvec.h"
    integer :: kpg, ksp, imat, nmat, nvi, iret
    character(len=16) :: loi, comp(*)
    character(len=8) :: mod
    character(len=*) :: fami
    character(len=3) :: matcst
    real(kind=8) :: x, dtime, sigi(6), coeft(nmat), vini(nvi), dvin(nvi)
!
    loi=comp(1)
    if (loi .eq. 'VISCOCHAB') then
        call rkdcha(nvi, vini, coeft, nmat, sigi,&
                    dvin)
!
    else if (loi.eq.'VENDOCHAB') then
        call rkdvec(fami, kpg, ksp, imat, matcst,&
                    nvi, vini, coeft, x, dtime,&
                    nmat, sigi, dvin)
!
    else if (loi.eq.'HAYHURST') then
        call rkdhay(mod, nvi, vini, coeft, nmat,&
                    sigi, dvin, iret)
!
    else if (loi.eq.'NORTON') then
        call norton(nvi, vini, coeft, nmat, sigi,&
                    dvin, iret)
!
    endif
end subroutine
