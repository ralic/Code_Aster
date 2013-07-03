subroutine radial(nbsig, sigm, sigp, indm, indp,&
                  icine, xm, xp, normdn)
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
!     BUT:
!       CALCUL DE L'INDICATEUR LOCAL DE RADIALITE
!       NORME DE LA DIFFERENCE DES NORMALES
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   NBSIG    : CDIMENSION DES VECTEURS CONTRAINTES
! IN   SIGM     : CONTRAINTES INSTANT + AVEC RAC2
! IN   SIGP     : CONTRAINTES INSTANT - AVEC RAC2
! IN   INDM     : INDICATEUR PLASTICITE INSTANT -
! IN   INDP     : INDICATEUR PLASTICITE INSTANT +
! IN   ICINE    : INDICATEUR ECROUISSAGE CINAMATIQUE
! IN   XM       : VARIABLES INTERNES X INSTANT - AVEC RAC2
! IN   XP       : VARIABLES INTERNES X INSTANT + AVEC RAC2
!
!      SORTIE :
!-------------
! OUT  NORMDN   : INDICATEUR DE PERTE DE RADIALITE=NORME DE DN
!
! ......................................................................
    implicit none
#include "asterc/r8prem.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/ddot.h"
#include "blas/dscal.h"
    integer :: nbsig, icine, i
    real(kind=8) :: n1(6), n2(6), xm(6), xp(6), indm, indp
    real(kind=8) :: tensm(6), tensp(6), normdn, sigm(nbsig), sigp(nbsig)
    real(kind=8) :: zernor, devm(6), devp(6), smeq, speq
    real(kind=8) :: dn1n2(6), ndn, preci, trt1, trt2
! ......................................................................
!
    if ((nint(indm).gt.0.d0) .and. (nint(indp).gt.0.d0)) then
!
        call dcopy(nbsig, sigm, 1, tensm, 1)
        call dcopy(nbsig, sigp, 1, tensp, 1)
!
        zernor = r8prem()
        if (icine .eq. 1) then
!
            call daxpy(nbsig, -1.d0, xm, 1, tensm,&
                       1)
            call daxpy(nbsig, -1.d0, xp, 1, tensp,&
                       1)
!
        endif
!
!        PART HYDROSTATIQUE DES CONTRAINTES
        trt1 = ( tensm(1)+tensm(2)+tensm(3))/3.d0
        trt2 = ( tensp(1)+tensp(2)+tensp(3))/3.d0
!        PART DEVIATORIQUE DES CONTRAINTES
!
        call dcopy(nbsig, tensm, 1, devm, 1)
        call dcopy(nbsig, tensp, 1, devp, 1)
        do 10 i = 1, 3
            devm(i)=devm(i)-trt1
            devp(i)=devp(i)-trt2
10      continue
!         CALL DSCAL(NBSIG-3,SQRT(2.D0),DEVM(4),1)
!         CALL DSCAL(NBSIG-3,SQRT(2.D0),DEVP(4),1)
        smeq=sqrt(ddot(nbsig,devm,1,devm,1))
        speq=sqrt(ddot(nbsig,devp,1,devp,1))
        preci=1.d3*zernor*max(smeq,speq)
!
! ----      DANS LE CAS OU NORME(TENSM) = 0  OU NORME(DTENSMA) = 0 :
! ----      ON MET L'INDICATEUR A 0 :
!           -----------------------
        if (smeq .le. preci .or. speq .le. preci) then
            normdn = 0.d0
        else
            call dcopy(nbsig, devm, 1, n1, 1)
            call dcopy(nbsig, devp, 1, n2, 1)
            call dscal(nbsig, 1.d0/smeq, n1, 1)
            call dscal(nbsig, 1.d0/speq, n2, 1)
            call dcopy(nbsig, n1, 1, dn1n2, 1)
!
!          CALCUL DE LA DIFFERENCE N1 - N2
!
            call daxpy(nbsig, -1.d0, n2, 1, dn1n2,&
                       1)
            ndn=sqrt(ddot(nbsig,dn1n2,1,dn1n2,1))
            normdn = ndn/2.d0
        endif
!
    else
!        PAS D'INDICATEUR. ON NE FAIT RIEN
        normdn = 0.d0
!
    endif
!
end subroutine
