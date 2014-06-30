subroutine lceob1(intmax, tole, eps, bm, dm,&
                  lambda, mu, alpha, ecrob, ecrod,&
                  seuil, bdim, b, d, mult,&
                  elas, dbloq, iret)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterfort/ceobfb.h"
#include "asterfort/ceobfd.h"
#include "asterfort/dfbdb.h"
#include "asterfort/r8inir.h"
#include "asterfort/utmess.h"
    real(kind=8) :: eps(6)
    real(kind=8) :: bm(6), dm, b(6), d, mult
    real(kind=8) :: lambda, mu, alpha, seuil, ecrob, ecrod
    real(kind=8) :: tole
!
    integer :: intmax, iret, bdim
!
    logical(kind=1) :: elas, dbloq
! ----------------------------------------------------------------------
!     LOI DE COMPORTEMENT DU MODELE D'ENDOMMAGEMENT ANISOTROPE
!     ROUTINE DE RESOLUTION DU SYSTEME NON LINEAIRE
!     ALGORITHME DE NEWTON
!
!
!
!  IN INTMAX  : NBRE D'ITERATION MAX POUR LE NEWTON LOCAL
!  IN TOLE    : RESIDU TOLERE POUR LE NEWTON LOCAL
!  IN  BDIM   : DIMENSION DE L'ESPACE
!  IN  CRIT   : CRITERES DE CONVERGENCE LOCAUX
!  IN  EPSM   : DEFORMATION EN T- REPERE GLOBAL
!  IN  DEPS   : INCREMENT DE DEFORMATION
!  IN  BM DM  : VARIABLES INTERNES EN T-
!  IN  LAMBDA : /
!  IN  MU     : / COEFFICIENTS DE LAME
!  IN  ALPHA  : /
!  IN  ECROB  : /
!  IN  ECROD  : / PARAMETRES DU MODELE
!  IN  SEUIL  : SEUIL DU CRITERE D'ENDOMMAGEMENT
!  IN  BDIM   : DIMENSION DE L ESPACE
!
! OUT  B D    : VARIABLES INTERNES EN T+
! OUT MULT    : MULTIPLICATEUR PLASTIQUE DU PRINCIPE DE NORMALITE
! OUT ELAS    : ELASTIQUE OU DISSIPATION?
! OUT DBLOQ   : BLOQUAGE DE L'ENDOMMAGEMENT DE COMPRESSION
! OUT IRET    : CODE RETOUR
! ----------------------------------------------------------------------
!
    integer :: i, compte
!
    real(kind=8) :: bs, bms, deux, un, tolc
    real(kind=8) :: fb(6), dbs, fd, dd, fbm(6)
    real(kind=8) :: resb, delta1, delta2, ddg
    real(kind=8) :: normrb, rtemp, crit
    real(kind=8) :: mte1, mte2(6, 6), mte2s
    real(kind=8) :: fbs, fbsm, ksi, iksi, coupl
    real(kind=8) :: resd, dfddd, psi
    real(kind=8) :: inter1, inter2, inter3, inter4
!
    tolc=seuil*tole
!
    compte=0
    mult=0.d0
    un=1.d0
    deux=2.d0
    do 100 i = 1, 6
        b(i)=bm(i)
100  end do
    d=dm
!-------------------------------------------------------
!-------------------------------------------------------
!----CALCUL DE FB: FORCE THERMO ASSOCIEE A
!-------------------ENDOMMAGEMENT ANISOTROPE DE TRACTION
!
    call ceobfb(b, eps, lambda, mu, ecrob,&
                bdim, fb, rtemp, fbm)
!
    fbs=fb(1)
    fbsm=fbm(1)
    bs=b(1)
    bms=bm(1)
!
!----CALCUL DE FD: PARTIE POSITIVE DE LA FORCE THERMO ASSOCIEE A
!-------------------ENDOMMAGEMENT ISOTROPE DE COMPRESSION
!
    if (dbloq) then
        fd=0.d0
    else
        call ceobfd(d, eps, lambda, mu, ecrod,&
                    fd)
    endif
!
!----CALCUL DU CRITERE-------------------------------------
    coupl=sqrt(alpha*rtemp+(1-alpha)*fd**2)
    crit=coupl-seuil
!
    elas=.false.
!
    if (crit .le. tolc) then
        elas=.true.
        goto 999
!
    else
!
        resb=-bs+bms+alpha*mult*fbsm
        resd=-d+dm+(1-alpha)*mult*fd
!
        normrb=abs(resb)
!
        ddg=0.d0
!
!--------------------------------------------------------
!--BOUCLE DU NEWTON SUR LES VARIABLES INTERNES-----------
!--------------------------------------------------------
!
38      continue
        if (((crit.gt.tolc).or.(normrb.gt.tole).or.(abs(resd).gt.tole))) then
            if ((compte.lt.intmax) .and. (coupl.ne.0.d0)) then
! Rajout du test sur COUPL (fiche 15020) : lorsque c'est le cas,
! la derivee du residu est une matrice singuliere et le systeme ne
! peut etre resolu. On sort pour enclencher la decoupe du pas de temps
                if (fbs .gt. 0.d0) then
                    mte1=0.d0
                else
                    mte1=1.d0
                endif
!
                call dfbdb(3, b, eps, deux*mu, lambda,&
                           ecrob, mte2)
!
                mte2s=mte2(1,1)
!
                dfddd=0.d0
!
                if ((.not.dbloq) .and. (fd.ne.0.d0)) then
                    dfddd=-(fd+deux*ecrod)/(un-d)
                endif
!
                ksi=-mult*alpha*mte1*mte2s+un
!
                if (ksi .ne. 0.d0) then
                    iksi=un/ksi
                else
                    call utmess('F', 'ALGORITH4_54')
                endif
!
                psi=1-mult*(1-alpha)*dfddd
!
                delta1=alpha*fbsm*mte2s
!
                delta2=(1-alpha)*fd*dfddd
!
                inter1=delta1*iksi*resb
                inter3=alpha*delta1*iksi*fbsm
!
                inter2=delta2/psi*resd
                inter4=delta2/psi*(1-alpha)*fd
!
                ddg=-(crit*coupl+inter1+inter2)/(inter3+inter4)
!
                dd=resd/psi+ddg*(1-alpha)*fd/psi
                dbs=iksi*(resb+ddg*alpha*fbsm)
!
                bs=bs+dbs
                d=d+dd
!
                compte=compte+1
                mult=mult+ddg
!
!----CALCUL DE FB DANS NEWTON---------------------------
!
                call r8inir(6, 0.d0, b, 1)
!
                b(1)=bs
!
                call ceobfb(b, eps, lambda, mu, ecrob,&
                            bdim, fb, rtemp, fbm)
                fbs=fb(1)
                fbsm=fbm(1)
!
!----CALCUL DE FD: PARTIE POSITIVE DE LA FORCE THERMO ASSOCIEE A
!-------------------ENDOMMAGEMENT ISOTROPE DE COMPRESSION
!
                if (dbloq) then
                    fd=0.d0
                else
                    call ceobfd(d, eps, lambda, mu, ecrod,&
                                fd)
                endif
!
!----CALCUL DU CRITERE-------------------------------------
                coupl=sqrt(alpha*rtemp+(1-alpha)*fd**2)
                crit=coupl-seuil
!
                resb=-bs+bms+alpha*mult*fbsm
                resd=-d+dm+(1-alpha)*mult*fd
!
                normrb=abs(resb)
!
                goto 38
            else
                iret = 1
                goto 999
            endif
        endif
!
    endif
999  continue
!
end subroutine
