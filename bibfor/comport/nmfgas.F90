subroutine nmfgas(fami, npg, icodma, pgl, nno,&
                  nc, ugl, effnom, pm, crit,&
                  tmoins, tplus, xlong0, a, coeffl,&
                  irram, irrap, kls, flc, effnoc,&
                  pp)
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
! ----------------------------------------------------------------------
! aslint: disable=W1504
    implicit none
#include "asterc/r8t0.h"
#include "asterfort/assert.h"
#include "asterfort/granac.h"
#include "asterfort/matela.h"
#include "asterfort/moytem.h"
#include "asterfort/nmcri4.h"
#include "asterfort/r8inir.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvgl.h"
#include "asterfort/verifm.h"
#include "asterfort/zerofr.h"
#include "blas/dcopy.h"
    integer :: nno, nc, neq, nbt, ncoeff, nitmax, iret
!-----------------------------------------------------------------------
    integer :: icodma, itemp, iter, npg
    real(kind=8) :: a, depsi, dlong0, dp, prec, sigm, t1
    real(kind=8) :: t2, tempp2, theta
!-----------------------------------------------------------------------
    parameter (neq = 12, nbt = 78, ncoeff = 7)
    real(kind=8) :: crit(*)
    real(kind=8) :: xlong0
    real(kind=8) :: coeffl(ncoeff)
    real(kind=8) :: tmoins, tplus, effnom, pm(3)
    real(kind=8) :: ugl(neq), pgl(3, 3)
    real(kind=8) :: kls(nbt), effnoc, flc, pp(3)
    character(len=*) :: fami
! ----------------------------------------------------------------------
!
!     TRAITEMENT DE LA RELATION DE COMPORTEMENT NON LINEAIRE
!     (FLUAGE + GRANDISSEMENT ) POUR LES ELEMENTS DE
!     POUTRE : CALCUL DE LA MATRICE DE RAIDEUR TANGENTE ET DES FORCES
!     NODALES.
!
! ----------------------------------------------------------------------
!
! IN  :
!       PGL    : MATRICE DE PASSAGE
!       NNO    : NOMBRE DE NOEUDS
!       NC     : NOMBRE DE DDL
!       UGL    : ACCROIS. DEPLACEMENTS EN REPERE GLOBAL
!       EFFNOM : EFFORT NORMAL ELASTIQUE PRECEDENT
!       PM     : MULTIPLICATEUR PLASTIQUE PRECEDENT
!       CRIT   : CRITERES DE CONVERGENCE LOCAUX
!       TMOINS : INSTANT PRECEDENT
!       TPLUS  : INSTANT COURANT
!       XLONG0 : LONGUEUR DE L'ELEMENT DE POUTRE AU REPOS
!       E      : MODULE D'YOUNG
!       A      : SECTION DE LA POUTRE
!       ALPHA  : COEFFICIENT DE DILATATION THERMIQUE
!       COEFFL : COEFFICIENTS CONSTANTS POUR LE FLUAGE
!                1 : N
!                2 : 1/K
!                3 : 1/M
!                4 : Q/R
!                5 : BETA
!                6 : PHI_ZERO
!                7 : L
!
! OUT : KLS    : SOUS MATRICE DE RAIDEUR TANGENTE EN REPERE LOCAL
!       FLC    : FORCE NODALE AXIALE CORRIGEE EN REPERE LOCAL
!       EFFNOC : EFFORT NORMAL CORRIGE
!       PP     : MULTIPLICATEUR PLASTIQUE COURANT
!
! *************** DECLARATION DES VARIABLES LOCALES ********************
!
    real(kind=8) :: ul(12)
    real(kind=8) :: fgrand, depgrd, rigela, depthe(1), expon
    real(kind=8) :: preci, ba, bb, fa, correc, xrig, sigp, tabs
    real(kind=8) :: e, em, tempm, tempp
    real(kind=8) :: nu, num, irram, irrap
!
! *********** FIN DES DECLARATIONS DES VARIABLES LOCALES ***************
!
! **************** COMMON COMMUN A NMCRI4 ET NMFGAS  *******************
!
    common /rconm4/ you,cfluag,sige,pmm,sdt
    real(kind=8) :: cfluag(ncoeff), you, sige, pmm, sdt
!
! ********************* DEBUT DE LA SUBROUTINE *************************
! --- INITIALISATIONS
!
    tabs = r8t0()
!
!JMP  SEMI-IMPLICITE   SEULES VALEURS POSSIBLES : THETA = 1 OU 1/2
!
    theta = crit(4)
    t1 = abs(theta-0.5d0)
    t2 = abs(theta-1.d0)
    prec = 0.01d0
    if ((t1.gt.prec) .and. (t2.gt.prec)) then
        call utmess('F', 'ALGORITH6_55')
    endif
    if (coeffl(1) .eq. 0.d0) then
        call utmess('F', 'ALGORITH7_79')
    endif
!
    call r8inir(nbt, 0.d0, kls, 1)
    call r8inir(12, 0.d0, ul, 1)
!
    call dcopy(ncoeff, coeffl, 1, cfluag, 1)
!
    call utpvgl(nno, nc, pgl, ugl, ul)
!
    sigm = effnom/a
    pmm = pm(1)
    itemp = 0
!
! --- COEFFICIENTS DE FLUAGE ET GRANDISSEMENT
    call verifm(fami, npg, 1, 'T', icodma,&
                'ELAS', 1, depthe, iret)
!
    call moytem(fami, npg, 1, '+', tempp,&
                iret)
    if (iret .eq. 1) then
        call utmess('F', 'CALCULEL_31')
    endif
    call matela(icodma, ' ', itemp, tempp, e,&
                nu)
!
    call moytem(fami, npg, 1, '-', tempm,&
                iret)
    if (iret .eq. 1) then
        call utmess('F', 'CALCULEL_31')
    endif
    call matela(icodma, ' ', itemp, tempm, em,&
                num)
    you = e
!
    dlong0 = ul(7) - ul(1)
    depsi = dlong0/xlong0
!
    depsi = depsi * theta
    tempp2=tempm+(tempp-tempm)*theta
!
    call granac(fami, 1, 1, icodma, '        ',&
                'LEMAITRE_IRRA   ', irrap, irram, tempm, tempp,&
                fgrand)
    pp(3) = pm(3)+fgrand
!
    depgrd = theta * (fgrand + depthe(1))
!
!JMP  CETTE FORMULE NE FONCTIONNE QUE POUR THETA=1 OU 0.5
    sige=e/em*sigm+(1.d0-theta)*(1.d0-e/em)*sigm+e*depsi-e*depgrd
!
    preci = crit(3)*crit(3)
!
    expon = exp(-1.d0*coeffl(4)/(tempp2+tabs))
    sdt = (irrap-irram)/(coeffl(6)*(tplus-tmoins))
    if (coeffl(6) .le. 0.d0) then
        call utmess('F', 'ALGORITH7_80')
    endif
    if (sdt .lt. 0.d0) then
        call utmess('F', 'ALGORITH6_57')
    endif
    sdt = sdt * coeffl(2) + coeffl(7)
    if (sdt .lt. 0.d0) then
        call utmess('F', 'ALGORITH7_81')
    endif
    if (sdt .eq. 0.d0) then
        if (coeffl(5) .eq. 0.d0) sdt=1.d0
        if (coeffl(5) .lt. 0.d0) then
            call utmess('F', 'ALGORITH7_82')
        endif
    endif
    if (sdt .gt. 0.d0) then
        sdt = sdt**coeffl(5)
    endif
    sdt = sdt*expon*theta*(tplus-tmoins)
!
    ba = 0.d0
    fa = nmcri4(ba)
!      FA0 = FA
    ASSERT(fa.le.0.d0)
!
    if (sige .ne. 0.d0) then
        bb = abs(sige)/e
!
        nitmax = int(crit(1))
        call zerofr(1, 'BRENT', nmcri4, ba, bb,&
                    preci, nitmax, dp, iret, iter)
        ASSERT(iret.eq.0)
!
        pp(1) = pm(1) + dp/theta
!
        sigp = sige * (1.d0 - e * dp/abs(sige) )
!
    else
        pp(1) = pm(1)
        sigp = sige
    endif
!
    sigp = (sigp-sigm)/theta + sigm
!
    effnoc = sigp * a
!
! --- CALCUL DES COEFFICIENTS NON ELASTIQUES DE LA MATRICE TANGENTE
!
!      FFLUAG = 0.D0
!      CORREC = (E*FFLUAG*COEFFL(2)*(EFFNOC/A)**(COEFFL(2)-1.D0))
    correc = 0.d0
    rigela = e*a/xlong0
    xrig = rigela / (1.d0 + correc)
    kls(1) = xrig
    kls(22) = -xrig
    kls(28) = xrig
!
! --- CALCUL DES FORCES NODALES
!
    flc = effnoc
!
! ----------------------------------------------------------------------
!
end subroutine
