subroutine hujpre(etat, mod, crit, imat, mater,&
                  deps, sigd, sigf, vind, iret)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!       ================================================================
!                   CALCUL DE LA PREDICTION EN CONTRAINTE
!       ================================================================
!       IN      ETAT    COMPORTEMENT DU POINT DU POINT DE CALCUL
!                               'ELASTIC'     > ELASTIQUE
!                               'PLASTIC'     > PLASTIQUE
!               MOD     TYPE DE MODELISATION
!               CRIT    CRITERES  LOCAUX
!                       CRIT(1) = NOMBRE D ITERATIONS MAXI A CONVERGENCE
!                                 (ITER_INTE_MAXI == ITECREL)
!                       CRIT(2) = TYPE DE JACOBIEN A T+DT
!                                 (TYPE_MATR_COMP == MACOMP)
!                                 0 = EN VITESSE     > SYMETRIQUE
!                                 1 = EN INCREMENTAL > NON-SYMETRIQUE
!                       CRIT(3) = VALEUR DE LA TOLERANCE DE CONVERGENCE
!                                 (RESI_INTE_RELA == RESCREL)
!                       CRIT(5) = NOMBRE D'INCREMENTS POUR LE
!                                 REDECOUPAGE LOCAL DU PAS DE TEMPS
!                                 (RESI_INTE_PAS == ITEDEC )
!                                 0 = PAS DE REDECOUPAGE
!                                 N = NOMBRE DE PALIERS
!               DEPS    INCREMENT DE DEFORMATION TOTALE
!               SIGD    CONTRAINTE A T
!               VIND    VARIABLES INTERNES A T    + INDICATEUR ETAT T
!               OPT     OPTION DE CALCUL A FAIRE
!                               'RIGI_MECA_TANG'> DSDE(T)
!                               'FULL_MECA'     > DSDE(T+DT) , SIG(T+DT)
!                               'RAPH_MECA'     > SIG(T+DT)
!       OUT     SIGF    CONTRAINTE A T+DT
!               IRET    CODE RETOUR DE  L'INTEGRATION DE LA LOI CJS
!                              IRET=0 => PAS DE PROBLEME
!                              IRET=1 => ECHEC
!
#include "jeveux.h"
#include "asterfort/hujela.h"
#include "asterfort/hujprj.h"
#include "asterfort/hujtid.h"
#include "asterfort/lcprmv.h"
#include "asterfort/lcsovn.h"
#include "asterfort/tecael.h"
#include "asterfort/trace.h"
    integer :: ndt, ndi, imat, iret, iadzi, iazk24, i
    real(kind=8) :: crit(*), vind(*)
    real(kind=8) :: deps(6), dev(3), pf(3), q, pd(3), dp(3)
    real(kind=8) :: sigd(6), sigf(6), dsig(6), dsde(6, 6), rtrac
    real(kind=8) :: mater(22, 2), i1, d13, tole1, un, zero
    real(kind=8) :: ptrac, pref, maxi, cohes, factor
    character(len=7) :: etat
    character(len=8) :: mod, nomail
    logical(kind=1) :: debug
!
    common /tdim/   ndt, ndi
    common /meshuj/ debug
!
    data   un, zero / 1.d0, 0.d0/
    data   d13, tole1 /0.33333333334d0, 1.0d-7/
!
!
    pref = mater(8,2)
    ptrac = mater(21,2)
    rtrac = abs(pref*1.d-6)
!
    if (etat .eq. 'ELASTIC') then
!
        call hujela(mod, crit, mater, deps, sigd,&
                    sigf, iret)
!
    else if (etat .eq. 'PLASTIC') then
!
        call hujtid(mod, imat, sigd, vind, dsde,&
                    iret)
        if (iret .eq. 0) then
            call lcprmv(dsde, deps, dsig)
            call lcsovn(ndt, sigd, dsig, sigf)
            i1 =d13*trace(ndi,sigf)
        else
            iret =0
            i1 = -un
            if (debug) then
                call tecael(iadzi, iazk24)
                nomail = zk24(iazk24-1+3) (1:8)
                write(6,'(10(A))')&
     &     'HUJPRE :: ECHEC DANS LA PSEUDO-PREDICTION ELASTIQUE DANS ',&
     &     'LA MAILLE ',nomail
            endif
        endif
!
        if ((i1 + un)/abs(pref) .ge. tole1) then
            if (debug) then
                call tecael(iadzi, iazk24)
                nomail = zk24(iazk24-1+3) (1:8)
                write(6,'(10(A))')&
     &      'HUJPRE :: TRACTION DANS LA PSEUDO-PREDICTION ELASTIQUE ',&
     &      'DANS LA MAILLE ',nomail
            endif
            call hujela(mod, crit, mater, deps, sigd,&
                        sigf, iret)
        endif
!
    endif
!
!
! ---> CONTROLE QU'AUCUNE COMPOSANTE DU VECTEUR SIGF NE SOIT POSITIVE
    do 10 i = 1, ndt
        dsig(i)= sigf(i) - sigd(i)
10  continue
!
    maxi = un
    cohes = -rtrac+ptrac
    factor = un
!
    do 20 i = 1, ndi
        call hujprj(i, sigf, dev, pf(i), q)
        call hujprj(i, sigd, dev, pd(i), q)
        call hujprj(i, dsig, dev, dp(i), q)
        if (pf(i) .gt. cohes .and. dp(i) .gt. tole1) then
            factor = (-pd(i)+cohes)/dp(i)
            if ((factor.gt.zero) .and. (factor.lt.maxi)) then
                maxi = factor
            endif
        endif
20  continue
!
!
! ---> SI IL EXISTE PF(I)>0, ALORS MODIFICATION DE LA PREDICTION
    if (maxi .lt. un) then
        do 30 i = 1, ndt
            dsig(i) = maxi * dsig(i)
30      continue
        call lcsovn(ndt, sigd, dsig, sigf)
        if (debug) then
            write (6,'(A,A,E12.5)')&
     &    'HUJPRE :: APPLICATION DE FACTOR POUR MODIFIER ',&
     &    'LA PREDICTION -> FACTOR =',maxi
            write(6,'(A,6(1X,E12.5))')'SIGF =',(sigf(i),i=1,ndt)
        endif
    endif
!
end subroutine
