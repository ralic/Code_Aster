subroutine vp1ite(lmasse, lraide, ldynam, x, imode,&
                  valp, neq, mxiter, tol, iter,&
                  x0, mx, err, iexcl, place,&
                  iquoti, solveu)
    implicit none
!
#include "jeveux.h"
#include "asterfort/ggubs.h"
#include "asterfort/mrmult.h"
#include "asterfort/resoud.h"
#include "asterfort/vpmort.h"
#include "asterfort/vpstur.h"
!
    integer :: neq
    real(kind=8) :: x(neq, 1), mx(neq, *), err, x0(neq)
    real(kind=8) :: valp
    integer :: place, iexcl(*), imode, mxiter, iter
    integer :: lmasse, lraide, ldynam
    character(len=19) :: solveu
!     ----------------------- ------------------------------------------
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
!     CALCUL D'UN COUPLE VECTEUR ET VALEUR PROPRE PAR ITERATION INVERSE
!     ------------------------------------------------------------------
! VAR X      :    :  VECTEUR(S) PROPRE(S)
! VAR VALP : R8 :    VALEUR PROPRE, LA VALEUR INITIALE EST CORRIGEE
!     X0     :    :  VECTEUR PROPRE OBTENU A L'ITERATION PRECEDENTE
! IN  MXITER : IS : NOMBRE MAXIMUM D'ITERATION
! OUT ITER   : IS : NOMBRE D'ITERATION EFFECTUEE
!                   EN CAS DE NON CONVERGENCE ITER = -MXITER
! IN  TOL    : R8 : TOLERENCE (CRITERE DE CONVERGENCE SUR LE MODE)
! OUT ERR    : R8 : ERREUR SUR LE DERNIER ITERE
! IN  SOLVEU : K19 : SD SOLVEUR POUR PARAMETRER LE SOLVEUR LINEAIRE
!     ------------------------------------------------------------------
!     QUOTIENT DE RAYLEIGH GENERALISE
!                Y.A.Y / Y.X  = L + Y.( A.X - L.X) / Y.X
!
!     REFERENCE: F.L. BAUER - J.H. WILKINSON - C. REINSCH
!        HANDBOOK FOR AUTOMATIC COMPUTATION - LINEAR ALGEBRA - VOL.2
!        PAGE 73
!     ------------------------------------------------------------------
!
!
    real(kind=8) :: xmx, x0mx, xxx, det0, pvalp
    real(kind=8) :: coef, coeft, rmg
    complex(kind=8) :: cbid
    character(len=1) :: kbid
    character(len=19) :: k19bid, matass, chcine, criter
!     ------------------------------------------------------------------
!
!     INIT. OBJETS ASTER
!-----------------------------------------------------------------------
    integer :: idet0, ieq, ier, iquoti, jter
    real(kind=8) :: dseed, tol
    integer :: iret
    cbid = dcmplx(0.d0, 0.d0)
!-----------------------------------------------------------------------
    matass=zk24(zi(ldynam+1))
    chcine=' '
    criter=' '
    k19bid=' '
!
!     --- VECTEUR INITIAL ALEATOIRE ---
    dseed = 526815.0d0
    call ggubs(dseed, neq, x0)
    call vpmort(neq, x0, x, mx, imode)
    call mrmult('ZERO', lmasse, x0, mx(1, imode), 1,&
                .false.)
    do ieq = 1, neq
        mx(ieq,imode) = mx(ieq,imode)*iexcl(ieq)
    end do
!
    x0mx = 0.d0
    do ieq = 1, neq
        x0mx = x0mx + x0(ieq)*mx(ieq,imode)
    end do
!
    coef = 1.d0/sqrt(abs(x0mx))
    coeft = sign(1.d0,x0mx)*coef
    do ieq = 1, neq
        x0(ieq) = coef*x0(ieq)
        mx(ieq,imode) = coeft*mx(ieq,imode)
    end do
!
    do jter = 1, mxiter
        iter = jter
!
!        --- ELIMINATION DES DDL EXTERNES ---
        do ieq = 1, neq
            x(ieq,imode) = mx(ieq,imode)*iexcl(ieq)
        end do
!
!        --- RESOLUTION DE (K-W.M) X = (M).X ---
        call resoud(matass, k19bid, solveu, chcine, 1,&
                    k19bid, k19bid, kbid, x(1, imode), [cbid],&
                    criter, .false., 0, iret)
!
!        --- ORTHOGONALISATION EN CAS DE MODES MULTIPLES  ---
        call vpmort(neq, x(1, imode), x, mx, imode)
!
!        --- CALCUL DE M.XN ---
        call mrmult('ZERO', lmasse, x(1, imode), mx(1, imode), 1,&
                    .false.)
        do ieq = 1, neq
            mx(ieq,imode) = mx(ieq,imode)*iexcl(ieq)
        end do
!
!        --- CALCUL DE XN.M.XN ---
        xmx = 0.d0
        do ieq = 1, neq
            xmx = xmx + x(ieq,imode)*mx(ieq,imode)
        end do
!
!        --- NORMALISATION DE XN ---
        coef = 1.d0/sqrt(abs(xmx))
        coeft = sign(1.d0,xmx)*coef
        do ieq = 1, neq
            x0(ieq) = coef*x0(ieq)
            mx(ieq,imode) = coeft*mx(ieq,imode)
        end do
!
!        --- CALCUL DE LA NORME DE XN-1.M.XN ---
        xxx = 0.d0
        do ieq = 1, neq
            xxx = xxx + x0(ieq)*mx(ieq,imode)
        end do
!
!        --- CALCUL DE L'ERREUR ---
        coef = xxx/xmx/coef
        err = abs(abs(xxx)-1.d0)
        if (err .lt. tol) goto 900
!
!        --- SAUVEGARDE DE XN DANS XN-1 ---
        do ieq = 1, neq
            x0(ieq) = x(ieq,imode)
        end do
!
!        --- SHIFT ---
        if (iquoti .gt. 0) then
            pvalp = valp + coef
!
            if (pvalp .gt. valp*0.9d0 .and. pvalp .lt. valp*1.1d0) then
! --- POUR OPTIMISER ON NE CALCULE PAS LE DET
                valp = pvalp
                call vpstur(lraide, valp, lmasse, ldynam, det0,&
                            idet0, place, ier, solveu, .false.,&
                            .true.)
            endif
!
        endif
!
    end do
!
!     --- SORTIE SANS CONVERGENCE ---
    iter = -mxiter
900 continue
!
!     --- FREQUENCE CORRIGEE ---
    valp = valp + coef
!
!     --- NORMALISATION DU VECTEUR ---
    rmg = 0.d0
    do ieq = 1, neq
        rmg = max(abs(x(ieq,imode)),rmg)
    end do
    do ieq = 1, neq
        x(ieq,imode) = x(ieq,imode)/rmg
    end do
!
end subroutine
