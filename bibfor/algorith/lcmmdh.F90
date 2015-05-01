subroutine lcmmdh(coeft, ifa, nmat, nbcomm, alphap,&
                  nfs, nsg, hsr, nbsys, is,&
                  nuecou, hs, soms1, soms2, soms3)
    implicit none
#include "asterc/r8miem.h"
#include "asterfort/assert.h"
#include "asterfort/lcmmdc.h"
    integer :: ifa, nmat, nbcomm(nmat, 3), is, nbsys, nfs, nsg
    real(kind=8) :: coeft(*), alphap(12), hs, hsr(nsg, nsg), soms1, soms2, soms3
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jean-michel.proix at edf.fr
! ======================================================================
!  CALCUL DE LA FONCTION H(OMEGA) POUR LA LOI D'ECOULEMENT  DD-CFC
!       IN  COEFT   :  PARAMETRES MATERIAU
!           IFA     :  NUMERO DE FAMILLE
!           NBCOMM  :  NOMBRE DE COEF MATERIAU PAR FAMILLE
!           NMAT    :  NOMBRE DE MATERIAUX
!           ALPHAP  :  ALPHA =RHO*B**2 (TOTAL) A T+DT
!     OUT:
!           HS      :  FONCTION D'EVOLUTION DENSITE DISLOCATION
!           SOMS1    :  SOMME(j=1,12)(SQRT(a_sj omega_j))
!           SOMS2    :  SOMME(j=forest(s))(SQRT(a_sj) omega_j)
!           SOMS3    :  SOMME(j=copla(s))(SQRT(a_sj omega_j))
!     ----------------------------------------------------------------
    real(kind=8) :: a, b, y, termea, termeb, termey, denom, ceff, rmin, beta
    real(kind=8) :: numer
    real(kind=8) :: alphas, dcdals, unsurd, gc0, k
    integer :: iei, iu, iv, ifl, is3, iv3, nuecou
!     ----------------------------------------------------------------
!
!
    rmin=r8miem()
    ifl=nbcomm(ifa,1)
    iei=nbcomm(ifa,3)
!
!     LOI D'ECOULEMENT DD-CFC
!
    if ((nuecou.eq.5) .or. (nuecou.eq.8)) then
        a =coeft(ifl+3)
        b =coeft(ifl+4)
        y =coeft(ifl+6)
!
        beta =coeft(iei+2)
!         NUMHSR=NINT(COEFT(IEI+5))
!
!        EVOLUTION DE LA DENSITE DE DISLO
        termea=0.d0
        denom=0.d0
        numer=0.d0
        do 50 iu = 1, 12
!            PARTIE POSITIVE DE ALPHA
            if (alphap(iu) .gt. 0.d0) then
                denom=denom+sqrt(hsr(is,iu)*alphap(iu))
            endif
50      continue
!        SOMME SUR FOREST(S)
        if (denom .gt. rmin) then
!           TERME AU NUMERATEUR SUR FOREST(S)
            termea=0.d0
            do 51 iv = 1, 12
                is3=(is-1)/3
                iv3=(iv-1)/3
                if (is3 .ne. iv3) then
!                 PARTIE POSITIVE DE ALPHA
                    if (alphap(iv) .gt. 0.d0) then
                        numer=numer+sqrt(hsr(is,iv))*alphap(iv)
                    endif
                endif
51          continue
            termea=a*numer/denom
        endif
!
!        SOMME SUR COPLA(S)
        termeb=0.d0
        if (nbsys .eq. 12) then
            do 52 iv = 1, 12
                is3=(is-1)/3
                iv3=(iv-1)/3
!           PARTIE POSITIVE DE ALPHA
                if (is3 .eq. iv3) then
                    if (alphap(iv) .gt. 0.d0) then
                        termeb=termeb+sqrt(hsr(is,iv)*alphap(iv))
                    endif
                endif
52          continue
        else if (nbsys.eq.1) then
            alphas=alphap(is)
!           PARTIE POSITIVE DE ALPHA
            if (alphas .gt. 0.d0) then
                termeb=termeb+sqrt(hsr(is,is)*alphas)
            endif
        else
            ASSERT(.false.)
        endif
!
        call lcmmdc(coeft, ifa, nmat, nbcomm, alphap,&
                    is, ceff, dcdals)
!
!        TERME -Y*RHO_S
        if (alphap(is) .gt. 0.d0) then
            termey=-y*alphap(is)/beta
        else
            termey=0.d0
        endif
        hs=(termea+termeb*b*ceff+termey)
        soms1 = denom
        soms2 = numer
        soms3 = termeb
    endif
!
!     LOI D'ECOULEMENT ECP-CFC
!
    if (nuecou .eq. 6) then
        beta =coeft(ifl+3)
        unsurd=coeft(ifl+4)
        gc0 =coeft(ifl+6)
        k =coeft(ifl+7)
!
!        NUMHSR=NINT(COEFT(IEI+2))
!
!       EVOLUTION DE LA DENSITE DE DISLO
!
        denom = 0.d0
        do 60 iu = 1, 12
            if ((iu.ne.is) .and. (alphap(iu).gt.0.d0)) then
                denom = denom + alphap(iu)
            endif
60      continue
        denom = sqrt(denom)
!
        hs = beta*unsurd + denom/k
!
        if (alphap(is) .gt. 0.d0) then
            hs=hs-gc0*alphap(is)/beta
        endif
!
        soms1=0.d0
        soms2=0.d0
        soms3=0.d0
    endif
!
end subroutine
