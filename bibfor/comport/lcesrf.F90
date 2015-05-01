function lcesrf(am, gameps, r, v, prec, itemax, iret)
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
    implicit none
    real(kind=8) :: lcesrf
#include "asterfort/lcesvf.h"
#include "asterfort/zerop3.h"
    integer :: itemax, iret
    real(kind=8) :: gameps, r, v, am, prec
! ----------------------------------------------------------------------
!   ENDO_SCALAIRE:       RESOLUTION DE -DRDA(A)*GAMEPS = V + R*A
! ----------------------------------------------------------------------
!  IN  AM      VALEUR DE L'ENDOMMAGEMENT EN T- (BORNE INF)
!  IN  GAMEPS  PSEUDO-ENERGIE DE DEFORMATION POUR LE CRITERE
!  IN  R       PARAMETRE D'AUGMENTATION
!  IN  V       CSTE DU SECOND MEMBRE (K-PHI EN PRATIQUE)
!  IN  PREC    PRECISION : A ET A+PREC ENCADRENT LA SOLUTION
!  IN  ITEMAX  NOMBRE D'ITERATIONS MAXI AUTORISE
!  OUT IRET    CONVERGENCE (0=OK, 1=PB)
!  OUT ITER    NOMBRE D'ITERATIONS (POUR INFO)
! ----------------------------------------------------------------------
    integer :: iter, nrac
    real(kind=8) :: amin, amax, c1, c2, p2, p1, p0, rac(3), cmax, cmin
    real(kind=8) :: ai, an, cn, ln, sens, pente, da
    real(kind=8) :: prectr, acvg, ccvg, lcvg, small, fn, fcvg, dfn, fmin
    real(kind=8) :: un
! ----------------------------------------------------------------------
    real(kind=8) :: pk, pm, pp, pq
    common /lces/ pk,pm,pp,pq
! ----------------------------------------------------------------------
!
!    INITIALISATION
    iret = 0
    un = 1.d0
    small = 1.d-2
    prectr = 1.d-3
!
!    INTERVALLE DE RECHERCHE
    amin = max(am,-v/r)
    amax = min(un,(pm*gameps-v)/r)
    if (abs(amax-amin) .le. prec) then
        lcesrf = (amin+amax)/2.d0
        goto 999
    endif
!
!
!    ESTIMATION INITIALE GAMEPS*M*(1-A)/(1+M*A)**2 = V+R*A
!
!     write (6,*) 'lcesrf - preca: ',prec
    c1 = v/(pm*gameps)
    c2 = r/(pm*gameps)
    fmin = (1-amin)/(1+pm*amin)**2-c1-c2*amin
    if (fmin .le. 0) then
        ai = amin
        goto 100
    endif
!
    if (1-c1 .le. small) then
        ai = (1-c1)/(c2+2*pm*c1+1)
!
    else if (c2.le.small*c1) then
        if (c1.le.small/pm**2) then
          ai = 1-c1
        else
          p1 = (1+2*c1*pm)/(c1*pm**2)
          p0 = (c1-1)/(c1*pm**2)
          ai = 0.5d0*(-p1+sqrt(p1**2-4*p0))
        endif

    else if (c2.le.small/pm**2) then
        if (c1.le.small/pm**2) then
          ai = (1-c1)/(1+2*pm*c1+c2)
        else
          p1 = (1+2*c1*pm+c2)/(c1*pm**2+2*c2*pm)
          p0 = (c1-1)/(c1*pm**2+2*c2*pm)
          ai = 0.5d0*(-p1+sqrt(p1**2-4*p0))
        endif

    else
        p2 = (2*c2+c1*pm)/(c2*pm)
        p1 = (c2+2*c1*pm+1)/(c2*pm**2)
        p0 = (c1-1)/(c2*pm**2)
        call zerop3(p2, p1, p0, rac, nrac)
        ai=rac(1)
    endif
!
!    CORRECTION DE NEWTON POUR SE PREMUNIR DES PROBLEMES DE PRECISION
    do iter = 1, itemax
        fn = (1-ai)/(1+pm*ai)**2-c1-c2*ai
        sens = sign(1.d0,fn)
        acvg = ai+sens*prec
        fcvg = (1-acvg)/(1+pm*acvg)**2-c1-c2*acvg
        if (fcvg*fn .le. 0) goto 100
        dfn = (pm*ai-1-2*pm)/(1+pm*ai)**3 - c2
        ai = max(ai-fn/dfn,amin)
    end do
    iret = 1
    goto 999
100  continue
!     write (6,*) 'bornes: ',amin,amax,ai
!
!    INITIALISATION NEWTON
    an = max(amin,ai)
    an = min(amax,an)
    cn = -gameps*lcesvf(1,an)
    ln = v+r*an
    sens = sign(un,cn-ln)
!
!    INITIALISATION DES BORNES
    cmax = -gameps*lcesvf(1,amax)
    cmin = -gameps*lcesvf(1,amin)
!
!    METHODE DE NEWTON
    do iter = 1, itemax
!
!      REACTUALISATION DES BORNES DE L'INTERVALLE DE RECHERCHE
        if (sens .ge. 0) then
            amin = an
            cmin = cn
        else
            amax = an
            cmax = cn
        endif
!
!      CALCUL DU NOUVEL ITERE PAR LA METHODE DE NEWTON
        pente = gameps*lcesvf(2,an)
        da = (cn-ln)/(r+pente)
!
!      CONTROLE DES BORNES ET METHODE DE CORDE SI NECESSAIRE
        if (an+da .le. amin .or. an+da .ge. amax) then
            da = sens*(amax-amin)/2
        endif
!
!      ACTUALISATION DE LA SOLUTION
        an = an+da
        cn = -gameps*lcesvf(1,an)
        ln = v+r*an
        sens = sign(un,cn-ln)
!         write (6,*) 'run: ',iter,an,cn-ln
!
!      CONTROLE DE LA CONVERGENCE
        if (abs(cn-ln) .lt. prectr*pk) then
            acvg = an+sens*prec
            ccvg = -gameps*lcesvf(1,acvg)
            lcvg = v+r*acvg
!             write (6,*) 'cvg: ',acvg,ccvg-lcvg
            if (sens*(lcvg-ccvg) .ge. 0) then
                lcesrf = an
!                if (iter.ge.10) write (6,*) 'R(a)=0: ',iter,prec
                goto 999
            endif
        endif
!
    end do
    iret = 1
!
999 continue
end function
