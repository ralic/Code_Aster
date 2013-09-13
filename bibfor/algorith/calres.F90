subroutine calres(np3, ic, typch, nbseg, choc,&
                  rc, theta, vloc, xloc, vloc0,&
                  xloc0, excloc, tetaj, jacobc, jacobk,&
                  floc, flres, old, oldia, iforn,&
                  toln)
! aslint: disable=W1504
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
! DESCRIPTION : CALCUL DES FORCES NON-LINEAIRES RESIDUELLES
! -----------
!               APPELANT : MDCHOE
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
#include "asterfort/disbut.h"
#include "asterfort/fornor.h"
#include "asterfort/fortan.h"
#include "asterfort/matini.h"
#include "asterfort/prmave.h"
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
    integer :: np3, ic, typch(*), nbseg(*)
    real(kind=8) :: choc(6, *), rc(np3, *), theta(np3, *), vloc(*), xloc(*)
    real(kind=8) :: vloc0(*), xloc0(*), excloc(*), tetaj, jacobc(3, *)
    real(kind=8) :: jacobk(3, *), floc(*), flres(*), old(9, *)
    integer :: oldia(*), iforn
    real(kind=8) :: toln
!
! VARIABLES LOCALES
! -----------------
    integer :: typobs, nbs, i, iadher, ier
    real(kind=8) :: ytemp(3), xjeu, sint, cost, dnorm, tolch, fn, vn, cfrotd
    real(kind=8) :: cfrots, kn, cn, kt, ct, oldvt(2), oldft(2), oldxlo(3), ft(2)
    real(kind=8) :: vt(2), xlocj(3), ulocj(3), vlocj(3)
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC  ABS
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL   DISBUT, FORNOR, FORTAN, MATINI, LCINVN, PRMAVE
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    tolch = 10.0d0 * toln
!
    do 10 i = 1, 3
        vlocj(i)=(tetaj*vloc(i)) + ((1.0d0-tetaj)*vloc0(i))
        xlocj(i)=(tetaj*xloc(i)) + ((1.0d0-tetaj)*xloc0(i))
        ulocj(i)=xlocj(i) - excloc(i)
10  end do
!
! 1.  CALCUL DE LA DISTANCE NORMALE
!     -----------------------------
!
    typobs = typch(ic)
    nbs = nbseg(ic)
    if ((typobs.eq.0) .or. (typobs.eq.1) .or. (typobs.eq.2)) xjeu = rc(1,ic)
!
    call disbut(np3, ic, xlocj, typobs, xjeu,&
                rc, theta, nbs, cost, sint,&
                dnorm)
!
    if (abs(dnorm) .lt. tolch) dnorm = 0.0d0
!
! 2.  SI CHOC CALCUL DES FORCES NORMALES ET TANGENTIELLES
!     ---------------------------------------------------
!
    if (dnorm .lt. 0.0d0) then
!
        kn = choc(1,ic)
        cn = choc(2,ic)
        cfrotd = choc(5,ic)
        cfrots = choc(6,ic)
!
        call fornor(dnorm, vlocj, kn, cn, cost,&
                    sint, fn, floc, vn, iforn)
!
        old(8,ic) = fn
        old(9,ic) = vn
!
        if (( cfrotd.ne.0.0d0 ) .or. ( cfrots.ne.0.0d0 )) then
!
            kt = choc(3,ic)
            ct = choc(4,ic)
            iadher = oldia(ic)
            oldvt(1) = old(1,ic)
            oldvt(2) = old(2,ic)
            oldft(1) = old(3,ic)
            oldft(2) = old(4,ic)
            oldxlo(1) = old(5,ic)
            oldxlo(2) = old(6,ic)
            oldxlo(3) = old(7,ic)
            call fortan(fn, xlocj, vlocj, cfrotd, cfrots,&
                        kt, ct, iadher, oldvt, oldft,&
                        oldxlo, cost, sint, ft, floc,&
                        vt)
            oldia(ic) = iadher
            old(1,ic) = oldvt(1)
            old(2,ic) = oldvt(2)
            old(3,ic) = oldft(1)
            old(4,ic) = oldft(2)
            old(5,ic) = oldxlo(1)
            old(6,ic) = oldxlo(2)
            old(7,ic) = oldxlo(3)
!
        endif
!
        ier = 0
        call prmave(0, jacobc, 3, 3, 3,&
                    vlocj, 3, ytemp, 3, ier)
        if (ier .ne. 0) then
            call utmess('F', 'ALGORITH_71')
        endif
!
        ier = 0
        call prmave(1, jacobk, 3, 3, 3,&
                    ulocj, 3, ytemp, 3, ier)
        if (ier .ne. 0) then
            call utmess('F', 'ALGORITH_71')
        endif
!
        flres(1) = floc(1) - ytemp(1)
        flres(2) = floc(2) - ytemp(2)
        flres(3) = floc(3) - ytemp(3)
!
    else
!
! 3. SINON INITIALISATION DES MATR. JACOB. ET DE FORCES RESIDUELLES.
!    ---------------------------------------------------------------
!
        call matini(3, 3, 0.d0, jacobc)
        call matini(3, 3, 0.d0, jacobk)
        old(1,ic) = -sint*vloc(2) + cost*vloc(3)
        old(2,ic) = vloc(1)
        old(3,ic) = 0.d0
        old(4,ic) = 0.d0
        old(5,ic) = xloc(1)
        old(6,ic) = xloc(2)
        old(7,ic) = xloc(3)
        old(8,ic) = 0.d0
        old(9,ic) = cost*vloc(2) + sint*vloc(3)
        oldia(ic) = 0
        call vecini(3, 0.d0, floc)
        call vecini(3, 0.d0, flres)
!
    endif
!
! --- FIN DE CALRES.
end subroutine
