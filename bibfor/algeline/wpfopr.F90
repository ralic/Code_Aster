subroutine wpfopr(lmasse, lamor, lraide, appr, fmin,&
                  sigma, matopa, matpsc, raide, lqz,&
                  solveu)
    implicit none
#include "jeveux.h"
#include "asterc/r8depi.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtcmbl.h"
#include "asterfort/mtdefs.h"
#include "asterfort/mtdscr.h"
#include "asterfort/preres.h"
#include "asterfort/u2mesr.h"
#include "asterfort/u2mess.h"
!
    character(len=*) :: appr, matopa, matpsc, raide
    character(len=19) :: solveu
    integer :: lmasse, lamor, lraide
    real(kind=8) :: fmin
    complex(kind=8) :: sigma
    logical :: lqz
!     -----------------------------------
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
! ======================================================================
!     DETERMINATION D'UN SHIFT ET CALCUL DE LA MATRICE SHIFTEE
!     DANS LE CAS QUADRATIQUE REEL
!     ------------------------------------------------------------------
! OUT LDYNAM  : IS : POINTEUR SUR LA FACTORISEE DE LA MATRICE DYNAMIQUE
!                    INDUITE PAR L'OPTION
! OUT SIGMA   : C16: SHIFT
! IN  LQZ     : METHODE QZ OU NON
! IN  SOLVEU : K19 : SD SOLVEUR POUR PARAMETRER LE SOLVEUR LINEAIRE
!     ------------------------------------------------------------------
!
!
    integer :: lmat(3), lmatra, lmtpsc, jpomr, jrefe, ibid, jbid
    real(kind=8) :: ashift, constr(3), constc(6), valr(2)
    character(len=1) :: typcst(3), base
    character(len=8) :: namddl
    character(len=19) :: nomi, nomt, matpre, matass
    character(len=24) :: nmat(3), nmatra, nmtpsc
!
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: icomb
    real(kind=8) :: fshift
!-----------------------------------------------------------------------
    data namddl/'        '/
!     ------------------------------------------------------------------
!
    call jemarq()
!
    lmat(1) = lmasse
    nmat(1) = zk24(zi(lmat(1)+1))
    lmat(2) = lamor
    nmat(2) = zk24(zi(lmat(2)+1))
    lmat(3) = lraide
    nmat(3) = zk24(zi(lmat(3)+1))
!
    fshift = r8depi()*fmin
    ashift = 0.d0
!
    call getvr8('CALC_FREQ', 'AMOR_REDUIT', iocc=1, scal=ashift, nbret=ibid)
!
    if (abs(ashift) .ge. 1.d0) then
        ashift = 0.95d0
        valr (1) = 1.d0
        valr (2) = 0.95d0
        call u2mess('I+', 'ALGELINE4_95')
        call u2mesr('I', 'ALGELINE4_96', 2, valr)
    endif
!
    ashift = - (ashift*fshift)/sqrt(1.d0-ashift*ashift)
    sigma = dcmplx(ashift,fshift)
!
! --- POUR QZ CALCUL DE LA MATRICE SHIFTEE ET DE SA FACTORISEE INUTILE
    if (lqz) goto 999
!
    if (fmin .eq. 0.d0) then
        jpomr=0
        do 15 icomb = 1, 3
!           ON RECHERCHE UNE EVENTUELLE MATRICE NON SYMETRIQUE
            nomi=nmat(icomb)
            call jeveuo(nomi//'.REFA', 'L', jrefe)
            if (zk24(jrefe-1+9) .eq. 'MR') then
                jpomr=icomb
            endif
15      continue
!
!        --- DECALAGE REEL ---
        if (jpomr .eq. 0) then
            call mtdefs(matopa, raide, 'V', 'R')
        else
            nomt = nmat(jpomr)
            call mtdefs(matopa, nomt, 'V', 'C')
        endif
        call mtdscr(matopa)
        nmatra=matopa(1:19)//'.&INT'
        call jeveuo(matopa(1:19)//'.&INT', 'E', lmatra)
        do 10 icomb = 1, 3
            typcst(icomb) = 'R'
10      continue
        constr(1) = ashift*ashift
        constr(2) = ashift
        constr(3) = 1.d0
        call mtcmbl(3, typcst, constr, nmat, nmatra,&
                    namddl, ' ', 'ELIM=')
        lmtpsc = 0
!
    else
        jpomr=0
        do 25 icomb = 1, 3
!           ON RECHERCHE UNE EVENTUELLE MATRICE NON SYMETRIQUE
            nomi=nmat(icomb)
            call jeveuo(nomi//'.REFA', 'L', jrefe)
            if (zk24(jrefe-1+9) .eq. 'MR') then
                jpomr=icomb
            endif
25      continue
!
!        --- DECALAGE COMPLEXE ---
        if (jpomr .eq. 0) then
            call mtdefs(matopa, raide, 'V', 'C')
        else
            nomt = nmat(jpomr)
            call mtdefs(matopa, nomt, 'V', 'C')
        endif
        call mtdscr(matopa)
        nmatra=matopa(1:19)//'.&INT'
        call jeveuo(matopa(1:19)//'.&INT', 'E', lmatra)
        do 20 icomb = 1, 3
            typcst(icomb) = 'C'
20      continue
        constc(1) = dble(sigma*sigma)
        constc(2) = dimag(sigma*sigma)
        constc(3) = dble(sigma)
        constc(4) = dimag(sigma)
        constc(5) = 1.d0
        constc(6) = 0.d0
        call mtcmbl(3, typcst, constc, nmat, nmatra,&
                    namddl, ' ', 'ELIM=')
        if (appr .eq. 'R') then
            if (jpomr .eq. 0) then
                call mtdefs(matpsc, raide, 'V', 'R')
            else
                nomt = nmat(jpomr)
                call mtdefs(matpsc, nomt, 'V', 'R')
            endif
            call mtdscr(matpsc)
            nmtpsc=matpsc(1:19)//'.&INT'
            call jeveuo(matpsc(1:19)//'.&INT', 'E', lmtpsc)
            do 30 icomb = 1, 3
                typcst(icomb) = 'R'
30          continue
            constr(1) = ashift*ashift
            constr(2) = ashift
            constr(3) = 1.d0
            call mtcmbl(3, typcst, constr, nmat, nmtpsc,&
                        namddl, ' ', 'ELIM=')
!
        else
!
            lmtpsc = 0
!
        endif
!
    endif
!
!     --- FACTORISATION DES MATRICES ---
!
    base='V'
    matpre=' '
    matass=zk24(zi(lmatra+1))
    call preres(solveu, base, ibid, matpre, matass,&
                jbid, 1)
    if (lmtpsc .ne. 0) then
        matass=zk24(zi(lmtpsc+1))
        call preres(solveu, base, ibid, matpre, matass,&
                    jbid, 1)
    endif
!
999  continue
    call jedema()
end subroutine
