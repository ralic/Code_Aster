subroutine sepavp(ck, cm, cmat, ndim, alpha,&
                  beta, nbmod, lambd1, lambd2, interv)
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
!-----------------------------------------------------------------------
!  BUT:  < SEPARATION DES VALEURS PROPRES >
    implicit none
!
!   CETTE ROUTINE ENCADRE CHACUNE DES VALEURS PROPRES D'UN PROBLEME A
!   MATRICES RAIDEUR ET MASSE COMPLEXES HERMITIENNES SYMETRIQUES
!   STOCKEES CARREES PLEINES PAR DES REELS RANGES DANS LES TABLEAUX
!   ALPHA ET BETA
!
!-----------------------------------------------------------------------
!
! CK       /I/: MATRICE RAIDEUR DU PROBLEME
! CM       /I/: MATRICE MASSE DU PROBLEME
! CMAT     /M/: MATRICE COMPLEXE DE TRAVAIL
! NDIM     /I/: DIMENSION DES MATRICES
! BETA     /O/: BORNE SUP DE L'INTERVALE CONTENANT LA VP
! ALPHA    /O/: BORNE INF DE L'INTERVALE CONTENANT LA VP
! NBMOD    /M/: NOMBRE DE MODES PROPRES DESIRE/EXISTANT
! LAMBD1   /I/: BORNE INFERIEURE DE L'INTERVE DE RECHERCHE
! LAMBD2   /I/: BORNE SUPERIEURE DE L'INTERVE DE RECHERCHE
! INTERV   /I/: LONGUEUR MAXIMAL D'UN INTERVE CONTENANT UNE VP
!
!-----------------------------------------------------------------------
!
#include "asterfort/assert.h"
#include "asterfort/nbval.h"
#include "asterfort/u2mesg.h"
    integer :: ndim, nbmod
    complex(kind=8) :: ck(*), cm(*), cmat(*)
    real(kind=8) :: alpha(ndim+1), beta(ndim+1)
    real(kind=8) :: lambd1, lambd2, valr(2)
    real(kind=8) :: interv
    integer :: i, n1, n2, nb, ct
    real(kind=8) :: a, b, c
    logical :: sortie
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    do 10 i = 1, nbmod
        alpha(i)=-1
        beta(i) =-1
10  end do
    alpha(1)=lambd1
    call nbval(ck, cm, cmat, ndim, lambd1,&
               n1)
    call nbval(ck, cm, cmat, ndim, lambd2,&
               n2)
    nbmod=min(n2-n1,nbmod)
    beta(nbmod)=lambd2
    valr(1)=lambd1
    valr(2)=lambd2
    call u2mesg('I', 'ALGELINE6_9', 0, ' ', 1,&
                nbmod, 2, valr)
    do 20 i = 1, nbmod
        if (alpha(i) .ge. 0.d0) then
            a=alpha(i)
        else
            sortie=.false.
            ct=i
70          continue
            ct=ct-1
            if (ct .le. 1) then
                sortie=.true.
                a=lambd1
            endif
            if (beta(ct) .ge. 0.d0) then
                sortie=.true.
                a=beta(ct)
            endif
            if (sortie) goto 80
            goto 70
80          continue
        endif
        if (beta(i) .ge. 0.d0) then
            b=beta(i)
        else
            sortie=.false.
            ct=i
50          continue
            ct=ct+1
            if (ct .ge. nbmod) then
                sortie=.true.
                b=lambd2
            endif
            if (alpha(ct) .ge. 0.d0) then
                sortie=.true.
                b=alpha(ct)
            endif
            if (sortie) goto 60
            goto 50
60          continue
        endif
30      continue
        if (beta(i) .ge. 0.d0) then
            if (alpha(i) .ge. 0.d0) then
                if ((beta(i)-alpha(i)) .le. interv) goto 40
            endif
        endif
        c=(a+b)/2
        call nbval(ck, cm, cmat, ndim, c,&
                   nb)
        nb=nb-n1
        ASSERT(nb.ge.0 .and.nb.le.nbmod)
        if (nb .gt. 0) then
            if (beta(nb) .lt. 0.d0) then
                beta(nb)=c
            else
                beta(nb)=min(c,beta(nb))
            endif
        endif
        if (alpha(nb+1) .lt. 0.d0) then
            alpha(nb+1)=c
        else
            alpha(nb+1)=max(c,alpha(nb+1))
        endif
        if (nb .lt. i) a=c
        if (nb .ge. i) b=c
        goto 30
40      continue
20  end do
end subroutine
