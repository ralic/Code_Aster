subroutine ampcpr(cmat, nb1, nb2, bmat, n1,&
                  n2, i, j, fac, npar,&
                  nsym)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "asterfort/utmess.h"
!
!***********************************************************************
!    P. RICHARD     DATE 12/03/91
!-----------------------------------------------------------------------
!  BUT:  AJOUTER UNE MATRICE PLEINE REELLE A UNE MATRICE PLEINE
!       COMPLEXE (SOIT A LA PARTIE REELLE SOIT A LA PARTIE IMAGINAIRE)
!            MULTIPLICATION POSSIBLE PAR UN FACTEUR
!-----------------------------------------------------------------------
!
! CMAT     /M/: MATRICE RECEPTRICE COMPLEXE
! NB1      /I/: NB DE LIGNES DE LA MATRICE RECEPTRICE
! NB2      /I/: NB DE COLONNES DE LA MATRICE RECEPTRICE
! BMAT     /M/: MATRICE PLEINE RELLE, A AJOUTER
! N1       /I/: NB DE LIGNE DE LA MATRICE A AJOUTER
! N2       /I/: NB DE COLONNE DE LA MATRICE A AJOUTER
! I        /I/: INDICE DU PREMIER TERME DANS RECEPTRICE
! J        /I/: INDICE DE COLONNE TERME  DANS RECEPTRICE
! FAC      /I/: FACTEUR MULTIPLICATIF DE LA MATRICE RELLE AVANT ASSEMBLA
! NPAR     /I/: INDICATEUR PARTIE RELLE (1) OU IMAGINAIRE(2)
! NSYM     /I/: INDICATEUR TRANSPOSITION (-1) MATRICE REELLE OU NON(1)
!
!-----------------------------------------------------------------------
!
    real(kind=8) :: bmat(n1, n2)
    complex(kind=8) :: cmat(*)
!
!-----------------------------------------------------------------------
!
!   CAS SANS TRANSPOSITION
!
!-----------------------------------------------------------------------
    integer :: i, icol, ideb, ifin, ii, iideb, iifin
    integer :: ilig, iterme, j, jdeb, jfin, jj, jjdeb
    integer :: jjfin, n1, n2, nb1, nb2, npar, nsym
!
    real(kind=8) :: fac
!-----------------------------------------------------------------------
    if (nsym .eq. 1) then
!
        jdeb=j
        jfin=min(j+n2-1,nb2)
        if ((j+n2-1) .gt. nb2) then
            call utmess('F', 'ALGORITH11_88')
        endif
        if (jfin .lt. jdeb) goto 9999
        jjdeb=jdeb-j+1
        jjfin=jfin-j+1
!
        ideb=i
        if ((i+n1-1) .gt. nb1) then
            call utmess('F', 'ALGORITH11_88')
        endif
        ifin=min(i+n1-1,nb1)
        if (ifin .lt. ideb) goto 9999
        iideb=ideb-i+1
        iifin=ifin-i+1
!
!    PARTIE RELLE
!
        if (npar .eq. 1) then
!
            do 10 ii = iideb, iifin
                do 20 jj = jjdeb, jjfin
                    ilig = i+ii-1
                    icol = j+jj-1
                    if (icol .ge. ilig) then
                        iterme = icol*(icol-1)/2+1+icol-ilig
                        cmat(iterme)=cmat(iterme)+dcmplx(bmat(ii,jj)*&
                        fac,0.d0)
                    endif
20              continue
10          continue
!
        else
!
!    PARTIE IMAGINAIRE
!
            do 30 ii = iideb, iifin
                do 40 jj = jjdeb, jjfin
                    ilig = i+ii-1
                    icol = j+jj-1
                    if (icol .ge. ilig) then
                        iterme = icol*(icol-1)/2+1+icol-ilig
                        cmat(iterme)=cmat(iterme)+dcmplx(0.d0,bmat(ii,&
                        jj)*fac)
                    endif
40              continue
30          continue
!
        endif
    endif
!
!
!     CAS AVEC TRANSPOSITION
!
    if (nsym .eq. -1) then
!
        jdeb=j
        jfin=min(j+n1-1,nb2)
        if ((j+n1-1) .gt. nb2) then
            call utmess('F', 'ALGORITH11_90')
        endif
        if (jfin .lt. jdeb) goto 9999
        jjdeb=jdeb-j+1
        jjfin=jfin-j+1
!
        ideb=i
        ifin=min(i+n2-1,nb1)
        if ((i+n2-1) .gt. nb1) then
            call utmess('F', 'ALGORITH11_88')
        endif
        if (ifin .lt. ideb) goto 9999
        iideb=ideb-i+1
        iifin=ifin-i+1
!
!    PARTIE RELLE
!
        if (npar .eq. 1) then
!
            do 50 ii = iideb, iifin
                do 60 jj = jjdeb, jjfin
                    ilig = i+ii-1
                    icol = j+jj-1
                    if (icol .ge. ilig) then
                        iterme = icol*(icol-1)/2+1+icol-ilig
                        cmat(iterme)=cmat(iterme)+dcmplx(bmat(jj,ii)*&
                        fac,0.d0)
                    endif
60              continue
50          continue
!
        else
!
!    PARTIE IMAGINAIRE
!
            do 70 ii = iideb, iifin
                do 80 jj = jjdeb, jjfin
                    ilig = i+ii-1
                    icol = j+jj-1
                    if (icol .ge. ilig) then
                        iterme = icol*(icol-1)/2+1+icol-ilig
                        cmat(iterme)=cmat(iterme)+dcmplx(0.d0,bmat(jj,&
                        ii)*fac)
                    endif
80              continue
70          continue
!
        endif
    endif
!
!
9999  continue
end subroutine
