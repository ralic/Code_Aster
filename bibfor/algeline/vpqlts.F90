subroutine vpqlts(diag, surdia, neq, vecpro, mxcmp,&
                  mxiter, ier, nitqr)
    implicit none
    include 'asterc/r8prem.h'
    integer :: neq, mxcmp, mxiter, ier, nitqr
    real(kind=8) :: diag(1), surdia(1), vecpro(mxcmp, 1)
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     CALCUL DE TOUTES LES VALEURS PROPRES ET DES VECTEURS PROPRES
!     ASSOCIES PAR LA METHODE QL-IMPLICITE POUR UNE MATRICE TRIDIAGONALE
!     SYMETRIQUE
!     ------------------------------------------------------------------
! VAR  DIAG  :    : TABLEAU DE R8 (1 .. NEQ)
!            EN ENTREE: VECTEUR CONTENANT LA DIAGONALE DE LA MATRICE
!            EN SORTIE: CONTIENT LES VALEURS PROPRES EN ORDRE QUELCONQUE
! VAR  SURDIA:    : TABLEAU DE R8 (1 .. NEQ)
!            EN ENTREE: CONTIENT LA  SUR-DIAGONALE     2,...,NEQ.
!            EN SORTIE: LA SUR-DIAGONALE EST PERDUE (ZONE DE TRAVAIL)
! IN   NEQ   : IS : ORDRE DE LA MATRICE
! OUT  VECPRO :   : TABLEAU DE R8 (1 .. NEQ)X(1 .. NEQ)
!            EN SORTIE:  CONTIENT LES VECTEURS PROPRES
!                        LA COLONNE J CONTIENT LE J-IEME VECTEUR PROPRE
!                        QUI CORRESPOND A LA VALEUR PROPRE DIAG(J).
! IN   MXCMP :  1-ERE DIMENSION (EXACTE) DE VECPRO (POUR LE FORTRAN)
!                   SI MXCMP < NEQ ALORS ON NE CALCULE PAS LES VECTEURS
!                                     ET VECPRO N'EST ALORS PAS UTILISE
! IN   MXITER:  NOMBRE MAXIMUM D'ITERATION (MXITER=30 EST UN BON CHOIX)
! OUT  IER   : IS : 0  PAS DE PROBLEME
!                   J  CONVERGENCE NON ATTEINTE A LA J VALEUR PROPRE
! OUT  NITQR : NOMBRE MAXIMAL ATTEINT D'ITERATIONS POUR AVOIR CONVERGE
!     ------------------------------------------------------------------
!     REFERENCE: F.L. BAUER - J.H. WILKINSON - C. REINSCH
!        HANDBOOK FOR AUTOMATIC COMPUTATION - LINEAR ALGEBRA - VOL.2
!        PAGE ????
!     ------------------------------------------------------------------
    real(kind=8) :: b, c, f, g, h, p, r, s
    real(kind=8) :: zero, un, deux, epsmac
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ieq, j, jeq, jter, k, m
!
!-----------------------------------------------------------------------
    ier = 0
    zero = 0.0d0
    epsmac = r8prem()
    un = 1.0d0
    deux = 2.0d0
    nitqr = 0
!
    if (neq .eq. 1) goto 99999
!
!     --- TRANSFERT DES ELEMENTS SUR-DIAGONAUX ---
    do 5 i = 2, neq
        surdia(i-1) = surdia(i)
 5  end do
!
!     --- SI NECESSAIRE CREATION DE LA MATRICE UNITE ---
    if (mxcmp .ge. neq) then
        do 6 ieq = 1, neq
            do 7 jeq = 1, neq
                vecpro(ieq,jeq) = zero
 7          continue
            vecpro(ieq,ieq) = un
 6      continue
    endif
!
    surdia(neq) = zero
    b = zero
    f = zero
    do 60 j = 1, neq
        jter = 0
        h = epsmac*(abs(diag(j))+abs(surdia(j)))
        if (b .lt. h) b = h
!
!        --- RECHERCHE DU PLUS PETIT ELEMENT SUR-DIAGONAL ---
        do 10 m = j, neq
            k=m
            if (abs(surdia(k)) .le. b) goto 15
10      continue
15      continue
        m = k
        if (m .eq. j) goto 55
20      continue
        if (jter .eq. mxiter) goto 9999
        jter = jter+1
        if (jter .gt. nitqr) then
            nitqr = jter
        endif
!
!        --- PREPARATION DU DECALAGE ---
        g = diag(j)
        p = (diag(j+1)-g)/(deux*surdia(j))
        r = abs(p)
        if (epsmac*abs(p) .lt. un) r = sqrt(p*p+un)
        diag(j) = surdia(j)/(p+sign(r,p))
        h = g-diag(j)
        do 25 i = j+1, neq
            diag(i) = diag(i)-h
25      continue
        f = f+h
!
!        --- ON APPLIQUE LA TRANSFORMATION QL ---
        p = diag(m)
        c = un
        s = zero
        do 45 i = m-1, j, -1
            g = c*surdia(i)
            h = c*p
            if (abs(p) .ge. abs(surdia(i))) then
                c = surdia(i)/p
                r = sqrt(c*c+un)
                surdia(i+1) = s*p*r
                s = c/r
                c = un/r
            else
                c = p/surdia(i)
                r = sqrt(c*c+un)
                surdia(i+1) = s*surdia(i)*r
                s = un/r
                c = c*s
            endif
            p = c*diag(i)-s*g
            diag(i+1) = h+s*(c*g+s*diag(i))
            if (mxcmp .ge. neq) then
!              --- CALCUL DU VECTEUR PROPRE ---
                do 40 k = 1, neq
                    h = vecpro(k,i+1)
                    vecpro(k,i+1) = s*vecpro(k,i)+c*h
                    vecpro(k,i) = c*vecpro(k,i)-s*h
40              continue
            endif
45      continue
        surdia(j) = s*p
        diag(j) = c*p
        if (abs(surdia(j)) .gt. b) goto 20
55      continue
        diag(j) = diag(j) + f
60  end do
    goto 99999
!
9999  continue
    ier = j
99999  continue
end subroutine
