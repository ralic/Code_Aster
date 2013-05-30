subroutine nmcb13(eps, sig, esec, e, dd,&
                  d, beta, a, b, y,&
                  y00, z, crit)
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! --- CALCUL DE SIGMA DANS LE CAS 1 & 3 PAR DICHOTOMIE
    include 'asterfort/u2mess.h'
    real(kind=8) :: eps, sig, esec, e, dd, d, beta, a, b, y, y00, z, crit(*)
    real(kind=8) :: xg, xd, fctd, erreur, tol
    integer :: nmaxit, iterat
    real(kind=8) :: d13, yy, zero, un, deux, presq1
    parameter (zero=0.0d+0,un=1.d+0,deux=2.d+0)
!
! --- CET ROUTINE EST BASEE SUR LA DOC R7.01.07 CHAPITRE 3
!
!
!     COPIE DES PARAMETRES DE CONVERGENCE
    nmaxit = nint(crit(1))
    tol = crit(3)
    presq1=un-tol
!     L'ENDOMMAGEMENT NE PEUT PAS ETRE SUPERIEUR A 1
    if (dd .gt. un) then
        call u2mess('F', 'ELEMENTS2_37')
    endif
!
! --- VERIFICATION DE DEPASSEMENT DU SEUIL
!     POUR CAS 1 : EQUATION  3.2.1.1.
!     POUR CAS 3 : EQUATION  3.2.3.1
!     ATTENTION : EPS N'EST PAS EGAL A LA MEME CHOSE DANS LES DEUX CAS
!     VOIR ROUTINE NMCB1D L.70 (APRES "IF (X1.LE.EPS") )
    yy = (e*eps+beta)* (e*eps+beta) - beta*beta/ (un-dd)/ (un-dd)
    yy = yy/ (deux*e)
    if (yy .le. z) then
! --- LE SEUIL N'EST PAS DEPASSE, D N'EVOLUE PAS
        y = yy
        esec = e* (un-dd)
        sig = eps*esec - beta*dd
        goto 20
    endif
! --- VERIFICATION DU CRITERE :
!     LE CRITERE FCTD EST OBTENU A PARTIR DES SYSTEMES D'EQUATIONS
!     3.2.1.2. ET 3.2.3.2.(RESP. CAS 1 ET 3) EN INTEGRANT LA 1ERE
!     LIGNE DANS LA SECONDE.
!     (S'IL EST POSITIF CELA IMPLIQUE QUE LA SOLUTION DE LA RECHERCHE
!     DE ZERO SUR LE CRITERE EST INFERIEURE A L'ENDOMMAGEMENT ACTUELLE
! --- DONC ON GARDE DONC L'ANCIENNE VALEUR)
    fctd = un - (un+ (a* (yy-y00))**b)* (un-dd)
    if (fctd .gt. zero) then
        y = yy
        esec = e* (un-dd)
        sig = eps*esec - beta*dd
        goto 20
    endif
!
! --- DEBUT DE LA DICHOTOMIE
!     ON RECHERCHE D13 DANS (D,1),
!     CAR L'ENDOMMAGEMENT NE DIMINUE PAS
    iterat=0
    xg=dd
    xd=un
! --- BOUCLE TANT QUE ERREUR
10  continue
!     ON EVALUE LE CRITERE AU MILIEU DE L'INTERVALLE
    d13=xg+(xd-xg)/deux
!
    if (d13 .gt. presq1) then
        d13=presq1
        goto 15
    endif
    yy = (e*eps+beta)* (e*eps+beta)/ (deux*e) - (beta*beta/ (un-d13)/ (un-d13))/ (deux*e)
! --- TEST POUR NE PAS CALCULER UN NOMBRE NEGATIF A UNE PUISSANCE NON
! --- ENTIERE
    if (yy .gt. y00) then
        fctd = un - (un+ (a* (yy-y00))**b)* (un-d13)
    else
        fctd = un
    endif
!
    if (fctd .lt. zero) then
        xg = d13
    else
        xd = d13
    endif
    erreur = abs(fctd)
    iterat=iterat+1
    if ((erreur.gt.tol) .and. (iterat.le.nmaxit)) goto 10
! --- FIN BOUCLE TANT QUE ERREUR
!
15  continue
!
!     MISE A JOUR DE Y A PARTIR DE L'EXPRESSION DE FTCB
    y = y00 + un/a* (d13/ (un-d13))** (un/b)
!     ENREGISTREMENT DU NOUVEL ENDOMMAGEMENT
    d = d13
!
! --- MODULE SECANT
    esec = e* (un-d)
! --- CONTRAINTE : EQUATIONS 3.2.1.3 ET 3.2.3.3
! --- (ON NE SOUVIENT QUE EPS EST DIFFERENT DANS LES DEUX CAS)
    sig = eps*esec - beta*d
!
20  continue
end subroutine
