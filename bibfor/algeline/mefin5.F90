function mefin5(nbz, nbgrp, imod, icyl, jmod,&
                jcyl, z, f1, f2, f3,&
                g)
    implicit none
!
    integer :: nbz, nbgrp, imod, icyl, jmod, jcyl
    real(kind=8) :: z(*), f1(nbz*nbgrp, *), f2(nbz*nbgrp, *), f3(*), g(*)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!     CALCUL DE L'INTEGRALE SUR (0,L) DE F3(Z)*F1(Z)*F2'(Z)
!     PAR LA METHODE DES TRAPEZES OU F1 EST LA DEFORMEE
!     DU MODE (IMOD) SUR LE CYLINDRE (ICYL) ET F2 CELLE
!     DU MODE (JMOD) SUR LE CYLINDRE (JCYL)
!     OPERATEUR APPELANT : OP0144 , FLUST3, MEFIST, MEFMAT
! ----------------------------------------------------------------------
!     OPTION DE CALCUL   : CALC_FLUI_STRU , CALCUL DES PARAMETRES DE
!     COUPLAGE FLUIDE-STRUCTURE POUR UNE CONFIGURATION DE TYPE "FAISCEAU
!     DE TUBES SOUS ECOULEMENT AXIAL"
! ----------------------------------------------------------------------
! IN  : NBZ    : NOMBRE DE NOEUDS DE LA DISCRETISATION AXIALE
! IN  : NBGRP  : NOMBRE DE GROUPES D EQUIVALENCE
! IN  : IMOD   : NUMERO DU MODE POUR LA FONCTION F1
! IN  : ICYL   : INDICE DU CYLINDRE POUR LA FONCTION F1
! IN  : JMOD   : NUMERO DU MODE POUR LA FONCTION F2
! IN  : JCYL   : INDICE DU GROUPE DE CYLINDRE POUR LA FONCTION F2
! IN  : Z      : COORDONNEES 'Z' DANS LE REPERE AXIAL DES
!                POINTS DISCRETISES, IDENTIQUES POUR TOUS LES CYLINDRES
! IN  : F1     : PREMIERE FONCTION
! IN  : F2     : DEUXIEME FONCTION
! IN  : F3     : TROISIEME FONCTION
! --  : G      : TABLEAU DE TRAVAIL
! OUT : MEFIN5 : INTEGRALE CALCULEE
! ----------------------------------------------------------------------
    real(kind=8) :: mefin5
! ----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
    integer :: n, nbz1, nbz2
!-----------------------------------------------------------------------
    nbz1 = nbz*(icyl-1)
    nbz2 = nbz*(jcyl-1)
!
!     ---------------------------------------
!     CALCUL DE F2'(Z,J) -> G(Z)
!     MINIMISATION QUADRATIQUE DES RESTES DES
!     DEVELOPPEMENTS DE TAYLOR DE F2(Z,J)
!     A GAUCHE ET A DROITE
!     ---------------------------------------
!
    g(1) = (f2(nbz2+2,jmod)-f2(nbz2+1,jmod))/(z(2)-z(1))
!
    do 1 n = 2, nbz-1
        g(n) = (&
               (&
               f2(n+nbz2+1,jmod)-f2(n+nbz2,jmod))*(z(n+1)-z(n)) +(f2(n+nbz2-1,jmod)-f2(n+nbz2,jmo&
               &d))*(z(n-1)-z(n))) /((z(n+1)- z(n))*(z(n+1)-z(n)) +(z(n-1)-z(n))*(z(n-1)-z(n)&
               )&
               )
 1  end do
!
    g(nbz) = (f2(nbz*jcyl,jmod)-f2(nbz*jcyl-1,jmod))/ (z(nbz)-z(nbz-1))
!
    mefin5 = 0.d0
!
    do 2 n = 1, nbz-1
        mefin5 = mefin5+0.5d0*(&
                 z(n+1)-z(n))* (f3(n+1)*f1(n+nbz1+1, imod)*g(n+1) +f3(n)*f1(n+nbz1,imod)*g(n))
 2  end do
!
end function
