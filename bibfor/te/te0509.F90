subroutine te0509(option, nomte)
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
#include "jeveux.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES CARACTERISTIQUES SUIVANTES :
!               .LA CONSTANTE DE TORSION         (OPTION 'CARA_TORSION')
!
!              .LE CENTRE DE TORSION/CISAILLEMENT
!              .LES COEFFICIENTS DE CISAILLEMENT (OPTION 'CARA_CISA')
!
!               .L'INERTIE DE GAUCHISSEMENT      (OPTION 'CARA_GAUCHI')
!
!          .LE DOMAINE SUR-LEQUEL ON TRAVAILLE REPRESENTE LA
!           SECTION DE LA POUTRE MAILLEE AVEC DES ELEMENTS 2D
!           ISOPARAMETRIQUES THERMIQUES (THERMIQUES CAR ON
!           DOIT RESOUDRE DES EQUATIONS DE LAPLACE).
!
!-------------------------------------------------------------------
!  OPTION : 'CARA_TORSION' :
!
!          .LA CONSTANTE DE TORSION CT EST DETERMINEE EN FAISANT
!           LA RESOLUTION DE L'EQUATION :
!                LAPLACIEN(PHI) = -2     DANS LA SECTION
!       AVEC     PHI = 0                 SUR LE CONTOUR DE LA SECTION
!           ON A ALORS CT = 2*SOMME_S(PHI.DS)
!
!-------------------------------------------------------------------
!  OPTION : 'CARA_CISA' :
!
!          .LES COEFFICIENTS DE CISAILLEMENT AY ET AZ SONT
!           DETERMINES EN FAISANT RESPECTIVEMENT LA RESOLUTION
!           DE L' EQUATION :
!                G*LAPLACIEN(PSI_Z) = -Z*TZ/IY     DANS LA SECTION
!       AVEC     D(PSI_Z )/DN = 0     SUR LE CONTOUR DE LA SECTION
!       ET       PSI_Z = 0    EN UN NOEUD ARBITRAIRE DE LA SECTION
!
!           ET DE L' EQUATION :
!                G*LAPLACIEN(PSI_Y) = -Y*TY/IZ     DANS LA SECTION
!       AVEC     D(PSI_Y )/DN = 0     SUR LE CONTOUR DE LA SECTION
!       ET       PSI_Y = 0    EN UN NOEUD ARBITRAIRE DE LA SECTION
!
!               AY = 2*S*U1_Y/TY**2
!               AZ = 2*S*U1_Z/TZ**2
!       AVEC U1_Y = 0.5*SOMME_S(G*(GRAD(PSI_Y)**2).DS)
!       AVEC U1_Z = 0.5*SOMME_S(G*(GRAD(PSI_Z)**2).DS)
!
!          X DESIGNE L'AXE DE LA POUTRE
!          Y ET Z DESIGNENT LES AXES PRINCIPAUX D'INERTIE
!          DU PLAN DE LA SECTION
!          DANS LE ROUTINE CES AXES SERONT NOMMES RESPECTIVEMENT X ET Y
!          L'ORIGINE DES AXES DE COORDONNEESEST SITUEE AU CENTRE DE
!          GRAVITE DE LA SECTION
!          N DESIGNE LE VECTEUR NORMAL A LA FRONTIERE
!
!         TY ET TZ DESIGNENT LES EFFORTS TRANCHANTS
!         ON PREND TY = 1 ET TZ = 1
!         ON FAIT L'HYPOTHESE QUE LE MATERIAU EST ISOTROPE
!         AUQUEL CAS LE MODULE DE CISAILLEMENT G N'INTERVIENT PAS
!         LES INERTIES IY ET IZ SONT PRISES EN COMPTE ULTERIEUREMENT
!         AU MOMENT OU L'ON FAIT LA SOMMATION SUR LA SECTION TOTALE
!         DES QUANTITES ELEMENTAIRES.
!
!
!          .LES COORDONNEES DU CENTRE DE TORSION/CISAILLEMENT
!           SONT EGALES A :
!             EY =  MX0_Y/TZ
!             EZ = -MX0_Z/TY
!
!           AVEC MX0_Y = SOMME_S((SIGMA_XZ*Y - SIGMA_XY*Z).DS)
!           SACHANT QUE SIGMA_XY = G*D(PSI_Z)/DY
!                   ET  SIGMA_XZ = G*D(PSI_Z)/DZ
!
!           ET  MX0_Z = SOMME_S((SIGMA_XZ*Y - SIGMA_XY*Z).DS)
!           SACHANT QUE SIGMA_XY = G*D(PSI_Y)/DY
!                   ET  SIGMA_XZ = G*D(PSI_Y)/DZ
!
!-------------------------------------------------------------------
!  OPTION : 'CARA_GAUCHI' :
!
!          .LA CONSTANTE DE GAUCHISSEMENT I_OMEGA EST DETERMINEE
!           EN FAISANT LA RESOLUTION DE L'EQUATION :
!
!                LAPLACIEN(OMEGA) = 0     DANS LA SECTION
!          AVEC :
!     1) D(OMEGA)/D(N) = Z*NY-Y*NZ   SUR LE CONTOUR DE LA SECTION
!     NY ET NZ ETANT LES COMPOSANTES DU VECTEUR N NORMAL A CE CONTOUR
!
!     2) SOMME_S(OMEGA.DS) = 0
!        (VOIR L'ENTETE DE LA ROUTINE PECAP3 POUR PLUS D'EXPLICATIONS)
!
!           ON A ALORS I_OMEGA = SOMME_S(OMEGA**2.DS)
!
!           OMEGA  EST LA FONCTION DE GAUCHISSEMENT
!           I_OMEGA EST L'INERTIE DE GAUCHISSEMENT
!
!-------------------------------------------------------------------
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfde, jgano
    real(kind=8) :: mx0y, mx0z
    real(kind=8) :: dfdx(9), dfdy(9)
!
!
! --- INITIALISATIONS :
!     ---------------
!-----------------------------------------------------------------------
    integer :: i, icase, igau, igeom, ino, itemp1, itemp2
    integer :: itempe, k
    real(kind=8) :: dpsydy, dpsydz, dpszdy, dpszdz, ey, ez, poids
    real(kind=8) :: sigmxy, sigmxz, someg2, sphids, u1y, u1z, xgau
    real(kind=8) :: ygau, zero
!-----------------------------------------------------------------------
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
    zero = 0.0d0
    sphids = zero
    mx0y = zero
    mx0z = zero
    u1y = zero
    u1z = zero
    someg2 = zero
!
! --- RECUPERATION DES COORDONNEES DES CONNECTIVITES DE L'ELEMENT :
!     -----------------------------------------------------------
    call jevech('PGEOMER', 'L', igeom)
!
! --- RECUPERATION DU CHAMP DE SACLAIRES EN SORTIE DU TE :
!     --------------------------------------------------
    call jevech('PCASECT', 'E', icase)
!
!----------------------------------------
! --- OPTION : 'CARA_TORSION'           -
! --- CALCUL DE LA CONSTANTE DE TORSION -
!----------------------------------------
    if (option .eq. 'CARA_TORSION') then
!
!   --- RECUPERATION DU CHAMP D'INCONNUES SCALAIRES SOLUTION DE
!   --- L'EQUATION  : LAPLACIEN(PHI) = -2     DANS LA SECTION
!   --- AVEC          PHI = 0     SUR LE CONTOUR DE LA SECTION :
!       ------------------------------------------------------
        call jevech('PTEMPER', 'L', itempe)
!
!         -----------------------------------
!   ---   -CALCUL DE SOMME/S_ELEMENT(PHI.DS) :
!         -----------------------------------
!
!   --- BOUCLE SUR LES POINTS D'INTEGRATION :
!       -----------------------------------
        do 10 igau = 1, npg
            k=(igau-1)*nno
!
!   ---    CALCUL DES DERIVEES DES FONCTIONS DE FORME  ET DU PRODUIT
!   ---    JACOBIEN*POIDS_INTEGRATION (DANS LA VARIABLE POIDS)
!   ---    AU POINT D'INTEGRATION COURANT :
!          ------------------------------
            call dfdm2d(nno, igau, ipoids, idfde, zr(igeom),&
                        poids, dfdx, dfdy)
!
!   ---    CALCUL DE SOMME/S_ELEMENT(PHI.DS) :
!          ---------------------------------
            do 20 ino = 1, nno
                sphids = sphids + zr(ivf+k+ino-1)*zr(itempe+ino-1)* poids
20          continue
10      continue
!
!   --- AFFECTATION DU CHAMP DE SCALAIRES EN SORTIE
!   --- A LA VALEUR LA VALEUR SOMME/S_ELEMENT(PHI.DS) :
!       ---------------------------------------------
        zr(icase) = sphids
!
!--------------------------------------------------------------
! --- OPTION : 'CARA_CISA'                                    -
! --- CALCUL DES COORDONNES DU CENTRE DE CISAILLEMENT/TORSION -
! --- ET DES COEFFICIENTS DE CISAILLEMENT                     -
!--------------------------------------------------------------
    else if (option.eq.'CARA_CISA') then
!
!   --- RECUPERATION DU CHAMP D'INCONNUES SCALAIRES SOLUTION DE
!   --- L'EQUATION  : LAPLACIEN(PSI_Y) = -Y    DANS LA SECTION
!   --- AVEC  D(PSI_Y )/DN = 0  SUR LE CONTOUR DE LA SECTION
!   --- (C'EST LA CONDITION PAR DEFAUT)
!   --- ET PSI_Y = 0    EN UN NOEUD ARBITRAIRE DE LA SECTION :
!       ----------------------------------------------------
        call jevech('PTEMPE1', 'L', itemp1)
!
!       ---------------------------------------------------------------
!       -CALCUL DE MX0_Z=SOMME/S_ELEMENT((SIGMA_XZ*Y - SIGMA_XY*Z).DS)-
! ---   - AVEC SIGMA_XY = D(PSI_Y)/DY                                 -
!       -  ET  SIGMA_XZ = D(PSI_Y)/DZ                                 -
!       ---------------------------------------------------------------
!
! --- BOUCLE SUR LES POINTS D'INTEGRATION :
!     -----------------------------------
        do 30 igau = 1, npg
            k=(igau-1)*nno
!
! ---    CALCUL DES DERIVEES DES FONCTIONS DE FORME  ET DU PRODUIT
! ---    JACOBIEN*POIDS_INTEGRATION (DANS LA VARIABLE POIDS)
! ---    AU POINT D'INTEGRATION COURANT :
!        ------------------------------
            call dfdm2d(nno, igau, ipoids, idfde, zr(igeom),&
                        poids, dfdx, dfdy)
!
! ---    CALCUL DES CONTRAINTES SIGMA_XY = D(PSI_Y)/DY ET
! ---    SIGMA_XZ = D(PSI_Y)/DZ  AU POINT D'INTEGRATION COURANT :
!        ------------------------------------------------------
            sigmxy = zero
            sigmxz = zero
            xgau = zero
            ygau = zero
!
            do 40 ino = 1, nno
                i = igeom + 2*(ino-1) -1
!
                xgau = xgau + zr(ivf+k+ino-1)*zr(i+1)
                ygau = ygau + zr(ivf+k+ino-1)*zr(i+2)
!
                sigmxy = sigmxy + dfdx(ino)*zr(itemp1+ino-1)
                sigmxz = sigmxz + dfdy(ino)*zr(itemp1+ino-1)
40          continue
!
! ---    CALCUL DE SOMME/S_ELEMENT(SIGMA_XZ*X - SIGMA_XY*Y).DS)
! ---    (Y EST DEVENU X ET Z EST DEVENU Y) :
!        ----------------------------------
            mx0z = mx0z + (sigmxz*xgau - sigmxy*ygau)*poids
30      continue
!
! --- AFFECTATION DU CHAMP DE SCALAIRES EN SORTIE AVEC LA COORDONNEE
! --- SELON Z DU CENTRE DE CISAILLEMENT/TORSION :
!     -----------------------------------------
        ez = -mx0z
        zr(icase+2-1) = ez
!
! --- RECUPERATION DU CHAMP D'INCONNUES SCALAIRES SOLUTION DE
! --- L'EQUATION  : LAPLACIEN(PSI_Z) = -Z    DANS LA SECTION
! --- AVEC  D(PSI_Z )/DN = 0  SUR LE CONTOUR DE LA SECTION
! --- (C'EST LA CONDITION PAR DEFAUT)
! --- ET PSI_Z = 0    EN UN NOEUD ARBITRAIRE DE LA SECTION :
!     ----------------------------------------------------
        call jevech('PTEMPE2', 'L', itemp2)
!
!       ---------------------------------------------------------------
!       -CALCUL DE MX0_Y=SOMME/S_ELEMENT((SIGMA_XZ*Y - SIGMA_XY*Z).DS)-
! ---   - AVEC SIGMA_XY = D(PSI_Z)/DY                                 -
!       -  ET  SIGMA_XZ = D(PSI_Z)/DZ                                 -
!       ---------------------------------------------------------------
!
! --- BOUCLE SUR LES POINTS D'INTEGRATION :
!     -----------------------------------
        do 50 igau = 1, npg
            k=(igau-1)*nno
!
! ---    CALCUL DES DERIVEES DES FONCTIONS DE FORME  ET DU PRODUIT
! ---    JACOBIEN*POIDS_INTEGRATION (DANS LA VARIABLE POIDS)
! ---    AU POINT D'INTEGRATION COURANT :
!        ------------------------------
            call dfdm2d(nno, igau, ipoids, idfde, zr(igeom),&
                        poids, dfdx, dfdy)
!
! ---    CALCUL DES CONTRAINTES SIGMA_XY = D(PSI_Z)/DY ET
! ---    SIGMA_XZ = D(PSI_Z)/DZ  AU POINT D'INTEGRATION COURANT :
!        ------------------------------------------------------
            sigmxy = zero
            sigmxz = zero
            xgau = zero
            ygau = zero
!
            do 60 ino = 1, nno
                i = igeom + 2*(ino-1) -1
!
                xgau = xgau + zr(ivf+k+ino-1)*zr(i+1)
                ygau = ygau + zr(ivf+k+ino-1)*zr(i+2)
!
                sigmxy = sigmxy + dfdx(ino)*zr(itemp2+ino-1)
                sigmxz = sigmxz + dfdy(ino)*zr(itemp2+ino-1)
60          continue
!
! ---    CALCUL DE SOMME/S_ELEMENT(SIGMA_XZ*X - SIGMA_XY*Y).DS)
! ---    (Y EST DEVENU X ET Z EST DEVENU Y) :
!        ----------------------------------
            mx0y = mx0y + (sigmxz*xgau - sigmxy*ygau)*poids
50      continue
!
! --- AFFECTATION DU CHAMP DE SCALAIRES EN SORTIE AVEC LA COORDONNEE
! --- SELON Z DU CENTRE DE CISAILLEMENT/TORSION :
!     -----------------------------------------
        ey = mx0y
        zr(icase+1-1) = ey
!
!----------------------------------------------
! --- CALCUL DES COEFFICIENTS DE CISAILLEMENT -
!----------------------------------------------
!
! ---  CALCUL DE U1_Y =  SOMME_S_ELEMENT(GRAD(PSI_Y)**2.DS)
! ---  ET        U1_Z =  SOMME_S_ELEMENT(GRAD(PSI_Z)**2.DS) :
!       ---------------------------------------------------
!
! --- BOUCLE SUR LES POINTS D'INTEGRATION :
!     -----------------------------------
        do 70 igau = 1, npg
            k=(igau-1)*nno*2
!
! ---    CALCUL DES DERIVEES DES FONCTIONS DE FORME  ET DU PRODUIT
! ---    JACOBIEN*POIDS_INTEGRATION (DANS LA VARIABLE POIDS)
! ---    AU POINT D'INTEGRATION COURANT :
!        ------------------------------
            call dfdm2d(nno, igau, ipoids, idfde, zr(igeom),&
                        poids, dfdx, dfdy)
!
!
! ---    CALCUL D(PSI_Y)/DY, D(PSI_Y)/DZ ET D(PSI_Z)/DY, D(PSI_Z)/DZ
! ---    AU POINT D'INTEGRATION COURANT :
!        -----------------------------
            dpsydy = zero
            dpsydz = zero
            dpszdy = zero
            dpszdz = zero
!
            do 80 ino = 1, nno
!
                dpsydy = dpsydy + dfdx(ino)*zr(itemp1+ino-1)
                dpsydz = dpsydz + dfdy(ino)*zr(itemp1+ino-1)
!
                dpszdy = dpszdy + dfdx(ino)*zr(itemp2+ino-1)
                dpszdz = dpszdz + dfdy(ino)*zr(itemp2+ino-1)
80          continue
!
! ---    CALCUL DE U1_Y ET U1_Z :
!        ----------------------
            u1y = u1y + (dpsydy*dpsydy + dpsydz*dpsydz)*poids
            u1z = u1z + (dpszdy*dpszdy + dpszdz*dpszdz)*poids
70      continue
!
! --- AFFECTATION DU CHAMP DE SCALAIRES EN SORTIE AVEC U1Y ET U1Z
! --- QUI SONT LES CONTRIBUTIONS DE L'ELEMENT AUX COEFFICIENTS DE
! --- CISAILLEMENT DE LA POUTRE A UN COEFFICIENT MULTIPLICATIF PRES :
!     -------------------------------------------------------------
        zr(icase+3-1) = u1z
        zr(icase+4-1) = u1y
!----------------------------------------------
! --- OPTION : 'CARA_GAUCHI'                  -
! --- CALCUL DE LA CONSTANTE DE GAUCHISSEMENT -
! --- SOMME/S__ELEMENT(OMEGA**2.DS)           -
!----------------------------------------------
    else if (option.eq.'CARA_GAUCHI') then
!
!   --- RECUPERATION DU CHAMP D'INCONNUES SCALAIRES SOLUTION DE
!   --- L'EQUATION  : LAPLACIEN(OMEGA) = 0     DANS LA SECTION
!   --- AVEC     1) D(OMEGA)/D(N) = Z*NY-Y*NZ
!   --- SUR LE CONTOUR DE LA SECTION
!   --- NY ET NZ ETANT LES COMPOSANTES DU VECTEUR N NORMAL A CE CONTOUR
!   ---     ET   2) SOMME_S(OMEGA.DS) = 0 :
!       ---------------------------------
        call jevech('PTEMPER', 'L', itempe)
!
!         ----------------------------------------
!   ---   -CALCUL DE SOMME/S_ELEMENT(OMEGA**2.DS) :
!         ----------------------------------------
!
!   --- BOUCLE SUR LES POINTS D'INTEGRATION :
!       -----------------------------------
        do 90 igau = 1, npg
            k=(igau-1)*nno
!
!   ---    CALCUL DES DERIVEES DES FONCTIONS DE FORME  ET DU PRODUIT
!   ---    JACOBIEN*POIDS_INTEGRATION (DANS LA VARIABLE POIDS)
!   ---    AU POINT D'INTEGRATION COURANT :
!          ------------------------------
            call dfdm2d(nno, igau, ipoids, idfde, zr(igeom),&
                        poids, dfdx, dfdy)
!
!   ---    CALCUL DE SOMME/S_ELEMENT(OMEGA**2.DS) :
!          --------------------------------------
            do 100 ino = 1, nno
                someg2 = someg2 + zr(ivf+k+ino-1)* zr(itempe+ino-1)* zr(itempe+ino-1)*poids
100          continue
90      continue
!
!   --- AFFECTATION DU CHAMP DE SCALAIRES EN SORTIE
!   --- A LA VALEUR LA VALEUR SOMME/S_ELEMENT(OMEGA**2.DS) :
!       --------------------------------------------------
        zr(icase+1-1) = someg2
    endif
!
end subroutine
