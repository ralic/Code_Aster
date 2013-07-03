subroutine dalp3d(nelem, nnoem, degre, nsommx, icnc,&
                  nelcom, numeli, xy, erreur, energi,&
                  volume, alpha, nalpha)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!*******************************************************************
!              BUT DE CETTE ROUTINE :                              *
! CALCULER LE DEGRE DE LA SINGULARITE PE EN CHAQUE NOEUD           *
! PUIS EN CHAQUE EF A PARTIR DE PE                                 *
! 1) COMMENT REPERE T-ON UN NOEUD SINGULIER ?                      *
!    L ERREUR LOCALE EST GRANDE EN CE NOEUD                        *
!    COMPAREE A L ERREUR DE REFERENCE SUR TOUTE LA STRUCTURE       *
!    DEROULEMENT DE LA ROUTINE :                                   *
!                                                                  *
!    CALCUL DE L ERREUR DE REFERENCE                               *
!    NOEU1 EST IL SINGULIER ?                                      *
!      CALCUL DE L ERREUR LOCALE SUR LES EFS CONNECTES A NOEU1     *
!      TEST1 : SI ERREUR (NOEU1) > ERREUR GLOBALE                  *
!        NOEU2 = NOEUDS CONNECTES A NOEU1                          *
!        CALCUL DE L ERREUR LOCALE SUR LES EFS CONNECTES A NOEU2   *
!        TEST2 : SI ERREUR (NOEU2) > ERREUR GLOBALE                *
!        LE NOEU1 EST SINGULIER                                    *
!    REMARQUE : CONTRAIREMENT AU 2D NOEU1 EST SINGULIER UNIQUEMENT *
!               SI L UN DE SES VOISINS L EST AUSSI                 *
!               => ON OUBLIE LES NOEUDS SINGULIERS ISOLES          *
! 2) COMMENT CALCULE T-ON PE=ALPHAN(NOEU1) ?                       *
!    SI NOEU1 REGULIER ALPHAN(NOEU1)=DEGRE D INTERPOLATION DE L EF *
!    SI NOEU1 SINGULIER CALCUL DE PE       *
!      ALPHAN(NOEU1)=MIN(ALPHAN(NOEU1),PE) ET                      *
!      ALPHAN(NOEU2)=MIN(ALPHAN(NOEU2),PE)                         *
! 3) COMMENT CALCULE T-ON PE ?                                     *
!    CONSTRUCTION DE LA COURBE ENERGIE EN FONCTION DU RAYON        *
!    EN 3D L ENERGIE EST CALCULEE SUR DES CYLINDRES D EXTREMITE    *
!    NOEU1 ET NOEU2 ET POUR DIFFERENTS RAYONS                      *
!    CALCUL DE PE PAR REFERENCE A L ENERGIE EN POINTE DE FISSURE   *
! 4) REMARQUE : EN 3D ON REGARDE UNIQUEMENT LES NOEUDS SOMMET BORD *
!*******************************************************************
!
! IN  NELEM                  : NOMBRE D ELEMENTS FINIS
! IN  NNOEM                  : NOMBRE DE NOEUDS
! IN  DEGRE                  : DEGRE DES EF (1 POUR P1 2 POUR P2)
! IN  NSOMMX                 : NBRE DE NOEUDS SOMMETS MAX PAR EF
! IN  ICNC(NSOMMX+2,NELEM)   : EF => NOEUDS SOMMETS CONNECTES A EF
!     1ERE VALEUR = NBRE DE NOEUDS SOMMETS CONNECTES A L EF N°X
!     2EME VALEUR = 1 SI EF UTILE 0 SINON
!     CONNECTIVITE  EF N°X=>N° DE NOEUDS SOMMETS CONNECTES A X
!     EN 2D EF UTILE = QUAD OU TRIA
!     EN 3D EF UTILE = TETRA OU HEXA
! IN  NELCOM                 : NBRE D EF SURFACIQUE MAX PAR NOEUD
! IN  NUMELI(NELCOM+2,NNOEM) : NOEUD =>EF SURFACIQUE CONNECTES A NOEUD
!     1ERE VALEUR = NBRE D EFS UTILES CONNECTES AU NOEUD N°X
!     2EME VALEUR = 0 NOEUD MILIEU OU NON CONNECTE A UN EF UTILE
!                   1 NOEUD SOMMET A L INTERIEUR + LIE A UN EF UTILE
!                   2 NOEUD SOMMET BORD + LIE A UN EF UTILE
!     CONNECTIVITE  NOEUD N
! IN  XY(3,NNOEM)            : COORDONNEES DES NOEUDS
! IN  ERREUR(NELEM)          : ERREUR SUR CHAQUE EF
! IN  ENERGI(NELEM)          : ENERGIE SUR CHAQUE EF
! IN  VOLUME(NELEM)          : VOLUME DE CHAQUE EF
! OUT ALPHA(NELEM)           : DEGRE DE LA SINGULARITE PAR ELEMENT
! OUT NALPHA                 : NOMBRE DE CPE PAR ELEMENT DIFFERENTS
!                              1 PAR DEFAUT SI PAS DE SINGULARITE
!
! aslint: disable=W1306
    implicit none
!
! DECLARATION GLOBALE
!
#include "asterfort/dfort3.h"
#include "asterfort/dzonfg.h"
    integer :: nelem, nnoem, degre, nsommx, nelcom
    integer :: icnc(nsommx+2, nelem), numeli(nelcom+2, nnoem)
    integer :: nalpha
    real(kind=8) :: xy(3, nnoem), erreur(nelem), energi(nelem), volume(nelem)
    real(kind=8) :: alpha(nelem)
!
! DECLARATION LOCALE
!
    integer :: i, inno, inel, nuef, noeu1, noeu2
    integer :: tbnozo(1000), nbnozo(3), tbelzo(1000), nbelzo(3)
    integer :: nbnoe
    real(kind=8) :: precmo, precre, prec, vol
    real(kind=8) :: dtyp, alphan(nnoem), pe
!
! 1 - DEGRE D INTERPOLATION
!
    if (degre .eq. 1) then
        dtyp = 1.d+0
    else
        dtyp = 2.d+0
    endif
!
! 2 - INITIALISATION DES ALPHA = DEGRE D INTERPOLATION
!
    do 10 inno = 1, nnoem
        alphan(inno)= dtyp
10  end do
    do 20 inel = 1, nelem
        alpha(inel) = dtyp
20  end do
!
! 3 - CALCUL DES PRECISIONS MOYENNE ET DE REFERENCE
!
    precmo = 0.d+0
    vol = 0.d+0
    do 30 inel = 1, nelem
        precmo = precmo + erreur(inel)**2
        vol = vol + volume(inel)
30  end do
    precmo = sqrt( precmo / vol )
!
    if (dtyp .eq. 1.0d0) then
        precre = 3.d0 * precmo
    else if (dtyp.eq.2.0d0) then
        precre = 2.d0 * precmo
    endif
!
! 4 - BOUCLE SUR LES NOEUDS SOMMET BORD POUR DETECTER
!     SI LE NOEUD CONSIDERE EST SINGULIER OU PAS
!
    do 40 inno = 1, nnoem
!
        if (numeli(2,inno) .ne. 2) goto 40
!
! 4.1 - ERREUR LOCALE SUR COUCHE 1 (=EF CONNECTES A INNO)
!
        prec = 0.0d0
        vol = 0.0d0
!
        do 50 inel = 1, numeli(1, inno)
            nuef=numeli(2+inel,inno)
            prec = prec + erreur(nuef)**2
            vol = vol + volume(nuef)
50      continue
        if (prec .ne. 0.d+0 .and. vol .ne. 0.d+0) then
            prec = sqrt(prec/vol)
        else
            prec = 0.d+0
        endif
!
! 4.2 - TEST1 : SI PREC > PRECREF ON VA CHERCHER LES NOEUDS NOEU2
!       CONNECTES A INNO
!
        if (prec .ge. precre) then
!
! 4.2.1 - RECHERCHE DES NOEUDS ET EFS COMPOSANTS LES COUCHES 1,2 ET 3
!
            call dzonfg(nsommx, icnc, nelcom, numeli, inno,&
                        tbelzo, nbelzo, tbnozo, nbnozo)
            noeu1=inno
!
! 4.2.2 - TEST2 : POUR CHAQUE NOEU2 CONNECTE A NOEU1
!         ON DETECTE SI NOEU2 EST SINGULIER
!         SI OUI ON DETERMINE PE ET UNIQUEMENT SI NOEU2>NOEU1
!         POUR EVITER DE FAIRE DEUX FOIS LE CALCUL
!
            do 60 i = 1, nbnozo(1)
                noeu2 = tbnozo(i)
                if (numeli(2,noeu2) .ne. 2) goto 60
                if (noeu2 .gt. noeu1) then
                    vol = 0.0d0
                    prec = 0.0d0
                    do 70 inel = 1, numeli(1, noeu2)
                        nuef=numeli(2+inel,noeu2)
                        prec = prec + erreur(nuef)**2
                        vol = vol + volume(nuef)
70                  continue
                    if (prec .ne. 0.d+0 .and. vol .ne. 0.d+0) then
                        prec = sqrt(prec/vol)
                    else
                        prec = 0.d+0
                    endif
!
                    if (prec .ge. precre) then
!
! 4.2.2.1 - CALCUL DE PE
!
                        nbnoe=nbnozo(1)+nbnozo(2)+nbnozo(3)
                        call dfort3(nsommx, icnc, noeu1, noeu2, tbelzo,&
                                    nbelzo(3), tbnozo, nbnoe, xy, volume,&
                                    energi, pe)
!
                        if (alphan(noeu1) .gt. pe) alphan(noeu1)=pe
                        if (alphan(noeu2) .gt. pe) alphan(noeu2)=pe
                    endif
                endif
60          continue
        endif
40  end do
!
! 5 - ON REMPLIT LE TABLEAU ALPHA = DEGRE DE LA SINGULARITE PAR EF
!
    nalpha = 1
    do 110 inel = 1, nelem
        if (icnc(2,inel) .lt. 1) goto 110
        do 120 inno = 1, icnc(1, inel)
            alpha(inel) = min(alpha(inel),alphan(icnc(inno+2,inel)))
120      continue
        if (alpha(inel) .lt. dtyp) then
            nalpha = nalpha + 1
        endif
110  end do
!
end subroutine
