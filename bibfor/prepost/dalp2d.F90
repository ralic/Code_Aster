subroutine dalp2d(nelem, nnoem, degre, nsommx, icnc,&
                  nelcom, numeli, xy, erreur, energi,&
                  aire, alpha, nalpha)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! TOLE CRS_1404
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
!        TEST2 : SI ERREUR (NOEU1-2EM COUCHE) < ERREUR (NOEU1)     *
!          TEST3 : SI ERREUR (NOEU1) > 6*ERREUR (NOEU1-3EM COUCHE) *
!          NOEU1 EST SINGULIER                                     *
!    RQ : CES DIFFERENTS TESTS PERMETTENT D ETRE SUR QUE           *
!    NOEU1 EST BIEN SINGULIER                                      *
! 2) COMMENT CALCULE T-ON PE=ALPHAN(NOEU1) ?                       *
!    SI NOEU1 REGULIER ALPHAN(NOEU1)=DEGRE D INTERPOLATION DE L EF *
!    SI NOEU1 SINGULIER CALCUL DE PE                               *
! 3) COMMENT CALCULE T-ON PE ?                                     *
!    CONSTRUCTION DE LA COURBE ENERGIE EN FONCTION DU RAYON        *
!    EN 2D L ENERGIE EST CALCULEE SUR DES CERCLES DE CENTRE NOEU1  *
!    ET POUR DIFFERENTS RAYONS                                     *
!    CALCUL DE PE PAR REFERENCE A L ENERGIE EN POINTE DE FISSURE   *
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
!     CONNECTIVITE  NOEUD N°X=>N° DES EF UTILE CONNECTES A X
! IN  NDIM                   : DIMENSION DU PROBLEME
! IN  XY(3,NNOEM)            : COORDONNEES DES NOEUDS
! IN  ERREUR(NELEM)          : ERREUR SUR CHAQUE EF
! IN  ENERGI(NELEM)          : ENERGIE SUR CHAQUE EF
! IN  AIRE(NELEM)            : SURFACE DE CHAQUE EF
! OUT ALPHA(NELEM)           : DEGRE DE LA SINGULARITE PAR ELEMENT
! OUT NALPHA                 : NOMBRE DE CPE PAR ELEMENT DIFFERENTS
!                              1 PAR DEFAUT SI PAS DE SINGULARITE
!
    implicit none
!
! DECLARATION GLOBALE
!
    include 'asterfort/dfort2.h'
    include 'asterfort/dzonfg.h'
    integer :: nelem, nnoem, degre, nsommx, nelcom
    integer :: icnc(nsommx+2, nelem), numeli(nelcom+2, nnoem)
    integer :: nalpha
    real(kind=8) :: xy(3, nnoem), erreur(nelem), energi(nelem), aire(nelem)
    real(kind=8) :: alpha(nelem)
!
! DECLARATION LOCALE
!
    integer :: inno, inel, nuef
    integer :: tbnozo(1000), nbnozo(3), tbelzo(1000), nbelzo(3)
    integer :: nbnoe
    real(kind=8) :: factpm, factp
    parameter (factpm = 2.0d+0,factp=3.0d+0)
    real(kind=8) :: precmo, precre, prec1, prec2, prec3, airtot
    real(kind=8) :: dtyp, alphan(nnoem), alpref, pe
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
    airtot = 0.d+0
    do 30 inel = 1, nelem
        precmo = precmo + erreur(inel)**2
        airtot = airtot + aire(inel)
30  end do
    precmo = sqrt( precmo / airtot )
    precre = precmo * factpm
!
! 4 - BOUCLE SUR LES NOEUDS SOMMETS POUR DETECTER
!     SI LE NOEUD CONSIDERE EST SINGULIER OU PAS
!     RQ : ICNC(2,INNO)=0 NOEUD MILIEU
!
    do 40 inno = 1, nnoem
        if (numeli(2,inno) .eq. 0) goto 40
!
! 4.1 - ERREUR LOCALE SUR LA COUCHE 1 (=EF CONNECTES A INNO)
!
        prec1 = 0.d+0
        airtot = 0.d+0
!
        do 50 inel = 1, numeli(1, inno)
            nuef = numeli(2+inel,inno)
            prec1 = prec1 + erreur(nuef)**2
            airtot = airtot + aire(nuef)
50      continue
        if (prec1 .ne. 0.d+0 .and. airtot .ne. 0.d+0) then
            prec1 = sqrt(prec1/airtot)
        else
            prec1 = 0.d+0
        endif
!
! 4.2 - TEST1 : SI PREC1 > PRECREF ON PASSE AU TEST2
!
        if (prec1 .gt. precre) then
!
! 4.2.1 - RECHERCHE DES NOEUDS ET EFS COMPOSANTS LES COUCHES 1,2 ET 3
!
            call dzonfg(nsommx, icnc, nelcom, numeli, inno,&
                        tbelzo, nbelzo, tbnozo, nbnozo)
!
! 4.2.2 - ERREUR LOCALE SUR LA COUCHE 2
!
            prec2 = 0.d+0
            airtot = 0.d+0
            do 60 inel = nbelzo(1)+1, nbelzo(2)
                nuef = tbelzo(inel)
                prec2 = prec2 + erreur(nuef)**2
                airtot = airtot + aire(nuef)
60          continue
            if (prec2 .ne. 0.d+0 .and. airtot .ne. 0.d+0) then
                prec2 = sqrt(prec2/airtot)
            else
                prec2 = prec1
            endif
!
! 4.2.3 - TEST2 : SI PREC2<PREC1 ON PASSE AU TEST3
!
            if (prec2 .lt. prec1) then
!
! 4.2.3.1 - ERREUR LOCALE SUR LA COUCHE 3
!
                prec3 = 0.d+0
                airtot = 0.d+0
                do 70 inel = nbelzo(2)+1, nbelzo(3)
                    nuef = tbelzo(inel)
                    prec3 = prec3 + erreur(nuef)**2
                    airtot = airtot + aire(nuef)
70              continue
                if (prec3 .ne. 0.d+0 .and. airtot .ne. 0.d+0) then
                    prec3 = sqrt(prec3/airtot)
                else
                    prec3 = prec2
                endif
                prec3 = min(prec3,prec2)
!
! 4.2.3.2 - TEST3 : SI PREC1>6*PREC3 LE NOEUD INNO EST SINGULIER
!
                if (prec1/prec3 .ge. factp) then
!
! 4.2.3.2.1 - CALCUL DE PE
!
                    nbnoe=nbnozo(1)+nbnozo(2)+nbnozo(3)
                    call dfort2(nsommx, icnc, inno, tbelzo, nbelzo(3),&
                                tbnozo, nbnozo, nbnoe, xy, aire,&
                                energi, pe)
!
                    alpref=0.95d+0
                    if (pe .lt. alpref) then
                        alphan(inno) = 0.5d+0
                        if (pe .ge. 0.5d0) alphan(inno)=pe
                    else
                        alphan(inno) = alphan(inno)*0.9999d+0
                    endif
!
! 4.2.3.2 - FIN DU TEST3 PREC1>6*PREC3
                endif
! 4.2.3 - FIN DU TEST2 PREC2<PREC1
            endif
! 4.2 - FIN DU TEST1 PREC1>PRECREF
        endif
! 4 - FIN DE LA BOUCLE SUR LES NOEUDS
40  end do
!
! 5 - ON REMPLIT LE TABLEAU ALPHA = DEGRE DE LA SINGULARITE PAR EF
!
    nalpha=1
    do 100 inel = 1, nelem
        if (icnc(2,inel) .lt. 1) goto 100
        do 110 inno = 1, icnc(1, inel)
            alpha(inel) = min(alpha(inel),alphan(icnc(inno+2,inel)))
110      continue
        if (alpha(inel) .lt. dtyp) then
            nalpha = nalpha + 1
        endif
100  end do
!
end subroutine
