subroutine te0104(option, nomte)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/cq3d2d.h'
    include 'asterfort/dfdm1d.h'
    include 'asterfort/dfdm2d.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/jevete.h'
    character(len=16) :: option, nomte
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
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
!                          OPTION : 'RIGI_THER_COEH_R'
!                          CAS COQUE
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
    integer :: ndimax, ind, nbddl, kp, kq
    parameter (ndimax=27)
    real(kind=8) :: b(3, 3), lamb, theta, rigith(ndimax, ndimax)
    real(kind=8) :: coor2d(18), cour, cosa, sina, r, zero
    real(kind=8) :: dfdx(9), dfdy(9), poids, poi1, poi2, hplus, hmoins, pk
    real(kind=8) :: long, matn(3, 3), matp(3, 3)
    integer :: nno, nnos, npg2, gi, pi, gj, pj, k, imattt, jgano, ndim
    integer :: ipoids, ivf, idfde, igeom, npg1, i, j, icoefh, itemps, mzr
!
!
    if (nomte .ne. 'THCPSE3 ' .and. nomte .ne. 'THCASE3 ' .and. nomte .ne. 'THCOSE3 ' .and.&
        nomte .ne. 'THCOSE2 ') then
        call elref4(' ', 'MASS', ndim, nno, nnos,&
                    npg2, ipoids, ivf, idfde, jgano)
    else
        call elref4(' ', 'RIGI', ndim, nno, nnos,&
                    npg1, ipoids, ivf, idfde, jgano)
        npg2 = 3
    endif
!
! --- INITIALISATIONS :
!     ---------------
    zero = 0.0d0
!
    do 20 i = 1, ndimax
        do 10 j = 1, ndimax
            rigith(i,j) = zero
10      continue
20  end do
!
! --- RECUPERATION DES COORDONNEES DES NOEUDS DE L'ELEMENT :
!     ----------------------------------------------------
    call jevech('PGEOMER', 'L', igeom)
!
! --- RECUPERATION DU COEFFICIENT D'ECHANGE :
!     -------------------------------------
    call jevech('PCOEFHR', 'L', icoefh)
!
! --- RECUPERATION DE L'INSTANT DU CALCUL ET
! --- DU PARAMETRE THETA DE LA METHODE 'THETA' UTILISEE
! --- POUR RESOUDRE L'EQUATION DIFFERENTIELLE EN TEMPS DE LA
! --- TEMPERATURE (EN STATIONNAIRE THETA =1 ) :
!     ---------------------------------------
    call jevech('PTEMPSR', 'L', itemps)
    theta = zr(itemps+2)
!
! --- DETERMINATION DE LA CONTRIBUTION A LA RIGIDITE THERMIQUE
! --- DES ECHANGES DE LA COQUE AVEC L'EXTERIEUR AU NIVEAU DES
! --- FEUILLETS INFERIEUR ET SUPERIEUR :
!     ================================
!
! --- CAS DES COQUES SURFACIQUES :
!     --------------------------
    if (nomte(1:8) .ne. 'THCOSE3 ' .and. nomte(1:8) .ne. 'THCOSE2 ') then
!
! ---   CALCUL DU COEFFICIENT D'ECHANGE DU FEUILLET INFERIEUR
! ---   DE LA COQUE AVEC L'EXTERIEUR :
!       ----------------------------
        hmoins = zr(icoefh)
!
! ---   CALCUL DU COEFFICIENT D'ECHANGE DU FEUILLET SUPERIEUR
! ---   DE LA COQUE AVEC L'EXTERIEUR :
!       ----------------------------
        hplus = zr(icoefh+1)
!
! ---   CONTRIBUTION AU TENSEUR DE CONDUCTIVITE TRANSVERSE B DES
! ---   ECHANGES AVEC L'EXTERIEUR
! ---            (0 0  0 )
! ---       B =  (0 H- 0 )
! ---            (0 0  H+)
!       -------------------
        b(1,1) = zero
        b(2,1) = zero
        b(2,2) = hmoins
        b(3,1) = zero
        b(3,2) = zero
        b(3,3) = hplus
    endif
!
    if (nomte(1:8) .ne. 'THCPSE3 ' .and. nomte(1:8) .ne. 'THCASE3 ' .and. nomte(1:8) .ne.&
        'THCOSE3 ' .and. nomte(1:8) .ne. 'THCOSE2 ') then
!
! ---  CALCUL DE LA RIGIDITE THERMIQUE DUE AU TERME D'ECHANGE B :
!      ========================================================
!
! ---   DETERMINATION DES COORDONNEES COOR2D DES NOEUDS DE L'ELEMENT
! ---   DANS LE REPERE DE L'ELEMENT :
!       ---------------------------
        call cq3d2d(nno, zr(igeom), 1.d0, zero, coor2d)
!
! ---   BOUCLE SUR LES POINTS D'INTEGRATION :
!       -----------------------------------
        do 70 kp = 1, npg2
            k = (kp-1)*nno
            call dfdm2d(nno, kp, ipoids, idfde, coor2d,&
                        dfdx, dfdy, poids)
            do 60 gi = 1, nno
                do 50 gj = 1, gi
                    do 40 pi = 1, 3
                        do 30 pj = 1, pi
                            pk = b(pi,pj)*zr(ivf+k+gi-1)*zr(ivf+k+gj- 1)*poids* theta
!
! ---     AFFECTATION DES TERMES HORS DIAGONAUX DE LA TRIANGULAIRE
! ---     INFERIEURE DE LA SOUS-MATRICE :
!         -----------------------------
                            if ((pi.ne.pj) .and. (gi.ne.gj)) then
                                i = 3* (gi-1) + pj
                                j = 3* (gj-1) + pi
                                rigith(i,j) = rigith(i,j) + pk
                            endif
!
! ---     AFFECTATION DES TERMES DE LA TRIANGULAIRE SUPERIEURE
! ---     DE LA SOUS-MATRICE :
!         ------------------
                            i = 3* (gi-1) + pi
                            j = 3* (gj-1) + pj
                            rigith(i,j) = rigith(i,j) + pk
30                      continue
40                  continue
50              continue
60          continue
70      continue
!
! --- CAS DES COQUES LINEIQUES (EN CONTRAINTES PLANES ET AXI) :
!     -------------------------------------------------------
        else if (nomte(1:8).eq.'THCPSE3 ' .or. nomte(1:8).eq.'THCASE3 ')&
    then
!
! --- BOUCLE SUR LES POINTS D'INTEGRATION :
!     -----------------------------------
        do 130 kp = 1, npg2
            k = (kp-1)*nno
            call dfdm1d(nno, zr(ipoids+kp-1), zr(idfde+k), zr(igeom), dfdx,&
                        cour, poids, cosa, sina)
!
            if (nomte .eq. 'THCASE3') then
                r = zero
                do 80 i = 1, nno
                    r = r + zr(igeom+2* (i-1))*zr(ivf+k+i-1)
80              continue
                poids = poids*r
            endif
!
            do 120 gi = 1, nno
                do 110 gj = 1, gi
                    do 100 pi = 1, 3
                        do 90 pj = 1, pi
                            pk = b(pi,pj)*zr(ivf+k+gi-1)*zr(ivf+k+gj- 1)*poids* theta
!
! ---     AFFECTATION DES TERMES HORS DIAGONAUX DE LA TRIANGULAIRE
! ---     INFERIEURE DE LA SOUS-MATRICE :
!         -----------------------------
                            if ((pi.ne.pj) .and. (gi.ne.gj)) then
                                i = 3* (gi-1) + pj
                                j = 3* (gj-1) + pi
                                rigith(i,j) = rigith(i,j) + pk
                            endif
!
! ---     AFFECTATION DES TERMES DE LA TRIANGULAIRE SUPERIEURE
! ---     DE LA SOUS-MATRICE :
!         ------------------
                            i = 3* (gi-1) + pi
                            j = 3* (gj-1) + pj
                            rigith(i,j) = rigith(i,j) + pk
90                      continue
100                  continue
110              continue
120          continue
130      continue
!
! --- CAS DES COQUES LINEIQUES (AUTRES QUE CONTRAINTES PLANES ET AXI) :
!     --------------------------------------------------------------
        else if (nomte(1:8).eq.'THCOSE3 ' .or. nomte(1:8).eq.'THCOSE2 ')&
    then
!
!
        call jevete('&INEL.'//nomte(1:8)//'.DEMR', ' ', mzr)
!CC     CALL JEVECH('PCACOQU','L',ICACOQ)
!
!CC     EP=ZR(ICACOQ)
!
        long = (&
               zr(igeom+3)-zr(igeom))**2 + (zr(igeom+3+1)-zr(igeom+1) )**2 + (zr(igeom+3+2)-zr(ig&
               &eom+2)&
               )**2
        long = sqrt(long)/2.d0
!       EP  =EP/2.D0
!
!      IMPORTANT: LAMB = CONV * EPAISSEUR
!
        lamb = zr(icoefh)/2.d0
!
! ---   DETERMINATION DE LA 'PART' DE RIGIDITE THERMIQUE DU A L'ECHANGE
! ---   AVEC L'EXTERIEUR POUR LES COQUES LINEIQUES
! ---   ON RAPPELLE QUE LE TERME GENERIQUE DE CETTE MATRICE A POUR
! ---   EXPRESSION :
! ---   B(I,J) = SOMME_VOLUME(H*NI(X,Y,Z)*NJ(X,Y,Z).DX.DY.DZ)
! ---   SOIT
! ---   B(I,J) = SOMME_LONGUEUR (H*NI(X,Y)*NJ(X,Y).DX.DY)
! ---           *SOMME_EPAISSEUR(PK(Z)*PL(Z).DZ)
! ---   OU LES PK ET PL SONT LES FONCTIONS D'INTERPOLATION DANS
! ---   L'EPAISSEUR
! ---   PLUS EXACTEMENT P1(Z), P2(Z), P3(Z) SONT LES POLYNOMES
! ---   DE LAGRANGE (OU AUTRES) DE DEGRE 2 RELATIFS A L'INTERPOLATION
! ---   DE LA TEMPERATURE DANS L'EPAISSEUR TELS QUE
! ---   P1 EST RELATIF A LA TEMPERATURE MOYENNE
! ---   P2 EST RELATIF A LA TEMPERATURE INFERIEURE
! ---   P3 EST RELATIF A LA TEMPERATURE SUPERIEURE
! ---   (I.E. T(X,Y,Z) =    P1(Z)*TMOY(X,Y)
! ---                     + P2(Z)*TINF(X,Y)
! ---                     + P3(Z)*TSUP(X,Y)) :
!       ------------------------------------
        do 150 i = 1, 3
            do 140 j = 1, 3
                matp(i,j) = zero
                matn(i,j) = zero
140          continue
150      continue
!
! ---   DETERMINATION DE LA MATRICE MATP DONT LE TERME GENERIQUE
! ---   EST MATP(I,J) = SOMME_EPAISSEUR(PI(Z)*PJ(Z).DZ) :
!       -----------------------------------------------
        do 160 kp = 1, npg2
            kq = (kp-1)*3
!
            poi1 = zr(mzr-1+12+kp)
!
            matp(1,1) = matp(1,1) + poi1*zr(mzr-1+kq+1)**2
            matp(1,2) = matp(1,2) + poi1*zr(mzr-1+kq+1)*zr(mzr-1+kq+2)
            matp(1,3) = matp(1,3) + poi1*zr(mzr-1+kq+1)*zr(mzr-1+kq+3)
            matp(2,1) = matp(1,2)
            matp(2,2) = matp(2,2) + poi1*zr(mzr-1+kq+2)**2
            matp(2,3) = matp(2,3) + poi1*zr(mzr-1+kq+2)*zr(mzr-1+kq+3)
            matp(3,1) = matp(1,3)
            matp(3,2) = matp(2,3)
            matp(3,3) = matp(3,3) + poi1*zr(mzr-1+kq+3)**2
160      continue
!
! ---   DETERMINATION DE LA MATRICE MATN DONT LE TERME GENERIQUE
! ---   EST MATN(I,J) = SOMME_LONGUEUR (H*NI(X,Y)*NJ(X,Y).DX.DY) :
!       --------------------------------------------------------
        do 170 kp = 1, npg1
            kq = (kp-1)*nno
!
            poi2 = zr(ipoids-1+kp)
!
            matn(1,1) = matn(1,1) + poi2*zr(ivf-1+kq+1)**2
            matn(1,2) = matn(1,2) + poi2*zr(ivf-1+kq+1)*zr(ivf-1+kq+2)
            matn(2,1) = matn(1,2)
            matn(2,2) = matn(2,2) + poi2*zr(ivf-1+kq+2)**2
!
            if (nomte(1:8) .eq. 'THCOSE3 ') then
                matn(1,3) = matn(1,3) + poi2*zr(ivf-1+kq+1)*zr(ivf-1+ kq+3)
                matn(2,3) = matn(2,3) + poi2*zr(ivf-1+kq+2)*zr(ivf-1+ kq+3)
                matn(3,1) = matn(1,3)
                matn(3,2) = matn(2,3)
                matn(3,3) = matn(3,3) + poi2*zr(ivf-1+kq+3)**2
            endif
!
170      continue
!
        rigith(1,1) = matn(1,1)*matp(1,1)
        rigith(1,2) = matn(1,1)*matp(1,2)
        rigith(1,3) = matn(1,1)*matp(1,3)
        rigith(1,4) = matn(1,2)*matp(1,1)
        rigith(1,5) = matn(1,2)*matp(1,2)
        rigith(1,6) = matn(1,2)*matp(1,3)
!
        rigith(2,1) = rigith(1,2)
        rigith(2,2) = matn(1,1)*matp(2,2)
        rigith(2,3) = matn(1,1)*matp(2,3)
        rigith(2,4) = matn(1,2)*matp(2,1)
        rigith(2,5) = matn(1,2)*matp(2,2)
        rigith(2,6) = matn(1,2)*matp(2,3)
!
        rigith(3,1) = rigith(1,3)
        rigith(3,2) = rigith(2,3)
        rigith(3,3) = matn(1,1)*matp(3,3)
        rigith(3,4) = matn(1,2)*matp(3,1)
        rigith(3,5) = matn(1,2)*matp(3,2)
        rigith(3,6) = matn(1,2)*matp(3,3)
!
        rigith(4,1) = rigith(1,4)
        rigith(4,2) = rigith(2,4)
        rigith(4,3) = rigith(3,4)
        rigith(4,4) = matn(2,2)*matp(1,1)
        rigith(4,5) = matn(2,2)*matp(1,2)
        rigith(4,6) = matn(2,2)*matp(1,3)
!
        rigith(5,1) = rigith(1,5)
        rigith(5,2) = rigith(2,5)
        rigith(5,3) = rigith(3,5)
        rigith(5,4) = rigith(4,5)
        rigith(5,5) = matn(2,2)*matp(2,2)
        rigith(5,6) = matn(2,2)*matp(2,3)
!
        rigith(6,1) = rigith(1,6)
        rigith(6,2) = rigith(2,6)
        rigith(6,3) = rigith(3,6)
        rigith(6,4) = rigith(4,6)
        rigith(6,5) = rigith(5,6)
        rigith(6,6) = matn(2,2)*matp(3,3)
!
        if (nomte(1:8) .eq. 'THCOSE3 ') then
!
            rigith(1,7) = matn(1,3)*matp(1,1)
            rigith(1,8) = matn(1,3)*matp(1,2)
            rigith(1,9) = matn(1,3)*matp(1,3)
!
            rigith(2,7) = matn(1,3)*matp(2,1)
            rigith(2,8) = matn(1,3)*matp(2,2)
            rigith(2,9) = matn(1,3)*matp(2,3)
!
            rigith(3,7) = matn(1,3)*matp(3,1)
            rigith(3,8) = matn(1,3)*matp(3,2)
            rigith(3,9) = matn(1,3)*matp(3,3)
!
            rigith(4,7) = matn(2,3)*matp(1,1)
            rigith(4,8) = matn(2,3)*matp(1,2)
            rigith(4,9) = matn(2,3)*matp(1,3)
!
            rigith(5,7) = matn(2,3)*matp(2,1)
            rigith(5,8) = matn(2,3)*matp(2,2)
            rigith(5,9) = matn(2,3)*matp(2,3)
!
            rigith(6,7) = matn(2,3)*matp(3,1)
            rigith(6,8) = matn(2,3)*matp(3,2)
            rigith(6,9) = matn(2,3)*matp(3,3)
!
            rigith(7,1) = rigith(1,7)
            rigith(7,2) = rigith(2,7)
            rigith(7,3) = rigith(3,7)
            rigith(7,4) = rigith(4,7)
            rigith(7,5) = rigith(5,7)
            rigith(7,6) = rigith(6,7)
            rigith(7,7) = matn(3,3)*matp(1,1)
            rigith(7,8) = matn(3,3)*matp(1,2)
            rigith(7,9) = matn(3,3)*matp(1,3)
!
            rigith(8,1) = rigith(1,8)
            rigith(8,2) = rigith(2,8)
            rigith(8,3) = rigith(3,8)
            rigith(8,4) = rigith(4,8)
            rigith(8,5) = rigith(5,8)
            rigith(8,6) = rigith(6,8)
            rigith(8,7) = rigith(7,8)
            rigith(8,8) = matn(3,3)*matp(2,2)
            rigith(8,9) = matn(3,3)*matp(2,3)
!
            rigith(9,1) = rigith(1,9)
            rigith(9,2) = rigith(2,9)
            rigith(9,3) = rigith(3,9)
            rigith(9,4) = rigith(4,9)
            rigith(9,5) = rigith(5,9)
            rigith(9,6) = rigith(6,9)
            rigith(9,7) = rigith(7,9)
            rigith(9,8) = rigith(8,9)
            rigith(9,9) = matn(3,3)*matp(3,3)
        endif
!
!       LAMB=LAMB*LONG*THETA*EP
        lamb = lamb*long*theta
!
        do 190 i = 1, 3*nno
            do 180 j = 1, i
                rigith(i,j) = rigith(i,j)*lamb
180          continue
190      continue
!
    endif
!
! --- RECUPERATION DE LA MATRICE DE RIGIDITE THERMIQUE EN SORTIE DU TE :
!     ----------------------------------------------------------------
    call jevech('PMATTTR', 'E', imattt)
!
! --- AFFECTATION DE LA MATRICE DE RIGIDITE THERMIQUE EN SORTIE DU TE :
!     ---------------------------------------------------------------
    nbddl = 3*nno
    ind = 0
    do 210 i = 1, nbddl
        do 200 j = 1, i
            ind = ind + 1
            zr(imattt+ind-1) = rigith(i,j)
200      continue
210  end do
!
end subroutine
