subroutine te0282(option, nomte)
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!
!      CALCUL DU TAUX DE RESTITUTION D'ENERGIE ELEMENTAIRE
!      BORDS ELEMENTS ISOPARAMETRIQUES 2D AVEC CHARGEMENT DE BORD
!      PRESSION-CISAILLEMENT ET FORCE REPARTIE
!
!      OPTION : 'CALC_G'    (G AVEC CHARGES REELLES)
!               'CALC_G_F'  (G AVEC CHARGES FONCTIONS)
!
! ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!
! VECTEURS DIMENSIONNES POUR  NNO = 3 , NPG = 4
!
! ----------------------------------------------------------------------
!
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
    include 'jeveux.h'
    include 'asterc/r8prem.h'
    include 'asterfort/elref1.h'
    include 'asterfort/elref5.h'
    include 'asterfort/fointe.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jevech.h'
    include 'asterfort/u2mess.h'
    character(len=16) :: option, nomte
!
!
    integer :: nno, nnos, jgano, ndim, npg, kp, ipoids, ivf, idfdk, igeom, icode
    integer :: idepl, iforc, ipres, ithet, igthet, itemps, compt, i, j, k
    integer :: ipref, iforf
    integer :: jdfd2, jcoopg
!
    real(kind=8) :: xg, yg, ux, uy, fx, fy, thx, thy, the
    real(kind=8) :: tcla, tsurf, tsurp, epsi, pres, cisa, divthe, valpar(3)
    real(kind=8) :: vf, dfde, dxde, dyde, dsde, poids, dthxde, dthyde
    real(kind=8) :: dfxde, dfyde, presno, cisano, fxno, fyno
!                               2*NNO     2*NNO
    real(kind=8) :: presg(2), forcg(2), presn(6), forcn(6)
    real(kind=8) :: prod, dsde2
    real(kind=8) :: tsom
!
    character(len=2) :: chelem
    character(len=8) :: nompar(3), elrefe
!
    logical :: fonc, chargn
!
! =====================================================================
! INITIALISATIONS
! =====================================================================
    call elref1(elrefe)
    call jemarq()
    epsi = r8prem()
    chelem = nomte(3:4)
!
! RECUPERATION DES DONNEES GEOMETRIQUES LIEES AU CALCUL ELEMENTAIRE
    call elref5(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, jcoopg, ivf, idfdk,&
                jdfd2, jgano)
!
!
! INIT. POUR LE CALCUL DE G
    chargn = .false.
    tcla = 0.d0
    tsurf = 0.d0
    tsurp = 0.d0
    call jevech('PTHETAR', 'L', ithet)
    call jevech('PGTHETA', 'E', igthet)
!
! TEST SUR LA NULLITE DE THETA_FISSURE
    compt = 0
    do 250 i = 1, nno
        thx = zr(ithet + 2*(i - 1) )
        thy = zr(ithet + 2*(i - 1) + 1 )
        if ((abs(thx).lt.epsi) .and. (abs(thy).lt.epsi)) then
            compt = compt + 1
        endif
250  end do
    if (compt .eq. nno) goto 9999
!
! =====================================================================
! RECUPERATION DES CHAMPS LOCAUX
! =====================================================================
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PDEPLAR', 'L', idepl)
    if (option .eq. 'CALC_G_F') then
        fonc = .true.
        call jevech('PFF1D2D', 'L', iforf)
        call jevech('PPRESSF', 'L', ipref)
        call jevech('PTEMPSR', 'L', itemps)
        nompar(1) = 'X'
        nompar(2) = 'Y'
        nompar(3) = 'INST'
        valpar(3) = zr(itemps)
    else
        fonc =.false.
        call jevech('PFR1D2D', 'L', iforc)
        call jevech('PPRESSR', 'L', ipres)
    endif
!
! =====================================================================
! - SI CHARGE FONCTION RECUPERATION DES VALEURS AUX PG ET NOEUDS
! =====================================================================
!
    if (fonc) then
        do 70 i = 1, nno
            do 80 j = 1, 2
                valpar(j) = zr(igeom+2*(i-1)+j-1)
80          continue
            do 75 j = 1, 2
                call fointe('FM', zk8(ipref+j-1), 3, nompar, valpar,&
                            presn(2*(i-1)+j), icode)
                call fointe('FM', zk8(iforf+j-1), 3, nompar, valpar,&
                            forcn(2*(i-1)+j), icode)
75          continue
70      continue
    endif
!
! ======================================================================
! BOUCLE PRINCIPALE SUR LES POINTS DE GAUSS
! ======================================================================
!
    do 800 kp = 1, npg
!
! INITIALISATIONS
        k = (kp-1)*nno
        dxde = 0.d0
        dyde = 0.d0
        xg = 0.d0
        yg = 0.d0
        ux = 0.d0
        uy = 0.d0
        thx = 0.d0
        thy = 0.d0
        dfxde = 0.d0
        dfyde = 0.d0
        dthxde = 0.d0
        dthyde = 0.d0
        fx = 0.d0
        fy = 0.d0
!
! ===========================================
! CALCUL DES ELEMENTS GEOMETRIQUES
! ===========================================
!
! CALCUL DES DERIVEES PARTIELLES PREMIERES DU VECTEURS
! POSITIONS (DXDE,DYDE) AU POINT DE GAUSS,
! DU VECTEUR POSITION AU POINT DE GAUSS (XG,YG), DE SON VECTEUR
! DEPLACEMENT (UX,UY), DU CHAMP THETA FISSURE (THX,THY) ET DE SON
! GRADIENT (DTHXDE,DTHYDE).
        do 10 i = 1, nno
            vf = zr(ivf +k+i-1)
            dfde = zr(idfdk+k+i-1)
            dxde = dxde + dfde*zr(igeom+2*(i-1))
            dyde = dyde + dfde*zr(igeom+2*(i-1)+1)
            xg = xg + vf *zr(igeom+2*(i-1) )
            yg = yg + vf *zr(igeom+2*(i-1)+1)
            ux = ux + vf *zr(idepl+2*(i-1) )
            uy = uy + vf *zr(idepl+2*(i-1)+1)
            thx = thx + vf *zr(ithet+2*(i-1) )
            thy = thy + vf *zr(ithet+2*(i-1)+1)
            dthxde = dthxde + dfde*zr(ithet+2*(i-1) )
            dthyde = dthyde + dfde*zr(ithet+2*(i-1)+1)
10      continue
!
! ===========================================
! CALCUL DU CHARGEMENT ET DE SON GRADIENT
! ===========================================
!
        if (fonc) then
            valpar(1) = xg
            valpar(2) = yg
            do 65 j = 1, 2
                call fointe('FM', zk8(ipref+j-1), 3, nompar, valpar,&
                            presg(j), icode)
                call fointe('FM', zk8(iforf+j-1), 3, nompar, valpar,&
                            forcg(j), icode)
65          continue
        else
            presg(1) = 0.d0
            presg(2) = 0.d0
            forcg(1) = 0.d0
            forcg(2) = 0.d0
            do 4 i = 1, nno
                do 6 j = 1, 2
                    presg(j) = presg(j) + zr(ipres+2*(i-1)+j-1)*zr( ivf+k+i-1)
                    forcg(j) = forcg(j) + zr(iforc+2*(i-1)+j-1)*zr( ivf+k+i-1)
 6              continue
 4          continue
        endif
!
! VALEURS DU CHARGEMENT AUX POINTS DE GAUSS (FX,FY)
        dsde = sqrt(dxde*dxde+dyde*dyde)
        dsde2 = dsde*dsde
        pres = presg(1)
        cisa = presg(2)
        fx = forcg(1)-(dyde*pres-dxde*cisa)/dsde
        fy = forcg(2)+(dxde*pres+dyde*cisa)/dsde
!
! VALEURS DU CHARGEMENT AUX NOEUDS (FXNO,FYNO) ET DE SES DERIVEES
! AUX POINTS DE GAUSS (DFXDE,DFYDE,D2FXDE,D2FYDE)
        if (fonc) then
            do 300 i = 1, nno
                dfde = zr(idfdk+k+i-1)
                presno = presn(2*(i-1)+1)
                cisano = presn(2*(i-1)+2)
                fxno = forcn(2*(i-1)+1)-(dyde*presno-dxde*cisano)/ dsde
                fyno = forcn(2*(i-1)+2)+(dxde*presno+dyde*cisano)/ dsde
                dfxde = dfxde + dfde*fxno
                dfyde = dfyde + dfde*fyno
300          continue
        endif
!
! TESTS SUR LA NULLITE DES CHARGEMENTS ET DE LEURS GRADIENTS POUR EVITER
! DE FAIRE DES CALCULS INUTILES ET DETECTER LES VRAIS PROBLEMES
        if ((fx.eq.0.d0) .and. (fy.eq.0.d0) .and. (dfxde.eq.0.d0) .and. (dfyde.eq.0.d0)) then
            chargn = .true.
        endif
!
! CAS PARTICULIER D'UN CALCUL SUR L'AXE
        if (xg .eq. 0.d0) then
!
! ON EST SUR L'AXE AVEC CHARGEMENTS NULS DONC G (ET DG) = 0
            if (chargn) then
                goto 799
            else if (chelem .eq. 'AX') then
                call u2mess('F', 'RUPTURE1_23')
            endif
        else
!
! CAS GENERAL AVEC CHARGEMENTS NULS DONC G (ET DG) = 0
            if (chargn) goto 799
        endif
!
! CALCUL DU TERME ELEMENTAIRE
        if (chelem .eq. 'AX') then
            poids = zr(ipoids+kp-1)*dsde*xg
        else
            poids = zr(ipoids+kp-1)*dsde
        endif
        the = (thx*dxde+thy*dyde)/dsde2
        divthe = (dthxde*dxde+dthyde*dyde)/dsde2
!
! =======================================================
! PRISE EN COMPTE DE LA MODELISATION POUR G
! =======================================================
!
        if (chelem .eq. 'AX') divthe = divthe+(thx/xg)
!
! =======================================================
! CALCUL DU TAUX DE RESTITUTION G
! =======================================================
!
        prod = (divthe*fx+dfxde*the)*ux + (divthe*fy+dfyde*the)*uy
!
        tcla = tcla + prod*poids
!
! BRANCHEMENT POUR F=0 ET DF=0
799      continue
!
! ======================================================================
! FIN DE BOUCLE PRINCIPALE SUR LES POINTS DE GAUSS
! ======================================================================
800  end do
!
! EXIT EN CAS DE THETA FISSURE NUL PARTOUT
9999  continue
!
! ASSEMBLAGE FINAL DES TERMES DE G
    tsom = tcla + tsurf + tsurp
    zr(igthet) = tsom
!
    call jedema()
end subroutine
