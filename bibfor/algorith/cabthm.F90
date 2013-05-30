subroutine cabthm(nddls, nddlm, nno, nnos, nnom,&
                  dimuel, dimdef, ndim, kpi, ipoids,&
                  ipoid2, ivf, ivf2, idfde, idfde2,&
                  dfdi, dfdi2, geom, poids, poids2,&
                  b, nmec, yamec, addeme, yap1,&
                  addep1, yap2, addep2, yate, addete,&
                  np1, np2, axi)
!
    implicit none
! ======================================================================
! person_in_charge: sylvie.granet at edf.fr
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
! TOLE CRP_21
! ======================================================================
!     BUT:  CALCUL  DE LA MATRICE B EN MODE D'INTEGRATION MIXTE
!              AVEC ELEMENTS P2P1
!     EN MECANIQUE DES MILIEUX POREUX PARTIELLEMENT SATURE
!     AVEC COUPLAGE THM
! ======================================================================
!.......................................................................
! ARGUMENTS D'ENTREE
!
! ======================================================================
! AXI       AXISYMETRIQUE?
! TYPMOD    MODELISATION (D_PLAN, AXI, 3D ?)
! MODINT    METHODE D'INTEGRATION (CLASSIQUE,LUMPEE(D),REDUITE(R) ?)
! NNO       NB DE NOEUDS DE L'ELEMENT
! NNOS      NB DE NOEUDS SOMMETS DE L'ELEMENT
! NNOM      NB DE NOEUDS MILIEUX DE L'ELEMENT
! NDDLS     NB DE DDL SUR LES SOMMETS
! NDDLM     NB DE DDL SUR LES MILIEUX
! NPI       NB DE POINTS D'INTEGRATION DE L'ELEMENT
! NPG       NB DE POINTS DE GAUSS     POUR CLASSIQUE(=NPI)
!                 SOMMETS             POUR LUMPEE   (=NPI=NNOS)
!                 POINTS DE GAUSS     POUR REDUITE  (<NPI)
! NDIM      DIMENSION DE L'ESPACE
! DIMUEL    NB DE DDL TOTAL DE L'ELEMENT
! DIMCON    DIMENSION DES CONTRAINTES GENERALISEES ELEMENTAIRES
! DIMDEF    DIMENSION DES DEFORMATIONS GENERALISEES ELEMENTAIRES
! IVF       FONCTIONS DE FORMES QUADRATIQUES
! IVF2      FONCTIONS DE FORMES LINEAIRES
!
!                     sommets              |    milieux
!           u v p t u v p t u v p t u v p t u v u v u v u v
!          ------------------------------------------------
!        u|                                |               |
!        v|     Fonctions de forme         |               |
!        E|              P2                |       P2      |
!          ------------------------------------------------
!        P|                                |               |
!       DP|                                |         0     |
!        T|              P1                |               |
!       DT|                                |               |
!          ------------------------------------------------
!
! ======================================================================
! ======================================================================
    include 'jeveux.h'
    include 'asterfort/dfdm2d.h'
    include 'asterfort/dfdm3d.h'
    include 'asterfort/matini.h'
    logical :: axi
    integer :: nddls, nddlm, nmec, np1, np2, ndim, nno, i, n, kk, yamec
    integer :: nnos, nnom, kpi, dimdef, dimuel, ipoids, idfde, ivf
    integer :: addeme, yap1, yap2, addep1, addep2, yate, addete
    integer :: ipoid2, idfde2, ivf2
    real(kind=8) :: dfdi(nno, 3), dfdi2(nnos, 3), geom(ndim, nno), poids, poids2
    real(kind=8) :: b(dimdef, dimuel), rac, r, rmax
! ======================================================================
! --- CALCUL DE CONSTANTES UTILES --------------------------------------
! ======================================================================
    rac= sqrt(2.d0)
! ======================================================================
! --- INITIALISATION DE LA MATRICE B -----------------------------------
! ======================================================================
    call matini(dimdef, dimuel, 0.d0, b)
! ======================================================================
! --- RECUPERATION DES DERIVEES DES FONCTIONS DE FORME -----------------
! ======================================================================
! --- EN 3D ------------------------------------------------------------
! ======================================================================
    if (ndim .eq. 3) then
! ======================================================================
! --- CAS QUADRATIQUES -------------------------------------------------
! ======================================================================
        call dfdm3d(nno, kpi, ipoids, idfde, geom,&
                    dfdi(1, 1), dfdi(1, 2), dfdi(1, 3), poids)
! ======================================================================
! --- CAS LINEAIRES ----------------------------------------------------
! ======================================================================
        call dfdm3d(nnos, kpi, ipoid2, idfde2, geom,&
                    dfdi2(1, 1), dfdi2(1, 2), dfdi2(1, 3), poids2)
! ======================================================================
! --- EN 2D ------------------------------------------------------------
! ======================================================================
    else
! ======================================================================
! --- CAS QUADRATIQUES -------------------------------------------------
! ======================================================================
        call dfdm2d(nno, kpi, ipoids, idfde, geom,&
                    dfdi(1, 1), dfdi(1, 2), poids)
! ======================================================================
! --- CAS LINEAIRES ----------------------------------------------------
! ======================================================================
        call dfdm2d(nnos, kpi, ipoid2, idfde2, geom,&
                    dfdi2(1, 1), dfdi2(1, 2), poids2)
!
        do 200 n = 1, nnos
            dfdi2(n,3)=0.d0
200      continue
        do 201 n = 1, nno
            dfdi(n,3)=0.d0
201      continue
    endif
! ======================================================================
! --- MODIFICATION DU POIDS POUR LES MODELISATIONS AXIS ----------------
! ======================================================================
    if (axi) then
        kk = (kpi-1)*nno
        r = 0.d0
        do 10 n = 1, nno
            r = r + zr(ivf + n + kk - 1)*geom(1,n)
10      continue
! ======================================================================
! --- DANS LE CAS OU R EGAL 0, ON A UN JACOBIEN NUL --------------------
! --- EN UN POINT DE GAUSS, ON PREND LE MAX DU RAYON -------------------
! --- SUR L ELEMENT MULTIPLIE PAR 1E-3 ---------------------------------
! ======================================================================
        if (r .eq. 0.d0) then
            rmax=geom(1,1)
            do 15 n = 2, nno
                rmax=max(geom(1,n),rmax)
15          continue
            poids = poids*1.d-03*rmax
        else
            poids = poids*r
        endif
    endif
! ======================================================================
! --- REMPLISSAGE DE L OPERATEUR B -------------------------------------
! ======================================================================
! --- ON COMMENCE PAR LA PARTIE GAUCHE DE B CORRESPONDANT --------------
! --- AUX NOEUDS SOMMETS -----------------------------------------------
! ======================================================================
    do 102 n = 1, nnos
! ======================================================================
        if (yamec .eq. 1) then
            do 103 i = 1, ndim
                b(addeme-1+i,(n-1)*nddls+i)= b(addeme-1+i,(n-1)*nddls+&
                i)+zr(ivf+n+(kpi-1)*nno-1)
103          continue
! ======================================================================
! --- CALCUL DE DEPSX, DEPSY, DEPSZ (DEPSZ INITIALISE A 0 EN 2D) -------
! ======================================================================
            do 104 i = 1, ndim
                b(addeme+ndim-1+i,(n-1)*nddls+i)= b(addeme+ndim-1+i,(&
                n-1)*nddls+i)+dfdi(n,i)
104          continue
! ======================================================================
! --- TERME U/R DANS EPSZ EN AXI ---------------------------------------
! ======================================================================
            if (axi) then
                if (r .eq. 0.d0) then
                    b(addeme+4,(n-1)*nddls+1)= dfdi(n,1)
                else
                    kk=(kpi-1)*nno
                    b(addeme+4,(n-1)*nddls+1)=zr(ivf+n+kk-1)/r
                endif
            endif
! ======================================================================
! --- CALCUL DE EPSXY --------------------------------------------------
! ======================================================================
            b(addeme+ndim+3,(n-1)*nddls+1)= b(addeme+ndim+3,(n-1)*&
            nddls+1)+dfdi(n,2)/rac
!
            b(addeme+ndim+3,(n-1)*nddls+2)= b(addeme+ndim+3,(n-1)*&
            nddls+2)+dfdi(n,1)/rac
! ======================================================================
! --- CALCUL DE EPSXZ ET EPSYZ EN 3D -----------------------------------
! ======================================================================
            if (ndim .eq. 3) then
                b(addeme+ndim+4,(n-1)*nddls+1)= b(addeme+ndim+4,(n-1)*&
                nddls+1)+dfdi(n,3)/rac
!
                b(addeme+ndim+4,(n-1)*nddls+3)= b(addeme+ndim+4,(n-1)*&
                nddls+3)+dfdi(n,1)/rac
!
                b(addeme+ndim+5,(n-1)*nddls+2)= b(addeme+ndim+5,(n-1)*&
                nddls+2)+dfdi(n,3)/rac
!
                b(addeme+ndim+5,(n-1)*nddls+3)= b(addeme+ndim+5,(n-1)*&
                nddls+3)+dfdi(n,2)/rac
            endif
        endif
! ======================================================================
! --- TERMES THERMO-HYDRAULIQUES (FONCTIONS DE FORMES P1) --------------
! ======================================================================
! --- SI PRESS1 --------------------------------------------------------
! ======================================================================
        if (yap1 .eq. 1) then
            b(addep1,(n-1)*nddls+nmec+1)= b(addep1,(n-1)*nddls+nmec+1)&
            +zr(ivf2+n+(kpi-1)*nnos-1)
            do 105 i = 1, ndim
                b(addep1+i,(n-1)*nddls+nmec+1)= b(addep1+i,(n-1)*&
                nddls+nmec+1)+dfdi2(n,i)
105          continue
        endif
! ======================================================================
! --- SI PRESS2 --------------------------------------------------------
! ======================================================================
        if (yap2 .eq. 1) then
            b(addep2,(n-1)*nddls+nmec+np1+1)= b(addep2,(n-1)*nddls+&
            nmec+np1+1)+zr(ivf2+n+(kpi-1)*nnos-1)
            do 106 i = 1, ndim
                b(addep2+i,(n-1)*nddls+nmec+np1+1)= b(addep2+i,(n-1)*&
                nddls+nmec+np1+1)+dfdi2(n,i)
106          continue
        endif
! ======================================================================
! --- SI TEMPE ---------------------------------------------------------
! ======================================================================
        if (yate .eq. 1) then
            b(addete,(n-1)*nddls+nmec+np1+np2+1)= b(addete,(n-1)*&
            nddls+nmec+np1+np2+1) +zr(ivf2+n+(kpi-1)*nnos-1)
            do 107 i = 1, ndim
                b(addete+i,(n-1)*nddls+nmec+np1+np2+1)= b(addete+i,(n-&
                1)*nddls+nmec+np1+np2+1)+dfdi2(n,i)
107          continue
        endif
102  end do
! ======================================================================
! --- ON REMPLIT MAINTENANT LE COIN SUPERIEUR DROIT DE B CORRESPONDANT -
! --- AUX NOEUDS MILIEUX (MECANIQUE - FONCTIONS DE FORMES P2) ----------
! ======================================================================
    if (yamec .eq. 1) then
        do 300 n = 1, nnom
            do 301 i = 1, ndim
                b(addeme-1+i,nnos*nddls+(n-1)*nddlm+i)= b(addeme-1+i,&
                nnos*nddls+(n-1)*nddlm+i) +zr(ivf+n+nnos+(kpi-1)*nno-&
                1)
301          continue
! ======================================================================
! --- CALCUL DE DEPSX, DEPSY, DEPSZ (DEPSZ INITIALISE A 0 EN 2D) -------
! ======================================================================
            do 304 i = 1, ndim
                b(addeme+ndim-1+i,nnos*nddls+(n-1)*nddlm+i)= b(addeme+&
                ndim-1+i,nnos*nddls+(n-1)*nddlm+i) +dfdi(n+nnos,i)
304          continue
! ======================================================================
! --- TERME U/R DANS EPSZ EN AXI ---------------------------------------
! ======================================================================
            if (axi) then
                if (r .eq. 0.d0) then
                    b(addeme+4,nnos*nddls+(n-1)*nddlm+1)=dfdi(n+nnos,&
                    1)
                else
                    kk=(kpi-1)*nno
                    b(addeme+4,nnos*nddls+(n-1)*nddlm+1)= zr(ivf+n+&
                    nnos+kk-1)/r
                endif
            endif
! ======================================================================
! --- CALCUL DE EPSXY POUR LES NOEUDS MILIEUX --------------------------
! ======================================================================
            b(addeme+ndim+3,nnos*nddls+(n-1)*nddlm+1)= b(addeme+ndim+&
            3,nnos*nddls+(n-1)*nddlm+1)+dfdi(n+nnos,2)/rac
!
            b(addeme+ndim+3,nnos*nddls+(n-1)*nddlm+2)= b(addeme+ndim+&
            3,nnos*nddls+(n-1)*nddlm+2)+dfdi(n+nnos,1)/rac
! ======================================================================
! --- CALCUL DE EPSXZ ET EPSYZ EN 3D POUR NOEUDS MILIEUX ---------------
! ======================================================================
            if (ndim .eq. 3) then
                b(addeme+ndim+4,nnos*nddls+(n-1)*nddlm+1)= b(addeme+&
                ndim+4,nnos*nddls+(n-1)*nddlm+1) +dfdi(n+nnos,3)/rac
!
                b(addeme+ndim+4,nnos*nddls+(n-1)*nddlm+3)= b(addeme+&
                ndim+4,nnos*nddls+(n-1)*nddlm+3) +dfdi(n+nnos,1)/rac
!
                b(addeme+ndim+5,nnos*nddls+(n-1)*nddlm+2)= b(addeme+&
                ndim+5,nnos*nddls+(n-1)*nddlm+2) +dfdi(n+nnos,3)/rac
!
                b(addeme+ndim+5,nnos*nddls+(n-1)*nddlm+3)= b(addeme+&
                ndim+5,nnos*nddls+(n-1)*nddlm+3) +dfdi(n+nnos,2)/rac
            endif
300      continue
    endif
! ======================================================================
! --- LE COIN INFERIEUR DROIT EST NUL ----------------------------------
! ======================================================================
end subroutine
