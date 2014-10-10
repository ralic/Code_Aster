subroutine xcabhm(nddls, nddlm, nnop, nnops, nnopm,&
                  dimuel, ndim, kpi, ff, ff2,&
                  dfdi, dfdi2, b, nmec, yamec,&
                  addeme, yap1, addep1, np1, axi,&
                  ivf, ipoids, idfde, poids, coorse,&
                  nno, geom, yaenrm, adenme, dimenr,&
                  he, jlsn)
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
! ======================================================================
!     BUT:  CALCUL  DE LA MATRICE B EN MODE D'INTEGRATION MIXTE
!           AVEC ELEMENTS P2P1 EN MECANIQUE DES MILIEUX POREUX
!           AVEC COUPLAGE HM EN XFEM
! ======================================================================
! AXI       AXISYMETRIQUE?
! TYPMOD    MODELISATION (D_PLAN, AXI, 3D ?)
! MODINT    METHODE D'INTEGRATION (CLASSIQUE,LUMPEE(D),REDUITE(R) ?)
! NNOP      NB DE NOEUDS DE L'ELEMENT PARENT
! NNOPS     NB DE NOEUDS SOMMETS DE L'ELEMENT PARENT
! NNOPM     NB DE NOEUDS MILIEUX DE L'ELEMENT PARENT
! NDDLS     NB DE DDL SUR LES SOMMETS
! NDDLM     NB DE DDL SUR LES MILIEUX
! NPI       NB DE POINTS D'INTEGRATION DE L'ELEMENT
! NPG       NB DE POINTS DE GAUSS     POUR CLASSIQUE(=NPI)
!                 SOMMETS             POUR LUMPEE   (=NPI=NNOS)
!                 POINTS DE GAUSS     POUR REDUITE  (<NPI)
! NDIM      DIMENSION DE L'ESPACE
! DIMUEL    NB DE DDL TOTAL DE L'ELEMENT
! DIMCON    DIMENSION DES CONTRAINTES GENERALISEES ELEMENTAIRES
! DIMENR    DIMENSION DES DEFORMATIONS GENERALISEES ELEMENTAIRES ENRICHI
!
!                                sommets           |         milieux
!           u v w p H1X H1Y H1Z u v w p H1X HIY H1Z u v w H1X H1Y H1Z u v w H1X HIY H1Z
!          ----------------------------------------------------------------------------
!        u|                                        |                                  |
!        v|          Fonctions de forme            |                                  |
!        w|                                        |                                  |
!        E|                   P2                   |                P2                |
!          ----------------------------------------------------------------------------
!        P|                                        |                                  |
!       DP|                   P1                   |                 0                |
!          ----------------------------------------------------------------------------
!      H1X|                                        |                                  |
!      H1Y|                   P2                   |                P2                |
!      H1Z|                                        |                                  |
!        E|                                        |                                  |
!          ----------------------------------------------------------------------------
!
! =====================================================================================
! =====================================================================================
#include "asterf_types.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/matini.h"
#include "asterfort/assert.h"
#include "asterfort/xcalf_he.h"
#include "jeveux.h"
    aster_logical :: axi
    integer :: nddls, nddlm, nmec, np1, ndim, nnop, i, n, kk, yamec
    integer :: nnops, nnopm, kpi, dimuel
    integer :: addeme, yap1, addep1
    integer :: yaenrm, adenme, dimenr
    integer :: ipoids, idfde, ivf, nno, jlsn
    real(kind=8) :: dfdi(nnop, ndim), dfdi2(nnops, ndim)
    real(kind=8) :: ff(nnop), ff2(nnops)
    real(kind=8) :: b(dimenr, dimuel), rac, r, rmax, geom(ndim, nnop)
    real(kind=8) :: rbid1(nno), rbid2(nno), rbid3(nno)
    real(kind=8) :: he, poids, coorse(81)
! ======================================================================
! --- CALCUL DE CONSTANTES UTILES --------------------------------------
! ======================================================================
    rac= sqrt(2.d0)
! ======================================================================
! --- INITIALISATION DE LA MATRICE B -----------------------------------
! ======================================================================
    call matini(dimenr, dimuel, 0.d0, b)
! ======================================================================
! --- CALCUL DU JACOBIEN DE LA TRANSFORMATION SSTET-> SSTET REF --------
! --- AVEC LES COORDONNEES DES SOUS-ELEMENTS ---------------------------
! ======================================================================
    ASSERT((ndim .eq. 2) .or. (ndim .eq. 3))
    if (ndim .eq. 2) then
        call dfdm2d(nno, kpi, ipoids, idfde, coorse,&
                    poids, rbid1, rbid2)
    else if (ndim .eq. 3) then
        call dfdm3d(nno, kpi, ipoids, idfde, coorse,&
                    poids, rbid1, rbid2, rbid3)
    endif
! ======================================================================
! --- MODIFICATION DU POIDS POUR LES MODELISATIONS AXIS ----------------
! ======================================================================
    if (axi) then
        kk = (kpi-1)*nnop
        r = 0.d0
        do 10 n = 1, nnop
            r = r + zr(ivf+n+kk-1)*geom(1,n)
 10     continue
! ======================================================================
! --- DANS LE CAS OU R EGAL 0, ON A UN JACOBIEN NUL --------------------
! --- EN UN POINT DE GAUSS, ON PREND LE MAX DU RAYON -------------------
! --- SUR L ELEMENT MULTIPLIE PAR 1E-3 ---------------------------------
! ======================================================================
        if (r .eq. 0.d0) then
            rmax=geom(1,1)
            do 15 n = 2, nnop
                rmax=max(geom(1,n),rmax)
 15         continue
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
    do 102 n = 1, nnops
! ======================================================================
        if (yamec .eq. 1) then
            do 103 i = 1, ndim
                b(addeme-1+i,(n-1)*nddls+i)= b(addeme-1+i,(n-1)*nddls+&
                i)+ff(n)
103         continue
! ======================================================================
! --- CALCUL DE DEPSX, DEPSY, DEPSZ (DEPSZ INITIALISE A 0 EN 2D) -------
! ======================================================================
            do 104 i = 1, ndim
                b(addeme+ndim-1+i,(n-1)*nddls+i)= b(addeme+ndim-1+i,(&
                n-1)*nddls+i)+dfdi(n,i)
104         continue
! ======================================================================
! --- TERME U/R DANS EPSZ EN AXI ---------------------------------------
! ======================================================================
            if (axi) then
                if (r .eq. 0.d0) then
                    b(addeme+4,(n-1)*nddls+1)=dfdi(n,1)
                else
                    b(addeme+4,(n-1)*nddls+1)=ff(n)/r
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
!
            if (ndim .eq. 3) then
! ======================================================================
! --- CALCUL DE EPSXZ --------------------------------------------------
! ======================================================================
                b(addeme+ndim+4,(n-1)*nddls+1)= b(addeme+ndim+4,(n-1)*&
            nddls+1)+dfdi(n,3)/rac
!
                b(addeme+ndim+4,(n-1)*nddls+3)= b(addeme+ndim+4,(n-1)*&
            nddls+3)+dfdi(n,1)/rac
! ======================================================================
! --- CALCUL DE EPSYZ --------------------------------------------------
! ======================================================================
                b(addeme+ndim+5,(n-1)*nddls+2)= b(addeme+ndim+5,(n-1)*&
            nddls+2)+dfdi(n,3)/rac
!
                b(addeme+ndim+5,(n-1)*nddls+3)= b(addeme+ndim+5,(n-1)*&
            nddls+3)+dfdi(n,2)/rac
            endif
        endif
! ======================================================================
! --- TERMES HYDRAULIQUES (FONCTIONS DE FORMES P1) ---------------------
! ======================================================================
! --- SI PRESS1 --------------------------------------------------------
! ======================================================================
        if (yap1 .eq. 1) then
            b(addep1,(n-1)*nddls+nmec+1)= b(addep1,(n-1)*nddls+nmec+1)&
            +ff2(n)
            do 105 i = 1, ndim
                b(addep1+i,(n-1)*nddls+nmec+1)= b(addep1+i,(n-1)*&
                nddls+nmec+1)+dfdi2(n,i)
105         continue
        endif
! ======================================================================
! --- TERMES ENRICHIS PAR FONCTIONS HEAVISIDE (XFEM) -------------------
! ======================================================================
        if (yaenrm .eq. 1) then
            do 106 i = 1, ndim
                b(adenme-1+i,(n-1)*nddls+nmec+np1+i)= b(adenme-1+i,(n-&
                1)*nddls+nmec+np1+i)+xcalf_he(he,zr(jlsn-1+n))*ff(n)
106         continue
!
            do 107 i = 1, ndim
                b(adenme-1+ndim+i,(n-1)*nddls+nmec+np1+i)= b(adenme-1+&
                ndim+i,(n-1)*nddls+nmec+np1+i)+xcalf_he(he,zr(jlsn-1+n))*dfdi(n,i)
107         continue
!
            if (axi) then
                if (r .eq. 0.d0) then
                    b(adenme+4,(n-1)*nddls+nmec+np1+1)=xcalf_he(he,zr(jlsn-1+n))*dfdi(n,1)
                else
                    b(adenme+4,(n-1)*nddls+nmec+np1+1)=xcalf_he(he,zr(jlsn-1+n))*ff(n)/r
                endif
            endif
!
            b(adenme+ndim+3,(n-1)*nddls+nmec+np1+1)= b(adenme+ndim+3,(&
            n-1)*nddls+nmec+np1+1)+xcalf_he(he,zr(jlsn-1+n))*dfdi(n,2)/rac
!
            b(adenme+ndim+3,(n-1)*nddls+nmec+np1+2)= b(adenme+ndim+3,(&
            n-1)*nddls+nmec+np1+2)+xcalf_he(he,zr(jlsn-1+n))*dfdi(n,1)/rac
!
            if (ndim .eq. 3) then
                b(adenme+ndim+4,(n-1)*nddls+nmec+np1+1)= b(adenme+ndim+4,(&
            n-1)*nddls+nmec+np1+1)+xcalf_he(he,zr(jlsn-1+n))*dfdi(n,3)/rac
!
                b(adenme+ndim+4,(n-1)*nddls+nmec+np1+3)= b(adenme+ndim+4,(&
            n-1)*nddls+nmec+np1+3)+xcalf_he(he,zr(jlsn-1+n))*dfdi(n,1)/rac
!
                b(adenme+ndim+5,(n-1)*nddls+nmec+np1+2)= b(adenme+ndim+5,(&
            n-1)*nddls+nmec+np1+2)+xcalf_he(he,zr(jlsn-1+n))*dfdi(n,3)/rac
!
                b(adenme+ndim+5,(n-1)*nddls+nmec+np1+3)= b(adenme+ndim+5,(&
            n-1)*nddls+nmec+np1+3)+xcalf_he(he,zr(jlsn-1+n))*dfdi(n,2)/rac
            endif
        endif
102 continue
! ======================================================================
! --- ON REMPLIT MAINTENANT LE COIN SUPERIEUR DROIT DE B CORRESPONDANT -
! --- AUX NOEUDS MILIEUX (MECANIQUE - FONCTIONS DE FORMES P2) ----------
! ======================================================================
    do 300 n = 1, nnopm
        if (yamec .eq. 1) then
            do 301 i = 1, ndim
                b(addeme-1+i,nnops*nddls+(n-1)*nddlm+i)= b(addeme-1+i,&
                nnops*nddls+(n-1)*nddlm+i)+ff(n+nnops)
301         continue
! ======================================================================
! --- CALCUL DE DEPSX, DEPSY, DEPSZ (DEPSZ INITIALISE A 0 EN 2D) -------
! ======================================================================
            do 304 i = 1, ndim
                b(addeme+ndim-1+i,nnops*nddls+(n-1)*nddlm+i)= b(&
                addeme+ndim-1+i,nnops*nddls+(n-1)*nddlm+i) +dfdi(n+&
                nnops,i)
304         continue
! ======================================================================
! --- TERME U/R DANS EPSZ EN AXI ---------------------------------------
! ======================================================================
            if (axi) then
                if (r .eq. 0.d0) then
                    b(addeme+4,nnops*nddls+(n-1)*nddlm+1)=dfdi(n+&
                    nnops,1)
                else
                    b(addeme+4,nnops*nddls+(n-1)*nddlm+1)=ff(n+nnops)/&
                    r
                endif
            endif
! ======================================================================
! --- CALCUL DE EPSXY POUR LES NOEUDS MILIEUX --------------------------
! ======================================================================
            b(addeme+ndim+3,nnops*nddls+(n-1)*nddlm+1)= b(addeme+ndim+&
            3,nnops*nddls+(n-1)*nddlm+1) +dfdi(n+nnops,2)/rac
!
            b(addeme+ndim+3,nnops*nddls+(n-1)*nddlm+2)= b(addeme+ndim+&
            3,nnops*nddls+(n-1)*nddlm+2) +dfdi(n+nnops,1)/rac
            if (ndim .eq. 3) then
! ======================================================================
! --- CALCUL DE EPSXZ POUR LES NOEUDS MILIEUX --------------------------
! ======================================================================
                b(addeme+ndim+4,nnops*nddls+(n-1)*nddlm+1)= b(addeme+ndim+&
            4,nnops*nddls+(n-1)*nddlm+1) +dfdi(n+nnops,3)/rac
!
                b(addeme+ndim+4,nnops*nddls+(n-1)*nddlm+3)= b(addeme+ndim+&
            4,nnops*nddls+(n-1)*nddlm+3) +dfdi(n+nnops,1)/rac
! ======================================================================
! --- CALCUL DE EPSYZ POUR LES NOEUDS MILIEUX --------------------------
! ======================================================================
                b(addeme+ndim+5,nnops*nddls+(n-1)*nddlm+2)= b(addeme+ndim+&
            5,nnops*nddls+(n-1)*nddlm+2) +dfdi(n+nnops,3)/rac
!
                b(addeme+ndim+5,nnops*nddls+(n-1)*nddlm+3)= b(addeme+ndim+&
            5,nnops*nddls+(n-1)*nddlm+3) +dfdi(n+nnops,2)/rac
            endif
        endif
! ======================================================================
! --- TERMES ENRICHIS PAR FONCTIONS HEAVISIDE (XFEM) -------------------
! ======================================================================
        if (yaenrm .eq. 1) then
            do 306 i = 1, ndim
                b(adenme-1+i,nnops*nddls+(n-1)*nddlm+nmec+i)= b(&
              adenme-1+i,nnops*nddls+(n-1)*nddlm+nmec+i) +xcalf_he(he,zr(jlsn-1+n+nnops))*ff(n+&
                nnops)
306         continue
!
            do 307 i = 1, ndim
                b(adenme-1+ndim+i,nnops*nddls+(n-1)*nddlm+nmec+i)=&
                b(adenme-1+ndim+i,nnops*nddls+(n-1)*nddlm+nmec+i)&
               +xcalf_he(he,zr(jlsn-1+n+nnops))*dfdi(n+nnops,i)
307         continue
!
            if (axi) then
                if (r .eq. 0.d0) then
                    b(adenme+4,nnops*nddls+(n-1)*nddlm+nmec+1)=&
                    xcalf_he(he,zr(jlsn-1+n+nnops))*dfdi(n+nnops,1)
                else
                    b(adenme+4,nnops*nddls+(n-1)*nddlm+nmec+1)=&
                    xcalf_he(he,zr(jlsn-1+n+nnops))*ff(n+nnops)/r
                endif
            endif
!
            b(adenme+ndim+3,nnops*nddls+(n-1)*nddlm+nmec+1)= b(adenme+&
          ndim+3,nnops*nddls+(n-1)*nddlm+nmec+1) +xcalf_he(he,zr(jlsn-1+n+nnops))*dfdi(n+nnops,2)&
            /rac
!
            b(adenme+ndim+3,nnops*nddls+(n-1)*nddlm+nmec+2)= b(adenme+&
          ndim+3,nnops*nddls+(n-1)*nddlm+nmec+2) +xcalf_he(he,zr(jlsn-1+n+nnops))*dfdi(n+nnops,1)&
            /rac
!
            if (ndim .eq. 3) then
                b(adenme+ndim+4,nnops*nddls+(n-1)*nddlm+nmec+1)= b(adenme+&
          ndim+4,nnops*nddls+(n-1)*nddlm+nmec+1) +xcalf_he(he,zr(jlsn-1+n+nnops))*dfdi(n+nnops,3)&
            /rac
!
                b(adenme+ndim+4,nnops*nddls+(n-1)*nddlm+nmec+3)= b(adenme+&
          ndim+4,nnops*nddls+(n-1)*nddlm+nmec+3) +xcalf_he(he,zr(jlsn-1+n+nnops))*dfdi(n+nnops,1)&
            /rac
!
                b(adenme+ndim+5,nnops*nddls+(n-1)*nddlm+nmec+2)= b(adenme+&
          ndim+5,nnops*nddls+(n-1)*nddlm+nmec+2) +xcalf_he(he,zr(jlsn-1+n+nnops))*dfdi(n+nnops,3)&
            /rac
!
                b(adenme+ndim+5,nnops*nddls+(n-1)*nddlm+nmec+3)= b(adenme+&
          ndim+5,nnops*nddls+(n-1)*nddlm+nmec+3) +xcalf_he(he,zr(jlsn-1+n+nnops))*dfdi(n+nnops,2)&
            /rac
            endif
        endif
300 continue
! ======================================================================
! --- LE COIN INFERIEUR DROIT EST NUL ----------------------------------
! ======================================================================
end subroutine
