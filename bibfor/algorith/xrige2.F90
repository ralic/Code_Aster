subroutine xrige2(elrefp, elrese, ndim, coorse, igeom,&
                  he, ddlh, ddlc, ddlm, nfe,&
                  basloc, nnop, npg, lsn, lst,&
                  sig, matuu)
!
! aslint: disable=W1306
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elref4.h"
#include "asterfort/elref5.h"
#include "asterfort/lteatt.h"
#include "asterfort/reeref.h"
#include "asterfort/vecini.h"
#include "asterfort/xcalf2.h"
    integer :: ndim, igeom, nnop, npg, ddlh, ddlc, nfe
    integer :: ddlm
    character(len=8) :: elrefp
    character(len=8) :: elrese
    real(kind=8) :: basloc(6*nnop), he, coorse(*)
    real(kind=8) :: lsn(nnop), lst(nnop), sig(48), matuu(*)
!
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
! person_in_charge: samuel.geniaut at edf.fr
!
!.......................................................................
!
!     BUT:  CALCUL  DE L'OPTION RIGI_MECA_GE AVEC X-FEM EN 2D
!.......................................................................
! IN  ELREFP  : ELEMENT DE REFERENCE PARENT
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  COORSE  : COORDONNEES DES SOMMETS DU SOUS-ELEMENT
! IN  IGEOM   : COORDONNEES DES NOEUDS DE L'ELEMENT PARENT
! IN  HE      : VALEUR DE LA FONCTION HEAVISIDE SUR LE SOUS-ELT
! IN  DDLH    : NOMBRE DE DDL HEAVYSIDE (PAR NOEUD)
! IN  DDLC    : NOMBRE DE DDL DE CONTACT (PAR NOEUD)
! IN  NFE     : NOMBRE DE FONCTIONS SINGULIÈRES D'ENRICHISSEMENT
! IN  BASLOC  : BASE LOCALE AU FOND DE FISSURE AUX NOEUDS
! IN  NNOP    : NOMBRE DE NOEUDS DE L'ELEMENT PARENT
! IN  NPG     : NOMBRE DE POINTS DE GAUSS DU SOUS-ELEMENT
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  LGPG    : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
!               CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
! IN  LSN     : VALEUR DE LA LEVEL SET NORMALE AUX NOEUDS PARENTS
! IN  LST     : VALEUR DE LA LEVEL SET TANGENTE AUX NOEUDS PARENTS
! IN  IDECPG  : POSITION DANS LA FAMILLE 'XFEM' DU 1ER POINT DE GAUSS
!               DU SOUS ELEMENT COURRANT (EN FAIT IDECPG+1)
! IN SIG     : CONTRAINTES DE CAUCHY
! OUT MATUU   : MATRICE DE MASSE PROFIL
!......................................................................
!
!
!
    integer :: kpg, kk, n, i, m, j, j1, kkd, ino, ig, iret, ij
    integer :: nno, nnos, npgbis, ddls, ddld, ddldn, cpt, ndimb
    integer :: jcoopg, jdfd2, jgano, idfde, ivf, ipoids
    real(kind=8) :: tmp1, fe(4), baslog(6)
    real(kind=8) :: xg(ndim), xe(ndim), ff(nnop), jac, lsng, lstg
    real(kind=8) :: rbid1(10), rbid2(10)
    real(kind=8) :: dfdi(nnop, ndim), pff(6, nnop, ndim), dgdgl(4, 3)
    real(kind=8) :: rac2
    integer :: nnops
!
    logical :: axi
!
    integer :: ibid
    real(kind=8) :: rbid
    data     rac2 / 1.4142135623731d0 /
!
!
!--------------------------------------------------------------------
!
!     NOMBRE DE DDL DE DEPLACEMENT À CHAQUE NOEUD SOMMET
    ddld=ndim+ddlh+ndim*nfe
    ddldn = ddld/ndim
!
!     NOMBRE DE DDL TOTAL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
    ddls=ddld+ddlc
!
!     ELEMENT DE REFERENCE PARENT : RECUP DE NNOPS
    call elref4(' ', 'RIGI', ibid, ibid, nnops,&
                ibid, ibid, ibid, ibid, ibid)
!
    axi = lteatt(' ','AXIS','OUI')
!
    call elref5(elrese, 'XINT', ndimb, nno, nnos,&
                npgbis, ipoids, jcoopg, ivf, idfde,&
                jdfd2, jgano)
    ASSERT(npg.eq.npgbis.and.ndim.eq.ndimb)
!
!-----------------------------------------------------------------------
!     BOUCLE SUR LES POINTS DE GAUSS DU SOUS-TÉTRA
    do 100 kpg = 1, npg
!
!       COORDONNEES DU PT DE GAUSS DANS LE REPERE REEL : XG
        call vecini(ndim, 0.d0, xg)
        do 110 i = 1, ndim
            do 111 n = 1, nno
                xg(i)=xg(i)+zr(ivf-1+nno*(kpg-1)+n)*coorse(ndim*(n-1)+&
                i)
111          continue
110      continue
!
!       JUSTE POUR CALCULER LES FF
        call reeref(elrefp, axi, nnop, nnops, zr(igeom),&
                    xg, 1, .false., ndim, he,&
                    rbid, rbid, ibid, ibid, ddlh,&
                    nfe, ddls, ddlm, fe, dgdgl,&
                    'NON', xe, ff, dfdi, rbid,&
                    rbid, rbid)
!
        if (nfe .gt. 0) then
!         BASE LOCALE AU POINT DE GAUSS
            call vecini(6, 0.d0, baslog)
            lsng = 0.d0
            lstg = 0.d0
            do 113 ino = 1, nnop
                lsng = lsng + lsn(ino) * ff(ino)
                lstg = lstg + lst(ino) * ff(ino)
                do 114 i = 1, 6
                    baslog(i) = baslog(i) + basloc(6*(ino-1)+i) * ff( ino)
114              continue
113          continue
!
!         FONCTION D'ENRICHISSEMENT AU POINT DE GAUSS ET LEURS DERIVEES
            call xcalf2(he, lsng, lstg, baslog, fe,&
                        dgdgl, iret)
!         ON A PAS PU CALCULER LES DERIVEES DES FONCTIONS SINGULIERES
!         CAR ON SE TROUVE SUR LE FOND DE FISSURE
            ASSERT(iret.ne.0)
!
        endif
!
!       COORDONNÉES DU POINT DE GAUSS DANS L'ELEMENT DE REF PARENT : XE
!       ET CALCUL DE FF, DFDI, ET EPS
!
        call reeref(elrefp, axi, nnop, nnops, zr(igeom),&
                    xg, 1, .false., ndim, he,&
                    rbid, rbid, ibid, ibid, ddlh,&
                    nfe, ddls, ddlm, fe, dgdgl,&
                    'DFF', xe, ff, dfdi, rbid,&
                    rbid, rbid)
!
!
!       POUR CALCULER LE JACOBIEN DE LA TRANSFO SSTET->SSTET REF
!       ON ENVOIE DFDM2D AVEC LES COORD DU SS-ELT
        call dfdm2d(nno, kpg, ipoids, idfde, coorse,&
                    jac)
!
!--------CALCUL DES FONCTIONS ENRICHIES--------------------------
!
        do 127 i = 1, ndim
            do 128 n = 1, nnop
!----------LA PARTIE CORRESPOINDANTE AU FF CLASSIQUES
                cpt= 1
                pff(cpt,n,i) = dfdi(n,i)
!----------LA PARTIE CORRESPONDANTE A L'ENRICHEMENT HEAVISIDE
                if (ddlh .eq. ndim) then
                    cpt = 2
                    pff(cpt,n,i) = dfdi(n,i)*he
                endif
!----------LA PARTIE CORRESPONDANTE A L'ENRICHEMENT CRACK-TIP
                do 126 ig = 1, nfe
                    cpt =cpt+1
                    pff(cpt,n,i) = dfdi(n,i)*fe(ig)+ff(n)*dgdgl(ig,i)
126              continue
                ASSERT(cpt.eq.ddldn)
128          continue
127      continue
!
        do 240 n = 1, nnop
            do 241 m = 1, n
                do 242 i = 1, ddldn
                    do 243 j = 1, ddldn
                        tmp1 = sig(&
                               (kpg-1)*4+1)*pff(i, n, 1)*pff(j, m, 1) + sig((kpg-1)*4+2)*pff(i,&
                               n, 2)*pff(j, m,&
                               2) + sig(&
                               (kpg-1)*4+4)*(pff(i, n, 1)*pff(j, m, 2) +pff(i, n, 2)*pff(j, m, 1)&
                               )/rac2
!              STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                        if (m .eq. n) then
                            j1 = i
                        else
                            j1 = ddldn
                        endif
                        if (j .le. j1) then
                            do 244 ij = 1, ndim
                                kkd = (ddls*(n-1)+(i-1)*ndim+ij-1) * (ddls*(n-1)+(i-1)*ndim+ij&
                                      ) /2
                                kk = kkd + ddls*(m-1)+(j-1)*ndim+ij
                                matuu(kk) = matuu(kk) + tmp1*jac
244                          continue
                        endif
!
243                  continue
242              continue
241          continue
240      continue
!
100  continue
!
end subroutine
