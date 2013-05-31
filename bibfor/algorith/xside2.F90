subroutine xside2(elrefp, ndim, coorse, elrese, igeom,&
                  he, nfh, ddlc, ddlm, nfe,&
                  basloc, nnop, npg, idecpg, typmod,&
                  imate, compor, idepl, lsn, lst,&
                  nfiss, fisno, sig)
!
! aslint: disable=W1306,W1504
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/dmatmc.h'
    include 'asterfort/elref4.h'
    include 'asterfort/elref5.h'
    include 'asterfort/epstmc.h'
    include 'asterfort/nbsigm.h'
    include 'asterfort/rccoma.h'
    include 'asterfort/reeref.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vecini.h'
    include 'asterfort/xcalf2.h'
    integer :: ndim, igeom, imate, nnop, npg, idepl, idecpg
    integer :: nfh, ddlc, nfe, nfiss, fisno(nnop, nfiss)
    character(len=8) :: elrefp, elrese, typmod(*)
    character(len=16) :: compor(4)
    real(kind=8) :: basloc(6*nnop), he(nfiss), coorse(*)
    real(kind=8) :: lsn(nnop), lst(nnop), sig(4, npg)
!
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!.......................................................................
!
!     BUT:  CALCUL DE L'OPTION SIEF_ELGA AVEC X-FEM EN 2D
!.......................................................................
! IN  ELREFP  : ELEMENT DE REFERENCE PARENT
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  COORSE  : COORDONNEES DES SOMMETS DU SOUS-ELEMENT
! IN  IGEOM   : COORDONNEES DES NOEUDS DE L'ELEMENT PARENT
! IN  HE      : VALEUR DE LA FONCTION HEAVISIDE SUR LE SOUS-ELT
! IN  NFH    : NOMBRE DE DDL HEAVYSIDE (PAR NOEUD)
! IN  DDLC    : NOMBRE DE DDL DE CONTACT (PAR NOEUD)
! IN  DDLM    : NOMBRE DE DDL PAR NOEUD MILIEU
! IN  NFE     : NOMBRE DE FONCTIONS SINGULIÈRES D'ENRICHISSEMENT
! IN  BASLOC  : BASE LOCALE AU FOND DE FISSURE AUX NOEUDS
! IN  NNOP    : NOMBRE DE NOEUDS DE L'ELEMENT PARENT
! IN  NPG     : NOMBRE DE POINTS DE GAUSS DU SOUS-ELEMENT
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  IMATE   : MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT
! IN  IDEPL   : ADRESSE DU DEPLACEMENT A PARTIR DE LA CONF DE REF
! IN  LSN     : VALEUR DE LA LEVEL SET NORMALE AUX NOEUDS PARENTS
! IN  LST     : VALEUR DE LA LEVEL SET TANGENTE AUX NOEUDS PARENTS
! IN  NFISS   : NOMBRE DE FISSURES "VUES" PAR L'ÉLÉMENT
! IN  JFISNO  : POINTEUR DE CONNECTIVITÉ FISSURE/HEAVISIDE
!
! OUT SIG     : CONTRAINTES (SIEF_ELGA)
!
!......................................................................
!
    character(len=2) :: k2bid
    character(len=16) :: phenom
    integer :: kpg, n, i, j, ino, iret, ipg
    integer :: nno, nnos, npgbis, ddls, ddld, ddlm, ndimb
    integer :: jcoopg, jdfd2, jgano, idfde, ivf, ipoids, nbsig
    logical :: grdepl, axi
    real(kind=8) :: f(3, 3), eps(6), baslog(6)
    real(kind=8) :: fe(4), instan, rac2
    real(kind=8) :: xg(ndim), xe(ndim), ff(nnop), lsng, lstg
    real(kind=8) :: rbid, r8bi7(7), r8bi3(3)
    real(kind=8) :: dfdi(nnop, ndim), dgdgl(4, 3)
    real(kind=8) :: grad(3, 3)
    real(kind=8) :: zero, s, sth, d(4, 4), r, epsth(6)
    integer :: nnops, ibid
!
    data    zero / 0d0 /
    data    rac2 / 1.4142135623731D0 /
!--------------------------------------------------------------------
!
!     ON AUTORISE UNIQUEMENT L'ISOTROPIE
    call rccoma(imate, 'ELAS', 1, phenom, iret)
    call assert(iret.eq.0 .and. phenom.eq.'ELAS')
!
!     INITIALISATIONS
    instan = 0.d0
    call vecini(7, 0.d0, r8bi7)
    call vecini(3, 0.d0, r8bi3)
!
!     NOMBRE DE DDL DE DEPLACEMENT À CHAQUE NOEUD SOMMET
    ddld=ndim*(1+nfh+nfe)
!
!     NOMBRE DE DDL TOTAL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
    ddls=ddld+ddlc
!
! ---- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
!      -----------------------------------------
    nbsig = nbsigm()
!
    call elref4(' ', 'RIGI', ibid, ibid, nnops,&
                ibid, ibid, ibid, ibid, ibid)
!
! - INITIALISATION
    grdepl = compor(3).eq. 'GROT_GDEP'
    axi = typmod(1) .eq. 'AXIS'
!
!
    k2bid = ' '
    if (grdepl) then
        call u2mess('F', 'XFEM2_2')
    endif
!
    call elref5(elrese, 'XINT', ndimb, nno, nnos,&
                npgbis, ipoids, jcoopg, ivf, idfde,&
                jdfd2, jgano)
    call assert(npg.eq.npgbis.and.ndim.eq.ndimb)
!
! - CALCUL POUR CHAQUE POINT DE GAUSS
!
    do 10 kpg = 1, npg
!
!       NUMERO DU PG DANS LE FAMILLE 'XFEM'
        ipg = idecpg + kpg
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
!       CALCUL DES FF
        call reeref(elrefp, axi, nnop, nnops, zr(igeom),&
                    xg, idepl, grdepl, ndim, he,&
                    rbid, rbid, fisno, nfiss, nfh,&
                    nfe, ddls, ddlm, fe, dgdgl,&
                    'NON', xe, ff, dfdi, f,&
                    eps, grad)
!
!-----------------------------------------------------------------------
!         BOUCLE SUR LES POINTS DE GAUSS DU SOUS-ELT
!-----------------------------------------------------------------------
! -     CALCUL DE LA DISTANCE A L'AXE (AXISYMETRIQUE)
        if (axi) then
            r = 0.d0
            do 1000 ino = 1, nnop
                r = r + ff(ino)*zr(igeom-1+2*(ino-1)+1)
1000          continue
            call assert(r.ge.0d0)
!          ATTENTION : LE POIDS N'EST PAS X R
!          CE SERA FAIT PLUS TARD AVEC JAC = JAC X R
        endif
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
!
!         FONCTION D'ENRICHISSEMENT AU POINT DE GAUSS ET LEURS DERIVEES
            call xcalf2(he(1), lsng, lstg, baslog, fe,&
                        dgdgl, iret)
!         ON A PAS PU CALCULER LES DERIVEES DES FONCTIONS SINGULIERES
!         CAR ON SE TROUVE SUR LE FOND DE FISSURE
            call assert(iret.ne.0)
        endif
!
!       CALCUL DES DEFORMATIONS EPS
        call reeref(elrefp, axi, nnop, nnops, zr(igeom),&
                    xg, idepl, grdepl, ndim, he,&
                    r, rbid, fisno, nfiss, nfh,&
                    nfe, ddls, ddlm, fe, dgdgl,&
                    'OUI', xe, ff, dfdi, f,&
                    eps, grad)
!
!       CALCUL DES DEFORMATIONS THERMIQUES EPSTH
        call vecini(6, 0.d0, epsth)
        call epstmc('XFEM', ndim, instan, '+', ipg,&
                    1, r8bi3, r8bi7, imate, 'CHAR_MECA_TEMP_R',&
                    epsth)
!
!       CALCUL DE LA MATRICE DE HOOKE (MATERIAU ISOTROPE)
        call dmatmc('XFEM', k2bid, imate, instan, '+',&
                    ipg, 1, r8bi7, r8bi3, nbsig,&
                    d)
!
!       VECTEUR DES CONTRAINTES
        do 30 i = 1, nbsig
            s = zero
            sth = zero
            do 40 j = 1, nbsig
                s = s + eps(j)*d(i,j)
                sth = sth + epsth(j)*d(i,j)
40          continue
            sig(i,kpg) = s - sth
30      continue
        sig(4,kpg) = sig(4,kpg)*rac2
!
10  end do
!
end subroutine
