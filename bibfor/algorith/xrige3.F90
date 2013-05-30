subroutine xrige3(elrefp, ndim, coorse, igeom, he,&
                  ddlh, ddlc, ddlm, nfe, basloc,&
                  nnop, npg, lsn, lst, sig,&
                  matuu)
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/dfdm3d.h'
    include 'asterfort/elref4.h'
    include 'asterfort/elref5.h'
    include 'asterfort/reeref.h'
    include 'asterfort/vecini.h'
    include 'asterfort/xcalfe.h'
    integer :: ndim, igeom, nnop, npg, ddlh, ddlc, nfe
    character(len=8) :: elrefp
    real(kind=8) :: basloc(9*nnop), he, coorse(*)
    real(kind=8) :: lsn(nnop), lst(nnop), sig(90), matuu(*)
! ----------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! TOLE CRS_1404
!
!
!
!     BUT :
!         CALCUL  DE L'OPTION RIGI_MECA_GE AVEC X-FEM EN 3D
!
!
! IN  ELREFP  : ÉLÉMENT DE RÉFÉRENCE PARENT
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  COORSE  : COORDONNÉES DES SOMMETS DU SOUS-ÉLÉMENT
! IN  IGEOM   : COORDONNÉES DES NOEUDS DE L'ÉLÉMENT PARENT
! IN  HE      : VALEUR DE LA FONCTION HEAVISIDE SUR LE SOUS-ÉLT
! IN  DDLH    : NOMBRE DE DDL HEAVYSIDE (PAR NOEUD)
! IN  DDLC    : NOMBRE DE DDL DE CONTACT (PAR NOEUD)
! IN  NFE     : NOMBRE DE FONCTIONS SINGULIÈRES D'ENRICHISSEMENT
! IN  BASLOC  : BASE LOCALE AU FOND DE FISSURE AUX NOEUDS
! IN  NNOP    : NOMBRE DE NOEUDS DE L'ELEMENT PARENT
! IN  NPG     : NOMBRE DE POINTS DE GAUSS DU SOUS-ÉLÉMENT
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  LGPG    : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
!               CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
! IN  LSN     : VALEUR DE LA LEVEL SET NORMALE AUX NOEUDS PARENTS
! IN  LST     : VALEUR DE LA LEVEL SET TANGENTE AUX NOEUDS PARENTS
! IN  IDECPG  : POSITION DANS LA FAMILLE 'XFEM' DU 1ER POINT DE GAUSS
!               DU SOUS ELEMENT COURRANT (EN FAIT IDECPG+1)
! IN SIG     : CONTRAINTES DE CAUCHY
! OUT MATUU   : MATRICE DE MASSE PROFIL
!
!......................................................................
!
!
!
!
    integer :: kpg, kk, n, i, m, j, j1, kkd, ino, ig, iret, ij, ibid
    integer :: nno, nnos, npgbis, ddls, ddld, ddldn, cpt, ndimb
    integer :: jcoopg, jdfd2, jgano, idfde, ivf, ipoids
    integer :: nnops, ddlm
    real(kind=8) :: f(3, 3), eps(6)
    real(kind=8) :: tmp1, fe(4), baslog(9)
    real(kind=8) :: xg(ndim), xe(ndim), ff(nnop), jac, lsng, lstg
    real(kind=8) :: dfdx(4), dfdy(4), dfdz(4)
    real(kind=8) :: dfdi(nnop, ndim), pff(6, nnop, ndim), dgdgl(4, 3)
    real(kind=8) :: grad(3, 3), rbid
    real(kind=8) :: rac2
    data    rac2 / 1.4142135623731D0 /
!--------------------------------------------------------------------
!
!     ATTENTION, DEPL ET VECTU SONT ICI DIMENSIONNÉS DE TELLE SORTE
!     QU'ILS NE PRENNENT PAS EN COMPTE LES DDL SUR LES NOEUDS MILIEU
!
!     NOMBRE DE DDL DE DEPLACEMENT À CHAQUE NOEUD SOMMET
    ddld=ndim+ddlh+ndim*nfe
    ddldn = ddld/ndim
!
!     NOMBRE DE DDL TOTAL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
    ddls=ddld+ddlc
!     ELEMENT DE REFERENCE PARENT : RECUP DE NNOPS
    call elref4(' ', 'RIGI', ibid, ibid, nnops,&
                ibid, ibid, ibid, ibid, ibid)
!
!       TE4-'XINT' : SCHÉMAS À 15 POINTS
    call elref5('TE4', 'XINT', ndimb, nno, nnos,&
                npgbis, ipoids, jcoopg, ivf, idfde,&
                jdfd2, jgano)
!
    call assert(npg.eq.npgbis.and.ndim.eq.ndimb)
!
!-----------------------------------------------------------------------
!     BOUCLE SUR LES POINTS DE GAUSS DU SOUS-TÉTRA
    do 100 kpg = 1, npg
!
!       COORDONNÉES DU PT DE GAUSS DANS LE REPÈRE RÉEL : XG
        call vecini(ndim, 0.d0, xg)
        do 110 i = 1, ndim
            do 111 n = 1, nno
                xg(i)=xg(i)+zr(ivf-1+nno*(kpg-1)+n)*coorse(3*(n-1)+i)
111          continue
110      continue
!
!       JUSTE POUR CALCULER LES FF
        call reeref(elrefp, .false., nnop, nnops, zr(igeom),&
                    xg, ibid, .false., ndim, he,&
                    rbid, rbid, ibid, ibid, ddlh,&
                    nfe, ddls, ddlm, fe, dgdgl,&
                    'NON', xe, ff, dfdi, f,&
                    eps, grad)
!
        if (nfe .gt. 0) then
!         BASE LOCALE  ET LEVEL SETS AU POINT DE GAUSS
            call vecini(9, 0.d0, baslog)
            lsng = 0.d0
            lstg = 0.d0
            do 113 ino = 1, nnop
                lsng = lsng + lsn(ino) * ff(ino)
                lstg = lstg + lst(ino) * ff(ino)
                do 114 i = 1, 9
                    baslog(i) = baslog(i) + basloc(9*(ino-1)+i) * ff( ino)
114              continue
113          continue
!
!         FONCTION D'ENRICHISSEMENT AU POINT DE GAUSS ET LEURS DÉRIVÉES
            call xcalfe(he, lsng, lstg, baslog, fe,&
                        dgdgl, iret)
!         ON A PAS PU CALCULER LES DERIVEES DES FONCTIONS SINGULIERES
!         CAR ON SE TROUVE SUR LE FOND DE FISSURE
            call assert(iret.ne.0)
!
        endif
!
!       COORDONNÉES DU POINT DE GAUSS DANS L'ÉLÉMENT DE RÉF PARENT : XE
!       ET CALCUL DE FF, DFDI, ET EPS
!
        call reeref(elrefp, .false., nnop, nnops, zr(igeom),&
                    xg, ibid, .false., ndim, he,&
                    rbid, rbid, ibid, ibid, ddlh,&
                    nfe, ddls, ddlm, fe, dgdgl,&
                    'DFF', xe, ff, dfdi, f,&
                    eps, grad)
!
!       POUR CALCULER LE JACOBIEN DE LA TRANSFO SSTET->SSTET REF
!       ET LES DERIVEES DES FONCTIONS DE FORME,
!       ON ENVOIE DFDM3D AVEC LES COORD DU SS-ELT
        call dfdm3d(nno, kpg, ipoids, idfde, coorse,&
                    dfdx, dfdy, dfdz, jac)
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
                call assert(cpt.eq.ddldn)
128          continue
127      continue
!
        do 240 n = 1, nnop
            do 241 m = 1, n
                do 242 i = 1, ddldn
                    do 243 j = 1, ddldn
                        tmp1 = sig(&
                               (kpg-1)*6+1)*pff(i, n, 1)*pff(j, m, 1) + sig((kpg-1)*6+2)*pff(i,&
                               n, 2)*pff(j, m, 2) + sig((kpg-1)*6+3)*pff(i, n, 3)*pff(j, m,&
                               3) + sig(&
                               (kpg-1)*6+4)*(pff(i, n, 1)*pff(j, m, 2) +pff(i, n, 2)*pff(j, m, 1)&
                               &)/rac2 + sig((kpg-1)*6+ 5)*(pff(i, n, 1)*pff(j, m, 3) +pff(i, n, &
                               &3)*pff(j, m, 1))/rac2 + sig((kpg-1)*6+6)*(pff(i, n, 3)*pff(j, m, &
                               &2) +pff(i, n, 2)*pff(j, m, 3&
                               )&
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
243                  continue
242              continue
241          continue
240      continue
!
100  end do
!
end subroutine
