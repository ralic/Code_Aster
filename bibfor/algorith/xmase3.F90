subroutine xmase3(elrefp, ndim, coorse, igeom, he,&
                  ddlh, ddlc, nfe, basloc, nnop,&
                  npg, imate, lsn, lst, matuu, heavn)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/reeref.h"
#include "asterfort/vecini.h"
#include "asterfort/xcalfe.h"
#include "asterfort/xcalc_heav.h"
#include "asterfort/xcalc_code.h"
    integer :: ndim, igeom, imate, nnop, npg, ddlh, ddlc, nfe, heavn(27,5)
    character(len=8) :: elrefp
    real(kind=8) :: basloc(9*nnop), he, coorse(*)
    real(kind=8) :: lsn(nnop), lst(nnop), matuu(*)
! ----------------------------------------------------------------------
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
!
!     BUT:  CALCUL  DE L'OPTION MASS_MECA AVEC X-FEM EN 3D
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
! IN  IMATE   : MATERIAU CODE
! IN  LGPG    : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
!               CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
! IN  LSN     : VALEUR DE LA LEVEL SET NORMALE AUX NOEUDS PARENTS
! IN  LST     : VALEUR DE LA LEVEL SET TANGENTE AUX NOEUDS PARENTS
! IN  IDECPG  : POSITION DANS LA FAMILLE 'XFEM' DU 1ER POINT DE GAUSS
!               DU SOUS ELEMENT COURRANT (EN FAIT IDECPG+1)
! OUT MATUU   : MATRICE DE MASSE PROFIL
!
!
!
!
    integer :: retour(1)
    integer :: kpg, kk, n, i, m, j, j1, kkd, ino, ig, iret
    integer :: nno, nnos, npgbis, ddlt, ddld, cpt, ndimb
    integer :: jcoopg, jdfd2, jgano, idfde, ivf, ipoids, hea_se
!
    real(kind=8) :: rho(1)
    real(kind=8) :: fe(4), baslog(9)
    real(kind=8) :: xg(ndim), xe(ndim), ff(nnop), jac, lsng, lstg
    real(kind=8) :: dgdgl(4, 3)
    real(kind=8) :: enr(nnop, ndim+ddlh+ndim*nfe)
    real(kind=8) :: depl0(ndim+ddlh+ndim*nfe+ddlc, nnop)
!
    character(len=16) :: phenom
!
!--------------------------------------------------------------------
!
!     ATTENTION, DEPL ET VECTU SONT ICI DIMENSIONNÉS DE TELLE SORTE
!     QU'ILS NE PRENNENT PAS EN COMPTE LES DDL SUR LES NOEUDS MILIEU
!
!     NOMBRE DE DDL DE DEPLACEMENT À CHAQUE NOEUD SOMMET
    ddld=ndim+ddlh+ndim*nfe
!
!     NOMBRE DE DDL TOTAL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
    ddlt=ddld+ddlc
! CALCUL DE L IDENTIFIANT DU SS ELEMENT
    hea_se=xcalc_code(1, he_real=[he])
!
!       TE4-'XINT' : SCHÉMAS À 15 POINTS
    call elrefe_info(elrefe='TE4',fami='XINT',ndim=ndimb,nno=nno,nnos=nnos,&
  npg=npgbis,jpoids=ipoids,jcoopg=jcoopg,jvf=ivf,jdfde=idfde,&
  jdfd2=jdfd2,jgano=jgano)
!
    ASSERT(npg.eq.npgbis.and.ndim.eq.ndimb)
!
!-----------------------------------------------------------------------
!     BOUCLE SUR LES POINTS DE GAUSS DU SOUS-TÉTRA
    do kpg = 1, npg
!
!       COORDONNÉES DU PT DE GAUSS DANS LE REPÈRE RÉEL : XG
        call vecini(ndim, 0.d0, xg)
        do i = 1, ndim
            do n = 1, nno
                xg(i)=xg(i)+zr(ivf-1+nno*(kpg-1)+n)*coorse(3*(n-1)+i)
            end do
        end do
!
!
        do i = 1, nnop
            do j = 1, ddlt
                depl0(j,i)=0.d0
            end do
        end do
!
!       JUSTE POUR CALCULER LES FF
        call reeref(elrefp, nnop, zr(igeom), xg, ndim, xe, ff)
!
        if (nfe .gt. 0) then
!         BASE LOCALE  ET LEVEL SETS AU POINT DE GAUSS
            call vecini(9, 0.d0, baslog)
            lsng = 0.d0
            lstg = 0.d0
            do ino = 1, nnop
                lsng = lsng + lsn(ino) * ff(ino)
                lstg = lstg + lst(ino) * ff(ino)
                do i = 1, 9
                    baslog(i) = baslog(i) + basloc(9*(ino-1)+i) * ff( ino)
                end do
            end do
!
!         FONCTION D'ENRICHISSEMENT AU POINT DE GAUSS ET LEURS DÉRIVÉES
            call xcalfe(he, lsng, lstg, baslog, fe,&
                        dgdgl, iret)
!         ON A PAS PU CALCULER LES DERIVEES DES FONCTIONS SINGULIERES
!         CAR ON SE TROUVE SUR LE FOND DE FISSURE
            ASSERT(iret.ne.0)
!
        endif
!
!--------CALCUL DES FONCTIONS ENRICHIES--------------------------
        do n = 1, nnop
            cpt=0
!         FONCTIONS DE FORME CLASSIQUES
            do i = 1, ndim
                cpt=cpt+1
                enr(n,i) = ff(n)
            end do
!         ENRICHISSEMENT PAR HEAVYSIDE
            do i = 1, ddlh
                cpt=cpt+1
                enr(n,cpt) = enr(n,i) * xcalc_heav(heavn(n,i),hea_se,heavn(n,5))
            end do
!         ENRICHISSEMENT PAR LES NFE FONTIONS SINGULIÈRES
            do ig = 1, nfe
                do i = 1, ndim
                    cpt=cpt+1
                    enr(n,cpt)=ff(n)*fe(ig)
                end do
            end do
!
            ASSERT(cpt.eq.ddld)
!
        end do
!
!       POUR CALCULER LE JACOBIEN DE LA TRANSFO SSTET->SSTET REF
!       ON ENVOIE DFDM3D AVEC LES COORD DU SS-ELT
        call dfdm3d(nno, kpg, ipoids, idfde, coorse,&
                    jac)
!
!       ON RECUPERE LA MASSE VOLUMIQUE
!
        call rccoma(imate, 'ELAS', 1, phenom, retour(1))
        call rcvalb('RIGI', kpg, 1, '+', imate,&
                    ' ', phenom, 0, ' ', [0.d0],&
                    1, 'RHO', rho, retour, 1)
!
        do n = 1, nnop
            do i = 1, ddld
                kkd = (ddld*(n-1)+i-1) * (ddld*(n-1)+i) /2
                do j = 1, ddld
                    do m = 1, n
                        if (m .eq. n) then
                            j1 = i
                        else
                            j1 = ddld
                        endif
                        if (j .le. j1) then
                            kk = kkd + ddld*(m-1)+j
                            matuu(kk)= matuu(kk)+enr(n,i)*enr(m,j)*&
                            jac*rho(1)
                        endif
!
                    end do
                end do
            end do
        end do
!
    end do
!
end subroutine
