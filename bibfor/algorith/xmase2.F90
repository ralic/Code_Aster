subroutine xmase2(elrefp, ndim, coorse, igeom, he,&
                  ddlh, ddlc, nfe, basloc, nnop,&
                  npg, imate, lsn, lst, matuu)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elref5.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/reeref.h"
#include "asterfort/vecini.h"
#include "asterfort/xcalf2.h"
    integer :: ndim, igeom, imate, nnop, npg, ddlh, ddlc, nfe
    character(len=8) :: elrefp
    real(kind=8) :: basloc(6*nnop), he, coorse(*)
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
! person_in_charge: samuel.geniaut at edf.fr
!
!     BUT:  CALCUL  DE L'OPTION MASS_MECA AVEC X-FEM EN 2D
!
!
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
! IN  IMATE   : MATERIAU CODE
! IN  LGPG    : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
!               CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
! IN  LSN     : VALEUR DE LA LEVEL SET NORMALE AUX NOEUDS PARENTS
! IN  LST     : VALEUR DE LA LEVEL SET TANGENTE AUX NOEUDS PARENTS
! IN  IDECPG  : POSITION DANS LA FAMILLE 'XFEM' DU 1ER POINT DE GAUSS
!               DU SOUS ELEMENT COURRANT (EN FAIT IDECPG+1)
! OUT MATUU   : MATRICE DE MASSE PROFIL
!
! ......................................................................
!
!
!
!
    integer :: retour(1)
    integer :: kpg, kk, n, i, m, j, j1, kkd, ino, ig, iret
    integer :: nno, nnos, npgbis, ddlt, ddld, cpt, ndimb
    integer :: jcoopg, jdfd2, jgano, idfde, ivf, ipoids
!
    real(kind=8) :: rho(1)
    real(kind=8) :: fe(4), baslog(6)
    real(kind=8) :: xg(ndim), xe(ndim), ff(nnop), jac, lsng, lstg
    real(kind=8) :: dgdgl(4, 3)
    real(kind=8) :: enr(nnop, ndim+ddlh+ndim*nfe)
    real(kind=8) :: depl0(ndim+ddlh+ndim*nfe+ddlc, nnop)
!
    character(len=16) :: phenom
!
!--------------------------------------------------------------------
!
!     NOMBRE DE DDL DE DEPLACEMENT À CHAQUE NOEUD SOMMET
    ddld=ndim+ddlh+ndim*nfe
!
!     NOMBRE DE DDL TOTAL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
    ddlt=ddld+ddlc
!
    call elref5('TR3', 'XINT', ndimb, nno, nnos,&
                npgbis, ipoids, jcoopg, ivf, idfde,&
                jdfd2, jgano)
    ASSERT(npg.eq.npgbis.and.ndim.eq.ndimb)
!
!
! - CALCUL POUR CHAQUE POINT DE GAUSS
    do kpg = 1, npg
!
!       COORDONNEES DU PT DE GAUSS DANS LE REPERE REEL : XG
        call vecini(ndim, 0.d0, xg)
        do i = 1, ndim
            do n = 1, nno
                xg(i)=xg(i)+zr(ivf-1+nno*(kpg-1)+n)*coorse(ndim*(n-1)+&
                i)
            end do
        end do
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
!         BASE LOCALE AU POINT DE GAUSS
            call vecini(6, 0.d0, baslog)
            lsng = 0.d0
            lstg = 0.d0
            do ino = 1, nnop
                lsng = lsng + lsn(ino) * ff(ino)
                lstg = lstg + lst(ino) * ff(ino)
                do i = 1, 6
                    baslog(i) = baslog(i) + basloc(6*(ino-1)+i) * ff( ino)
                end do
            end do
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
! - CALCUL DES ELEMENTS GEOMETRIQUES
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
                enr(n,cpt) = enr(n,i) * he
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
!       ON ENVOIE DFDM2D AVEC LES COORD DU SS-ELT
        call dfdm2d(nno, kpg, ipoids, idfde, coorse,&
                    jac)
!
!
!       ON RECUPERE LA MASSE VOLUMIQUE
!
        call rccoma(imate, 'ELAS', 1, phenom, retour(1))
        call rcvalb('RIGI', kpg, 1, '+', imate,&
                    ' ', phenom, 0, ' ', [0.d0],&
                    1, 'RHO', rho, retour, 1)
!
!
        do n = 1, nnop
            do i = 1, ddld
                kkd = (ddlt*(n-1)+i-1) * (ddlt*(n-1)+i) /2
                do j = 1, ddld
                    do m = 1, n
                        if (m .eq. n) then
                            j1 = i
                        else
                            j1 = ddld
                        endif
                        if (j .le. j1) then
                            kk = kkd + ddlt*(m-1)+j
                            matuu(kk) = matuu(kk)+enr(n,i)*enr(m,j)* jac*rho(1)
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
