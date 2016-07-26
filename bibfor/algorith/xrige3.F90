subroutine xrige3(elrefp, ndim, coorse, igeom, he,&
                  heavn, ddlh, ddlc, nfe, basloc,&
                  nnop, npg, lsn, lst, sig,&
                  matuu, jstno, imate)
!
! aslint: disable=W1306
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/reeref.h"
#include "asterfort/vecini.h"
#include "asterfort/xcalc_code.h"
#include "asterfort/xcalc_heav.h"
#include "asterfort/xcalfev_wrap.h"
#include "asterfort/xkamat.h"
#include "asterfort/xnbddl.h"
#include "asterfort/iimatu.h"
    integer :: ndim, igeom, nnop, npg, ddlh, ddlc, nfe, heavn(27,5)
    integer :: jstno, imate
    character(len=8) :: elrefp
    real(kind=8) :: basloc(9*nnop), he, coorse(*)
    real(kind=8) :: lsn(nnop), lst(nnop), sig(90), matuu(*)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: kpg, kk, n, i, m, j, j1, kkd, ij
    integer :: nno, nnos, npgbis, ddls, ddld, ddldn, cpt, ndimb
    integer :: jcoopg, jdfd2, jgano, idfde, ivf, ipoids, hea_se, nfiss
    integer :: nnops
    integer :: alp, ii, jj, k, nfh
    real(kind=8) :: tmp1
    real(kind=8) :: xg(ndim), xe(ndim), ff(nnop), jac
    real(kind=8) :: dfdi(nnop, ndim), pff(1+ddlh+nfe*ndim**2, nnop, ndim)
    real(kind=8) :: rac2
    real(kind=8) :: fk(27,3,3), dkdgl(27,3,3,3), ka, mu
    data    rac2 / 1.4142135623731d0 /
!--------------------------------------------------------------------
!
!     ATTENTION, DEPL ET VECTU SONT ICI DIMENSIONNÉS DE TELLE SORTE
!     QU'ILS NE PRENNENT PAS EN COMPTE LES DDL SUR LES NOEUDS MILIEU
!
!     NOMBRE DE DDL DE DEPLACEMENT À CHAQUE NOEUD
    nfh=int(ddlh/ndim)
    call xnbddl(ndim, nfh, nfe, ddlc, ddld, ddls, nfe)
    ddldn = 1+nfh+nfe*ndim**2
!     ELEMENT DE REFERENCE PARENT : RECUP DE NNOPS
    call elrefe_info(fami='RIGI',nnos=nnops)
!
!       TE4-'XINT' : SCHÉMAS À 15 POINTS
    call elrefe_info(elrefe='TE4',fami='XINT',ndim=ndimb,nno=nno,nnos=nnos,&
  npg=npgbis,jpoids=ipoids,jcoopg=jcoopg,jvf=ivf,jdfde=idfde,&
  jdfd2=jdfd2,jgano=jgano)
!
    ASSERT(npg.eq.npgbis.and.ndim.eq.ndimb)
!
! CALCUL DE L IDENTIFIANT DU SS ELEMENT :: LE MULTI-HEAVISIDE N EST PAS PRIS EN COMPTE SANS FISNO
    nfiss=1
    hea_se=xcalc_code(nfiss, he_real=[he])
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
!       JUSTE POUR CALCULER LES FF
        call reeref(elrefp, nnop, zr(igeom), xg, ndim, xe, ff, dfdi=dfdi)
!
        if (nfe .gt. 0) then
!            call xkamat(imate, ndim, .false._1, ka, mu)
            imate=imate
            ka=3.
            mu=sum(abs(sig(((kpg-1)*6+1):((kpg-1)*6+3))))/3.
            call xcalfev_wrap(ndim, nnop, basloc, zi(jstno), he,&
                         lsn, lst, zr(igeom), ka, mu, ff, fk, dfdi=dfdi, dkdgl=dkdgl)
        endif
!
!       POUR CALCULER LE JACOBIEN DE LA TRANSFO SSTET->SSTET REF
!       ET LES DERIVEES DES FONCTIONS DE FORME,
!       ON ENVOIE DFDM3D AVEC LES COORD DU SS-ELT
        call dfdm3d(nno, kpg, ipoids, idfde, coorse,&
                    jac)
        do i = 1, ndim
            do n = 1, nnop
!----------LA PARTIE CORRESPOINDANTE AU FF CLASSIQUES
                cpt= 1
                pff(cpt,n,i) = dfdi(n,i)
!----------LA PARTIE CORRESPONDANTE A L'ENRICHEMENT HEAVISIDE
                if (ddlh .eq. ndim) then
                    cpt = 2
                    pff(cpt,n,i) = dfdi(n,i)*xcalc_heav(heavn(n,1),hea_se,heavn(n,5))
                endif
!----------LA PARTIE CORRESPONDANTE A L'ENRICHEMENT CRACK-TIP
                do alp = 1, nfe*ndim
                  do k = 1, ndim
                    cpt =cpt+1
                    pff(cpt,n,i) = dkdgl(n,alp,i,k)
                  enddo
                end do
                ASSERT(cpt.eq.ddldn)
            end do
        end do
!
        do n = 1, nnop
            do m = 1, n
                do i = 1, ddldn
                    do j = 1, ddldn
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
                        do ij = 1, ndim
                            ii=iimatu((i-1)*ndim+ij,ndim,nfh,nfe)
                            jj=iimatu((j-1)*ndim+ij,ndim,nfh,nfe)
                            if (m .eq. n) then
                                j1 = ii
                            else
                                j1 = ddldn
                            endif
                            if (jj .le. j1) then
                                kkd = (ddls*(n-1)+ii-1) * (ddls*(n-1)+ii) /2
                                kk = kkd + ddls*(m-1)+jj
                                matuu(kk) = matuu(kk) + tmp1*jac
                            endif
                        end do
!
                    end do
                end do
            end do
        end do
!
    end do
!
end subroutine
