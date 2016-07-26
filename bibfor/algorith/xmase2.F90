subroutine xmase2(elrefp, ndim, coorse, igeom, he,&
                  nfh, ddlc, nfe, basloc, nnop,&
                  npg, imate, lsn, lst, matuu, heavn,&
                  jstno, nnops, ddlm)
    implicit none
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/reeref.h"
#include "asterfort/vecini.h"
#include "asterfort/xcalc_heav.h"
#include "asterfort/xcalc_code.h"
#include "asterfort/xcalfev_wrap.h"
#include "asterfort/iselli.h"
#include "asterfort/xnbddl.h"
#include "asterfort/xkamat.h"
#include "asterfort/iimatu.h"
#include "asterfort/lteatt.h"
#include "asterfort/indent.h"
    integer :: ndim, igeom, imate, nnop, npg, nfh, ddlc, nfe, heavn(27,5)
    integer :: jstno, nnops, ddlm
    character(len=8) :: elrefp
    real(kind=8) :: basloc(6*nnop), he, coorse(*)
    real(kind=8) :: lsn(nnop), lst(nnop), matuu(*)
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
! IN  nfh    : NOMBRE DE DDL HEAVYSIDE (PAR NOEUD)
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
    integer :: kpg, kk, n, i, m, j, j1, kkd
    integer :: nno, nnos, npgbis, ddls, ddld, cpt, ndimb
    integer :: jcoopg, jdfd2, jgano, idfde, ivf, ipoids, hea_se
!
    real(kind=8) :: rho(1)
    real(kind=8) :: xg(ndim), xe(ndim), ff(nnop), jac
    real(kind=8) :: enr(ndim, nnop, 1+nfh+ndim*nfe)
    real(kind=8) :: fk(27,3,3), ka, mu
    integer :: alp, dec(nnop), nn, mn, ii, jj, irese, singu
    integer :: ddln, ij, kddl(ndim, 1+nfh+ndim*nfe)
!
    character(len=16) :: phenom
    aster_logical :: axi
    character(len=8) :: elrese(6), fami(6)
!
    data    elrese /'SE2','TR3','TE4','SE3','TR6','T10'/
    data    fami   /'BID','XINT','XINT','BID','XINT','XINT'/
!
!--------------------------------------------------------------------
!
!     NOMBRE DE DDL DE DEPLACEMENT À CHAQUE NOEUD SOMMET
    call xnbddl(ndim, nfh, nfe, ddlc, ddld, ddls, singu)
    ddln=int(ddld/ndim)
    axi=lteatt('AXIS','OUI')
    enr(:,:,:)=0.d0
    kddl(:,:)=0
!
! DECALAGES CALCULES EN AMONT: PERF
    do n = 1, nnop
        call indent(n, ddls, ddlm, nnops, dec(n))
    end do
!
! CALCUL DE L IDENTIFIANT DU SS ELEMENT
    hea_se=xcalc_code(1, he_real=[he])
!
    if (.not.iselli(elrefp)) then
        irese=3
    else
        irese=0
    endif
!
    call elrefe_info(elrefe=elrese(ndim+irese),fami=fami(ndim+irese),&
                     ndim=ndimb,nno=nno,nnos=nnos,&
                     npg=npgbis,jpoids=ipoids,jcoopg=jcoopg,jvf=ivf,jdfde=idfde,&
                     jdfd2=jdfd2,jgano=jgano)
!
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
!       JUSTE POUR CALCULER LES FF
        call reeref(elrefp, nnop, zr(igeom), xg, ndim, xe, ff)
!
        if (nfe .gt. 0) then
            call xkamat(imate, ndim, axi, ka, mu)
            call xcalfev_wrap(ndim, nnop, basloc, zi(jstno), he,&
                         lsn, lst, zr(igeom), ka, mu, ff, fk)
        endif
!
! - CALCUL DES ELEMENTS GEOMETRIQUES
!
!--------CALCUL DES FONCTIONS ENRICHIES--------------------------
        do n = 1, nnop
!         FONCTIONS DE FORME CLASSIQUES
            cpt=0
            do i = 1, ndim
                cpt=cpt+1
                enr(i,n,1) = ff(n)
                kddl(i,1)=cpt
            end do
!         ENRICHISSEMENT PAR HEAVYSIDE
            do i = 1, ndim
              do j = 1, nfh
                cpt=cpt+1
                enr(i,n,1+j) = ff(n) * xcalc_heav(heavn(n,j),hea_se,heavn(n,5))
                kddl(i,1+j)=cpt
              enddo
            end do
!         ENRICHISSEMENT PAR LES NFE FONTIONS SINGULIÈRES
            do alp = 1, nfe*ndim
                do i = 1, ndim
                    cpt=cpt+1
                    enr(i,n,1+nfh+alp)=fk(n,alp,i)
                    kddl(i,1+nfh+alp)=cpt
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
          nn=dec(n)
          do ij = 1, ndim
            do i = 1, ddln
                ii=iimatu(kddl(ij,i),ndim,nfh,nfe)
                kkd = (nn+ii-1) * (nn+ii) /2
                do j = 1, ddln
                    jj=iimatu(kddl(ij,j),ndim,nfh,nfe)
                    do m = 1, n
                        mn=dec(m)
                        if (m .eq. n) then
                            j1 = kddl(ij,i)
                        else
                            j1 = ddld
                        endif
                        if (jj .le. j1) then
                            kk = kkd + mn + jj
                            matuu(kk) = matuu(kk)+enr(ij,n,i)*enr(ij,m,j)* jac*rho(1)
                        endif
                    end do
                end do
            end do
          end do
        end do
!
    end do
!
end subroutine
