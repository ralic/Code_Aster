subroutine nmgz2d(fami, nno, npg, ipoids, ivf,&
                  idfde, geomi, typmod, option, imate,&
                  compor, lgpg, crit, instam, instap,&
                  ideplm, ideplp, angmas, sigm, vim,&
                  dfdi, pff, def, sigp, vip,&
                  matuu, ivectu, codret)
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
! person_in_charge: jean-michel.proix at edf.fr
!
! aslint: disable=W1306,W1504
    implicit none
!
#include "jeveux.h"
#include "asterfort/codere.h"
#include "asterfort/lcegeo.h"
#include "asterfort/nmcomp.h"
#include "asterfort/nmgeom.h"
#include "asterfort/u2mess.h"
    integer :: nno, npg, imate, lgpg, codret, cod(9)
    integer :: ipoids, ivf, idfde, ivectu, ideplm, ideplp
    character(len=*) :: fami
    character(len=8) :: typmod(*)
    character(len=16) :: option, compor(4)
!
    real(kind=8) :: instam, instap, angmas(3)
    real(kind=8) :: geomi(2, nno), crit(3)
    real(kind=8) :: dfdi(nno, 2)
    real(kind=8) :: pff(4, nno, nno), def(4, nno, 2)
    real(kind=8) :: sigm(4, npg), sigp(4, npg)
    real(kind=8) :: vim(lgpg, npg), vip(lgpg, npg)
    real(kind=8) :: matuu(*)
!
!.......................................................................
!
!     BUT:  CALCUL  DES OPTIONS RIGI_MECA_TANG, RAPH_MECA ET FULL_MECA
!           EN GRANDES DEFORMATIONS 2D COROTATIONNEL ZMAT
!.......................................................................
! IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  POIDSG  : POIDS DES POINTS DE GAUSS
! IN  VFF     : VALEUR  DES FONCTIONS DE FORME
! IN  DFDE    : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  DFDK    : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  GEOMI   : COORDONEES DES NOEUDS SUR CONFIG INITIALE
! IN  TYPMOD  : TYPE DE MODEELISATION
! IN  OPTION  : OPTION DE CALCUL
! IN  IMATE   : MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT
! IN  LGPG    : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
!               CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  INSTAM  : INSTANT PRECEDENT
! IN  INSTAP  : INSTANT DE CALCUL
! IN  PHASM   : PHASE METALLURGIQUE A L'INSTANT PRECEDENT
! IN  PHASP   : PHASE METALLURGIQUE A L'INSTANT DE CALCUL
! IN  DEPLM   : DEPLACEMENT A L'INSTANT PRECEDENT
! IN  DEPLP   : DEPLACEMENT A L'INSTANT COURANT
! IN  ANGMAS  : LES TROIS ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
! IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
! IN  VIM     : VARIABLES INTERNES A L'INSTANT PRECEDENT
! OUT DFDI    : DERIVEE DES FONCTIONS DE FORME  AU DERNIER PT DE GAUSS
! OUT DEF     : PRODUIT DER. FCT. FORME PAR F   AU DERNIER PT DE GAUSS
! OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA)
! OUT VIP     : VARIABLES INTERNES    (RAPH_MECA ET FULL_MECA)
! OUT MATUU   : MATRICE DE RIGIDITE PROFIL (RIGI_MECA_TANG ET FULL_MECA)
! OUT VECTU   : FORCES NODALES (RAPH_MECA ET FULL_MECA)
!.......................................................................
!
!
    logical :: grand, axi, resi, rigi
!
    integer :: kpg, kk, kkd, n, i, m, j, j1, kl, pq
!
    real(kind=8) :: dsidep(6, 6), f(3, 3), fm(3, 3), fr(3, 3), epsm(6), epsp(6)
    real(kind=8) :: r, sigma(6), sign(6), sig(6), sigg(4)
    real(kind=8) :: poids, tmp1, tmp2, rbid
    real(kind=8) :: elgeom(10, 9), fp(3, 3)
    real(kind=8) :: kron(3, 3), geomp(2, nno), rac2
    data kron/1.d0,0.d0,0.d0, 0.d0,1.d0,0.d0, 0.d0,0.d0,1.d0/
!
    if (compor(1)(1:4) .ne. 'ZMAT') then
        call u2mess('F', 'ALGORITH7_96')
    endif
!
!
! 1 - INITIALISATION
    rac2 = sqrt(2.d0)
    grand = .true.
    axi = typmod(1) .eq. 'AXIS'
    resi = option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL'
    rigi = option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL'
!
! 3 - CALCUL DES ELEMENTS GEOMETRIQUES SPECIFIQUES AU COMPORTEMENT
!
    call lcegeo(nno, npg, ipoids, ivf, idfde,&
                geomi, typmod, compor, 2, dfdi,&
                zr(ideplm), zr(ideplp), elgeom)
!
! 4 - INITIALISATION CODES RETOURS
!
    do 1955 kpg = 1, npg
        cod(kpg)=0
1955  end do
!
! 5 - CALCUL POUR CHAQUE POINT DE GAUSS
!
    do 800 kpg = 1, npg
!
! 5.2 - CALCUL DES ELEMENTS GEOMETRIQUES
!
! 5.2.1 - CALCUL DE EPSM EN T- POUR LDC
!
        do 20 j = 1, 6
            epsm (j)=0.d0
            epsp (j)=0.d0
20      continue
        call nmgeom(2, nno, axi, grand, geomi,&
                    kpg, ipoids, ivf, idfde, zr( ideplm),&
                    .true., poids, dfdi, fm, epsm,&
                    r)
!
! 5.2.2 - CALCUL DE F, EPSP, DFDI, R ET POIDS EN T+
!
        call nmgeom(2, nno, axi, grand, geomi,&
                    kpg, ipoids, ivf, idfde, zr( ideplp),&
                    .true., poids, dfdi, f, epsp,&
                    r)
!
!
        do 55 n = 1, nno
            do 56 i = 1, 2
                geomp(i,n) = geomi(i,n) + zr(ideplp-1+2*(n-1)+i)
56          continue
55      continue
!
        call nmgeom(2, nno, axi, grand, geomp,&
                    kpg, ipoids, ivf, idfde, zr( ideplp),&
                    .true., poids, dfdi, fp, epsp,&
                    r)
!
        do 57 i = 1, 3
            do 58 j = 1, 3
                fr(i,j) = kron(i,j)
58          continue
57      continue
!
!
        do 40 n = 1, nno
            do 30 i = 1, 2
                def(1,n,i) = fr(i,1)*dfdi(n,1)
                def(2,n,i) = fr(i,2)*dfdi(n,2)
                def(3,n,i) = 0.d0
                def(4,n,i) = (fr(i,1)*dfdi(n,2) + fr(i,2)*dfdi(n,1))/ rac2
30          continue
40      continue
!
! 5.2.5 - TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1
!
        if (axi) then
            do 50 n = 1, nno
                def(3,n,1) = fr(3,3)*zr(ivf+n+(kpg-1)*nno-1)/r
50          continue
        endif
!
! 5.2.6 - CALCUL DES PRODUITS DE FONCTIONS DE FORMES (ET DERIVEES)
!
        if (rigi) then
            do 125 n = 1, nno
                do 126 m = 1, n
                    pff(1,n,m) = dfdi(n,1)*dfdi(m,1)
                    pff(2,n,m) = dfdi(n,2)*dfdi(m,2)
                    pff(3,n,m) = 0.d0
                    pff(4,n,m) =(dfdi(n,1)*dfdi(m,2)+dfdi(n,2)*dfdi(m,&
                    1))/rac2
126              continue
125          continue
        endif
!
!         CAUCHY
        do 59 i = 1, 4
            sign(i)=sigm(i,kpg)
59      continue
        sign(4)=sign(4)*rac2
!
! 5.3.2 - INTEGRATION
        call nmcomp(fami, kpg, 1, 2, typmod,&
                    imate, compor, crit, instam, instap,&
                    9, fm, f, 6, sign,&
                    vim(1, kpg), option, angmas, 10, elgeom( 1, kpg),&
                    sigma, vip(1, kpg), 36, dsidep, 1,&
                    rbid, cod(kpg))
!
        if (cod(kpg) .eq. 1) then
            goto 1956
        endif
!
! 5.4 - CALCUL DE LA MATRICE DE RIGIDITE
!
        if (rigi) then
            do 160 n = 1, nno
                do 150 i = 1, 2
                    do 151,kl=1,4
                    sig(kl)=0.d0
                    sig(kl)=sig(kl)+def(1,n,i)*dsidep(1,kl)
                    sig(kl)=sig(kl)+def(2,n,i)*dsidep(2,kl)
                    sig(kl)=sig(kl)+def(3,n,i)*dsidep(3,kl)
                    sig(kl)=sig(kl)+def(4,n,i)*dsidep(4,kl)
151                  continue
                    do 140 j = 1, 2
                        do 130 m = 1, n
                            if (m .eq. n) then
                                j1 = i
                            else
                                j1 = 2
                            endif
!
! 5.4.1 - RIGIDITE GEOMETRIQUE
!
                            if (option(1:4) .eq. 'RIGI') then
                                sigg(1)=sign(1)
                                sigg(2)=sign(2)
                                sigg(3)=sign(3)
                                sigg(4)=sign(4)
                            else
                                sigg(1)=sigma(1)
                                sigg(2)=sigma(2)
                                sigg(3)=sigma(3)
                                sigg(4)=sigma(4)
                            endif
!
                            tmp1 = 0.d0
                            if (i .eq. j) then
                                tmp1 = pff(1,n,m)*sigg(1) + pff(2,n,m) *sigg(2) + pff(3,n,m)*sigg&
                                       &(3) + pff(4, n,m)*sigg(4)
!
! TERME DE CORRECTION AXISYMETRIQUE
!
                                if (axi .and. i .eq. 1) then
                                    tmp1=tmp1+zr(ivf+n+(kpg-1)*nno-1)*&
                                    zr(ivf+m+(kpg-1)*nno-1)/(r*r)*&
                                    sigg(3)
                                endif
                            endif
!
! 5.4.2 - RIGIDITE ELASTIQUE
!
                            tmp2=0.d0
                            tmp2=tmp2+sig(1)*def(1,m,j)
                            tmp2=tmp2+sig(2)*def(2,m,j)
                            tmp2=tmp2+sig(3)*def(3,m,j)
                            tmp2=tmp2+sig(4)*def(4,m,j)
!
! 5.4.3 - STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                            if (j .le. j1) then
                                kkd = (2*(n-1)+i-1) * (2*(n-1)+i) /2
                                kk = kkd + 2*(m-1)+j
                                matuu(kk) = matuu(kk) + (tmp1+tmp2)* poids
                            endif
!
130                      continue
140                  continue
150              continue
160          continue
        endif
!
! 5.5 - CALCUL DE LA FORCE INTERIEURE
!
        if (resi) then
            do 230 n = 1, nno
                do 220 i = 1, 2
                    do 210 kl = 1, 4
!            VECTU(I,N)=VECTU(I,N)+DEF(KL,N,I)*SIGMA(KL)*POIDS
                        zr(ivectu-1+2*(n-1)+i)= zr(ivectu-1+2*(n-1)+i)&
                        +def(kl,n,i)*sigma(kl)*poids
210                  continue
220              continue
230          continue
!
! 5.6 - CALCUL DES CONTRAINTES DE CAUCHY, CONVERSION LAGRANGE -> CAUCHY
!
!
            do 255 pq = 1, 4
                sigp(pq,kpg) = sigma(pq)
255          continue
!
!
        endif
!
800  end do
!
1956  continue
!
! - SYNTHESE DES CODES RETOURS
!
    call codere(cod, npg, codret)
end subroutine
