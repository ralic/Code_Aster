subroutine nmel3d(fami, poum, nno, npg, ipoids,&
                  ivf, idfde, geom, typmod, option,&
                  imate, compor, lgpg, crit, depl,&
                  angmas, dfdi, pff, def, sig,&
                  vi, matuu, vectu, codret)
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
! aslint: disable=W1504
    implicit none
!
    include 'jeveux.h'
    include 'asterfort/nmcpel.h'
    include 'asterfort/nmgeom.h'
    integer :: nno, npg, imate, lgpg, codret, ipoids, ivf, idfde
    character(len=8) :: typmod(*)
    character(len=16) :: option, compor(4)
    character(len=*) :: fami, poum
    real(kind=8) :: geom(3, nno), crit(3)
    real(kind=8) :: angmas(3)
    real(kind=8) :: depl(1:3, 1:nno), dfdi(nno, 3)
    real(kind=8) :: pff(6, nno, nno), def(6, nno, 3)
    real(kind=8) :: sig(6, npg), vi(lgpg, npg), sigp(6)
    real(kind=8) :: matuu(*), vectu(3, nno)
!.......................................................................
!
!     BUT:  CALCUL  DES OPTIONS RIGI_MECA_TANG, RAPH_MECA ET FULL_MECA
!           EN HYPER-ELASTICITE
!.......................................................................
! IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  POIDSG  : POIDS DES POINTS DE GAUSS
! IN  VFF     : VALEUR  DES FONCTIONS DE FORME
! IN  DFDE    : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  DFDN    : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  DFDK    : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  GEOM    : COORDONEES DES NOEUDS
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  OPTION  : OPTION DE CALCUL
! IN  IMATE   : MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT
! IN  LGPG  : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
!              CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  DEPL    : DEPLACEMENT A PARTIR DE LA CONF DE REF
! OUT DFDI    : DERIVEE DES FONCTIONS DE FORME  AU DERNIER PT DE GAUSS
! OUT PFF     : PRODUIT DES FCT. DE FORME       AU DERNIER PT DE GAUSS
! OUT DEF     : PRODUIT DER. FCT. FORME PAR F   AU DERNIER PT DE GAUSS
! OUT SIG     : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA)
! OUT VI      : VARIABLES INTERNES    (RAPH_MECA ET FULL_MECA)
! OUT MATUU   : MATRICE DE RIGIDITE PROFIL (RIGI_MECA_TANG ET FULL_MECA)
! OUT VECTU   : FORCES NODALES (RAPH_MECA ET FULL_MECA)
!......................................................................
!
!
    integer :: kpg, kk, n, i, m, j, j1, kl, pq, kkd
    logical :: grdepl
    real(kind=8) :: dsidep(6, 6), f(3, 3), eps(6), r, sigma(6), ftf, detf
    real(kind=8) :: poids, tmp1, tmp2
!
    integer :: indi(6), indj(6)
    real(kind=8) :: rind(6), rac2
    data    indi / 1 , 2 , 3 , 1 , 1 , 2 /
    data    indj / 1 , 2 , 3 , 2 , 3 , 3 /
    data    rind / 0.5d0,0.5d0,0.5d0,0.70710678118655d0,&
     &               0.70710678118655d0,0.70710678118655d0 /
    data    rac2 / 1.4142135623731d0 /
!
!
! - INITIALISATION
!
    grdepl = compor(3) .eq. 'GROT_GDEP'
!
    do 10 kpg = 1, npg
!
! - CALCUL DES ELEMENTS GEOMETRIQUES
!
!      CALCUL DE DFDI, F, EPS, R (EN AXI) ET POIDS
        call nmgeom(3, nno, .false., grdepl, geom,&
                    kpg, ipoids, ivf, idfde, depl,&
                    .true., poids, dfdi, f, eps,&
                    r)
!
!      CALCUL DES PRODUITS SYMETR. DE F PAR N,
        do 120 n = 1, nno
            do 121 i = 1, 3
                def(1,n,i) = f(i,1)*dfdi(n,1)
                def(2,n,i) = f(i,2)*dfdi(n,2)
                def(3,n,i) = f(i,3)*dfdi(n,3)
                def(4,n,i) = (f(i,1)*dfdi(n,2) + f(i,2)*dfdi(n,1))/ rac2
                def(5,n,i) = (f(i,1)*dfdi(n,3) + f(i,3)*dfdi(n,1))/ rac2
                def(6,n,i) = (f(i,2)*dfdi(n,3) + f(i,3)*dfdi(n,2))/ rac2
121          continue
120      continue
!
!      CALCUL DES PRODUITS DE FONCTIONS DE FORMES (ET DERIVEES)
        if (( option(1:10) .eq. 'RIGI_MECA_' .or. option(1: 9) .eq. 'FULL_MECA' ) .and.&
            grdepl) then
            do 125 n = 1, nno
                do 126 m = 1, n
                    pff(1,n,m) = dfdi(n,1)*dfdi(m,1)
                    pff(2,n,m) = dfdi(n,2)*dfdi(m,2)
                    pff(3,n,m) = dfdi(n,3)*dfdi(m,3)
                    pff(4,n,m) =(dfdi(n,1)*dfdi(m,2)+dfdi(n,2)*dfdi(m,&
                    1))/rac2
                    pff(5,n,m) =(dfdi(n,1)*dfdi(m,3)+dfdi(n,3)*dfdi(m,&
                    1))/rac2
                    pff(6,n,m) =(dfdi(n,2)*dfdi(m,3)+dfdi(n,3)*dfdi(m,&
                    2))/rac2
126              continue
125          continue
        endif
!
!
! - LOI DE COMPORTEMENT : S(E) ET DS/DE
!
        call nmcpel(fami, kpg, 1, poum, 3,&
                    typmod, angmas, imate, compor, crit,&
                    option, eps, sigma, vi(1, kpg), dsidep,&
                    codret)
!
!
! - CALCUL DE LA MATRICE DE RIGIDITE
!
        if (option(1:10) .eq. 'RIGI_MECA_' .or. option(1: 9) .eq. 'FULL_MECA') then
!
            do 130 n = 1, nno
                do 131 i = 1, 3
                    kkd = (3*(n-1)+i-1) * (3*(n-1)+i) /2
                    do 151,kl=1,6
                    sigp(kl)=0.d0
                    sigp(kl)=sigp(kl)+def(1,n,i)*dsidep(1,kl)
                    sigp(kl)=sigp(kl)+def(2,n,i)*dsidep(2,kl)
                    sigp(kl)=sigp(kl)+def(3,n,i)*dsidep(3,kl)
                    sigp(kl)=sigp(kl)+def(4,n,i)*dsidep(4,kl)
                    sigp(kl)=sigp(kl)+def(5,n,i)*dsidep(5,kl)
                    sigp(kl)=sigp(kl)+def(6,n,i)*dsidep(6,kl)
151                  continue
                    do 140 j = 1, 3
                        do 141 m = 1, n
                            if (m .eq. n) then
                                j1 = i
                            else
                                j1 = 3
                            endif
!
!                 RIGIDITE GEOMETRIQUE
                            tmp1 = 0.d0
                            if (grdepl .and. i .eq. j) then
                                tmp1 = 0.d0
                                tmp1 = tmp1+pff(1,n,m)*sigma(1)
                                tmp1 = tmp1+pff(2,n,m)*sigma(2)
                                tmp1 = tmp1+pff(3,n,m)*sigma(3)
                                tmp1 = tmp1+pff(4,n,m)*sigma(4)
                                tmp1 = tmp1+pff(5,n,m)*sigma(5)
                                tmp1 = tmp1+pff(6,n,m)*sigma(6)
                            endif
!
!                 RIGIDITE ELASTIQUE
                            tmp2=0.d0
                            tmp2=tmp2+sigp(1)*def(1,m,j)
                            tmp2=tmp2+sigp(2)*def(2,m,j)
                            tmp2=tmp2+sigp(3)*def(3,m,j)
                            tmp2=tmp2+sigp(4)*def(4,m,j)
                            tmp2=tmp2+sigp(5)*def(5,m,j)
                            tmp2=tmp2+sigp(6)*def(6,m,j)
!                 STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                            if (j .le. j1) then
                                kk = kkd + 3*(m-1)+j
                                matuu(kk) = matuu(kk) + (tmp1+tmp2)* poids
                            endif
!
141                      continue
140                  continue
131              continue
130          continue
        endif
!
!
! - CALCUL DE LA FORCE INTERIEURE ET DES CONTRAINTES DE CAUCHY
!
        if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
!
            do 180 n = 1, nno
                do 181 i = 1, 3
                    do 182 kl = 1, 6
                        vectu(i,n)=vectu(i,n)+def(kl,n,i)*sigma(kl)*&
                        poids
182                  continue
181              continue
180          continue
!
            if (grdepl) then
!          CONVERSION LAGRANGE -> CAUCHY
                detf = f(3,3)*(f(1,1)*f(2,2)-f(1,2)*f(2,1)) - f(2,3)*( f(1,1)*f(3,2)-f(3,1)*f(1,2&
                       &)) + f(1,3)*(f(2,1)*f(3,2)- f(3,1)*f(2,2))
                do 190 pq = 1, 6
                    sig(pq,kpg) = 0.d0
                    do 200 kl = 1, 6
                        ftf = (&
                              f(&
                              indi(pq), indi(kl))*f(indj(pq), indj( kl)) + f(indi(pq),&
                              indj(kl))*f(indj(pq), indi( kl))&
                              )*rind(kl&
                              )
                        sig(pq,kpg) = sig(pq,kpg)+ ftf*sigma(kl)
200                  continue
                    sig(pq,kpg) = sig(pq,kpg)/detf
190              continue
            else
!          SIMPLE CORRECTION DES CONTRAINTES
                do 210 kl = 1, 3
                    sig(kl,kpg) = sigma(kl)
210              continue
                do 220 kl = 4, 6
                    sig(kl,kpg) = sigma(kl)/rac2
220              continue
            endif
        endif
10  end do
end subroutine
