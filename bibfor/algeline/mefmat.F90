subroutine mefmat(ndim, numgrp, nbz, nbgrp, nbmod,&
                  matma, dcent, cp, cf, vit,&
                  rho, pstat, dpstat, rint, phix,&
                  phiy, z, matm, matr, mata,&
                  itypg, axg, zg, rhog, vitg,&
                  cdg, cpg)
! aslint: disable=W1504
    implicit none
!
    include 'jeveux.h'
    include 'asterc/r8pi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/matini.h'
    include 'asterfort/mefin1.h'
    include 'asterfort/mefin2.h'
    include 'asterfort/mefin3.h'
    include 'asterfort/mefin4.h'
    include 'asterfort/mefin5.h'
    include 'asterfort/wkvect.h'
    integer :: ndim(14), numgrp(*), nbmod
    real(kind=8) :: matma(*), dcent(*)
    real(kind=8) :: cp(*), cf(*), vit(0:*), rho(0:*), pstat(*), dpstat(*)
    real(kind=8) :: matm(nbmod, nbmod), rint(*), phix(nbz*nbgrp, nbmod)
    real(kind=8) :: phiy(nbz*nbgrp, nbmod), z(*)
    real(kind=8) :: matr(nbmod, nbmod), mata(nbmod, nbmod)
!
    integer :: itypg(*)
    real(kind=8) :: zg(*), axg(*), rhog(*), vitg(*), cdg(*), cpg(*)
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!     CALCUL DES MATRICES DE MASSE, DE RAIDEUR, D AMORTISSEMENT SOUS
!     ECOULEMENT
!     OPERATEUR APPELANT : OP0144 , FLUST3, MEFIST
! ----------------------------------------------------------------------
!     OPTION DE CALCUL   : CALC_FLUI_STRU , CALCUL DES PARAMETRES DE
!     COUPLAGE FLUIDE-STRUCTURE POUR UNE CONFIGURATION DE TYPE "FAISCEAU
!     DE TUBES SOUS ECOULEMENT AXIAL"
! ----------------------------------------------------------------------
! IN  : NDIM   : TABLEAU DES DIMENSIONS
! IN  : NUMGRP : INDICES DES GROUPES D EQUIVALENCE
! IN  : NBZ     : NOMBRE DE POINTS DE DISCRETISATION
! IN  : NBGRP  : NOMBRE DE GROUPES D'EQUIVALENCE DE CYLINDRES
! IN  : NBMOD  : NOMBRE DE MODES PRIS EN COMPTE POUR LE COUPLAGE
! IN  : MATMA  : VECTEUR CONTENANT LES MATRICES MODALES, MASSE,RIGIDITE,
!                AMORTISSEMENT
! IN  : DCENT  : VECTEUR CONTENANT LES TABLEAUX DE COEFFICIENTS ET
!                LES MATRICES EN AIR
! IN  : CP     : COEFFICIENT DE PORTANCE CP DU FLUIDE AUTOUR D UN
!                CYLINDRE INCLINE, AUX POINTS DE DISCRETISATION
! IN  : CF     : COEFFICIENT DE TRAINEE VISQUEUSE DU FLUIDE LE LONG DES
!                PAROIS, AUX POINTS DE DISCRETISATION
! IN  : VIT    : VITESSE D ECOULEMENT DU FLUIDE AUX POINTS DE
!                DISCRETISATION
! IN  : RHO    : MASSE VOLUMIQUE DU FLUIDE AUX POINTS DE DISCRETISATION
! IN  : PSTAT  : PROFIL DE PRESSION STATIONNAIRE
! IN  : DPSTAT : PROFIL DE GRADIENT DE PRESSION STATIONNAIRE
! IN  : RINT   : RAYONS DES CYLINDRES
! IN  : PHIX   : DEFORMEES MODALES INTERPOLEES DANS LE REPERE AXIAL
! IN  : PHIY   : DEFORMEES MODALES INTERPOLEES DANS LE REPERE AXIAL
! OUT : MATM   : MATRICE DE MASSE AJOUTEE REPRESENTANT LA PROJECTION DES
!                EFFORTS FLUIDES INERTIELS DANS LA BASE DES DEFORMEES
!                MODALES DES CYLINDRES
! OUT : MATR   : MATRICE DE RAIDEUR  AJOUTEE REPRESENTANT LA PROJECTION
!                DES EFFORTS FLUIDES DE RAIDEUR DANS LA BASE DES
!                DEFORMEES MODALES DES CYLINDRES
! OUT : MATA   : MATRICE D AMORTISSEMENT AJOUTEE REPRESENTANT LA
!                PROJECTION DES EFFORTS FLUIDES D AMORTISSEMENT DANS LA
!                BASE DES DEFORMEES MODALES DES CYLINDRES
!
! IN  : ITYPG  : VECTEUR DES GROUPES D'APPARTENANCE DES GRILLES
! IN  : ZG     : POINTS DE DISCRETISATION DES GRILLES (EN LEURMILIEU)
! IN  : AXG    : SECTION SOLIDE DES TYPES DE GRILLES
! IN  : RHOG   : PROFIL DE MASSE VOLUMIQUE DE L'ECOULEMENT AU NIVEAU
!                         DES GRILLES
! IN  : VITG   : PROFIL DE VITESSE DE L'ECOULEMENT AU NIVEAU DES GRILLES
! IN  : CDG    : COEFF DE TRAINEE POUR CHAQUE TYPE DE GRILLES
! IN  : CPG    : PENTE DU COEFF DE PORTANCE POUR CHAQUE TYPE DE GRILLES
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: i, j
    integer :: imod, igrp, jmod, jgrp
    integer :: ncyl
    real(kind=8) :: rayo
!
    integer :: kg, k, ngz1, ngz2
    real(kind=8) :: ecart
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: iaireg, idphxg, idphyg, ig, ih, imataa, imatra
    integer :: iphixg, iphiyg, ippxx, ippxy, ippyx, ippyy, ivnxx
    integer :: ivnxy, ivnyx, ivnyy, nbcyl, nbgrp, nbgtot, nbz
    integer :: ntypg
    real(kind=8) :: aire, pi, rho0, vit0
!-----------------------------------------------------------------------
    call jemarq()
!
! --- LECTURE DES DIMENSIONS
    nbcyl = ndim(3)
    ntypg = ndim(13)
    nbgtot = ndim(14)
!
! --- CREATION DES OBJETS DE TRAVAIL
    call wkvect('&&MEFMAT.TMP.GH', 'V V R', nbz*2, ig)
    ih = ig + nbz
    if (ntypg .ne. 0) then
        call wkvect('&&MEFMAT.AIREG', 'V V R', ntypg, iaireg)
        call wkvect('&&MEFMAT.PHIXG', 'V V R', nbgrp*nbgtot*nbmod, iphixg)
        call wkvect('&&MEFMAT.PHIYG', 'V V R', nbgrp*nbgtot*nbmod, iphiyg)
        call wkvect('&&MEFMAT.DPHIXG', 'V V R', nbgrp*nbgtot*nbmod, idphxg)
        call wkvect('&&MEFMAT.DPHIYG', 'V V R', nbgrp*nbgtot*nbmod, idphyg)
    endif
!
    pi = r8pi()
!
! --- VITESSE MOYENNE D ECOULEMENT ET MASSE VOLUMIQUE MOYENNE
!
    rho0 = rho(0)
    vit0 = vit(0)
!
! --- DECALAGES DES TABLEAUX DE COEFFICIENTS ET DES MATRICES EN AIR
! --- DANS LE VECTEUR DCENT
!
    ippxx = nbcyl + nbcyl + nbcyl*nbcyl + nbcyl*nbcyl
    ippxy = ippxx + nbcyl*nbgrp
    ippyx = ippxy + nbcyl*nbgrp
    ippyy = ippyx + nbcyl*nbgrp
    ivnxx = ippyy + nbcyl*nbgrp
    ivnxy = ivnxx + nbcyl*nbgrp
    ivnyx = ivnxy + nbcyl*nbgrp
    ivnyy = ivnyx + nbcyl*nbgrp
!
! --- DECALAGES DES MATRICES MODALES DANS LE VECTEUR MATMA
!
    imatra = nbmod
    imataa = imatra + nbmod
!
! --- INITIALISATIONS
!
    call matini(nbmod, nbmod, 0.d0, matm)
    call matini(nbmod, nbmod, 0.d0, mata)
    call matini(nbmod, nbmod, 0.d0, matr)
!
!---  SECTION SOLIDE D'UNE CELLULE ELEMENTAIRE DE GRILLE
!
    if (ntypg .ne. 0) then
!
        do 45 k = 1, ntypg
            zr(iaireg+k-1) = axg(k)/dble(nbcyl)
45      continue
!
!---  CALCUL (PAR INTERPOLATION  LINEAIRE) DES DEFORMEES MODALES
!---  ET DE LEUR GRADIENT AUX COTES ZG DES GRILLES
!
        do 30 imod = 1, nbmod
            do 37 igrp = 1, nbgrp
                do 32 i = 1, nbz
                    do 33 j = 1, nbgtot
                        ecart = (z(i)-zg(j))*(z(i-1)-zg(j))
                        if (ecart .le. 0.d0) then
                            zr(iphixg+(imod-1)*nbgrp*nbgtot+(igrp-1)*&
                            nbgtot+j-1) = phix((igrp-1)*nbz+i-1,imod)*&
                            (z(i)-zg(j))/(z(i)-z(i-1))+ phix((igrp-1)*&
                            nbz+i,imod)*(zg(j)-z(i-1))/(z(i)-z(i-1))
!
                            zr(iphiyg+(imod-1)*nbgrp*nbgtot+(igrp-1)*&
                            nbgtot+j-1) = phiy((igrp-1)*nbz+i-1,imod)*&
                            (z(i)-zg(j))/(z(i)-z(i-1))+ phiy((igrp-1)*&
                            nbz+i,imod)*(zg(j)-z(i-1))/(z(i)-z(i-1))
!
                            zr(idphxg+(imod-1)*nbgrp*nbgtot+(igrp-1)*&
                            nbgtot+j-1) = ( phix((igrp-1)*nbz+i,imod)-&
                            phix((igrp-1)*nbz+i-1,imod) ) / ( z(i)-z(&
                            i-1) )
!
                            zr(idphyg+(imod-1)*nbgrp*nbgtot+(igrp-1)*&
                            nbgtot+j-1) = ( phiy((igrp-1)*nbz+i,imod)-&
                            phiy((igrp-1)*nbz+i-1,imod) ) / ( z(i)-z(&
                            i-1) )
!
                        endif
33                  continue
32              continue
37          continue
30      continue
!
    endif
!
    do 4 jmod = 1, nbmod
        do 41 imod = 1, nbmod
!
            do 411 jgrp = 1, nbgrp
                do 4111 igrp = 1, nbgrp
!
                    do 41112 i = 1, nbcyl
                        if (numgrp(i) .eq. igrp) then
                            rayo = rint(i)
                        endif
41112                  continue
                    aire = pi*rayo*rayo
!
! --- CONTRIBUTION DES EFFORTS NORMAUX DE FROTTEMENT VISQUEUX
! --- -> TERMES D'AMORTISSEMENT ET DE RAIDEUR AJOUTES
!
! --- AMORTISSEMENT AJOUTE
!
                    ncyl = 0
                    if (igrp .eq. jgrp) then
                        do 41111 i = 1, nbcyl
                            if (numgrp(i) .eq. igrp) ncyl = ncyl - 1
41111                      continue
                    endif
!
                    mata(imod,jmod) = mata(imod,jmod)-rho0*abs(vit0)* rayo* ( ( dcent(ivnxx+nbcyl&
                                      &*(jgrp-1)+igrp) + dble( ncyl) ) * mefin1(nbz,nbgrp,imod,ig&
                                      &rp,jmod,jgrp,z, phix,phix,cf) + dcent(ivnxy+nbcyl*(jgrp-1)&
                                      &+igrp) * mefin1(nbz,nbgrp,imod,igrp,jmod,jgrp,z,phix,phiy,&
                                      & cf) + dcent(ivnyx+nbcyl*(jgrp-1)+igrp) * mefin1( nbz,nbgr&
                                      &p,imod,igrp,jmod,jgrp,z,phiy,phix,cf) + ( dcent(ivnyy+nbcy&
                                      &l*(jgrp-1)+igrp) + dble(ncyl) ) * mefin1(nbz,nbgrp,imod,ig&
                                      &rp,jmod,jgrp,z,phiy, phiy,cf) )
!
!
                    mata(imod,jmod) = mata(imod,jmod)-rho0*abs(vit0)* rayo* ( ( dcent(ivnxx+nbcyl&
                                      &*(jgrp-1)+igrp) + dble( ncyl) ) * mefin1(nbz,nbgrp,imod,ig&
                                      &rp,jmod,jgrp,z, phix,phix,cp) + dcent(ivnxy+nbcyl*(jgrp-1)&
                                      &+igrp) * mefin1(nbz,nbgrp,imod,igrp,jmod,jgrp,z,phix,phiy,&
                                      & cp) + dcent(ivnyx+nbcyl*(jgrp-1)+igrp) * mefin1( nbz,nbgr&
                                      &p,imod,igrp,jmod,jgrp,z,phiy,phix,cp) + ( dcent(ivnyy+nbcy&
                                      &l*(jgrp-1)+igrp) + dble(ncyl) ) * mefin1(nbz,nbgrp,imod,ig&
                                      &rp,jmod,jgrp,z,phiy, phiy,cp) )
!
!
! ---  RAIDEUR AJOUTEE
!
                    matr(imod,jmod) = matr(imod,jmod)-rho0*abs(vit0)* rayo* ( dcent(ivnxx+nbcyl*(&
                                      &jgrp-1)+igrp) * mefin4( nbz,nbgrp, imod,igrp,jmod,jgrp,z,p&
                                      &hix,phix,vit,cf, zr(ig)) + dcent(ivnxy+nbcyl*(jgrp-1)+igrp&
                                      &) * mefin4(nbz,nbgrp, imod,igrp,jmod,jgrp,z,phix,phiy, vit&
                                      &,cf,zr(ig)) + dcent(ivnyx+nbcyl*(jgrp-1)+igrp) * mefin4(nb&
                                      &z,nbgrp, imod,igrp,jmod,jgrp,z,phiy, phix,vit,cf,zr(ig)) +&
                                      & dcent(ivnyy+nbcyl*(jgrp-1)+ igrp) * mefin4(nbz,nbgrp, imo&
                                      &d,igrp,jmod,jgrp,z, phiy,phiy,vit,cf,zr(ig)) )
!
!
                    matr(imod,jmod) = matr(imod,jmod)-rho0*abs(vit0)* rayo* ( ( dcent(ivnxx+nbcyl&
                                      &*(jgrp-1)+igrp) + dble( ncyl) ) * mefin4(nbz,nbgrp,imod,ig&
                                      &rp,jmod,jgrp,z, phix,phix,vit, cp,zr(ig)) + dcent(ivnxy+nb&
                                      &cyl*( jgrp-1)+igrp) * mefin4(nbz,nbgrp,imod,igrp,jmod, jgr&
                                      &p,z,phix,phiy,vit, cp,zr(ig)) + dcent(ivnyx+ nbcyl*(jgrp-1&
                                      &)+igrp) * mefin4(nbz,nbgrp,imod,igrp, jmod,jgrp,z,phiy,phi&
                                      &x,vit, cp,zr(ig)) + ( dcent( ivnyy+nbcyl*(jgrp-1)+igrp) + &
                                      &dble(ncyl) ) * mefin4(nbz,nbgrp,imod,igrp,jmod,jgrp,z,phiy&
                                      &,phiy, vit, cp,zr(ig)) )
!
!
!
! --- CONTRIBUTION DES EFFORTS DE PRESSION PERTURBEE
! --- -> TERMES DE MASSE, AMORTISSEMENT ET RAIDEUR AJOUTES
!
! --- MASSE AJOUTEE
!
                    matm(imod,jmod) = matm(imod,jmod) - aire * ( dcent(ippxx+nbcyl*(jgrp-1)+igrp)&
                                      & * mefin1(nbz, nbgrp,imod,igrp,jmod,jgrp,z,phix,phix,rho) &
                                      &+ dcent(ippxy+nbcyl*(jgrp-1)+igrp) * mefin1(nbz, nbgrp,imo&
                                      &d,igrp,jmod,jgrp,z,phix,phiy,rho) + dcent(ippyx+nbcyl*(jgr&
                                      &p-1)+igrp) * mefin1(nbz, nbgrp,imod,igrp,jmod,jgrp,z,phiy,&
                                      &phix,rho) + dcent(ippyy+nbcyl*(jgrp-1)+igrp) * mefin1(nbz,&
                                      & nbgrp,imod,igrp,jmod,jgrp,z,phiy,phiy,rho) )
!
! --- AMORTISSEMENT AJOUTE
!
                    mata(imod,jmod) = mata(imod,jmod) - 2.d0 * rho0 * vit0 * aire * ( dcent(ippxx&
                                      &+nbcyl*(jgrp-1)+igrp) * mefin2(nbz,nbgrp,imod,igrp,jmod,jg&
                                      &rp,z,phix,phix, zr(ig)) + dcent(ippxy+nbcyl*(jgrp-1)+igrp)&
                                      & * mefin2(nbz,nbgrp,imod,igrp,jmod,jgrp,z,phix,phiy, zr(ig&
                                      &)) + dcent(ippyx+nbcyl*(jgrp-1)+igrp) * mefin2(nbz,nbgrp,i&
                                      &mod,igrp,jmod,jgrp,z,phiy,phix, zr(ig)) + dcent(ippyy+nbcy&
                                      &l*(jgrp-1)+igrp) * mefin2(nbz,nbgrp,imod,igrp,jmod,jgrp,z,&
                                      &phiy,phiy, zr(ig)) )
!
! --- RAIDEUR AJOUTEE
!
                    matr(imod,jmod) = matr(imod,jmod) - rho0 * vit0 * aire * ( dcent(ippxx+nbcyl*&
                                      &(jgrp-1)+igrp) * mefin3(nbz,nbgrp,imod, igrp,jmod,jgrp,z,p&
                                      &hix,phix, vit,zr(ig),zr(ih)) + dcent(ippxy+nbcyl*(jgrp-1)+&
                                      & igrp) * mefin3(nbz,nbgrp,imod, igrp,jmod,jgrp,z, phix,phi&
                                      &y,vit,zr(ig),zr(ih)) + dcent(ippyx+nbcyl*( jgrp-1)+igrp) *&
                                      & mefin3(nbz,nbgrp,imod, igrp,jmod, jgrp,z,phiy,phix,vit,zr&
                                      &(ig),zr(ih)) + dcent(ippyy+ nbcyl*(jgrp-1)+igrp) * mefin3(&
                                      &nbz,nbgrp,imod, igrp,jmod,jgrp,z,phiy,phiy,vit,zr(ig),zr(i&
                                      &h)) )
!
4111              continue
411          continue
!
            do 412 igrp = 1, nbgrp
!
                ncyl = 0
                do 4121 i = 1, nbcyl
                    if (numgrp(i) .eq. igrp) ncyl = ncyl + 1
4121              continue
!
                do 4122 i = 1, nbcyl
                    if (numgrp(i) .eq. igrp) then
                        rayo = rint(i)
                    endif
4122              continue
                aire = pi*rayo*rayo
!C
! ---    CONTRIBUTION DES EFFORTS DE PRESSION STATIONNAIRE
! ---    -> TERMES DE RAIDEUR AJOUTEE
!
! ---    RAIDEUR AJOUTEE
!
                matr(imod,jmod) = matr(imod,jmod)-aire*ncyl* (mefin3( nbz,nbgrp,imod,igrp,jmod, i&
                                  &grp,z,phix,phix,pstat,zr( ig),zr(ih))+ mefin3(nbz,nbgrp,imod,i&
                                  &grp,jmod, igrp,z, phiy,phiy,pstat,zr(ig),zr(ih))+ mefin5(nbz,n&
                                  &bgrp,imod, igrp,jmod, igrp,z,phix,phix,dpstat,zr(ig))+ mefin5(&
                                  & nbz,nbgrp,imod,igrp,jmod, igrp,z,phiy,phiy,dpstat,zr( ig)))
!
412          continue
!
!---    CONTRIBUTION DES EFFORTS DE CONTRAINTES SUR LES GRILLES
!           ---> TERMES D'AMORTISSEMENT ET DE RAIDEUR AJOUTES
!
            if (ntypg .ne. 0) then
!
                do 36 kg = 1, nbgtot
!
                    do 34 jgrp = 1, nbgrp
                        do 341 igrp = 1, nbgrp
!
                            ncyl = 0
                            if (igrp .eq. jgrp) then
                                do 35 i = 1, nbcyl
                                    if (numgrp(i) .eq. igrp) ncyl = ncyl-1
35                              continue
                            endif
!
                            ngz1 = (igrp-1)*nbgtot+kg
                            ngz2 = (jgrp-1)*nbgtot+kg
!
!---   AMORTISSEMENT AJOUTE
!
                            do 46 k = 1, ntypg
                                if (itypg(kg) .eq. k) then
                                    mata(imod,jmod) = mata(imod,jmod) - 0.5d0 * rhog(kg) * abs(vi&
                                                      &tg(kg)) * zr(iaireg+k-1) * cpg(k) * ( ( dc&
                                                      &ent(ivnxx+nbcyl*(jgrp-1)+ igrp) + dble(ncy&
                                                      &l) ) * zr(iphixg+( imod-1)*nbgtot*nbgrp+ng&
                                                      &z1-1) * zr(iphixg+(jmod-1)*nbgtot*nbgrp+ n&
                                                      &gz2-1) + dcent(ivnxy+nbcyl*(jgrp- 1)+igrp)&
                                                      & * zr(iphixg+(imod-1)* nbgtot*nbgrp+ngz1-1&
                                                      &) * zr(iphiyg+( jmod-1)*nbgtot*nbgrp+ngz2-&
                                                      &1) + dcent(ivnyx+nbcyl*(jgrp-1)+igrp) * zr&
                                                      &(iphiyg+(imod-1)*nbgtot*nbgrp+ ngz1-1) * z&
                                                      &r(iphixg+(jmod-1)* nbgtot*nbgrp+ngz2-1) + &
                                                      &( dcent( ivnyy+nbcyl*(jgrp-1)+igrp) + dble&
                                                      &( ncyl) ) * zr(iphiyg+(imod-1)* nbgtot*nbg&
                                                      &rp+ngz1-1) * zr(iphiyg+( jmod-1)*nbgtot*nb&
                                                      &grp+ngz2-1) )
!
                                    mata(imod,jmod) = mata(imod,jmod) - 0.5d0 * rhog(kg) * abs(vi&
                                                      &tg(kg)) * zr(iaireg+k-1) * cdg(k) * ( ( dc&
                                                      &ent(ivnxx+nbcyl*(jgrp-1)+ igrp) + dble(ncy&
                                                      &l) ) * zr(iphixg+( imod-1)*nbgtot*nbgrp+ng&
                                                      &z1-1) * zr(iphixg+(jmod-1)*nbgtot*nbgrp+ n&
                                                      &gz2-1) + dcent(ivnxy+nbcyl*(jgrp- 1)+igrp)&
                                                      & * zr(iphixg+(imod-1)* nbgtot*nbgrp+ngz1-1&
                                                      &) * zr(iphiyg+( jmod-1)*nbgtot*nbgrp+ngz2-&
                                                      &1) + dcent(ivnyx+nbcyl*(jgrp-1)+igrp) * zr&
                                                      &(iphiyg+(imod-1)*nbgtot*nbgrp+ ngz1-1) * z&
                                                      &r(iphixg+(jmod-1)* nbgtot*nbgrp+ngz2-1) + &
                                                      &( dcent( ivnyy+nbcyl*(jgrp-1)+igrp) + dble&
                                                      &( ncyl) ) * zr(iphiyg+(imod-1)* nbgtot*nbg&
                                                      &rp+ngz1-1) * zr(iphiyg+( jmod-1)*nbgtot*nb&
                                                      &grp+ngz2-1) )
                                endif
46                          continue
!
! ---  RAIDEUR AJOUTEE
!
                            do 47 k = 1, ntypg
                                if (itypg(kg) .eq. k) then
                                    matr(imod,jmod) = matr(imod,jmod) - 0.5d0 * rhog(kg) * abs(vi&
                                                      &tg(kg)) * zr(iaireg+k-1) * cdg(k) * vitg( &
                                                      &kg) * ( dcent(ivnxx+nbcyl*(jgrp-1) +igrp) &
                                                      &* zr(iphixg+(imod-1)*nbgrp* nbgtot+ngz1-1)&
                                                      & * zr(idphxg+( jmod-1)*nbgrp*nbgtot+ngz2-1&
                                                      &) + dcent(ivnxy+nbcyl*(jgrp-1)+igrp) * zr(&
                                                      &iphixg+(imod-1)*nbgrp*nbgtot+ ngz1-1) * zr&
                                                      &(idphyg+(jmod-1)* nbgrp*nbgtot+ngz2-1) + d&
                                                      &cent( ivnyx+nbcyl*(jgrp-1)+igrp) * zr(iphi&
                                                      &yg+(imod-1)*nbgrp*nbgtot+ ngz1-1) * zr(idp&
                                                      &hxg+(jmod-1)* nbgrp*nbgtot+ngz2-1) + dcent&
                                                      &( ivnyy+nbcyl*(jgrp-1)+igrp) * zr(iphiyg+(&
                                                      &imod-1)*nbgrp*nbgtot+ ngz1-1) * zr(idphyg+&
                                                      &(jmod-1)* nbgrp*nbgtot+ngz2-1) )
!
                                    matr(imod,jmod) = matr(imod,jmod) - 0.5d0 * rhog(kg) * abs(vi&
                                                      &tg(kg)) * zr(iaireg+k-1) * cpg(k) * vitg( &
                                                      &kg) * ( ( dcent(ivnxx+nbcyl*(jgrp- 1)+igrp&
                                                      &) + dble(ncyl) ) * zr( iphixg+(imod-1)*nbg&
                                                      &rp*nbgtot+ngz1- 1) * zr(idphxg+(jmod-1)*nb&
                                                      &grp* nbgtot+ngz2-1) + dcent(ivnxy+ nbcyl*(&
                                                      &jgrp-1)+igrp) * zr(iphixg+( imod-1)*nbgrp*&
                                                      &nbgtot+ngz1-1) * zr(idphyg+(jmod-1)*nbgrp*&
                                                      &nbgtot+ ngz2-1) + dcent(ivnyx+nbcyl*(jgrp-&
                                                      & 1)+igrp) * zr(iphiyg+(imod-1)* nbgrp*nbgt&
                                                      &ot+ngz1-1) * zr(idphxg+( jmod-1)*nbgrp*nbg&
                                                      &tot+ngz2-1) + ( dcent(ivnyy+nbcyl*(jgrp-1)&
                                                      &+igrp) + dble(ncyl) ) * zr(iphiyg+( imod-1&
                                                      &)*nbgrp*nbgtot+ngz1-1) * zr(idphyg+(jmod-1&
                                                      &)*nbgrp*nbgtot+ ngz2-1) )
!
                                endif
47                          continue
!
341                      continue
34                  continue
36              continue
!
            endif
!
! ---    TERMES DE MASSE, AMORTISSEMENT ET RAIDEUR DE STRUCTURE
!
            if (imod .eq. jmod) then
                matm(imod,jmod) = matm(imod,jmod)+matma(imod)
                mata(imod,jmod) = mata(imod,jmod)+matma(imataa+imod)
                matr(imod,jmod) = matr(imod,jmod)+matma(imatra+imod)
            endif
!
41      end do
 4  end do
!
! --- MENAGE
!
    call jedetr('&&MEFMAT.TMP.GH')
    call jedetr('&&MEFMAT.AIREG')
    call jedetr('&&MEFMAT.PHIXG')
    call jedetr('&&MEFMAT.PHIYG')
    call jedetr('&&MEFMAT.DPHIXG')
    call jedetr('&&MEFMAT.DPHIYG')
!
    call jedema()
end subroutine
