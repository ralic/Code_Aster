subroutine mefpre(ndim, alpha, z, cf, dh,&
                  vit, rho, pstat, dpstat, dvit,&
                  itypg, zg, hg, axg, pm,&
                  xig, afluid, cdg, cfg, vitg,&
                  rhog)
    implicit   none
!
    include 'jeveux.h'
    include 'asterc/r8pi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/wkvect.h'
    integer :: ndim(14)
    real(kind=8) :: alpha, z(*), cf(*), dh, vit(*), rho(*), pstat(*)
    real(kind=8) :: dpstat(*), dvit(*)
!
    integer :: itypg(*)
    real(kind=8) :: zg(*), hg(*), axg(*), xig(*), afluid, pm
    real(kind=8) :: cdg(*), cfg(*), vitg(*), rhog(*)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! TOLE CRP_21
! ----------------------------------------------------------------------
!     CALCUL DE LA PRESSION ET DU GRADIENT DE PRESSION STATIONNAIRE
!     OPERATEUR APPELANT : OP0144 , FLUST3, MEFIST
! ----------------------------------------------------------------------
!     OPTION DE CALCUL   : CALC_FLUI_STRU , CALCUL DES PARAMETRES DE
!     COUPLAGE FLUIDE-STRUCTURE POUR UNE CONFIGURATION DE TYPE "FAISCEAU
!     DE TUBES SOUS ECOULEMENT AXIAL"
! ----------------------------------------------------------------------
! IN  : NDIM   : TABLEAU DES DIMENSIONS
! IN  : ALPHA  : COEFFICIENT DE PROPORTIONALITE DE LA PESENTEUR PAR
!                RAPPORT A LA VALEUR STANDARD (9.81). LA PROJECTION DU
!                VECTEUR V SUIVANT Z VAUT 9.81*ALPHA.
! IN  : Z      : COORDONNEES 'Z'  DES DES POINTS DE DISCRETISATION DANS
!                LE REPERE AXIAL
! IN  : CF     : COEFFICIENT DE TRAINEE VISQUEUSE DU FLUIDE LE LONG DES
!                PAROIS, AUX POINTS DE DISCRETISATION
! IN  : DH     : DIAMETRE HYDRAULIQUE
! IN  : VIT    : VITESSE D ECOULEMENT DU FLUIDE AUX POINTS DE
!                DISCRETISATION
! IN  : RHO    : MASSE VOLUMIQUE DU FLUIDE AUX POINTS DE DISCRETISATION
! OUT : PSTAT  : PROFIL DE PRESSION STATIONNAIRE
! OUT : DPSTAT : PROFIL DE GRADIENT DE PRESSION STATIONNAIRE
! --  : DVIT   : TABLEAU DE TRAVAIL, GRADIENT DE VITESSE D ECOULEMENT DU
!                FLUIDE
!
! IN  : ITYPG  : VECTEUR DES TYPES DE GRILLES
! IN  : ZG     : COORDONNEES 'Z' DES POSITIONS DES GRILLES DANS LE
!                 REPERE AXIAL
! IN  : HG     :  VECTEUR DES HAUTEURS DE GRILLE
! IN  : AXG    : VECTEUR DES SECTIONS SOLIDE DES TYPES DE GRILLES
! IN  : XIG    : VECTEUR DES PERIMETRES MOUILLES DES TYPES DE GRILLES
! IN  : AFLUID: SECTION FLUIDE DE L'ECOULEMENT EN L'ABSENCE DE GRILLES
! IN  : PM     : PERIMETRE MOUILLE DE L'ECOULEMENT EN L'ABSENCE
!                DE GRILLES
! IN  : CDG    : VECTEUR DES COEFF DE TRAINEE DES TYPES DE GRILLES
! IN  : CFG    : VECTEUR DES COEEF DE FROTTEMENT DES TYPES DE GRILLES
! IN  : VITG   : VITESSE D'ECOULEMENT DU  FLUIDE AUX POINTS DE
!                POSITIONNEMENT DES GRILLES
! IN  : RHOG   : MASSE VOLUMIQUE DU FLUIDE AUX MEMES POINTS
! ----------------------------------------------------------------------
    integer :: i, j, k, n, icfnew, nbz, nbgtot, ntypg, ideltp
    real(kind=8) :: ecart, g, pi
! ----------------------------------------------------------------------
    call jemarq()
!
! --- LECTURE DES DIMENSIONS
    nbz = ndim(1)
    ntypg = ndim(13)
    nbgtot = ndim(14)
!
! --- CREATION DES OBJETS DE TRAVAIL
    if (ntypg .ne. 0) then
        call wkvect('&&MEFPRE.DELTAP', 'V V R', nbgtot, ideltp)
        call wkvect('&&MEFPRE.CFNEW', 'V V R', nbgtot, icfnew)
    endif
!
    pi = r8pi()
!
! --- ACCELERATION DE LA PESANTEUR
    g = 9.81d0*alpha
!
! --- VITESSE MOYENNE D ECOULEMENT ET MASSE VOLUMIQUE MOYENNE
!
!
! --- CALCUL DE VIT'(Z) -> G(Z)
! --- MINIMISATION QUADRATIQUE DES RESTES DES
! --- DEVELOPPEMENTS DE TAYLOR DE VIT(Z)
! --- A GAUCHE ET A DROITE
!
    dvit(1) = ( vit(2)-vit(1) ) / (z(2)-z(1))
!
    do 10 n = 2, nbz-1
        dvit(n) = (&
                  (&
                  vit(n+1)-vit(n))*(z(n+1)-z(n)) +(vit(n-1)-vit(n)) *(z(n-1)-z(n)) ) /( (z(n+1)-z&
                  &(n))*(z(n+1)-z(n)) +(z(n-1)-z(n)) *(z(n-1)-z(n)&
                  )&
                  )
10  end do
!
    dvit(nbz) = ( vit(nbz)-vit(nbz-1) ) / (z(nbz)-z(nbz-1))
!
! --- CALCUL DU PROFIL DE GRADIENT DE PRESSION STATIONNAIRE
!
    do 20 n = 1, nbz
        dpstat(n) = -rho(n)*vit(n)*dvit(n) + rho(n)*g -2.d0*rho(n)*cf( n)*abs(vit(n))*vit(n)/pi/d&
                    &h
20  end do
!
! --- CALCUL DU PROFIL DE PRESSION STATIONNAIRE
!
    pstat(1) = 0.d0
    do 30 n = 2, nbz
        pstat(n) = pstat(n-1)+(dpstat(n-1)+dpstat(n))* (z(n)-z(n-1))/ 2.d0
30  end do
!
!--- CALCUL DU SAUT DE PRESSION AU PASSAGE DE CHAQUE GRILLE
!
    if (ntypg .ne. 0) then
!
        do 6 j = 1, nbgtot
            zr(ideltp+j-1) = 0.d0
 6      continue
!
        do 18 i = 2, nbz
            do 19 j = 1, nbgtot
                ecart=(z(i)-zg(j))*(z(i-1)-zg(j))
!
                if (ecart .le. 0.d0) then
                    zr(icfnew+j-1)=( cf(i-1)*(z(i)-zg(j))+ cf(i)*(zg(&
                    j)-z(i-1)) ) / (z(i)-z(i-1))
                endif
19          continue
18      continue
!
        do 7 j = 1, nbgtot
            do 25 k = 1, ntypg
                if (itypg(j) .eq. k) then
                    zr(ideltp+j-1) = 0.5d0*rhog(j)*abs(vitg(j))*vitg( j)* (axg(k)*cdg(k)+xig(k)*h&
                                     &g(k)*cfg(j))/afluid + 0.5d0*rhog(j)*abs(vitg(j))*vitg(j)* (&
                                     &1.d0-(1.d0- axg(k)/afluid)**2)*pm*hg(k)*zr(icfnew+j-1)/aflu&
                                     &id
                endif
25          continue
 7      continue
!
        do 40 n = 2, nbz
            do 42 j = 1, nbgtot
                ecart = (z(n)-zg(j))*(z(n-1)-zg(j))
                if (ecart .le. 0.d0) then
                    do 44 k = n, nbz
                        pstat(k) = pstat(k)-zr(ideltp+j-1)
44                  continue
                endif
42          continue
40      continue
!
    endif
!
    if (ntypg .ne. 0) then
        call jedetr('&&MEFPRE.DELTAP')
        call jedetr('&&MEFPRE.CFNEW')
    endif
    call jedema()
end subroutine
