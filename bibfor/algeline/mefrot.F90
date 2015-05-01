subroutine mefrot(ndim, som, vit0, promas, provis,&
                  z, ru, rint, re, cp,&
                  cf, dh, vit, rho, visc,&
                  itypg, zg, tg, dg, rugg,&
                  axg, xig, afluid, pm, cfg,&
                  vitg, rhog, viscg)
! aslint: disable=W1504
    implicit   none
!
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterfort/fointe.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    integer :: ndim(14)
    real(kind=8) :: som(9), vit0, z(*), ru, rint(*), re(*), cp(*), cf(*), dh
    real(kind=8) :: vit(0:*), rho(0:*), visc(*)
    character(len=8) :: promas, provis
!
    integer :: itypg(*)
    real(kind=8) :: zg(*), tg(*), dg(*), rugg(*), axg(*), xig(*)
    real(kind=8) :: afluid, pm
    real(kind=8) :: cfg(*), vitg(*), rhog(*), viscg(*)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     CALCUL DU DIAMETRE HYDRAULIQUE, ET DES NOMBRES DE REYNOLDS
!     OPERATEUR APPELANT : OP0144 , FLUST3, MEFIST
! ----------------------------------------------------------------------
!     OPTION DE CALCUL   : CALC_FLUI_STRU , CALCUL DES PARAMETRES DE
!     COUPLAGE FLUIDE-STRUCTURE POUR UNE CONFIGURATION DE TYPE "FAISCEAU
!     DE TUBES SOUS ECOULEMENT AXIAL"
! ----------------------------------------------------------------------
! IN  : NDIM   : TABLEAU DES DIMENSIONS
! IN  : SOM    : COORDONNEES DES SOMMETS DE L'ENCEINTE RECTANGULAIRE
!                OU XEXT,YEXT,REXT
! IN  : VIT0   : VITESSE MOYENNE D ECOULEMENT DU FLUIDE
! IN  : PROMAS : PROFIL DE MASSE VOLUMIQUE DU FLUIDE, DE TYPE FONCTION
! IN  : PROVIS : PROFIL DE VISCOSITE DU FLUIDE, DE TYPE FONCTION
! IN  : Z      : COORDONNEES 'Z'  DES DES POINTS DE DISCRETISATION DANS
!                LE REPERE AXIAL
! IN  : RU     : RUGOSITE DES CYLINDRES
! IN  : RINT   : RAYONS DES CYLINDRES
! OUT : RE     : NOMBRE DE REYNOLDS AUX POINTS DE DISCRETISATION
! OUT : CP     : COEFFICIENT DE PORTANCE CP DU FLUIDE AUTOUR D UN
!                CYLINDRE INCLINE, AUX POINTS DE DISCRETISATION
! OUT : CF     : COEFFICIENT DE TRAINEE VISQUEUSE DU FLUIDE LE LONG DES
!                PAROIS, AUX POINTS DE DISCRETISATION
! OUT : DH     : DIAMETRE HYDRAULIQUE
! OUT : VIT    : VITESSE D ECOULEMENT DU FLUIDE AUX POINTS DE
!                DISCRETISATION
! OUT : RHO    : MASSE VOLUMIQUE DU FLUIDE AUX POINTS DE DISCRETISATION
! IN  : VISC   : VISCOSITE DU FLUIDE AUX POINTS DE DISCRETISATION
!
! IN  : ITYPG  : VECTEUR DES TYPES DE GRILLES (2=EXTREMITE,1=MELANGE)
! IN  : ZG     : COORDONNEES 'Z' DES POSITIONS DES GRILLES DANS LE
!                REPERE AXIAL
! IN  : TG     : VECTEUR DES EPAISSEUR DES PLAQUETTES
! IN  : DG     : VECTEUR DES LONGUEURS DE PLAQUETTES
! IN  : RUGG   : VECTEUR DES RUGOSITES DES TYPES DE GRILLES
! OUT : AXG    : VECTEUR DES SECTIONS SOLIDE DES TYPES DE GRILLES
! OUT : XIG    : VECTEUR DES PERIMETRES MOUILLES DES TYPES DE GRILLES
! OUT : AFLUID : SECTION FLUIDE DE L'ECOULEMENT EN L'ABSENCE DE GRILLES
! OUT : PM     : PERIMETRE MOUILLE DE L'ECOULEMENT EN L'ABSENCE
!                DE GRILLES
! OUT : CFG    : VECTEUR DES COEEF DE FROTTEMENT DES TYPES DE GRILLES
! OUT : VITG   : VITESSE D'ECOULEMENT DU  FLUIDE AUX POINTS DE
!               POSITIONNEMENT DES GRILLES
! OUT : RHOG   : MASSE VOLUMIQUE DU FLUIDE AUX MEMES POINTS
! OUT : VISCG  : VISCOSITE DU FLUIDE AUX MEMES POINTS
! ----------------------------------------------------------------------
    integer :: i, j, k, idhg, iencei, ier, ireg, nbcyl, nbgtot, nbz, ndir, ntypg
    real(kind=8) :: ecart, xsom(4), ysom(4), pi, rext, rho0
    character(len=8) :: nompar
    real(kind=8) :: a, b, relim1, relim2, nbplaq
    real(kind=8) :: x12, y12, long12, x23, y23, long23
! ----------------------------------------------------------------------
    call jemarq()
!
! --- LECTURE DES DIMENSIONS
    nbz = ndim(1)
    nbcyl = ndim(3)
    iencei = ndim(6)
    ndir = ndim(10)
    ntypg = ndim(13)
    nbgtot = ndim(14)
!
! --- CREATION DES OBJETS DE TRAVAIL
    if (ntypg .ne. 0) then
        call wkvect('&&MEFROT.TEMP.DHG', 'V V R', ntypg, idhg)
        call wkvect('&&MEFROT.TEMP.REG', 'V V R', nbgtot, ireg)
    endif
!
    pi = r8pi()
!
    if (iencei .eq. 1) rext = som(3)
!
! --- PARAMETRE POUR L APPEL DES FONCTIONS, PROFIL DE MASSE VOLUMIQUE ET
! --- PROFIL DE VISCOSITE
    if (ndir .eq. 1) then
        nompar = 'X'
    else if (ndir.eq.2) then
        nompar = 'Y'
    else if (ndir.eq.3) then
        nompar = 'Z'
    endif
!
! --- PROFIL DE MASSE VOLUMIQUE ET DE VISCOSITE AUX POINTS DE
! --- DISCRETISATION
    do 10 i = 1, nbz
        call fointe('F ', promas, 1, nompar, z(i),&
                    rho(i), ier)
        call fointe('F ', provis, 1, nompar, z(i),&
                    visc(i), ier)
10  end do
!
! --- CALCUL DE LA MASSE VOLUMIQUE DE REFERENCE
    rho0 = rho(1)
    rho(0) = rho0
!
! --- PROFIL DE VITESSE AUX POINTS DE DISCRETISATION
!
    do 30 i = 1, nbz
        vit(i) = rho0 * vit0 / rho(i)
30  end do
    vit(0) = vit0
!
!
! --- CALCUL DU DIAMETRE HYDRAULIQUE DH
! --- DU SYSTEME CYLINDRES+ENCEINTE
!
    a = 0.d0
    b = 0.d0
!
    do 40 i = 1, nbcyl
        a = a+rint(i)*rint(i)
        b = b+rint(i)
40  end do
!
! --- ENCEINTE CIRCULAIRE
    if (iencei .eq. 1) then
!
        rext = som(3)
        dh = 2.d0*(rext*rext-a)/(rext+b)
!
        if (ntypg .ne. 0) then
            afluid = pi*rext*rext-pi*a
            pm = 2.d0*(pi*rext+pi*b)
        endif
!
! --- ENCEINTE RECTANGULAIRE
    else if (iencei.eq.2) then
!
        do 50 i = 1, 4
            xsom(i) = som(2*i-1)
            ysom(i) = som(2*i)
50      continue
!
        x12 = xsom(2)-xsom(1)
        y12 = ysom(2)-ysom(1)
        long12 = (x12*x12+y12*y12)
        long12 = sqrt(long12)
!
        x23 = xsom(3)-xsom(2)
        y23 = ysom(3)-ysom(2)
        long23 = (x23*x23+y23*y23)
        long23 = sqrt(long23)
!
        dh = 2.d0*(long12*long23-pi*a)/ (long12+long23+pi*b)
!
        if (ntypg .ne. 0) then
            afluid = long12*long23-pi*a
            pm = 2.d0*(long12+long23+pi*b)
        endif
!
    else if (iencei.eq.0) then
        dh = 100.d0
    endif
!
    do 60 i = 1, nbz
!
! --- CALCUL DU NOMBRE DE REYNOLDS BASE SUR LE DIAMETRE
! --- HYDRAULIQUE DH ET LA VITESSE AXIALE DU FLUIDE
!
        re(i) = dh*abs(vit(i))/visc(i)
!
! ---    NOMBRES DE REYNOLDS DE TRANSITION
!
        relim1 = 23.d0*dh/ru
        relim2 = 560.d0*dh/ru
!
        if (vit(i) .ne. 0.d0) then
!
! ---       CALCUL DU COEFFICIENT DE PORTANCE CP
! ---       DU FLUIDE AUTOUR D UN CYLINDRE INCLINE
! ---       --------------------------------------
            cp(i) = 0.080d0
!
! ---       CALCUL DU COEFFICIENT DE TRAINEE VISQUEUSE
! ---       CF DU FLUIDE LE LONG DES PAROIS
! ---       ------------------------------------------
!
! ---       REGIME LAMINAIRE
!
            if (re(i) .lt. 2000.d0) then
                cf(i) = 16.d0*pi/re(i)
            endif
!
! ---       REGIME CRITIQUE
!
            if ((re(i).ge.2000.d0) .and. (re(i).le.4000.d0)) then
                cf(i) = 1.2d-04*pi*(re(i)**0.55d0)
            endif
!
! ---       REGIME HYDRAULIQUEMENT LISSE
!
            if ((re(i).gt.4000.d0) .and. (re(i).lt.relim1)) then
                cf(i) = 1.d0*pi/4.d0/( 1.8d0*log10(re(i))-1.64d0) /(1.8d0*log10(re(i))-1.64d0)
            endif
!
! ---       TRANSITION TURBULENTE
!
            if ((re(i).ge.relim1) .and. (re(i).le.relim2)) then
                cf(i) = 0.1d0*pi*((1.46d0*ru/dh+ 100.d0/re(i))** 0.25d0)/4.d0
            endif
!
! ---       REGIME QUADRATIQUE TURBULENT
!
            if (re(i) .gt. relim2) then
                cf(i) = pi/(2.d0*log10(3.7d0*dh/ru)) /(2.d0*log10( 3.7d0*dh/ru) ) /4.d0
            endif
!
!
        else
            cf(i) = 0.d0
            cp(i) = 0.d0
        endif
60  end do
!
!
!     ----------------------------------------------
!     CALCUL DU PROFIL DE MASSE VOLUMIQUE AU NIVEAU
!     DES GRILLES PAR INTERPOLATION LINEAIRE
!     ----------------------------------------------
!
    if (ntypg .ne. 0) then
!
        do 18 i = 2, nbz
            do 19 j = 1, nbgtot
                ecart=(z(i)-zg(j))*(z(i-1)-zg(j))
!
                if (ecart .le. 0.d0) then
                    rhog(j)=( rho(i-1)*(z(i)-zg(j))+ rho(i)*(zg(j)-z(&
                    i-1)) )/ (z(i)-z(i-1))
!
!    ----------------------------------------
!      CALCUL DU PROFIL DE VISCOSITE CINEMATIQUE AU NIVEAU
!     DES GRILLES PAR INTERPOLATION LINEAIRE
!     ---------------------------------------
!
                    viscg(j)=( visc(i-1)*(z(i)-zg(j))+ visc(i)*(zg(j)-&
                    z(i-1)) )/ (z(i)-z(i-1))
                endif
19          continue
18      continue
!
!     ---------------------------------------------------------
!     CALCUL DU PROFIL AXIAL DE VITESSE AU NIVEAU DES GRILLES
!     (CONSERVATION DU DEBIT MASSIQUE)
!     ---------------------------------------------------------
!
        do 15 j = 1, nbgtot
            vitg(j)=0.d0
15      continue
!
        nbplaq=2*(sqrt(dble(nbcyl))+1)
        do 85 k = 1, ntypg
            axg(k)=nbplaq*tg(k)*dg(k)-(0.5d0*nbplaq*tg(k))* (0.5d0*&
            nbplaq*tg(k))
            xig(k)=4*dg(k)+sqrt(dble(nbcyl))*4* (dg(k)-0.5d0*nbplaq*&
            tg(k))
85      continue
!
        do 11 j = 1, nbgtot
            do 84 k = 1, ntypg
                if (itypg(j) .eq. k) then
                    vitg(j)=1.d0/(1.d0-(axg(k)/afluid))* (1.d0/rhog(j)&
                    )*rho0*vit0
                endif
84          continue
11      continue
!
!     ------------------------------------------------------------------
!     CALCUL DU PROFIL DU NOMBRE DE REYNOLDS STATIONNAIRE
!     AU NIVEAU DES GRILLES
!     ------------------------------------------------------------------
!
        do 86 k = 1, ntypg
            zr(idhg+k-1)=4.d0*(afluid-axg(k))/(pm+xig(k))
86      continue
!
        do 81 i = 1, nbz
            do 82 j = 1, nbgtot
                ecart=(z(i)-zg(j))*(z(i-1)-zg(j))
!
                if (ecart .le. 0.d0) then
                    do 87 k = 1, ntypg
                        if (itypg(j) .eq. k) then
                            zr(ireg+j-1)=zr(idhg+k-1)*abs(vitg(j))/&
                            viscg(j)
                        endif
87                  continue
                endif
82          continue
81      continue
!
!     ----------------------------------------------------------------
!     CALCUL DU PROFIL DU COEFFICIENT DE FROTTEMENT CFG
!     AU NIVEAU DES GRILLES
!     ----------------------------------------------------------------
!
        do 13 j = 1, nbgtot
            cfg(j)=0.d0
13      continue
!
        do 14 j = 1, nbgtot
!
            if (vitg(j) .ne. 0.d0) then
!
!              ----------------
!              REGIME LAMINAIRE
!              ----------------
                if (zr(ireg+j-1) .lt. 2000.d0) then
                    cfg(j)=16.d0*pi/zr(ireg+j-1)
                endif
!
!              ---------------
!              REGIME CRITIQUE
!              ---------------
                if ((zr(ireg+j-1).ge.2000.d0) .and. (zr(ireg+j-1) .le.4000.d0)) then
                    cfg(j)=1.2d-04*pi*(zr(ireg+j-1)**0.55d0)
                endif
!
!     REDEFINITION DES COEFFICIENTS DE TRANSITION DANS LE CAS
!     DES GRILLES
!     ------------------------------------------------------------------
!
                do 89 k = 1, ntypg
                    if (itypg(j) .eq. k) then
                        relim1=23.d0*zr(idhg+k-1)/rugg(k)
                        relim2=560.d0*zr(idhg+k-1)/rugg(k)
                    endif
89              continue
!
!              ----------------------------
!              REGIME HYDRAULIQUEMENT LISSE
!              ----------------------------
                if ((zr(ireg+j-1).gt.4000.d0) .and. (zr(ireg+j-1) .lt.relim1)) then
                    cfg(j)=1.d0*pi/4.d0/(1.8d0*log10(zr(ireg+j-1))-&
                    1.64d0) /(1.8d0*log10(zr(ireg+j-1))-1.64d0)
                endif
!
!              ---------------------
!              TRANSITION TURBULENTE
!              ---------------------
                if ((zr(ireg+j-1).ge.relim1) .and. (zr(ireg+j-1) .le.relim2)) then
                    k = itypg(j)
                    cfg(j)=0.1d0*pi*((1.46d0*rugg(k)/zr(idhg+k-1)+&
                    100.d0/zr(ireg+j-1))**0.25d0)/4.d0
                endif
!
!              -----------------------------
!              REGIME QUADRATIQUE TURBULENT
!              -----------------------------
                if (zr(ireg+j-1) .gt. relim2) then
                    k = itypg(j)
                    cfg(j)=pi/(2.d0*log10(3.7d0*zr(idhg+k-1)/rugg(k)))&
                    /(2.d0*log10(3.7d0*zr(idhg+k-1)/rugg(k))) /4.d0
                endif
!
            else
                cfg(j)=0.d0
            endif
!
14      continue
!
    endif
!
    if (ntypg .ne. 0) then
        call jedetr('&&MEFROT.TEMP.DHG')
        call jedetr('&&MEFROT.TEMP.REG')
    endif
    call jedema()
end subroutine
