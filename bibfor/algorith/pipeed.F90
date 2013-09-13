subroutine pipeed(nno, npg, ipoids, ivf, idfde,&
                  geom, typmod, mate, lgpg, deplm,&
                  vim, ddepl, ddepl0, ddepl1, dfdi,&
                  dtau, copilo)
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
!
    implicit none
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/nmedpi.h"
#include "asterfort/nmgeom.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
    character(len=8) :: typmod(*)
    integer :: nno, npg
    integer :: mate
    integer :: lgpg, ipoids, ivf, idfde
    real(kind=8) :: geom(2, 4), ddepl(2, 4), deplm(2, 4)
    real(kind=8) :: vim(lgpg, npg)
    real(kind=8) :: ddepl0(2, 4), ddepl1(2, 4)
    real(kind=8) :: dtau, copilo(5, npg), dfdi(nno, 2)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (PILOTAGE)
!
! CALCUL  DES COEFFICIENTS DE PILOTAGE POUR PRED_ELAS/DEFORMATION
!
! ELEMENTS A DISCONTINUITES INTERNES
!
! ----------------------------------------------------------------------
!
!
! BUT : CALCULER LA SOLUTION DE L'EQUATION SUPLEMENTAIRE INTRODUITE POUR
!       LE PILOTAGE DE L'ELEMENT A DISONTINUITE INTERNE.
!
!       L'IDEE EST D'ADAPTER LE CHARGEMENT POUR FAIRE EVOLUER LE SAUT DE
!       L'ELEMENT A DISONTINUITE DE FACON CONTROLEE. PERMET D'EVITER
!       QUE LE SAUT EVOLUE BRUTALEMENT QUAND ON ATTEINT UNE BRANCHE
!       D'EQUILIBRE INSTABLE ET A POUR INTERET DE SUIVRE LA BRANCHE
!       INSTABLE DE LA COURBE GLOBALE U(F).
!
!       LE CHARGEMENT N'EST PLUS MONOTONE, IL DEPEND DU SAUT DE
!       L'ELEMENT.
!
! IN  TYPMOD : TYPE DE MODELISATION
! IN  MATE   : MATERIAU CODE
! IN  NNO    : NOMBRE DE NOEUDS DE L'ELEMENT
! IN  NPG    : NOMBRE DE POINTS DE GAUSS
! IN  IPOIDS : POIDS DES POINTS DE GAUSS
! IN  IVF    : VALEUR DES FONCTIONS DE FORME
! IN  IDFDE  : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  DFDI   : DERIVEE DES FONCTIONS DE FORME
! IN  LGPG   : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
!             CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
! IN  GEOM   : COORDONNEES DES NOEUDS
! IN  DEPLM  : DEPLACEMENT EN T-
! IN  DDEPL  : INCREMENT DE DEPLACEMENT A L'ITERATION NEWTON COURANTE
! IN  VIM    : VARIABLES INTERNES EN T-
! IN  DEPL0  : CORRECTION DE DEPLACEMENT POUR FORCES FIXES
! IN  DEPL1  : CORRECTION DE DEPLACEMENT POUR FORCES PILOTEES
! IN  DTAU   : PARAMETRE DE PILOTAGE
! OUT COPILO : COEFFICIENTS POUR CHAQUE POINT DE GAUSS
!
!
!
!
    integer :: nbres
    parameter (nbres=2)
!
    real(kind=8) :: valres(nbres)
    integer :: icodre(nbres)
    character(len=8) :: nomres(nbres), fami, poum
!
!
    integer :: i, j, k, kont, n, kpg, spt
    real(kind=8) :: up(8), ud(8), sm, rck, r, sigmc, gc, rac2, kappam, pkm, det
    real(kind=8) :: eta(2), poids, f(3, 3)
    real(kind=8) :: alfp(2), alfd(2), dist
    real(kind=8) :: alpha1, beta1, alpha2, beta2, gamma2, delta
    real(kind=8) :: alpha, beta, gamma
    real(kind=8) :: a0, a1, a2, a3, a4, sol(4), mat(2, 2), matinv(2, 2)
    real(kind=8) :: spg(2), sdg(2), qg(2, 2), sp(2), sd(2), q(2, 2), sigp(2)
    real(kind=8) :: sigd(2)
    real(kind=8) :: cotmp, sitmp, co, si, rot(2, 2), drot, xa, xb, ya, yb
    real(kind=8) :: def(4, 4, 2), bup(6), bud(6), d(4, 2), rtemp(4, 2)
    logical :: grand, axi
!
! ----------------------------------------------------------------------
!
!
!
! --- INITIALISATIONS
!
    axi = typmod(1) .eq. 'AXIS'
    grand = .false.
    rac2 = sqrt(2.d0)
!
! --- RECUPERATION DES PARAMETRES PHYSIQUES :
!
    nomres(1) = 'GC'
    nomres(2) = 'SIGM_C'
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    call rcvalb(fami, kpg, spt, poum, mate,&
                ' ', 'RUPT_FRAG', 0, ' ', 0.d0,&
                nbres, nomres, valres, icodre, 2)
    gc = valres(1)
    sigmc = valres(2)
    kappam = vim(3,1)
!
! INITIALISATION DES VARIABLES  :
    call r8inir(2, 0.d0, sp, 1)
    call r8inir(2, 0.d0, sd, 1)
    call r8inir(4, 0.d0, q, 1)
    call r8inir(2, 0.d0, sigp, 1)
    call r8inir(2, 0.d0, sigd, 1)
    call r8inir(2, 0.d0, alfp, 1)
    call r8inir(2, 0.d0, alfd, 1)
    call r8inir(8, 0.d0, dfdi, 1)
    call r8inir(2, 0.d0, eta, 1)
    call r8inir(4, 0.d0, sol, 1)
    call r8inir(4, 0.d0, mat, 1)
    call r8inir(4, 0.d0, matinv, 1)
!
!
    call dcopy(8, deplm, 1, up, 1)
    call daxpy(8, 1.d0, ddepl, 1, up,&
               1)
    call daxpy(8, 1.d0, ddepl0, 1, up,&
               1)
    call dcopy(8, ddepl1, 1, ud, 1)
!
! MATRICE DE CHANGEMENT DE REPERE (GLOBAL AU LOCAL) POUR L'ED :
!    ROT X = XLOC
!
! SOIT A ET B LES MILIEUX DES COTES [14] ET [23]
! t TANGENT AU COTE [AB]
!
    xa = ( geom(1,1) + geom(1,4) ) / 2
    ya = ( geom(2,1) + geom(2,4) ) / 2
    xb = ( geom(1,2) + geom(1,3) ) / 2
    yb = ( geom(2,2) + geom(2,3) ) / 2
!
    cotmp = (yb - ya)
    sitmp = (xa - xb)
!
    co = cotmp / sqrt(cotmp*cotmp + sitmp*sitmp)
    si = sitmp / sqrt(cotmp*cotmp + sitmp*sitmp)
!
    rot(1,1) = co
    rot(2,1) = -si
    rot(1,2) = si
    rot(2,2) = co
!
! ************************
! CALCUL DE SP, SD ET Q :
! ************************
!
    do 800 kpg = 1, npg
!
        call r8inir(6, 0.d0, bup, 1)
        call r8inir(6, 0.d0, bud, 1)
!
!       CALCUL DE DFDI,F,R(EN AXI) ET POIDS
        call nmgeom(2, nno, axi, grand, geom,&
                    kpg, ipoids, ivf, idfde, up,&
                    .true., poids, dfdi, f, bup,&
                    r)
!
!       CALCUL DE D (LES AUTRES TERMES SONT NULS):
        call r8inir(8, 0.d0, d, 1)
!
        d(1,1) = - (dfdi(1,1) + dfdi(2,1))
        d(4,1) = - rac2*(dfdi(1,2) + dfdi(2,2))/2
        d(2,2) = - (dfdi(1,2) + dfdi(2,2))
        d(4,2) = - rac2*(dfdi(1,1) + dfdi(2,1))/2
!
!       ON INCLU LE CHANGEMENT DE REPERE DANS D : ON REMPLACE D PAR DRt
        call r8inir(8, 0.d0, rtemp, 1)
!
        do 32 i = 1, 4
            do 33 j = 1, 2
                drot = 0.d0
                do 34 k = 1, 2
                    drot = drot + d(i,k)*rot(j,k)
34              continue
                rtemp(i,j) = drot
33          continue
32      continue
!
        do 35 i = 1, 4
            do 36 j = 1, 2
                d(i,j) = rtemp(i,j)
36          continue
35      continue
!
!       CALCUL DES PRODUITS SYMETR. DE F PAR N,
        call r8inir(32, 0.d0, def, 1)
        do 40 n = 1, nno
            do 30 i = 1, 2
                def(1,n,i) = f(i,1)*dfdi(n,1)
                def(2,n,i) = f(i,2)*dfdi(n,2)
                def(3,n,i) = 0.d0
                def(4,n,i) = (f(i,1)*dfdi(n,2) + f(i,2)*dfdi(n,1))/ rac2
30          continue
40      continue
!
!       TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1
        if (axi) then
            do 50 n = 1, nno
                def(3,n,1) = f(3,3)*zr(ivf+n+(kpg-1)*nno-1)/r
50          continue
        endif
!
!       CALCUL DE SP,SD ET Q AU POINT DE GAUSS COURANT :
        call nmedpi(spg, sdg, qg, d, npg,&
                    typmod, mate, up, ud, geom,&
                    nno, def)
!
!       CALCUL DES S ET Q POUR L'ELEMENT :
        do 64 i = 1, 2
            sp(i) = sp(i) + poids*spg(i)
            sd(i) = sd(i) + poids*sdg(i)
            do 65 j = 1, 2
                q(i,j) = q(i,j) + poids*qg(i,j)
65          continue
64      continue
!
!
800  end do
!
!
    do 66 i = 1, 2
        sigp(i) = sigp(i) + sp(i)
        do 67 j = 1, 2
            sigp(i) = sigp(i) + q(i,j)*vim(j,1)
67      continue
        sigd(i) = sd(i)
66  end do
!
!*****************************************************************
! CALCUL DES COPILO DANS LE CAS OU LE SEUIL EN SAUT EST NUL :
! ****************************************************************
! IL EXISTE DEUX CAS DE FIGURE :
!  INTERSECTION DE DEUX DEMI-DROITES AVEC LE CRITERE EN CONTRAINTE
! ****************************************************************
!
    if (kappam .eq. 0.d0) then
!
        rck = sigmc
        sm = dtau*rck + rck
!
! INTERSECTION DE LA PERMIERE DEMI-DROITE AVEC LE CRITERE EN CONTRAINTE
! ---------------------------------------------------------------------
!
! ON SUPPOSE QUE  SIGP(1) + ETA*SIGD(1) EST NEGATIF OU NUL
! RESOLUTION DE PM(ETA)  (INTERSECTION DE DROITES)
!
        kont = 0
!
        if (abs(sigd(2)) .le. 1.d-12) goto 888
!
        alpha1 = sigd(2)**2
        beta1 = sigd(2)*sigp(2)
!
        sol(1) = ( sm - sigp(2) ) / sigd(2)
        sol(2) = ( - sm - sigp(2) ) / sigd(2)
!
!       ON TEST SI POUR LES SOL TROUVEES SI ON A BIEN
!       SIGP(1) + ETA*SIGD(1)<=0
!
        if ((sigp(1) + sol(1)*sigd(1)) .le. 0.d0) then
            kont = kont + 1
            eta(kont) = sol(1)
            a0 = dtau*rck - eta(kont)*( (alpha1*eta(kont) + beta1) / sm)
            a1 = (alpha1*eta(kont) + beta1) / sm
        endif
        if ((sigp(1) + sol(2)*sigd(1)) .le. 0.d0) then
            kont = kont + 1
            eta(kont) = sol(2)
            if (kont .eq. 1) then
                a0 = dtau*rck - eta(kont)*( (alpha1*eta(kont) + beta1) / sm)
                a1 = (alpha1*eta(kont) + beta1) / sm
            else
                a2 = dtau*rck - eta(kont)*( (alpha1*eta(kont) + beta1) / sm)
                a3 = (alpha1*eta(kont) + beta1) / sm
                a4 = r8vide()
                goto 999
            endif
        endif
!
888      continue
!
! INTERSECTION DE LA DEUXIEME DEMI-DROITE AVEC LE CRITERE EN CONTRAINTE
! ---------------------------------------------------------------------
!
! RESOLUTION DE PP(ETA)  =  ALPHA2*ETA*ETA  +  2*BETA2*ETA  +  GAMMA2
! (INETRSECTION DROITE / DEMI-CERCLE)
!
        alpha2 = sigd(1)**2 + sigd(2)**2
        beta2 = sigd(1)*sigp(1) + sigd(2)*sigp(2)
        gamma2 = sigp(1)**2 + sigp(2)**2 - sm**2
        delta = beta2**2 - alpha2*gamma2
!
        if (delta .ge. 0.d0) then
!
            sol(3) = ( - beta2 + sqrt( delta ) ) / (alpha2)
            sol(4) = ( - beta2 - sqrt( delta ) ) / (alpha2)
!
! ON TEST SI POUR LES SOL TROUVEES SI ON A BIEN SIGP(1) + ETA*SIGD(1)>0
!
            if ((sigp(1) + sol(3)*sigd(1)) .gt. 0.d0) then
                kont = kont + 1
                eta(kont) = sol(3)
                if (kont .eq. 1) then
                    a0 = dtau*rck - eta(kont)*( (alpha2*eta(kont)+ beta2) / sm)
                    a1 = (alpha2*eta(kont) + beta2) / sm
                else
                    a2 = dtau*rck - eta(kont)*( (alpha2*eta(kont)+ beta2) / sm)
                    a3 = (alpha2*eta(kont) + beta2) / sm
                    a4 = r8vide()
                endif
            endif
!
            if ((sigp(1) + sol(4)*sigd(1)) .gt. 0.d0) then
                kont = kont + 1
                eta(kont) = sol(4)
                if (kont .eq. 1) then
                    a0 = dtau*rck - eta(kont)*( (alpha2*eta(kont)+ beta2) / sm)
                    a1 = (alpha2*eta(kont) + beta2) / sm
                else
                    a2 = dtau*rck - eta(kont)*( (alpha2*eta(kont)+ beta2) / sm)
                    a3 = (alpha2*eta(kont) + beta2) / sm
                    a4 = r8vide()
                endif
            endif
!
        else
!
            if (kont .eq. 0) then
                a4 = - beta2/alpha2
                a0 = sqrt(alpha2*a4*a4 + 2*beta2*a4 + gamma2 + sm**2) - rck
                a1 = r8vide()
                a2 = r8vide()
                a3 = r8vide()
                goto 999
            endif
        endif
!
        if (kont .eq. 1) then
            eta(2) = eta(1)
            a2 = a0
            a3 = a1
            a4 = r8vide()
        endif
!
        if (kont .ge. 3) then
            call utmess('F', 'PILOTAGE_85')
        endif
!
!****************************************************************
! CALCUL DES COPILO DANS LE CAS OU LE SEUIL EN SAUT EST NON NUL :
! ***************************************************************
! IL EXISTE DEUX CAS DE FIGURE :
!       -  INTERSECTION DROITE / ARC DE CERCLE
!       -  INTERSECTION DROITE / SEGMENT
! ***************************************************************
!
    else
!
        kont = 0
        rck = sigmc*exp(-sigmc*kappam/gc)
        pkm = rck / kappam
        sm = dtau*kappam + kappam
!
        mat(1,1) = pkm - q(1,1)
        mat(2,2) = pkm - q(2,2)
        mat(1,2) = - q(1,2)
        mat(2,1) = - q(2,1)
        det = mat(1,1)*mat(2,2) - mat(1,2)**2
!
        if (abs(det) .le. 1.d-12) then
            call utmess('F', 'PILOTAGE_86')
        endif
!
        matinv(1,1) = mat(2,2)/det
        matinv(2,2) = mat(1,1)/det
        matinv(1,2) = -mat(1,2)/det
        matinv(2,1) = -mat(2,1)/det
!
        do 68 i = 1, 2
            do 69 j = 1, 2
                alfp(i) = alfp(i) + matinv(i,j)*sp(j)
                alfd(i) = alfd(i) + matinv(i,j)*sd(j)
69          continue
68      continue
!
! -------------------------------
! INTERSECTION DROITE / SEGMENT :
! -------------------------------
!
!       CAS OU PAS D'INTERSECTION
        if (abs(alfd(1)) .le. 1.d-12) goto 777
        sol(3) = ( - alfp(1) - kappam*dtau ) / alfd(1)
        dist = kappam * sqrt(1.d0 + 2.d0*dtau)
        if (abs(alfp(2) + sol(3)*alfd(2)) .lt. dist) then
            kont = kont + 1
            eta(kont) = sol(3)
            a0 = kappam*dtau + eta(kont)*alfd(1)
            a1 = - alfd(1)
        endif
!
777      continue
!
! -------------------------------------
! INTERSECTION DROITE / ARC DE CERCLE :
! -------------------------------------
!
        alpha = alfd(1)**2 + alfd(2)**2
        beta = alfd(1)*alfp(1) + alfd(2)*alfp(2)
        gamma = alfp(1)**2 + alfp(2)**2 - sm**2
        delta = beta**2 - alpha*gamma
!
        if (delta .ge. 0.d0) then
!       --------------------------
            sol(1) = ( - beta + sqrt( delta ) ) / (alpha)
            sol(2) = ( - beta - sqrt( delta ) ) / (alpha)
!
            if ((alfp(1) + sol(1)*alfd(1) ) .gt. -kappam*dtau) then
                kont = kont + 1
                eta(kont) = sol(1)
                if (kont .eq. 1) then
                    a0 = dtau*kappam - eta(kont)*( (alpha*eta(kont)+ beta) /sm)
                    a1 = (alpha*eta(kont) + beta) / sm
                else
                    a2 = dtau*kappam - eta(kont)*( (alpha*eta(kont)+ beta) /sm)
                    a3 = (alpha*eta(kont) + beta) / sm
                    a4 = r8vide()
                endif
            endif
!
            if ((alfp(1) + sol(2)*alfd(1) ) .gt. -kappam*dtau) then
                kont = kont + 1
                eta(kont) = sol(2)
                if (kont .eq. 1) then
                    a0 = dtau*kappam - eta(kont)*( (alpha*eta(kont)+ beta) /sm)
                    a1 = (alpha*eta(kont) + beta) / sm
                else
                    a2 = dtau*kappam - eta(kont)*( (alpha*eta(kont)+ beta) /sm)
                    a3 = (alpha*eta(kont) + beta) / sm
                    a4 = r8vide()
                endif
            endif
!
!     CAS OU LA DROITE EST TANG AU CERCLE MAIS A GAUCHE DE L'AXE  X=DIST
            if (kont .eq. 0) then
                a4 = - beta/alpha
                a0 = sqrt( alpha*a4*a4 + 2*beta*a4 + gamma + sm**2 )- kappam
                a1 = r8vide()
                a2 = r8vide()
                a3 = r8vide()
            endif
        else
!       -----
            if (kont .ne. 0) then
                call utmess('F', 'PILOTAGE_87')
            endif
!
            a4 = - beta/alpha
            a0 = sqrt( alpha*a4*a4 + 2*beta*a4 + gamma + sm**2 ) - kappam
            a1 = r8vide()
            a2 = r8vide()
            a3 = r8vide()
!
        endif
!       -----
!
        if (kont .ge. 3) then
            call utmess('F', 'PILOTAGE_85')
        endif
!
    endif
!
999  continue
!
    if (kappam .eq. 0.d0) then
        if (kont .ne. 0) then
            copilo(1,1) = a0/rck
            copilo(2,1) = a1/rck
            copilo(3,1) = a2/rck
            copilo(4,1) = a3/rck
            copilo(5,1) = a4
        else
            copilo(1,1) = a0/rck
            copilo(2,1) = a1
            copilo(3,1) = a2
            copilo(4,1) = a3
            copilo(5,1) = a4/rck
        endif
    else
        if (kont .ne. 0) then
            copilo(1,1) = a0/kappam
            copilo(2,1) = a1/kappam
            copilo(3,1) = a2/kappam
            copilo(4,1) = a3/kappam
            copilo(5,1) = a4
        else
            copilo(1,1) = a0/kappam
            copilo(2,1) = a1
            copilo(3,1) = a2
            copilo(4,1) = a3
            copilo(5,1) = a4/kappam
        endif
    endif
!
    do 666 i = 2, npg
        copilo(1,i) = copilo(1,1)
        copilo(2,i) = copilo(2,1)
        copilo(3,i) = copilo(3,1)
        copilo(4,i) = copilo(4,1)
        copilo(5,i) = copilo(5,1)
666  end do
!
end subroutine
