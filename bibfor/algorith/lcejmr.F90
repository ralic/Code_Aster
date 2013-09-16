subroutine lcejmr(fami, kpg, ksp, ndim, mate,&
                  option, epsm, deps, sigmo, sigma,&
                  dsidep, vim, vip, coorot, typmod,&
                  instam, instap)
!
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
! person_in_charge: kyrylo.kazymyrenko at edf.fr
!
    implicit none
#include "asterc/r8pi.h"
#include "asterfort/matinv.h"
#include "asterfort/pmavec.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
    integer :: mate, ndim, kpg, ksp
    real(kind=8) :: epsm(6), deps(6)
    real(kind=8) :: sigmo(6), sigma(6), dsidep(6, 6)
    real(kind=8) :: vim(*), vip(*), instam, instap, coorot(ndim+ndim*ndim)
    character(len=8) :: typmod(*)
    character(len=16) :: option
    character(len=*) :: fami
!-----------------------------------------------------------------------
!     LOI DE COMPORTEMENT DES JOINTS DE BARRAGE : JOINT_MECA_RUPT
!     POUR LES ELEMENTS DE JOINT ET JOINT_HYME 2D ET 3D
!
! DEUX TYPES DE CALCUL SONT POSSIBLES:
!
! 1) MODELISATIONS *_JOINT_HYME, AVEC PARAMETRE HYDRO PRESENTS
!    CALCUL COUPLE HYDRO-MECANIQUE SUR UN MAILLAGE QUADRATIQUE
!
! 2) MODELISATIONS *_JOINT, PAS DE PARAMETRES HYDRO POSSIBLE
!    CALCUL MECA (AVEC ENVENTUELLEMENT PRES_CLAVAGE OU PRES_FLUIDE)
!    SUR UN MAILLAGE LINEAIRE OU QUADRATIQUE
!
! IN : EPSM SAUT INSTANT MOINS ET GRAD PRESSION ET PRES FLUIDE SI HYME
! IN : DEPS INC DE SAUT  ET INC GRAD PRESSION ET INC PRES FLUIDE SI HYME
! IN : MATE, OPTION, VIM, COOROT,INSTAM, INSTAP
! OUT : SIGMA , DSIDEP , VIP
!-----------------------------------------------------------------------
    integer :: nbpa
    parameter (nbpa=11)
    integer :: cod(nbpa)
    integer :: i, n, diss, cass
    real(kind=8) :: sc, lc, lct, k0, val(nbpa), presfl, presg, prescl, tmp
    real(kind=8) :: gp(ndim-1), gploc(ndim), gpglo(ndim), fhloc(ndim)
    real(kind=8) :: fhglo(ndim)
    real(kind=8) :: a(ndim), da(ndim), ka, kap, r0, rc, alpha, beta, rk, ra, rt
    real(kind=8) :: rt0, r8bid
    real(kind=8) :: oset, doset, inst, valpar(ndim+1), rhof, visf, amin
    real(kind=8) :: invrot(ndim, ndim), rigart
    character(len=8) :: nom(nbpa), nompar(ndim+1)
    character(len=1) :: poum
    logical :: resi, rigi, elas, ifpahm, ifhyme
!
! OPTION CALCUL DU RESIDU OU CALCUL DE LA MATRICE TANGENTE
    resi = option(1:9).eq.'FULL_MECA' .or. option.eq.'RAPH_MECA'
    rigi = option(1:9).eq.'FULL_MECA' .or. option(1:9).eq.'RIGI_MECA'
    elas = option.eq.'FULL_MECA_ELAS' .or. option.eq.'RIGI_MECA_ELAS'
!
! INDICATEUR AVEC/SANS HYDRO
    if (typmod(2) .eq. 'EJ_HYME') ifhyme=.true.
    if (typmod(2) .eq. 'ELEMJOIN') ifhyme=.false.
!
! SAUT DE DEPLACEMENT EN T- OU T+
    call dcopy(ndim, epsm, 1, a, 1)
    call dcopy(ndim, deps, 1, da, 1)
    if (resi) call daxpy(ndim, 1.d0, deps, 1, a,&
                         1)
!
! GRADIENT DE PRESSION ET PRESSION EN T- OU T+
    if (ifhyme) then
!
        do 10 n = 1, ndim-1
            gp(n) = epsm(ndim+n)
            if (resi) gp(n) = gp(n) + deps(ndim+n)
10      continue
!
        presg = epsm(2*ndim)
        if (resi) presg = presg + deps(2*ndim)
!
    endif
!
! INSTANT DE CALCUL T- OU T+
    inst = instam
    if (resi) inst = instap
!
! RECUPERATION DES PARAMETRES PHYSIQUES
!--------------------------------------
    nom(1) = 'K_N'
    nom(2) = 'SIGM_MAX'
    nom(3) = 'PENA_RUPT'
    nom(4) = 'PENA_CONTACT'
    nom(5) = 'ALPHA'
    nom(6) = 'K_T'
    nom(7) = 'PRES_FLUIDE'
    nom(8) = 'PRES_CLAVAGE'
    nom(9) = 'RHO_FLUIDE'
    nom(10) ='VISC_FLUIDE'
    nom(11) ='OUV_MIN'
!
    if (option .eq. 'RIGI_MECA_TANG') then
        poum = '-'
    else
        poum = '+'
    endif
!
    call rcvalb(fami, kpg, ksp, poum, mate,&
                ' ', 'JOINT_MECA_RUPT', 0, ' ', [0.d0],&
                5, nom, val, cod, 2)
!
! CONTRAINTE CRITIQUE SANS PENALISATION
    sc = val(2)*(1.d0 + val(3))/val(3)
! LONGUEUR CRITIQUE AVANT LA RUPTURE COMPLETE DU JOINT
    lc = (1.d0 + val(3))*val(2)/val(1)
! LONGUEUR AVANT L'ADOUCISSEMENT
    k0 = val(2)/val(1)
! PENTE NORMALE INITIAL
    r0 = val(1)
    beta = val(4)
! PARAMETRE QUI DEFINI LA LONGUEUR CRITIQUE TANGENTIELLE (0<ALPHA<=2)
    alpha= val(5)
! LONGUEUR CRITIQUE TANGENTIELLE
! ALPHA=0: LCT=0; ALPHA=1: LCT=LC; ALPHA=2;LCT=INFTY
    if (alpha .ne. 2.d0) then
        lct=lc*tan(alpha*r8pi()/4.d0)
    else
! PRESENTATION D'UNE INFINITE NUMERIQUE
        lct=(1.d0 + lc)*1.d8
    endif
! PENTE TANGENTIELLE INITIAL (SI ELLE N'EST PAS DEFINI ALORS K_T=K_N)
    call rcvalb(fami, kpg, ksp, poum, mate,&
                ' ', 'JOINT_MECA_RUPT', 0, ' ', [0.d0],&
                1, nom(6), val(6), cod(6), 0)
    if (cod(6) .eq. 0) then
        rt0 = val(6)
    else
        rt0 = r0
    endif
!
! DEFINITION DES PARAMETRES POUR LA RECUPERATION DES FONCTIONS
    nompar(1)='INST'
    nompar(2)='X'
    nompar(3)='Y'
    valpar(1)= inst
    valpar(2)= coorot(1)
    valpar(3)= coorot(2)
    if (ndim .eq. 3) then
        nompar(4)='Z'
        valpar(4)= coorot(3)
    endif
!
! RECUPERATION DE LA PRESS FLUIDE ET DE CLAVAGE (MODELISATION MECA PURE)
!-----------------------------------------------------------------------
! RECUPERATION DE LA PRESS FLUIDE (FONCTION DE L'ESPACE ET DU TEMPS)
    call rcvalb(fami, kpg, ksp, poum, mate,&
                ' ', 'JOINT_MECA_RUPT', ndim+1, nompar, valpar,&
                1, nom(7), val(7), cod(7), 0)
!
    if (cod(7) .eq. 0) then
        presfl = val(7)
    else
        presfl = 0.d0
    endif
!
! RECUPERATION DE LA PRESS CLAVAGE (FONCTION DE L'ESPACE ET DU TEMPS)
    call rcvalb(fami, kpg, ksp, poum, mate,&
                ' ', 'JOINT_MECA_RUPT', ndim+1, nompar, valpar,&
                1, nom(8), val(8), cod(8), 0)
!
    if (cod(8) .eq. 0) then
        prescl = val(8)
    else
        prescl = -1.d0
    endif
!
! RECUPERATION DE LA MASSE VOL ET DE LA VISCO (MODELISATION JOINT HM)
!--------------------------------------------------------------------
    call rcvalb(fami, kpg, ksp, poum, mate,&
                ' ', 'JOINT_MECA_RUPT', 0, ' ', [0.d0],&
                1, nom(9), val(9), cod(9), 0)
    call rcvalb(fami, kpg, ksp, poum, mate,&
                ' ', 'JOINT_MECA_RUPT', 0, ' ', [0.d0],&
                1, nom(10), val(10), cod(10), 0)
    call rcvalb(fami, kpg, ksp, poum, mate,&
                ' ', 'JOINT_MECA_RUPT', 0, ' ', [0.d0],&
                1, nom(11), val(11), cod(11), 0)
!
    if (cod(9) .eq. 0) rhof = val(9)
    if (cod(10) .eq. 0) visf = val(10)
    if (cod(11) .eq. 0) amin = val(11)
!
! INDICATEUR SI LES PARAMETRES HYDRO SONT RENSEIGNES
    ifpahm = (cod(9).eq.0).and.(cod(10).eq.0).and.(cod(11).eq.0)
!
! VERIFICATION DE LA PRESENCE/ABSENCE DE PARAMETRES
! EN FONCTION DE LA MODELISATION MECA PUR OU HYDRO MECA
!
    if (ifhyme) then
!       POUR LE CALCUL HYDRO => PAS DE PRES_CLAVAGE
        if (cod(8) .eq. 0) then
            call utmess('F', 'ALGORITH17_14')
        endif
!       POUR LE CALCUL HYDRO => PAS DE PRES_FLUIDE
        if (cod(7) .eq. 0) then
            call utmess('F', 'ALGORITH17_14')
        endif
!       POUR LE CALCUL HYDRO => PRESENCE DE PARA_HM
        if (.not.ifpahm) then
            call utmess('F', 'ALGORITH17_15')
        endif
    else
!       POUR LE CALCUL MECA => PAS DE PARAMETRE HYDRO
        if (ifpahm) then
            call utmess('F', 'ALGORITH17_16')
        endif
    endif
!
! DANS LE CAS DU CLAVAGE
!-----------------------
! INITIALISATION DU POINT D'EQUILIBRE POUR LA LDC (OFFSET)
    doset = a(1) + prescl/(beta*r0)
    if ((prescl.lt.0.d0) .or. (doset.lt.0.d0)) then
        oset = vim(10)
    else
        oset = vim(10) + doset
    endif
!
! LA LDC EST DEFINIE PAR RAPPORT A NOUVEAU POINT D'EQUILIBRE
    a(1) = a(1) - oset
!
! INITIALISATION DU SEUIL D'ENDOMMAGEMENT ACTUEL
    ka = max(k0,vim(1))
!
! CALCUL DES PENTES
!------------------
!
! PENTE ACTUEL EN DECHARGE DANS LA ZONE DE TRACTION
    if (lc*ka .ne. 0.d0) then
        rk = max(0.d0, sc*(1.d0-ka/lc)/ka)
    else
        rk = 0.d0
    endif
!
! DANS LE DOMAINE DE COMPRESSION
    rc = beta*r0
!
! PENTE TANGENTIELLE ACTUELLE
! SI ALPHA=2 RT CSTE, SI ALPHA=0 ALORS LCT=0 ET DONC RT=0)
    rt = rt0
    if (a(1) .gt. 0.d0) then
        if (lct .ne. 0.d0) rt = max(0.d0, rt0*(1.d0-a(1)/lct))
        if (lct .eq. 0.d0) rt = 0.d0
    endif
    if (alpha .eq. 2.d0) rt = rt0
!
! INITIALISATION COMPLEMENTAIRE POUR RIGI_MECA_TANG (SECANTE PENALISEE)
    if (.not. resi) then
        if (elas) then
            diss = 0
        else
            diss = nint(vim(2))
        endif
        cass = nint(vim(3))
        goto 5000
    endif
!
!     INITIALISATION DE LA CONTRAINTE
    call r8inir(6, 0.d0, sigma, 1)
!
! CALCUL DE LA CONTRAINTE HYDRO : DEBIT (LOI CUBIQUE)
! RECUP DE LA PRESSION AU PG
!----------------------------------------------------
!
    if (ifhyme) then
        do 44 n = 1, ndim-1
            sigma(ndim+n) = -rhof*gp(n)*(max(amin,a(1)+amin))**3/(12* visf)
44      continue
    endif
!
! CALCUL DE LA CONTRAINTE MECANIQUE
!----------------------------------
!    CONTRAINTE DE CONTACT PENALISE
!    ET PRISE EN COMPTE DE LA PRESSION DE FLUIDE EVENTUELLE
!    PRESFL : IMPOSEE, PRESG : CALCULEE (MODELISATION HYME)
!
    if (ifhyme) then
        sigma(1) = rc * min(0.d0,a(1)) - presg
    else
        sigma(1) = rc * min(0.d0,a(1)) - presfl
    endif
!
!    PARTIE TANGENTIELLE
    do 20 i = 2, ndim
        if (rt .ne. 0.d0) then
            sigma(i) = sigmo(i) + rt*deps(i)
        else
            sigma(i) = 0.d0
        endif
20  end do
!
!    CONTRAINTE DE FISSURATION NORMALE
    if ((a(1).ge.lc) .or. (ka.gt.lc)) then
        diss = 0
        cass = 2
    else
        if (a(1) .le. ka) then
!
            diss = 0
            if (ka .gt. k0) then
                cass = 1
            else
                cass = 0
            endif
            sigma(1) = sigma(1) + rk*max(0.d0,a(1))
!
        else
!
            diss = 1
            cass = 1
            if (lc .ne. 0.d0) then
                ra = max(0.d0,sc*(1.d0 - a(1)/lc)/a(1))
            else
                ra = 0.d0
            endif
            sigma(1) = sigma(1) + ra*max(0.d0,a(1))
!
        endif
    endif
!
! ACTUALISATION DES VARIABLES INTERNES
!-------------------------------------
! V1 : SEUIL, PLUS GRANDE NORME DU SAUT
! V2 : INDICATEUR DE DISSIPATION (0 : NON, 1 : OUI)
! V3 : INDICATEUR D'ENDOMMAGEMENT NORMAL (0 : SAIN, 1: ENDOM, 2: CASSE)
! V4 : POURCENTAGE D'ENDOMMAGEMENT NORMAL (DANS LA ZONE ADOUCISSANTE)
! V5 : INDICATEUR D'ENDOMMAGEMENT TANGENTIEL (0:SAIN, 1:ENDOM, 2:CASSE)
! V6 : POURCENTAGE D'ENDOMMAGEMENT TANGENTIEL
! V7 A V9 : VALEURS DU SAUT DANS LE REPERE LOCAL
! V10: EPAISSEUR DU JOINT CLAVE
! V11 : CONTRAINTE MECANIQUE NORMALE (SANS INFLUENCE PRESSION DE FLUIDE)
! V12 A V14 : COMPOSANTES DU GRADIENT DE PRESSION DANS LE REPERE GLOBAL
! V15 A V17 : COMPOSANTES DU FLUX HYDRO DANS LE REPERE GLOBAL
! V18 : PRESSION DE FLUIDE IMPOSEE OU CALCULEE ET INTERPOLEE (EN HYME)
!
    kap = max(ka,a(1))
    vip(1) = kap
    vip(2) = diss
    vip(3) = cass
    if (lc .ne. 0.d0) then
        tmp = max(0.d0, (kap - val(2)/val(1)) / (lc - val(2)/val(1)) )
        vip(4) = min(1.d0,tmp)
    else
        vip(4) = 1.d0
    endif
    vip(5) = 0.d0
    if (rt .lt. rt0) vip(5) = 1.d0
    if (rt .eq. 0.d0) vip(5) = 2.d0
    vip(6) = 1.d0 - rt/rt0
    vip(7) = a(1) + oset
    vip(8) = a(2)
    if (ndim .eq. 3) then
        vip(9) = a(3)
    else
        vip(9) = 0.d0
    endif
!
!     CALCUL DU NOUVEAU POINT D'EQUILIBRE V10 DANS LE CAS DE CLAVAGE
!     LE CLAVAGE NE FAIT QU'AUGMENTER L'EPAISSEUR DU JOINT
!     => OSET EST CROISSANT
    if ((prescl.lt.0.d0) .or. (doset.lt.0.d0)) then
        vip(10) = vim(10)
    else
        vip(10) = vim(10) + doset
    endif
!
!     FLUX, GRAD DE PRESSION ET PRESSION DANS LE REPERE GLOBAL
    if (ifhyme) then
!
        gploc(1) = 0.d0
        gploc(2) = gp(1)
        if (ndim .eq. 3) then
            gploc(3) = gp(2)
        endif
!
        fhloc(1) = 0.d0
        fhloc(2) = sigma(ndim+1)
        if (ndim .eq. 3) then
            fhloc(3) = sigma(2*ndim-1)
        endif
!
        call matinv('S', ndim, coorot(ndim+1), invrot, r8bid)
        call pmavec('ZERO', ndim, invrot, gploc, gpglo)
        call pmavec('ZERO', ndim, invrot, fhloc, fhglo)
!
!       CONTRAINTE MECANIQUE NORMALE SANS PRESSION DE FLUIDE CALCULEE
!       ON ANNULE SON INFLUENCE
        vip(11) = sigma(1) + presg
!
        vip(12) = gpglo(1)
        vip(13) = gpglo(2)
        vip(15) = fhglo(1)
        vip(16) = fhglo(2)
!
        if (ndim .eq. 3) then
            vip(14) = gpglo(3)
            vip(17) = fhglo(3)
        else
            vip(14) = 0.d0
            vip(17) = 0.d0
        endif
!
!       PRESSION DE FLUIDE CALCULEE AUX NOEUDS (DDL) ET INTERPOL AU PG
        vip(18) = presg
!
    else
!
!       CONTRAINTE MECANIQUE NORMALE SANS PRESSION DE FLUIDE IMPOSEE
!       ON ANNULE SON INFLUENCE
        vip(11) = sigma(1) + presfl
!
!       VI PAS UTILISEES EN MODELISATION NON HYME
        vip(12) = 0.d0
        vip(13) = 0.d0
        vip(14) = 0.d0
        vip(15) = 0.d0
        vip(16) = 0.d0
        vip(17) = 0.d0
!
!       PRESSION DE FLUIDE IMPOSEE AU PG :
        vip(18) = presfl
!
    endif
!
5000  continue
!
! INITIALISATION DE LA MATRICE TANGENTE
    call r8inir(6*6, 0.d0, dsidep, 1)
!
    if (.not. rigi) goto 9999
!
! CALCUL DE LA MATRICE TANGENTE HYDRO
!------------------------------------
    if (ifhyme) then
!
!       TERME : DW/DGP  (POUR KTAN P P)
        do 42 n = 1, ndim-1
            dsidep(ndim+n,ndim+n)=-rhof*(max(amin,a(1)+amin))**3/(12*&
            visf)
42      continue
!
!       TERME : DW/DDELTA_N  (POUR KTAN P U)
        do 43 n = 1, ndim-1
            if (a(1) .lt. 0.d0) then
                dsidep(ndim+n,1) = 0.d0
            else
                dsidep(ndim+n,1) = -3*rhof*gp(n)*(a(1)+amin)**2/(12* visf)
            endif
43      continue
!
    endif
!
! CALCUL DE LA MATRICE TANGENTE MECA (POUR KTAN U U)
!-----------------------------------
!
! MATRICE TANGENTE DE CONTACT FERME
    if (a(1) .le. 0.d0) then
        dsidep(1,1) = rc
        do 38 i = 2, ndim
            dsidep(i,i) = rt0
38      continue
        goto 9999
    endif
!
! MATRICE TANGENTE DE CONTACT OUVERT
!  (IL FAUT NOTER QUE DANS LA SUITE A(1)>0)
!
! MATRICE TANGENTE DE FISSURATION
    if ((diss.eq.0) .or. elas) then
        dsidep(1,1) = rk
    else
        if (lc .ne. 0.d0) dsidep(1,1) = -sc/lc
    endif
!
    do 40 i = 2, ndim
        dsidep(i,i) = rt
        if ((lct.ne.0.d0) .and. (a(1).lt.lct)) then
            dsidep(i,1) = -da(i)*rt0/lct
        endif
40  end do
!
! DANS LE CAS OU L'ELEMENT EST TOTALEMENT CASSE ON INTRODUIT UNE
! RIGIDITE ARTIFICIELLE DANS LA MATRICE TANGENTE POUR ASSURER
! LA CONVERGENCE
    rigart=1.d-8
    if (cass .eq. 2) dsidep(1,1) = rigart*r0
!
    if (abs(rt) .lt. rigart) then
        do 39 i = 2, ndim
            dsidep(i,i) = rigart*rt0
39      continue
    endif
!
9999  continue
end subroutine
