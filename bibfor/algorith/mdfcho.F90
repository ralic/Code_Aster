subroutine mdfcho(nbmode, depgen, vitgen, accgen, fexgen,&
                  nbchoc, logcho, dplmod, parcho, paincho, noecho,&
                  saucho, ltemps, nofdep, nofvit, nofacc,&
                  nbexci, psidel, nonmot, fextgt)
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! aslint: disable=W1306
!
    implicit none
!
#include "dtmdef.h"
#include "asterc/r8maem.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/distno.h"
#include "asterfort/fnorm.h"
#include "asterfort/fointe.h"
#include "asterfort/ftang.h"
#include "asterfort/gloloc.h"
#include "asterfort/locglo.h"
#include "asterfort/mdfdas.h"
#include "asterfort/mdflam.h"
#include "asterfort/mdtr74grd.h"
#include "asterfort/rk5adp.h"
#include "asterfort/togene.h"
#include "asterfort/tophy3.h"
#include "asterfort/tophys.h"
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
#include "asterfort/zengen.h"
#include "asterfort/disc_isotr.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbchoc, nbmode, logcho(nbchoc, *), paincho(nbchoc, *)
    real(kind=8) :: depgen(nbmode), vitgen(nbmode), fexgen(nbmode), accgen(nbmode)
    real(kind=8) :: parcho(nbchoc, *), saucho(nbchoc, *)
    real(kind=8) :: dplmod(nbchoc, nbmode, *)
    character(len=8) :: noecho(nbchoc, *)
    character(len=8) :: nonmot
    integer :: nbexci
    character(len=8) :: nofdep(nbexci), nofvit(nbexci), nofacc(nbexci)
    real(kind=8) :: ltemps(2), psidel(nbchoc, nbexci, *)
    real(kind=8), optional, intent(out) :: fextgt(nbmode)

! --------------------------------------------------------------------------------------------------
!
!     CALCUL LES FORCES DE CHOC DE LA STRUCTURE
!
! --------------------------------------------------------------------------------------------------
!
! IN  : NBMODE  : NOMBRE DE MODES
! IN  : DEPGEN  : DEPLACEMENTS GENERALISES
! IN  : VITGEN  : VITESSES GENERALISEES
! IN  : ACCGEN  : ACCELERATIONS GENERALISEES
! IN  : LTEMPS  : (1) Temps (2) Dtemps
! VAR : FEXGEN  : FORCES GENERALISEES
! VAR : LOGCHO  : INDICATEUR D'ADHERENCE ET DE FORCE FLUIDE ET DE
!                 PRESENCE D UN DISPOSITIF ANTI SISMIQUE
! IN  : DPLMOD  : TABLEAU DES DEPL MODAUX AUX NOEUDS DE CHOC
! IN  : PARCHO  : PARAMETRES REELS  DES NOEUDS DE CHOC
! IN  : PAINCHO : PARAMETRES ENTIER DES NOEUDS DE CHOC
! IN  : NOECHO  : NOM DES NOEUDS DE CHOC ET TYPE D'OBSTACLE
! OUT : SAUCHO  : SAUVEGARDE DES VALEURS DE CHOC
!
! IN  : NOFDEP  : NOM DE LA FONCTION DEPL_IMPO
! IN  : NOFVIT  : NOM DE LA FONCTION VITE_IMPO
! IN  : NOFACC  : NOM DE LA FONCTION ACCE_IMPO
! IN  : NBEXCI  : NOMBRE D'ACCELERO DIFFERENTS
! IN  : PSIDEL  : TABLEAU DE VALEURS DE PSI*DELTA
! IN  : NONMOT  : = OUI SI MULTI-APPUIS
!
! OUT : FEXTGT : FORCE TANGENTE EXTRAITE DE LA FORCE TOTALE FEXGEN
!
! --------------------------------------------------------------------------------------------------
    real(kind=8) :: knorm, ktang, deploc(6), depglo(6), flocal(3), fgloba(3)
    real(kind=8) :: vitglo(6), vitloc(6), orig(3), origob(3), accglo(6)
    real(kind=8) :: accloc(6), ftange(2), vtang(2), ddeplo(3), oldft(2)
    real(kind=8) :: oldxl(3), oldvt(2), signe(2), fdispo, fextgt0(nbmode)
! --------------------------------------------------------------------------------------------------
    integer :: iex, i, im
    character(len=8) :: nompar
    real(kind=8) :: coedep(nbexci), coevit(nbexci), coeacc(nbexci)
!
! --------------------------------------------------------------------------------------------------
    integer :: ier, nbschor
    real(kind=8) :: anorm, ax1, ax2, ay1, ay2, az1, az2
    real(kind=8) :: cfrotd, cfrots, cl, cnorm, coefa, coefad, coefb
    real(kind=8) :: coefc, coefcc, coefd, coefk1, coefk2, coefpy, cosa
    real(kind=8) :: cosb, cosg, cost, ctang, defpla, dist1, dist2
    real(kind=8) :: dnorm, ffluid, flim, fn, fseuil, rigifl, sina
    real(kind=8) :: sinb, sing, sint, ux1, ux2, uy1, uy2
    real(kind=8) :: uz1, uz2, vnorm, vx1, vx2, vy1, vy2
    real(kind=8) :: vz1, vz2, xjeu, xmax, zero,temps,dtemps, eps
! --------------------------------------------------------------------------------------------------
!   COMPORTEMENT DIS_VISC DIS_ECRO_TRAC
!   Nombre maximum  d'équations du système : nbequa
    integer :: nbequa, nbdecp, iret
    parameter  (nbequa=5)
    real(kind=8) :: y0(nbequa), dy0(nbequa), resu(nbequa*2)
    real(kind=8) :: errmax
!   Paramètres des lois de comportements
    real(kind=8) :: ldcpar(5)
    integer      :: ldcfct(3)
! --------------------------------------------------------------------------------------------------
    temps  = ltemps(1)
    dtemps = ltemps(2)
    eps = r8prem()
!
    zero    = 0.d0
    orig(1) = zero
    orig(2) = zero
    orig(3) = zero
!
    nompar = 'INST'
    if (nonmot(1:3) .ne. 'NON') then
        do iex = 1, nbexci
            coedep(iex) = zero
            coevit(iex) = zero
            coeacc(iex) = zero
            if (nofdep(iex) .ne. ' ') then
                call fointe('F', nofdep(iex), 1, [nompar], [temps],&
                            coedep(iex), ier)
            endif
            if (nofvit(iex) .ne. ' ') then
                call fointe('F', nofvit(iex), 1, [nompar], [temps],&
                            coevit(iex), ier)
            endif
            if (nofacc(iex) .ne. ' ') then
                call fointe('F', nofacc(iex), 1, [nompar], [temps],&
                            coeacc(iex), ier)
            endif
        enddo
    endif
!
!
    do i = 1, nbchoc
        fn = zero
        ftange(1) = zero
        ftange(2) = zero
        ffluid = zero
        fdispo = zero
        vnorm = zero
        anorm = zero
        vtang(1) = zero
        vtang(2) = zero
        defpla = zero
        deploc(1:6) = zero
        vitloc(1:6) = zero
        resu(1:nbequa*2) = zero
!
        origob(1) = parcho(i,14)
        origob(2) = parcho(i,15)
        origob(3) = parcho(i,16)
        sina = parcho(i,17)
        cosa = parcho(i,18)
        sinb = parcho(i,19)
        cosb = parcho(i,20)
        sing = parcho(i,21)
        cosg = parcho(i,22)
        signe(1) = parcho(i,37)
        signe(2) = parcho(i,38)
!
!       CONVERSION DDLS GENERALISES DDLS PHYSIQUES POUR LE NOEUD 1
        if (nonmot(1:3) .eq. 'NON') then
            call tophys(i, 0, dplmod, nbchoc, nbmode, depgen, ux1, uy1, uz1)
        else
            call tophy3(i, 0, dplmod, nbchoc, nbmode,&
                        depgen, ux1, uy1, uz1, nbexci,&
                        psidel, coedep)
        endif
!       Position du noeud 1 dans le repère global
!           Pour DIS_VISC, DIS_ECRO_TRAC il ne faut pas tenir compte de la distance entre les noeuds
!           Pour les autres cas on fait comme avant
        if ((logcho(i,6).eq._NB_DIS_VISC).or.(logcho(i,6).eq._NB_DIS_ECRO_TRAC)) then
            depglo(1) = ux1
            depglo(2) = uy1
            depglo(3) = uz1
        else
            depglo(1) = ux1 + parcho(i,8)
            depglo(2) = uy1 + parcho(i,9)
            depglo(3) = uz1 + parcho(i,10)
        endif
!       VITESSE DU NOEUD 1 DANS LE REPERE GLOBAL
        if (nonmot(1:3) .eq. 'NON') then
            call tophys(i, 0, dplmod, nbchoc, nbmode, vitgen, vx1, vy1, vz1)
        else
            call tophy3(i, 0, dplmod, nbchoc, nbmode,&
                        vitgen, vx1, vy1, vz1, nbexci, psidel, coevit)
        endif
        vitglo(1) = vx1
        vitglo(2) = vy1
        vitglo(3) = vz1
!       ACCELERATION DU NOEUD 1 DANS LE REPERE GLOBAL
        if (nonmot(1:3) .eq. 'NON') then
            call tophys(i, 0, dplmod, nbchoc, nbmode, accgen, ax1, ay1, az1)
        else
            call tophy3(i, 0, dplmod, nbchoc, nbmode,&
                        accgen, ax1, ay1, az1, nbexci, psidel, coeacc)
        endif
        accglo(1) = ax1
        accglo(2) = ay1
        accglo(3) = az1
!       PASSAGE DANS LE REPERE LOCAL POUR LE NOEUD 1
        call gloloc(depglo, origob, sina, cosa, sinb, cosb, sing, cosg, deploc)
        call gloloc(vitglo, orig,   sina, cosa, sinb, cosb, sing, cosg, vitloc)
        call gloloc(accglo, orig,   sina, cosa, sinb, cosb, sing, cosg, accloc)
!       DEPLACEMENT DIFFERENTIEL = DEPLOC SI 1 NOEUD
        ddeplo(1) = deploc(1)
        ddeplo(2) = deploc(2)
        ddeplo(3) = deploc(3)
!
        if (noecho(i,9)(1:2) .eq. 'BI') then
!           MEME TRAVAIL POUR LE NOEUD 2
            if (nonmot(1:3) .eq. 'NON') then
                call tophys(i, 3, dplmod, nbchoc, nbmode, depgen, ux2, uy2, uz2)
            else
                call tophy3(i, 3, dplmod, nbchoc, nbmode,&
                            depgen, ux2, uy2, uz2, nbexci, psidel, coedep)
            endif
!           Position du noeud 2 dans le repère global
!               Pour DIS_VISC, DIS_ECRO_TRAC
!                   Il ne faut pas tenir compte de la distance entre les noeuds
!               Pour les autres cas on fait comme avant
            if ((logcho(i,6).eq._NB_DIS_VISC).or.(logcho(i,6).eq._NB_DIS_ECRO_TRAC)) then
                depglo(4) = ux2
                depglo(5) = uy2
                depglo(6) = uz2
            else
                depglo(4) = ux2 + parcho(i,11)
                depglo(5) = uy2 + parcho(i,12)
                depglo(6) = uz2 + parcho(i,13)
            endif
!           VITESSE DU NOEUD 2 DANS LE REPERE GLOBAL
            if (nonmot(1:3) .eq. 'NON') then
                call tophys(i, 3, dplmod, nbchoc, nbmode, vitgen, vx2, vy2, vz2)
            else
                call tophy3(i, 3, dplmod, nbchoc, nbmode,&
                            vitgen, vx2, vy2, vz2, nbexci, psidel, coevit)
            endif
            vitglo(4) = vx2
            vitglo(5) = vy2
            vitglo(6) = vz2
!           ACCELERATION DU NOEUD 2 DANS LE REPERE GLOBAL
            if (nonmot(1:3) .eq. 'NON') then
                call tophys(i, 3, dplmod, nbchoc, nbmode, accgen, ax2, ay2, az2)
            else
                call tophy3(i, 3, dplmod, nbchoc, nbmode,&
                            accgen, ax2, ay2, az2, nbexci, psidel, coeacc)
            endif
            accglo(4) = ax2
            accglo(5) = ay2
            accglo(6) = az2
!           PASSAGE DANS LE REPERE LOCAL POUR LE NOEUD 2
            call gloloc(depglo(4), origob, sina, cosa, sinb, cosb, sing, cosg, deploc(4))
            call gloloc(vitglo(4), orig,   sina, cosa, sinb, cosb, sing, cosg, vitloc(4))
            call gloloc(accglo(4), orig,   sina, cosa, sinb, cosb, sing, cosg, accloc(4))
!           ACCELERATION DIFFERENTIELLE ENTRE NOEUD1 ET NOEUD2
            accloc(1) = accloc(1)-accloc(4)
            accloc(2) = accloc(2)-accloc(5)
            accloc(3) = accloc(3)-accloc(6)
!           VITESSE DIFFERENTIELLE ENTRE NOEUD1 ET NOEUD2
            vitloc(1) = vitloc(1)-vitloc(4)
            vitloc(2) = vitloc(2)-vitloc(5)
            vitloc(3) = vitloc(3)-vitloc(6)
!           DEPLACEMENT DIFFERENTIEL ENTRE NOEUD1 ET NOEUD2
            ddeplo(1) = deploc(1)-deploc(4)
            ddeplo(2) = deploc(2)-deploc(5)
            ddeplo(3) = deploc(3)-deploc(6)
        endif
!
        xjeu = parcho(i,1)
        knorm = parcho(i,2)
        cnorm = parcho(i,3)
        ktang = parcho(i,4)
        ctang = parcho(i,5)
        cfrotd = parcho(i,6)
        cfrots = parcho(i,7)
        dist1 = parcho(i,30)
        dist2 = parcho(i,31)
        coefa = parcho(i,32)
        coefb = parcho(i,33)
        coefc = parcho(i,34)
        coefd = parcho(i,35)
        cl = parcho(i,36)
        coefk1 = parcho(i,39)
        coefk2 = parcho(i,40)
        coefpy = parcho(i,41)
        coefcc = parcho(i,42)
        coefad = parcho(i,43)
        xmax = parcho(i,44)
!       PARAMETRES DE FLAMBAGE
        flim = parcho(i,50)
        fseuil = parcho(i,51)
        rigifl = parcho(i,52)
!
!       Si ce n'est pas DIS_VISC DIS_ECRO_TRAC
        if ((logcho(i,6).ne._NB_DIS_VISC).and.(logcho(i,6).ne._NB_DIS_ECRO_TRAC)) then
!           CALCUL DE LA DISTANCE NORMALE
            dnorm = 0.d0
            call distno(deploc, signe, noecho(i, 9), xjeu, dist1, dist2, dnorm, cost, sint)
        endif
!
! --------------------------------------------------------------------------------------------------
!       CAS DISPOSITIF ANTI SISMIQUE
        if (logcho(i,4).eq.1) then
!           CALCUL DE LA FORCE NORMALE REPERE LOCAL DU AU DISPOSITIF ANTI SISMIQUE
            call mdfdas(dnorm, vnorm, vitloc, cost, sint,&
                        coefk1, coefk2, coefpy, coefcc, coefad,&
                        xmax, fdispo, flocal)
!           PASSAGE DE LA FORCE DANS LE REPERE GLOBAL
            call locglo(flocal, sina, cosa, sinb, cosb,&
                        sing, cosg, fgloba)
!           PASSAGE A LA FORCE GENERALISEE NOEUD_1
            call togene(i, 0, dplmod, nbchoc, nbmode,&
                        fgloba(1), fgloba(2), fgloba(3), fexgen)
!           LA FORCE OPPOSEE SUR NOEUD_2
            if (noecho(i,9)(1:2) .eq. 'BI') then
                call togene(i, 3, dplmod, nbchoc, nbmode,&
                            -fgloba(1), - fgloba(2), -fgloba(3), fexgen)
            endif
            parcho(i,23) = ddeplo(1)
            parcho(i,24) = ddeplo(2)
            parcho(i,25) = ddeplo(3)
            parcho(i,26) = zero
            parcho(i,27) = zero
!           DE FACON PROVISOIRE ON STOCKE FDISPO DANS FN POUR VISU
            fn = fdispo
! --------------------------------------------------------------------------------------------------
!       Amortisseur de type DIS_VISC
        else if (logcho(i,6).eq._NB_DIS_VISC) then
!                PARCHO(I,53)   = S1 Souplesse en série avec les 2 autres branches.
!                PARCHO(I,54)   = K2 Raideur en parallèle de la branche visqueuse.
!                PARCHO(I,55)   = S3 Souplesse dans la branche visqueuse.
!                PARCHO(I,56)   = C  'Raideur' de la partie visqueuse.
!                PARCHO(I,57)   = A  Puissance de la loi visqueuse ]0.0, 1.0]
!                PARCHO(I,58)   = ITER_INTE_MAXI
!                PARCHO(I,59)   = RESI_INTE_RELA
!                PARCHO(I,60:63)= Variables internes
!           Protection
            ASSERT(noecho(i,9).eq.'BIDISVIS')
!           comportement non-linéaire suivant le x local
!           équations du système : 1      2         3     4
!                           yy   : sigma, epsivisq, epsi, puiss
!           variables internes   : 1      2         3     4
!                           vari : sigma, epsivisq, epsi, puiss
            nbschor = mdtr74grd('SCHOR')
            if ( dtemps .gt. 0.0d0 ) then
                y0(1) = saucho(i,nbschor+1)
                y0(2) = saucho(i,nbschor+2)
                y0(3) = saucho(i,nbschor+3)
                y0(4) = saucho(i,nbschor+4)
!               vitesse
                dy0(3) = -vitloc(1)
!               Paramètres de la loi de comportement :
                ldcpar(1) = parcho(i,53)
                ldcpar(2) = parcho(i,54)
                ldcpar(3) = parcho(i,55)
                ldcpar(4) = parcho(i,56)
                ldcpar(5) = parcho(i,57)
!               ldc : nb maxi de découpage, erreur à convergence
                nbdecp = int( parcho(i,58) )
                errmax = parcho(i,59)
                iret = 0
                call rk5adp(4, ldcpar, temps, dtemps, nbdecp,&
                        errmax, y0, dy0, zengen, resu, iret)
                if ( iret.ne.0 ) then
                    call utmess('A', 'DISCRETS_42',si=nbdecp, sr=errmax)
                endif
            else
                resu(1) = saucho(i,nbschor+1)
                resu(2) = saucho(i,nbschor+2)
                resu(3) = saucho(i,nbschor+3)
                resu(4) = saucho(i,nbschor+4)
            endif
!           Variables internes
            parcho(i,60) = resu(1)
            parcho(i,61) = resu(2)
            parcho(i,62) = resu(3)
            parcho(i,63) = resu(4)
!           Seul l'effort axial est calculé
            flocal(1:3) = 0.0d0
            flocal(1) = resu(1)
!           passage de la force dans le repere global
            call locglo(flocal, sina, cosa, sinb, cosb, sing, cosg, fgloba)
!           force généralisée sur noeud_1
            call togene(i,0,dplmod,nbchoc,nbmode, fgloba(1), fgloba(2), fgloba(3), fexgen)
!           force généralisée sur noeud_2
            if (noecho(i,9)(1:2) .eq. 'BI') then
                call togene(i,3,dplmod,nbchoc,nbmode, -fgloba(1), - fgloba(2), -fgloba(3), fexgen)
            endif
!
            fn = resu(1)
! --------------------------------------------------------------------------------------------------
!       Amortisseur de type DIS_ECRO_TRAC
        else if (logcho(i,6).eq._NB_DIS_ECRO_TRAC) then
!                PARCHO(I,53)   = Seuil Limite
!                PARCHO(I,54)   = Déplacement correspondant au seuil
!                PARCHO(I,55)   =
!                PARCHO(I,56)   =
!                PARCHO(I,57)   =
!                PARCHO(I,58)   = ITER_INTE_MAXI
!                PARCHO(I,59)   = RESI_INTE_RELA
!                PARCHO(I,60:64)= Variables internes
!                PAINCHO(I,1)   = fonction : adresse jpro
!                PAINCHO(I,2)   = fonction : nbvale
!                PAINCHO(I,3)   = fonction : jvale
!           Protection
            ASSERT(noecho(i,9).eq.'BIDISISO')
!           comportement non-linéaire suivant le x local
!           équations du système : 1        2       3       4       5
!                           yy   : force    Up      U       puiss   ip
!           variables internes   : 1        2       3       4       5
!                           vari : force    Up      U       puiss   ip
            nbschor = mdtr74grd('SCHOR')
            if ( dtemps .gt. 0.0d0 ) then
                y0(1) = saucho(i,nbschor+1)
                y0(2) = saucho(i,nbschor+2)
                y0(3) = saucho(i,nbschor+3)
                y0(4) = saucho(i,nbschor+4)
                y0(5) = saucho(i,nbschor+5)
!               vitesse
                dy0(3) = -vitloc(1)
!               Paramètres de la loi de comportement :
                ldcpar(1) = parcho(i,53)
                ldcpar(2) = parcho(i,54)
                ldcfct(1) = paincho(i,1)
                ldcfct(2) = paincho(i,2)
                ldcfct(3) = paincho(i,3)
!               ldc : nb maxi de découpage, erreur à convergence
                nbdecp = int( parcho(i,58) )
                errmax = parcho(i,59)
                iret = 0
                call rk5adp(5, ldcpar, temps, dtemps, nbdecp,&
                        errmax, y0, dy0, disc_isotr, resu, iret,fonction=ldcfct)
                if ( iret.ne.0 ) then
                    call utmess('A', 'DISCRETS_42',si=nbdecp, sr=errmax)
                endif
            else
                resu(1) = saucho(i,nbschor+1)
                resu(2) = saucho(i,nbschor+2)
                resu(3) = saucho(i,nbschor+3)
                resu(4) = saucho(i,nbschor+4)
                resu(5) = saucho(i,nbschor+5)
            endif
!           Variables internes
            parcho(i,60) = resu(1)
            parcho(i,61) = resu(2)
            parcho(i,62) = resu(3)
            parcho(i,63) = resu(4)
            parcho(i,64) = resu(5)
!           Seul l'effort axial est calculé
            flocal(1:3) = 0.0d0
            flocal(1) = resu(1)
!           passage de la force dans le repere global
            call locglo(flocal, sina, cosa, sinb, cosb, sing, cosg, fgloba)
!           force généralisée sur noeud_1
            call togene(i,0,dplmod,nbchoc,nbmode, fgloba(1), fgloba(2), fgloba(3), fexgen)
!           force généralisée sur noeud_2
            if (noecho(i,9)(1:2) .eq. 'BI') then
                call togene(i,3,dplmod,nbchoc,nbmode, -fgloba(1), - fgloba(2), -fgloba(3), fexgen)
            endif
!
            fn = resu(1)
! --------------------------------------------------------------------------------------------------
!       CAS DU FLAMBAGE
        else if (logcho(i,5).eq.1) then
!           Variables internes
            nbschor = mdtr74grd('SCHOR')
            defpla = saucho(i,nbschor+1)
            if (dnorm .le. zero) then
!               CALCUL DE LA FORCE NORMALE REPERE LOCAL
                call mdflam(dnorm, vitloc, knorm, cost, sint,&
                            flim, fseuil, rigifl, defpla, fn,&
                            flocal, vnorm)
!               PASSAGE DE LA FORCE DANS LE REPERE GLOBAL
                call locglo(flocal, sina, cosa, sinb, cosb,&
                            sing, cosg, fgloba)
!               PASSAGE A LA FORCE GENERALISEE NOEUD_1
                call togene(i, 0, dplmod, nbchoc, nbmode,&
                            fgloba(1), fgloba(2), fgloba(3), fexgen)
!               LA FORCE OPPOSEE SUR NOEUD_2 ---
                if (noecho(i,9)(1:2) .eq. 'BI') then
                    call togene(i, 3, dplmod, nbchoc, nbmode,&
                                -fgloba(1), - fgloba(2), -fgloba(3), fexgen)
                endif

                if (( abs(cfrots).gt.eps ) .or. ( abs(cfrotd) .gt. eps )) then
                    oldft(1) = parcho(i,26)
                    oldft(2) = parcho(i,27)
                    oldxl(1) = parcho(i,23)
                    oldxl(2) = parcho(i,24)
                    oldxl(3) = parcho(i,25)
                    oldvt(1) = parcho(i,28)
                    oldvt(2) = parcho(i,29)
                    if (max(vitloc(1),vitloc(2),vitloc(3)).gt.(0.5d0*sqrt(r8maem()))) then
                        call utmess('F', 'ALGORITH17_1', sr=temps)
                    end if
                    call ftang(fn, ddeplo, vitloc, cfrotd, cfrots,&
                               ktang, ctang, logcho(i, 1), oldvt, oldft,&
                               oldxl, cost, sint, ftange, flocal,&
                               vtang)
                    parcho(i,26) = oldft(1)
                    parcho(i,27) = oldft(2)
                    parcho(i,23) = oldxl(1)
                    parcho(i,24) = oldxl(2)
                    parcho(i,25) = oldxl(3)
                    parcho(i,28) = oldvt(1)
                    parcho(i,29) = oldvt(2)

!                   PASSAGE DE LA FORCE DANS LE REPERE GLOBAL
                    call locglo(flocal, sina, cosa, sinb, cosb,&
                                sing, cosg, fgloba)
!                   PASSAGE A LA FORCE GENERALISEE NOEUD_1
                    call vecini(nbmode, 0.d0, fextgt0)
                    call togene(i, 0, dplmod, nbchoc, nbmode,&
                                fgloba(1), fgloba(2), fgloba(3), fextgt0)
!                   LA FORCE OPPOSEE SUR NOEUD_2
                    if (noecho(i,9)(1:2) .eq. 'BI') then
                        call togene(i, 3, dplmod, nbchoc, nbmode,&
                                    -fgloba(1), - fgloba(2), -fgloba(3), fextgt0)
                    endif

                    do im = 1, nbmode
                        fexgen(im) = fexgen(im) + fextgt0(im)
                    end do
                    if (present(fextgt)) then
                        do im = 1, nbmode
                            fextgt(im) = fextgt(im) + fextgt0(im)
                        end do
                    end if
                endif
            else
                parcho(i,23) = ddeplo(1)
                parcho(i,24) = ddeplo(2)
                parcho(i,25) = ddeplo(3)
                parcho(i,26) = zero
                parcho(i,27) = zero
                logcho(i,1) = 0
                ftange(1) = zero
                ftange(2) = zero
                vtang(1) = zero
                vtang(2) = zero
            endif
        else
! --------------------------------------------------------------------------------------------------
!           CAS DU CHOC SEC
            if (dnorm .le. zero) then

!               CALCUL DE LA FORCE NORMALE REPERE LOCAL
                call fnorm(dnorm, vitloc, knorm, cnorm, cost,&
                           sint, fn, flocal, vnorm)

!               PASSAGE DE LA FORCE NORMALE DANS LE REPERE GLOBAL
                call locglo(flocal, sina, cosa, sinb, cosb,&
                            sing, cosg, fgloba)
!               PASSAGE A LA FORCE GENERALISEE NOEUD_1
                call togene(i, 0, dplmod, nbchoc, nbmode,&
                            fgloba(1), fgloba(2), fgloba(3), fexgen)
!               LA FORCE OPPOSEE SUR NOEUD_2
                if (noecho(i,9)(1:2) .eq. 'BI') then
                    call togene(i, 3, dplmod, nbchoc, nbmode,&
                                -fgloba(1), - fgloba(2), -fgloba(3), fexgen)
                endif

                if (( abs(cfrots) .gt. eps ) .or. ( abs(cfrotd) .gt. eps )) then
                    oldft(1) = parcho(i,26)
                    oldft(2) = parcho(i,27)
                    oldxl(1) = parcho(i,23)
                    oldxl(2) = parcho(i,24)
                    oldxl(3) = parcho(i,25)
                    oldvt(1) = parcho(i,28)
                    oldvt(2) = parcho(i,29)
                    if (max(vitloc(1),vitloc(2),vitloc(3)).gt.(0.5d0*sqrt(r8maem()))) then
                        call utmess('F', 'ALGORITH17_1', sr=temps)
                    end if
                    call ftang(fn, ddeplo, vitloc, cfrotd, cfrots,&
                               ktang, ctang, logcho(i, 1), oldvt, oldft,&
                               oldxl, cost, sint, ftange, flocal,&
                               vtang)
                    parcho(i,26) = oldft(1)
                    parcho(i,27) = oldft(2)
                    parcho(i,23) = oldxl(1)
                    parcho(i,24) = oldxl(2)
                    parcho(i,25) = oldxl(3)
                    parcho(i,28) = oldvt(1)
                    parcho(i,29) = oldvt(2)

!                   PASSAGE DE LA FORCE DANS LE REPERE GLOBAL
                    call locglo(flocal, sina, cosa, sinb, cosb,&
                                sing, cosg, fgloba)
                    call vecini(nbmode, 0.d0, fextgt0)
!                   PASSAGE A LA FORCE GENERALISEE NOEUD_1
                    call togene(i, 0, dplmod, nbchoc, nbmode,&
                                fgloba(1), fgloba(2), fgloba(3), fextgt0)
!                   LA FORCE OPPOSEE SUR NOEUD_2
                    if (noecho(i,9)(1:2) .eq. 'BI') then
                        call togene(i, 3, dplmod, nbchoc, nbmode,&
                                    -fgloba(1), - fgloba(2), -fgloba(3), fextgt0)
                    endif

                    do im = 1, nbmode
                        fexgen(im) = fexgen(im) + fextgt0(im)
                    end do
                    if (present(fextgt)) then
                        do im = 1, nbmode
                            fextgt(im) = fextgt(im) + fextgt0(im)
                        end do
                    end if
                endif
            else
                parcho(i,23) = ddeplo(1)
                parcho(i,24) = ddeplo(2)
                parcho(i,25) = ddeplo(3)
                parcho(i,26) = zero
                parcho(i,27) = zero
                logcho(i,1) = 0
                ftange(1) = zero
                ftange(2) = zero
                vtang(1) = zero
                vtang(2) = zero
            endif
        endif
! --------------------------------------------------------------------------------------------------
        saucho(i,1) = fn
        saucho(i,2) = ftange(1)
        saucho(i,3) = ftange(2)
!       DEPLACEMENT LOCAL DU NOEUD NOEUD_1
        saucho(i,4) = deploc(1)
        saucho(i,5) = deploc(2)
        saucho(i,6) = deploc(3)
        saucho(i,7) = vnorm
        saucho(i,8) = vtang(1)
        saucho(i,9) = vtang(2)
!       DEPLACEMENT LOCAL DU NOEUD NOEUD_2
        saucho(i,10) = deploc(4)
        saucho(i,11) = deploc(5)
        saucho(i,12) = deploc(6)
!       INDICATEUR ADHERENCE
        saucho(i,13) = logcho(i,1)
!       Variables internes
        if (logcho(i,5).eq.1) then
!           flambage : écrasement cumulé
            nbschor = mdtr74grd('SCHOR')
            saucho(i,nbschor+1) = defpla
        else if (logcho(i,6).eq._NB_DIS_VISC) then
!           DIS_VISC : sigma, epsivisq, puiss, tangente
            nbschor = mdtr74grd('SCHOR')
            saucho(i,nbschor+1) = parcho(i,60)
            saucho(i,nbschor+2) = parcho(i,61)
            saucho(i,nbschor+3) = parcho(i,62)
            saucho(i,nbschor+4) = parcho(i,63)
        else if (logcho(i,6).eq._NB_DIS_ECRO_TRAC) then
!           DIS_ECRO_TRAC : force, Up, puiss, ip, tangente
            nbschor = mdtr74grd('SCHOR')
            saucho(i,nbschor+1) = parcho(i,60)
            saucho(i,nbschor+2) = parcho(i,61)
            saucho(i,nbschor+3) = parcho(i,62)
            saucho(i,nbschor+4) = parcho(i,63)
            saucho(i,nbschor+5) = parcho(i,64)
        endif
!
    enddo
!
end subroutine
