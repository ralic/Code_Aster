subroutine dxglrc(nomte, option, compor, xyzl, ul, dul, btsig, ktan, pgl, crit, codret)
    implicit none
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     CALCUL LES OPTIONS NON LINEAIRES POUR L'ELEMENT DE PLAQUE DKTG
!     TOUTES LES ENTREES ET LES SORTIES SONT DANS LE REPERE LOCAL.
!
!     IN  OPTION : OPTION NON LINEAIRE A CALCULER
!                'RAPH_MECA' ,'FULL_MECA', OU 'RIGI_MECA_TANG'
!     IN  XYZL : COORDONNEES DES NOEUDS DANS LE REPERE LOCAL
!     IN  UL : DEPLACEMENT A L'INSTANT T "-"
!     IN  DUL : INCREMENT DE DEPLACEMENT
!     IN  PGL : MATRICE DE PASSAGE
!                DU REPERE GLOBAL AU REPERE LOCAL ELEMENT
!     IN  CRIT : CRITERES DE CONVERGENCE LOCAUX
!                (1) = NB ITERATIONS MAXI A CONVERGENCE
!                      (ITER_INTE_MAXI == ITECREL)
!                (2) = TYPE DE JACOBIEN A T+DT
!                      (TYPE_MATR_COMP == MACOMP)
!                      0 = EN VITESSE     >SYMETRIQUE
!                      1 = EN INCREMENTAL >NON-SYMETRIQUE
!                (3) = VALEUR TOLERANCE DE CONVERGENCE
!                      (RESI_INTE_RELA == RESCREL)
!                (5) = NOMBRE D'INCREMENTS POUR LE
!                      REDECOUPAGE LOCAL DU PAS DE TEMPS
!                      (ITER_INTE_PAS  == ITEDEC)
!                       -1,0,1 = PAS DE REDECOUPAGE
!                       N = NOMBRE DE PALIERS
!                (6) = TYPE D INTEGRATION LOCAL POUR LA LOI DE
!                      COMPORTEMENT (ALGO_INTE)
!     OUT KTAN : MATRICE DE RIGIDITE TANGENTE
!                    SI 'FULL_MECA' OU 'RIGI_MECA_TANG'
!     OUT BTSIG : DIV (SIGMA)
!                    SI 'FULL_MECA' OU 'RAPH_MECA'
!     OUt CODRET : CODE RETOUR DE L'INTEGRATION INTEGRATION DU
!                  0 => PAS DE PROBLEME
!                  1 => ABSENCE DE CONVERGENCE
! person_in_charge: sebastien.fayolle at edf.fr
!
!
! --------- VARIABLES LOCALES :
!  -- GENERALITES :
!  ----------------
!  CMPS DE DEPLACEMENT :
!   - MEMBRANE : DX(N1), DY(N1), DX(N2), ..., DY(NNO)
!   - FLEXION  : DZ(N1), BETAX(N1), BETAY(N1), DZ(N2), ..., BETAY(NNO)
!  CMPS DE DEFORMATION COQUE :
!   - MEMBRANE : EPSIXX,EPSIYY,2*EPSIXY
!   - FLEXION  : KHIXX,KHIYY,2*KHIXY
!  CMPS D' EFFORTS COQUE :
!   - MEMBRANE : NXX,NYY,NXY
!   - FLEXION  : MXX,MYY,MXY
!   - CISAILLEMENT : QX , QY
! --------------------------------------------------------------------
!            NPG:    NOMBRE DE POINTS DE GAUSS PAR ELEMENT
!            NC :    NOMBRE DE COTES DE L'ELEMENT
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterfort/assert.h"
#include "asterfort/coqrep.h"
#include "asterfort/crgdm.h"
#include "asterfort/dhrc_lc.h"
#include "asterfort/dhrc_recup_mate.h"
#include "asterfort/dkqbf.h"
#include "asterfort/dktbf.h"
#include "asterfort/dsqbfb.h"
#include "asterfort/dstbfb.h"
#include "asterfort/dxefro.h"
#include "asterfort/dxmate.h"
#include "asterfort/dxqbm.h"
#include "asterfort/dxqloc.h"
#include "asterfort/dxtbm.h"
#include "asterfort/dxtloc.h"
#include "asterfort/elref5.h"
#include "asterfort/glrcmm.h"
#include "asterfort/gquad4.h"
#include "asterfort/gtria3.h"
#include "asterfort/jevech.h"
#include "asterfort/jquad4.h"
#include "asterfort/lcgldm.h"
#include "asterfort/maglrc.h"
#include "asterfort/nmcoup.h"
#include "asterfort/pmrvec.h"
#include "asterfort/q4gbc.h"
#include "asterfort/r8inir.h"
#include "asterfort/t3gbc.h"
#include "asterfort/tecach.h"
#include "asterfort/utbtab.h"
#include "asterfort/utctab.h"
#include "asterfort/utmess.h"
!
    real(kind=8) :: poids
!            POIDS:  POIDS DE GAUSS (Y COMPRIS LE JACOBIEN)
!            AIRE:   SURFACE DE L'ELEMENT
    real(kind=8) :: um(2, 4), uf(3, 4), dum(2, 4), duf(3, 4)
!            UM:     DEPLACEMENT (MEMBRANE) "-"
!            UF:     DEPLACEMENT (FLEXION)  "-"
!           DUM:     INCREMENT DEPLACEMENT (MEMBRANE)
!           DUF:     INCREMENT DEPLACEMENT (FLEXION)
    real(kind=8) :: eps(3), khi(3), gam(2), deps(6), dkhi(3), dgam(2), n(3)
    real(kind=8) :: m(3), q(2)
!            EPS:    DEFORMATION DE MEMBRANE "-"
!            DEPS:   INCREMENT DE DEFORMATION DE MEMBRANE
!            KHI:    DEFORMATION DE FLEXION  "-" (COURBURE)
!            DKHI:   INCREMENT DE DEFORMATION DE FLEXION (COURBURE)
!            GAM :   DISTORSIONS TRANSVERSES
!            DGAM :  INCREMENT DES DISTORSIONS TRANSVERSES
!            N  :    EFFORT NORMAL "+"
!            M  :    MOMENT FLECHISSANT "+"
!            Q  :    CISAILLEMENT TRANSVERSE
!
    real(kind=8) :: sigmam(32), efform(32), effint(32), efforp(32)
!            SIGMAM : EFFORTS DANS LE REPERE UTILISATEUR  A T "-"
!            EFFORM : EFFORTS DANS LE REPERE DE L'ELEMENT A T "-"
!            EFFINT : EFFORTS DANS LE REPERE DE L'ELEMENT A T "+"
!            EFFORP : EFFORTS DANS LE REPERE UTILISATEUR  A T "+"
!
    real(kind=8) :: df(9), dm(9), dmf(9), dcc(2, 2), dc(2, 2)
    real(kind=8) :: dff(9), dmm(9), dmff(9)
!            DF :    MATRICE DE RIGIDITE TANGENTE MATERIELLE  (FLEXION)
!            DM :    MATRICE DE RIGIDITE TANGENTE MATERIELLE  (MEMBRANE)
!            DMF:    MATRICE DE RIGIDITE TANGENTE MATERIELLE  (COUPLAGE)
!            DC:     MATRICE DE RIGIDITE ELASTIQUE MATERIELLE
!                                                         (CISAILLEMENT)
!
    real(kind=8) :: dci(4), dmc(6), dfc(6)
!
    real(kind=8) :: bf(3, 3*4), bm(3, 2*4), bmq(2, 3), bc(2, 3*4)
!            BF :    MATRICE "B" (FLEXION)
!            BM :    MATRICE "B" (MEMBRANE)
!            BC :    MATRICE "B" (CISAILLEMENT)
    real(kind=8) :: flex(3*4, 3*4), memb(2*4, 2*4), flexi(3*4, 3*4)
    real(kind=8) :: mefl(2*4, 3*4), work(3, 3*4)
!           MEMB:    MATRICE DE RIGIDITE DE MEMBRANE
!           FLEX:    MATRICE DE RIGIDITE DE FLEXION
!           WORK:    TABLEAU DE TRAVAIL
!           MEFL:    MATRICE DE COUPLAGE MEMBRANE-FLEXION
!           LE MATERIAU EST SUPPOSE HOMOGENE
!
    real(kind=8) :: t2iu(4), t2ui(4), t1ve(9), c, s
!
    logical :: t3g, q4g
    logical :: leul, lrgm
    logical :: lbid, vecteu, matric
    logical :: q4gg
    logical :: coupmf, ther
!
    integer :: codret
    integer :: ndim, nno, nnos, npg, ipoids, icoopg, ivf, idfdx
    integer :: idfd2, jgano
    integer :: imate, iret, icontm, ivarim
    integer :: icompo, icacoq, icontp, ivarip, ino, nbcon
    integer :: nbvar, ipg
    integer :: i, j, k, l
    integer :: icpg, icpv
    integer :: icara, jtab(7), nbsig
    integer :: multic
!
    real(kind=8) :: xyzl(3, 4), ktan((6*4)*(6*4+1)/2), btsig(6, 4)
    real(kind=8) :: ul(6, 4), dul(6, 4), pgl(3, 3), crit(*)
    real(kind=8) :: delas(6, 6), dsidep(6, 6)
    real(kind=8) :: lambda, deuxmu, deumuf, lamf, gt, gc, gf, seuil, alphaf
    real(kind=8) :: r8bid, tref, dtmoy, dtgra, alphat, depsth, dkhith, epsth,win(1),wout(1)
    real(kind=8) :: khith
    real(kind=8) :: alpha, beta
! VARIABLES POUR DHRC
    real(kind=8) :: a0(6, 6), b0(6, 2), c0(2, 2, 2)
    real(kind=8) :: aa_t(6, 6, 2), ab_(6, 2, 2), ac_(2, 2, 2), aa_c(6, 6, 2)
    real(kind=8) :: ga_t(6, 6, 2), gb_(6, 2, 2), gc_(2, 2, 2), ga_c(6, 6, 2)
    real(kind=8) :: cstseu(2)
! ATTENTION LA TAILLE DE ECP DEPEND DU NOMBRE DE VARIABLE INTERNE
! LORS DE L AJOUT DE VARIABLE INTERNE IL FAUT INCREMENTER ECR ET ECRP
    real(kind=8) :: epst(6), ep, surfgp, sig(8), dsig(8), ecr(24), ecrp(24)
    real(kind=8) :: epsm(6), qsi, eta, ctor
    real(kind=8) :: carat3(21), jacob(5), caraq4(25)
    real(kind=8) :: matr(50), sigm(8), alfmc, tmoy, tgra
!
    character(len=8) :: k8bid
    character(len=16) :: option, nomte, compor(*)
    character(len=24) :: valk(2)
!
    codret = 0
    nbsig  = 6
    q4gg   = .false.
    t3g    = .false.
    q4g    = .false.
    leul   = .false.
!
    if (nomte(1:8) .eq. 'MEDKTG3 ' .or. nomte(1:8) .eq. 'MET3GG3 ') then
        t3g = .true.
    else if(nomte(1:8).eq.'MEDKQG4 ' .or. nomte(1:8).eq.'MEQ4GG4 ') then
        q4g = .true.
    else
        valk(1) = nomte
        valk(2) = option
        call utmess('F', 'CALCULEL3_27', nk=2, valk=valk)
    endif
!
    if (nomte(1:8) .eq. 'MEQ4GG4 ' .or. nomte(1:8) .eq. 'MET3GG3 ') then
        q4gg = .true.
        nbsig = 8
    endif
!
    vecteu = ((option.eq.'FULL_MECA') .or. (option.eq.'RAPH_MECA'))
    matric = ((option.eq.'FULL_MECA') .or. (option(1:9).eq.'RIGI_MECA'))
    lrgm = option.eq.'RIGI_MECA     '
!
    call elref5(' ', 'RIGI', ndim, nno, nnos, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano)
!
    call r8inir(8, 0.d0, sig, 1)
    call r8inir(8, 0.d0, dsig, 1)
!
    call jevech('PMATERC', 'L', imate)
    call jevech('PCACOQU', 'L', icacoq)
!
    if (.not. lrgm) then
        call tecach('OON', 'PCONTMR', 'L', iret, nval=7,&
                    itab=jtab)
        icontm=jtab(1)
        ASSERT(npg.eq.jtab(3))
        call jevech('PVARIMR', 'L', ivarim)
        call jevech('PCOMPOR', 'L', icompo)
        leul = zk16(icompo+2).eq.'GROT_GDEP'
    else
        ivarim=1
        icompo=1
        icontm=1
    endif
!
    if (vecteu) then
        call jevech('PCONTPR', 'E', icontp)
        call jevech('PVARIPR', 'E', ivarip)
    else
        icontp = icontm
        ivarip = ivarim
    endif
!
!     GRANDEURS GEOMETRIQUES :
!
    if (nno .eq. 3) then
        call gtria3(xyzl, carat3)
    else if (nno.eq.4) then
        call gquad4(xyzl, caraq4)
    endif
!
    ctor = zr(icacoq+3)
!
!     MISES A ZERO :
!
    if (matric) then
        call r8inir((3*nno)*(3*nno), 0.d0, flexi, 1)
        call r8inir((3*nno)*(3*nno), 0.d0, flex,  1)
        call r8inir((2*nno)*(2*nno), 0.d0, memb,  1)
        call r8inir((2*nno)*(3*nno), 0.d0, mefl,  1)
    endif
!
    if (vecteu) then
        call r8inir(6*nno, 0.d0, btsig, 1)
        call r8inir(   32, 0.d0, effint, 1)
        call r8inir(   32, 0.d0, efforp, 1)
    endif
!
    call r8inir(36, 0.d0,  delas, 1)
    call r8inir(32, 0.d0, sigmam, 1)
    call r8inir(32, 0.d0, efform, 1)
!
!     PARTITION DU DEPLACEMENT EN MEMBRANE/FLEXION :
!
    do ino = 1, nno
        um(1,ino)  =  ul(1,ino)
        um(2,ino)  =  ul(2,ino)
        uf(1,ino)  =  ul(3,ino)
        uf(2,ino)  =  ul(5,ino)
        uf(3,ino)  = -ul(4,ino)
        dum(1,ino) = dul(1,ino)
        dum(2,ino) = dul(2,ino)
        duf(1,ino) = dul(3,ino)
        duf(2,ino) = dul(5,ino)
        duf(3,ino) =-dul(4,ino)
    end do
!
!     INTEGRATION SUR LA SURFACE DE L'ELEMENT:
!
!     CONTRAINTE 2D : NXX,NYY,NXY,MXX,MYY,MXY,QX,QY
    nbcon = 8
!
!     NBVAR: NOMBRE DE VARIABLES INTERNES (2D) LOI COMPORTEMENT
    if (lrgm) then
        nbvar = 0
    else
        read (zk16(icompo-1+2),'(I16)') nbvar
        call tecach('OON', 'PVARIMR', 'L', iret, nval=7,&
                    itab=jtab)
! SIGMAM : EFFORTS DANS REPERE UTILISATEUR
!
        do i = 1, nbcon*npg
            sigmam(i)=zr(icontm-1+i)
        end do
!
! --- CALCUL DES MATRICES DE CHANGEMENT DE REPERE
!
!     T2IU : MATRICE DE PASSAGE (2x2) ; UTILISATEUR -> INTRINSEQUE
!     T2UI : MATRICE DE PASSAGE (2x2) ; INTRINSEQUE -> UTILISATEUR
!
        call jevech('PCACOQU', 'L', icara)
        alpha = zr(icara+1) * r8dgrd()
        beta  = zr(icara+2) * r8dgrd()
        call coqrep(pgl, alpha, beta, t2iu, t2ui, c, s)
!
! --- PASSAGE DES EFFORTS GENERALISES AUX POINTS D'INTEGRATION
!     DU REPERE UTILISATEUR AU REPERE INTRINSEQUE
!
        call dxefro(npg, t2ui, sigmam, efform)
    endif
!
!     ON VERIFIE QUE LE NOMBRE DE VARINT TIENT DANS ECR
    ASSERT(nbvar.le.24)
!
!     BOUCLE SUR LES POINTS DE GAUSS DE LA SURFACE:
!
    do ipg = 1, npg
        call r8inir(24, 0.d0, ecrp, 1)
        call r8inir( 3, 0.d0, n   , 1)
        call r8inir( 3, 0.d0, m   , 1)
        call r8inir( 2, 0.d0, q   , 1)
        call r8inir( 9, 0.d0, df  , 1)
        call r8inir( 9, 0.d0, dm  , 1)
        call r8inir( 9, 0.d0, dmf , 1)
        call r8inir( 4, 0.d0, dc  , 1)
!
        qsi = zr(icoopg-1+ndim*(ipg-1)+1)
        eta = zr(icoopg-1+ndim*(ipg-1)+2)
!
        icpg = (ipg-1)*nbcon
        icpv = (ipg-1)*nbvar
!
        if (nomte(1:8) .eq. 'MEDKTG3 ') then
            call dxtbm(carat3(9), bm)
            call dktbf(qsi, eta, carat3, bf)
            poids = zr(ipoids+ipg-1)*carat3(7)
        else if (nomte(1:8).eq.'MEDKQG4 ') then
            call jquad4(xyzl, qsi, eta, jacob)
            call dxqbm(qsi, eta, jacob(2), bm)
            call dkqbf(qsi, eta, jacob(2), caraq4, bf)
            poids = zr(ipoids+ipg-1)*jacob(1)
        else if (nomte(1:8).eq.'MEQ4GG4 ') then
            call jquad4(xyzl, qsi, eta, jacob)
            call dxqbm(qsi, eta, jacob(2), bm)
            call dsqbfb(qsi, eta, jacob(2), bf)
            call q4gbc(qsi, eta, jacob(2), caraq4, bc)
            poids = zr(ipoids+ipg-1)*jacob(1)
        else if (nomte(1:8).eq.'MET3GG3 ') then
            call dxtbm(carat3(9), bm)
            call dstbfb(carat3(9), bf)
            call t3gbc(xyzl, qsi, eta, bc)
            poids = carat3(8)
        endif
!
        call pmrvec('ZERO', 3, 2*nno, bm, um, eps)
        call pmrvec('ZERO', 3, 2*nno, bm, dum, deps)
        call pmrvec('ZERO', 3, 3*nno, bf, uf, khi)
        call pmrvec('ZERO', 3, 3*nno, bf, duf, dkhi)
        if (q4gg) then
            call pmrvec('ZERO', 2, 3*nno, bc, uf, gam)
            call pmrvec('ZERO', 2, 3*nno, bc, duf, dgam)
        endif
!
        call jevech('PCACOQU', 'L', icara)
!
!     EPAISSEUR TOTALE :
        ep = zr(icara)
!     EULER_ALMANSI - TERMES QUADRATIQUES
        if (leul) then
            call r8inir(6, 0.d0, bmq, 1)
!
            do i = 1, 2
                do k = 1, nno
                    do j = 1, 2
                        bmq(i,j) = bmq(i,j) + bm(i,2*(k-1)+i)*dum(j,k)
                    end do
                    bmq(i,3) = bmq(i,3) + bm(i,2*(k-1)+i)*duf(1,k)
                end do
            end do
!
            do k = 1, 3
                do i = 1, 2
                    deps(i) = deps(i) - 0.5d0*bmq(i,k)*bmq(i,k)
                end do
                deps(3) = deps(3) - bmq(1,k)*bmq(2,k)
            end do
        endif
!
        call r8inir(50, 0.d0, matr, 1)
!
        if (.not. lrgm) then
            do i = 1, 3
                epst(i)   = eps(i) + deps(i)
                epst(i+3) = khi(i) + dkhi(i)
                deps(i+3) = dkhi(i)
            end do
!
            do i = 1, 3
                epsm(i)   = eps(i)
                epsm(i+3) = khi(i)
            end do
!
            do i = 1, nbsig
                sig(i)  = efform(icpg + i)
                sigm(i) = sig(i)
            end do
        endif
!
        if (compor(1)(1:4) .eq. 'ELAS') then
            call dxmate('RIGI', dff, dmm, dmff, dcc, dci, dmc, dfc, nno, pgl, multic, coupmf,&
                        t2iu, t2ui, t1ve)
            call r8inir(36, 0.d0, dsidep, 1)
! -- MEMBRANE
            dsidep(1,1) = dmm(1)
            dsidep(2,1) = dmm(2)
            dsidep(1,2) = dmm(4)
            dsidep(2,2) = dmm(5)
            dsidep(3,3) = dmm(9)
! -- FLEXION
            dsidep(4,4) = dff(1)
            dsidep(5,4) = dff(2)
            dsidep(4,5) = dff(4)
            dsidep(5,5) = dff(5)
            dsidep(6,6) = dff(9)
!
! - CALCUL DE L'ACCROISSEMENT DE CONTRAINTE
!
            do i = 1, 6
                dsig(i) = 0.d0
                do j = 1, 6
                    dsig(i) = dsig(i)+dsidep(i,j)*deps(j)
                end do
            end do
!
! CALCUL DE L'ACCOISSEMENT EFFORT CISAILLEMENT
!
!
            if (q4gg) then
                dsig(7) = dcc(1,1)*dgam(1)
                dsig(8) = dcc(2,2)*dgam(2)
            endif
!
            do i = 1, nbsig
                sig(i) = sig(i) + dsig(i)
            end do
!
        else if (compor(1)(1:11).eq. 'GLRC_DAMAGE') then
            do i = 1, nbvar
                ecr(i) = zr(ivarim-1 + icpv + i)
            end do
!
            call maglrc(zi(imate), matr, delas, ecr)
            if (q4gg) then
                dcc(1,1) = matr(14)
                dcc(2,2) = matr(15)
                dcc(1,2) = 0.d0
                dcc(2,1) = 0.d0
            endif
!
!   AIRE DE SURFACE APPARTENANT AU POINT DE G.
            surfgp = poids
!
            call glrcmm(zi(imate), matr, ep, surfgp, pgl, epst, deps, dsig, ecr, delas,&
                        dsidep, crit, codret)
!
            do i = 1, 3
                dsig(i)   = dsig(i)*ep
                dsig(3+i) = dsig(3+i)*ep*ep/6.d0
            end do
!
            do i = 1, nbvar
                ecrp(i) = ecr(i)
            end do
!
            if (q4gg) then
                dsig(7) = dcc(1,1)*dgam(1)
                dsig(8) = dcc(2,2)*dgam(2)
            endif
!
            do i = 1, nbsig
                sig(i) = sig(i) + dsig(i)
            end do
!
        else if (compor(1)(1:7).eq. 'GLRC_DM') then
            if (.not. lrgm) then
                do i = 1, nbvar
                    ecr(i) = zr(ivarim-1 + icpv + i)
                end do
            endif
!
            call crgdm(zi(imate), compor(1), lambda, deuxmu, lamf, deumuf, gt, gc, gf, seuil,&
                       alphaf, alfmc, ep, lrgm, ipg, ther, tref, dtmoy, dtgra, tmoy, tgra, alphat)
!
!     CALCUL DE LA DEFORMATION THERMIQUE
            if (ther) then
                epsth = alphat * (tmoy-tref)
                khith = alphat * tgra
                do i = 1, 2
                    epsm(i)   = epsm(i)   - epsth
                    epsm(i+3) = epsm(i+3) - khith
                end do
                depsth = alphat * dtmoy
                dkhith = alphat * dtgra
                do i = 1, 2
                    deps(i)   = deps(i)   - depsth
                    deps(i+3) = deps(i+3) - dkhith
                end do
            endif
!
!     ENDOMMAGEMENT SEULEMENT
!
            call r8inir(36, 0.d0, dsidep, 1)
            call lcgldm(epsm, deps, ecr, option, sig, ecrp, dsidep, lambda, deuxmu, lamf, deumuf,&
                        gt, gc, gf, seuil, alphaf, alfmc, crit, codret)
!
        else if (compor(1)(1:4).eq. 'DHRC') then
!
!     LECTURE PARAMETRES MATERIAU
!
            if (.not. lrgm) then
                do i = 1, nbvar
                    ecr(i) = zr(ivarim-1 + icpv + i)
                end do
            endif
!
            call dhrc_recup_mate(zi(imate), compor(1), ep, a0, b0, c0,&
                                    aa_t, ga_t, ab_, gb_, ac_, gc_, aa_c, ga_c, cstseu)
!
!     ENDOMMAGEMENT COUPLÉ PLASTICITÉ
!
            call r8inir(36, 0.d0, dsidep, 1)
            call dhrc_lc(epsm, deps, ecr, pgl, option, sig, ecrp, a0, b0, c0,&
                        aa_t, ga_t, ab_, gb_, ac_, gc_, aa_c, ga_c, cstseu, crit, codret, dsidep)
!
        else if (compor(1)(1:7).eq. 'KIT_DDI') then
!     ENDOMMAGEMENT PLUS PLASTICITE
!
            if (.not. lrgm) then
                do i = 1, nbvar
                    ecr(i) = zr(ivarim-1 + icpv + i)
                end do
            endif
!
            call nmcoup('RIGI', ipg, 1, 3, k8bid, zi(imate), compor, lbid, crit, r8bid,&
                        r8bid, 6, epsm, deps, 6, sigm, ecr, option, 1, win,&
                        sig, ecrp, 36, dsidep, 1, wout, codret)
        else
            valk(1) = compor(1)
            call utmess('F', 'ELEMENTS4_79', nk=1, valk=valk)
        endif
!
        if (.not. lrgm) then
            do i = 1, nbvar
                zr(ivarip-1 + icpv + i) = ecrp(i)
            end do
        endif
!
!     EFFORTS RESULTANTS (N ET M)
!
        if (vecteu) then
            do i = 1, 3
                n(i) = sig(i)
                m(i) = sig(i+3)
            end do
            do i = 1, 2
                q(i) = sig(i+6)
            end do
        endif
!
!     CALCUL DES MATRICES TANGENTES MATERIELLES (DM,DF,DMF):
        if (matric) then
            l = 0
!
            do i = 1, 3
                do j = 1, 3
                    l = l + 1
                    dm(l) = dm(l) + poids*dsidep(j,i)
                    dmf(l)= dmf(l) + poids*dsidep(j,i+3)
                    df(l) = df(l) + poids*dsidep(j+3,i+3)
                end do
            end do
        endif
!
        if (q4gg) then
            dc(1,1) = poids*dcc(1,1)
            dc(2,2) = dc(1,1)
            dc(1,2) = 0.d0
            dc(2,1) = 0.d0
        endif
!
!     CALCUL DE DIV(SIGMA) ET RECOPIE DE N ET M DANS 'PCONTPR':
!
!     BTSIG = BTSIG + BFT*M + BMT*N + BCT*Q
        if (vecteu) then
            do k = 1, 3
                effint((ipg-1)*8+k) = n(k)
                effint((ipg-1)*8+3+k) = m(k)
            end do
!
            if (q4gg) then
                do k = 1, 2
                    effint((ipg-1)*8+6+k) = q(k)
                end do
            endif
            do ino = 1, nno
                do k = 1, 3
                    btsig(1,ino) = btsig(1,ino) + bm(k,2* (ino-1)+1)* n(k)*poids
                    btsig(2,ino) = btsig(2,ino) + bm(k,2* (ino-1)+2)* n(k)*poids
                    btsig(3,ino) = btsig(3,ino) + bf(k,3* (ino-1)+1)* m(k)*poids
                    btsig(5,ino) = btsig(5,ino) + bf(k,3* (ino-1)+2)* m(k)*poids
                    btsig(4,ino) = btsig(4,ino) - bf(k,3* (ino-1)+3)* m(k)*poids
                end do
!
! PRISE EN COMPTE DU CISAILLEMENT
!
                if (q4gg) then
                    do k = 1, 2
                        btsig(3,ino) = btsig(3,ino) + bc(k,3* (ino-1)+ 1)*q(k)*poids
                        btsig(5,ino) = btsig(5,ino) + bc(k,3* (ino-1)+ 2)*q(k)*poids
                        btsig(4,ino) = btsig(4,ino) - bc(k,3* (ino-1)+ 3)*q(k)*poids
                    end do
                endif
            end do
        endif
!     CALCUL DE LA MATRICE TANGENTE :
!
!     KTANG = KTANG + BFT*DF*BF + BMT*DM*BM + BMT*DMF*BF
!                   + BCT*DC*BC
        if (matric) then
!     MEMBRANE :
            call utbtab('CUMU', 3, 2*nno, dm, bm, work, memb)
!
!     FLEXION :
            call utbtab('CUMUL', 3, 3*nno, df, bf, work, flex)
!
!     CISAILLEMENT:
            if (q4gg) then
                call utbtab('CUMUL', 2, 3*nno, dc, bc, work, flex)
            endif
!     COUPLAGE:
            call utctab('CUMUL', 3, 3*nno, 2*nno, dmf, bf, bm, work, mefl)
        endif
!
!     FIN BOUCLE SUR LES POINTS DE GAUSS
    end do
!
    if (.not.lrgm) then
        if (compor(1)(1:7) .eq. 'GLRC_DM') then
            do i = 1, nbcon*npg
                zr(icontp-1+i) = effint(i)
            end do
        else
!
! --- PASSAGE DES EFFORTS GENERALISES AUX POINTS D'INTEGRATION
!     DU REPERE INTRINSEQUE AU REPERE LOCAL
!     STOCKAGE DES EFFORTS GENERALISES
!
            call dxefro(npg, t2iu, effint, efforp)
            do i = 1, nbcon*npg
                zr(icontp-1+i) = efforp(i)
            end do
        endif
    endif
!
!     ACCUMULATION DES SOUS MATRICES DANS KTAN :
    if (matric) then
        if (t3g) then
            call dxtloc(flex, memb, mefl, ctor, ktan)
        else if(q4g) then
            call dxqloc(flex, memb, mefl, ctor, ktan)
        endif
    endif
!
end subroutine
