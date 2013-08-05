subroutine dktnli(nomte, opt, xyzl, ul, dul,&
                  btsig, ktan, codret)
    implicit  none
#include "jeveux.h"
!
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/dkqbf.h"
#include "asterfort/dktbf.h"
#include "asterfort/dxqbm.h"
#include "asterfort/dxqloc.h"
#include "asterfort/dxtbm.h"
#include "asterfort/dxtloc.h"
#include "asterfort/elref5.h"
#include "asterfort/gquad4.h"
#include "asterfort/gtria3.h"
#include "asterfort/jevech.h"
#include "asterfort/jquad4.h"
#include "asterfort/nmcomp.h"
#include "asterfort/pmrvec.h"
#include "asterfort/r8inir.h"
#include "asterfort/tecach.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/utbtab.h"
#include "asterfort/utctab.h"
#include "blas/dcopy.h"
    integer :: codret
    real(kind=8) :: xyzl(3, *), ul(6, *), dul(6, *)
    real(kind=8) :: ktan(*), btsig(6, *)
    character(len=16) :: nomte, opt
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     ------------------------------------------------------------------
!     CALCUL DES OPTIONS NON LINEAIRES POUR L'ELEMENT DE PLAQUE DKT
!     TOUTES LES ENTREES ET LES SORTIES SONT DANS LE REPERE LOCAL.
!     (MEME SI LES TABLEAUX SONT DIMMENSIONNES EN 3D)
!     ------------------------------------------------------------------
!     IN  OPT  : OPTION NON LINEAIRE A CALCULER
!                  'RAPH_MECA'
!                  'FULL_MECA'      OU 'FULL_MECA_ELAS'
!                  'RIGI_MECA_TANG' OU 'RIGI_MECA_ELAS'
!     IN  XYZL : COORDONNEES DES NOEUDS
!     IN  UL   : DEPLACEMENT A L'INSTANT T "-"
!     IN  DUL  : INCREMENT DE DEPLACEMENT
!     IN  PGL  : MATRICE DE PASSAGE GLOBAL - LOCAL ELEMENT
!     OUT KTAN : MATRICE DE RIGIDITE TANGENTE
!                    SI 'FULL_MECA' OU 'RIGI_MECA_TANG'
!     OUT BTSIG: DIV (SIGMA)
!                    SI 'FULL_MECA' OU 'RAPH_MECA'
!     ------------------------------------------------------------------
!     CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
!
    character(len=8) :: typmod(2)
!CC      PARAMETER (NNO=3)  POUR LES DKT
!CC      PARAMETER (NNO=4)  POUR LES DKQ
    integer :: nno
    parameter (nno=4)
!            NNO:    NOMBRE DE NOEUDS DE L'ELEMENT
    real(kind=8) :: distn, angmas(3), wk
!
! --------- VARIABLES LOCALES :
!  -- GENERALITES :
!  ----------------
!  CMPS DE DEPLACEMENT :
!   - MEMBRANE : DX(N1), DY(N1), DX(N2), ..., DY(NNO)
!   - FLEXION  : DZ(N1), BETAX(N1), BETAY(N1), DZ(N2), ..., BETAY(NNO)
!  CMPS DE DEFORMATION ET CONTRAINTE PLANE (DANS UNE COUCHE) :
!   -            EPSIXX,EPSIYY,2*EPSIXY
!   -            SIGMXX,SIGMYY,SIGMXY
!  CMPS DE DEFORMATION ET CONTRAINTE PLANE POUR APPEL NMCOMP :
!   -            EPSIXX,EPSIYY,EPSIZZ,SQRT(2)*EPSIXY
!   -            SIGMXX,SIGMYY,SIGMZZ,SQRT(2)*SIGMXY
!  CMPS DE DEFORMATION COQUE :
!   - MEMBRANE : EPSIXX,EPSIYY,2*EPSIXY
!   - FLEXION  : KHIXX,KHIYY,2*KHIXY
!  CMPS D' EFFORTS COQUE :
!   - MEMBRANE : NXX,NYY,NXY
!   - FLEXION  : MXX,MYY,MXY
! --------------------------------------------------------------------
    integer :: nbcou, npgh, jnbspi
!            NBCOU:  NOMBRE DE COUCHES (INTEGRATION DE LA PLASTICITE)
!            NPG:    NOMBRE DE POINTS DE GAUSS PAR ELEMENT
!            NC :    NOMBRE DE COTES DE L'ELEMENT
!            NPGH:   NOMBRE DE POINT D'INTEGRATION PAR COUCHE
    real(kind=8) :: poids, hic, h, zic, zmin, instm, instp, coef
!            POIDS:  POIDS DE GAUSS (Y COMPRIS LE JACOBIEN)
!            AIRE:   SURFACE DE L'ELEMENT
!            HIC:    EPAISSEUR D'UNE COUCHE
!            H :     EPAISSEUR TOTALE DE LA PLAQUE
!            ZIC:    COTE DU POINT D'INTEGRATION DANS L'EPAISSEUR
!            ZMIN:   COTE DU POINT D'INTEGRATION LE PLUS "BAS"
!            INSTM:  INSTANT "-"
!            INSTP:  INSTANT "+"
!            COEF:   POIDS D'INTEGRATION PAR COUCHE
    real(kind=8) :: um(2, nno), uf(3, nno), dum(2, nno), duf(3, nno)
!            UM:     DEPLACEMENT (MEMBRANE) "-"
!            UF:     DEPLACEMENT (FLEXION)  "-"
!           DUM:     INCREMENT DEPLACEMENT (MEMBRANE)
!           DUF:     INCREMENT DEPLACEMENT (FLEXION)
    real(kind=8) :: eps2d(6), deps2d(6), dsidep(6, 6)
!            EPS2D:  DEFORMATION DANS UNE COUCHE (2D C_PLAN)
!           DEPS2D:  INCREMENT DEFORMATION DANS UNE COUCHE (2D C_PLAN)
!            SIG2D:  CONTRAINTE DANS UNE COUCHE (2D C_PLAN)
!           DSIDEP:  MATRICE D(SIG2D)/D(EPS2D)
    real(kind=8) :: eps(3), khi(3), deps(3), dkhi(3), n(3), m(3), sigm(4)
!            EPS:    DEFORMATION DE MEMBRANE "-"
!            DEPS:   INCREMENT DE DEFORMATION DE MEMBRANE
!            KHI:    DEFORMATION DE FLEXION  "-"
!            DKHI:   INCREMENT DE DEFORMATION DE FLEXION
!            N  :    EFFORT NORMAL "+"
!            M  :    MOMENT FLECHISSANT "+"
!            SIGM : CONTRAINTE "-"
    real(kind=8) :: df(9), dm(9), dmf(9), d2d(9)
!            D2D:    MATRICE DE RIGIDITE TANGENTE MATERIELLE (2D)
!            DF :    MATRICE DE RIGIDITE TANGENTE MATERIELLE (FLEXION)
!            DM :    MATRICE DE RIGIDITE TANGENTE MATERIELLE (MEMBRANE)
!            DMF:    MATRICE DE RIGIDITE TANGENTE MATERIELLE (COUPLAGE)
    real(kind=8) :: bf(3, 3*nno), bm(3, 2*nno), bmq(2, 3)
!            BF :    MATRICE "B" (FLEXION)
!            BM :    MATRICE "B" (MEMBRANE)
    real(kind=8) :: flex(3*nno*3*nno), memb(2*nno*2*nno)
    real(kind=8) :: mefl(2*nno*3*nno), work(3*nno*3*nno)
!           MEMB:    MATRICE DE RIGIDITE DE MEMBRANE
!           FLEX:    MATRICE DE RIGIDITE DE FLEXION
!           WORK:    TABLEAU DE TRAVAIL
!           MEFL:    MATRICE DE COUPLAGE MEMBRANE-FLEXION
!             LE MATERIAU EST SUPPOSE HOMOGENE
!             IL PEUT NEANMOINS Y AVOIR COUPLAGE PAR LA PLASTICITE
!     ------------------ PARAMETRAGE ELEMENT ---------------------------
    integer :: ndim, nnoel, nnos, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano
    integer :: jtab(7), cod, i, ksp
    integer :: icacoq, icarcr, icompo, icontm, icontp, icou, icpg, igauh, iinstm
    integer :: iinstp, imate, ino, ipg, iret, isp, ivarim, ivarip, ivarix, ivpg
    integer :: j, k, nbcon, nbsp, nbvar, ndimv
    real(kind=8) :: deux, rac2, qsi, eta, cara(25), jacob(5)
    real(kind=8) :: ctor, coehsd
    real(kind=8) :: lc
    logical :: vecteu, matric, dkt, dkq, leul
!     ------------------------------------------------------------------
!
    call elref5(' ', 'RIGI', ndim, nnoel, nnos,&
                npg, ipoids, icoopg, ivf, idfdx,&
                idfd2, jgano)
!
    deux = 2.d0
    rac2 = sqrt(deux)
    codret = 0
!
!     2 BOOLEENS COMMODES :
!     ---------------------
    vecteu = ((opt(1:9).eq.'FULL_MECA').or.(opt.eq.'RAPH_MECA'))
    matric = ((opt(1:9).eq.'FULL_MECA').or.(opt(1:9).eq.'RIGI_MECA'))
!     RECUPERATION DES OBJETS &INEL ET DES CHAMPS PARAMETRES :
!     --------------------------------------------------------
    dkt = .false.
    dkq = .false.
!
    if (nomte .eq. 'MEDKTR3 ') then
        dkt = .true.
    else if (nomte.eq.'MEDKQU4 ') then
        dkq = .true.
    else
        call u2mesk('F', 'ELEMENTS_34', 1, nomte)
    endif
!
    call jevech('PMATERC', 'L', imate)
!
    call tecach('OON', 'PCONTMR', 'L', 7, jtab,&
                iret)
    nbsp=jtab(7)
    icontm=jtab(1)
    ASSERT(npg.eq.jtab(3))
!
    call jevech('PVARIMR', 'L', ivarim)
    call jevech('PINSTMR', 'L', iinstm)
    call jevech('PINSTPR', 'L', iinstp)
    instm = zr(iinstm)
    instp = zr(iinstp)
!
    call jevech('PCOMPOR', 'L', icompo)
    call jevech('PCARCRI', 'L', icarcr)
    call jevech('PCACOQU', 'L', icacoq)
!
    leul = zk16(icompo+2).eq.'GROT_GDEP'
    if (leul .and. zk16(icompo)(1:4) .ne. 'ELAS') then
        call u2mess('F', 'ELEMENTS2_73')
    endif
!
    if (vecteu) then
        call jevech('PCONTPR', 'E', icontp)
        call jevech('PVARIPR', 'E', ivarip)
    else
!       -- POUR AVOIR UN TABLEAU BIDON A DONNER A NMCOMP :
        ivarip = ivarim
        icontp = icontm
    endif
!
!     -- GRANDEURS GEOMETRIQUES :
!     ---------------------------
    h = zr(icacoq)
    distn = zr(icacoq+4)
    if (dkt) then
        call gtria3(xyzl, cara)
        ctor = zr(icacoq+3)
    else if (dkq) then
        call gquad4(xyzl, cara)
        ctor = zr(icacoq+3)
    endif
!
!     -- MISES A ZERO :
!     ------------------
    if (matric) then
        call r8inir((3*nnoel)* (3*nnoel), 0.d0, flex, 1)
        call r8inir((2*nnoel)* (2*nnoel), 0.d0, memb, 1)
        call r8inir((2*nnoel)* (3*nnoel), 0.d0, mefl, 1)
    endif
    if (vecteu) then
        call r8inir(6*nnoel, 0.d0, btsig, 1)
    endif
!
!     -- PARTITION DU DEPLACEMENT EN MEMBRANE/FLEXION :
!     -------------------------------------------------
    do 30,ino = 1,nnoel
    um(1,ino) = ul(1,ino)
    um(2,ino) = ul(2,ino)
    uf(1,ino) = ul(3,ino)
    uf(2,ino) = ul(5,ino)
    uf(3,ino) = -ul(4,ino)
    dum(1,ino) = dul(1,ino)
    dum(2,ino) = dul(2,ino)
    duf(1,ino) = dul(3,ino)
    duf(2,ino) = dul(5,ino)
    duf(3,ino) = -dul(4,ino)
    30 end do
!
!     -- INTEGRATIONS SUR LA SURFACE DE L'ELEMENT:
!     --------------------------------------------
!     -- POUR POUVOIR UTILISER NMCOMP
    typmod(1) = 'C_PLAN  '
    typmod(2) = '        '
    npgh = 3
!
!     CONTRAINTE 2D : SIXX,SIYY,SIZZ,SQRT(2)*SIXY
    nbcon = 6
!     NBVAR: NOMBRE DE VARIABLES INTERNES (2D) LOI COMPORT
!     NBCOU : NOMBRE DE COUCHES
    read (zk16(icompo-1+2),'(I16)') nbvar
    call jevech('PNBSP_I', 'L', jnbspi)
    nbcou=zi(jnbspi-1+1)
    call tecach('OON', 'PVARIMR', 'L', 7, jtab,&
                iret)
    if (nbcou .le. 0) call u2mesk('F', 'ELEMENTS_36', 1, zk16(icompo-1+6))
!
    hic = h/nbcou
    zmin = -h/deux + distn
    if (vecteu) then
        ndimv=npg*nbsp*nbvar
        call jevech('PVARIMP', 'L', ivarix)
        call dcopy(ndimv, zr(ivarix), 1, zr(ivarip), 1)
    endif
!
!===============================================================
!
!     -- BOUCLE SUR LES POINTS DE GAUSS DE LA SURFACE:
!     -------------------------------------------------
    do 130,ipg = 1,npg
    call r8inir(3, 0.d0, n, 1)
    call r8inir(3, 0.d0, m, 1)
    call r8inir(9, 0.d0, df, 1)
    call r8inir(9, 0.d0, dm, 1)
    call r8inir(9, 0.d0, dmf, 1)
    qsi = zr(icoopg-1+ndim*(ipg-1)+1)
    eta = zr(icoopg-1+ndim*(ipg-1)+2)
    if (dkq) then
        call jquad4(xyzl, qsi, eta, jacob)
        poids = zr(ipoids+ipg-1)*jacob(1)
        call dxqbm(qsi, eta, jacob(2), bm)
        call dkqbf(qsi, eta, jacob(2), cara, bf)
    else
        poids = zr(ipoids+ipg-1)*cara(7)
        call dxtbm(cara(9), bm)
        call dktbf(qsi, eta, cara, bf)
    endif
!       -- CALCUL DE EPS, DEPS, KHI, DKHI :
!       -----------------------------------
    call pmrvec('ZERO', 3, 2*nnoel, bm, um,&
                eps)
    call pmrvec('ZERO', 3, 2*nnoel, bm, dum,&
                deps)
    call pmrvec('ZERO', 3, 3*nnoel, bf, uf,&
                khi)
    call pmrvec('ZERO', 3, 3*nnoel, bf, duf,&
                dkhi)
!
!     -- EULER_ALMANSI - TERMES QUADRATIQUES
    if (leul) then
        call r8inir(6, 0.d0, bmq, 1)
        do 145,i = 1,2
        do 146,k = 1,nnoel
        do 142,j = 1,2
        bmq(i,j) = bmq(i,j) + bm(i,2*(k-1)+i)*dum(j,k)
142      continue
        bmq(i,3) = bmq(i,3) + bm(i,2*(k-1)+i)*duf(1,k)
146      continue
145      continue
!
        do 150, k = 1,3
        do 155, i = 1,2
        deps(i) = deps(i) - 0.5d0*bmq(i,k)*bmq(i,k)
155      continue
        deps(3) = deps(3) - bmq(1,k)*bmq(2,k)
150      continue
    endif
!
!       -- CALCUL DE L'ECOULEMENT PLASTIQUE SUR CHAQUE COUCHE:
!          PAR INTEGRATION EN TROIS POINTS
!       ------------------------------------------------------
    do 80,icou = 1,nbcou
    do 70,igauh = 1,npgh
    ksp=(icou-1)*npgh+igauh
    isp=(icou-1)*npgh+igauh
    ivpg = ((ipg-1)*nbsp + isp-1)*nbvar
    icpg = ((ipg-1)*nbsp + isp-1)*nbcon
!
!       -- COTE DES POINTS D'INTEGRATION
!       --------------------------------
    if (igauh .eq. 1) then
        zic = zmin + (icou-1)*hic
        coef = 1.d0/3.d0
    else if (igauh.eq.2) then
        zic = zmin + hic/deux + (icou-1)*hic
        coef = 4.d0/3.d0
    else
        zic = zmin + hic + (icou-1)*hic
        coef = 1.d0/3.d0
    endif
!
!         -- CALCUL DE EPS2D ET DEPS2D :
!         --------------------------
    eps2d(1) = eps(1) + zic*khi(1)
    eps2d(2) = eps(2) + zic*khi(2)
    eps2d(3) = 0.0d0
    eps2d(4) = (eps(3)+zic*khi(3))/rac2
    eps2d(5) = 0.d0
    eps2d(6) = 0.d0
    deps2d(1) = deps(1) + zic*dkhi(1)
    deps2d(2) = deps(2) + zic*dkhi(2)
    deps2d(3) = 0.0d0
    deps2d(4) = (deps(3)+zic*dkhi(3))/rac2
    deps2d(5) = 0.d0
    deps2d(6) = 0.d0
!
!
!         -- APPEL A NMCOMP POUR RESOUDRE LE PB SUR LA COUCHE :
!         -----------------------------------------------------
    do 1 j = 1, 4
        sigm(j)=zr(icontm+icpg-1+j)
 1  continue
    sigm(4)=sigm(4)*rac2
! --- ANGLE DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
! --- INITIALISE A R8VIDE (ON NE S'EN SERT PAS)
    call r8inir(3, r8vide(), angmas, 1)
    call nmcomp('RIGI', ipg, ksp, 2, typmod,&
                zi(imate), zk16(icompo), zr(icarcr), instm, instp,&
                6, eps2d, deps2d, 6, sigm,&
                zr(ivarim+ivpg), opt, angmas, 1, lc,&
                zr(icontp+ icpg), zr(ivarip+ivpg), 36, dsidep, 1,&
                wk, cod)
!
!            DIVISION DE LA CONTRAINTE DE CISAILLEMENT PAR SQRT(2)
!            POUR STOCKER LA VALEUR REELLE
!
    zr(icontp+icpg+3)=zr(icontp+icpg+3)/rac2
!
!           COD=1 : ECHEC INTEGRATION LOI DE COMPORTEMENT
!           COD=3 : C_PLAN DEBORST SIGZZ NON NUL
    if (cod .ne. 0) then
        if (codret .ne. 1) then
            codret=cod
        endif
    endif
!
!         -- CALCUL DES EFFORTS RESULTANTS DANS L'EPAISSEUR (N ET M) :
!         ------------------------------------------------------------
    if (vecteu) then
        coehsd = coef*hic/deux
        n(1) = n(1) + coehsd*zr(icontp+icpg-1+1)
        n(2) = n(2) + coehsd*zr(icontp+icpg-1+2)
        n(3) = n(3) + coehsd*zr(icontp+icpg-1+4)
        m(1) = m(1) + coehsd*zic*zr(icontp+icpg-1+1)
        m(2) = m(2) + coehsd*zic*zr(icontp+icpg-1+2)
        m(3) = m(3) + coehsd*zic*zr(icontp+icpg-1+4)
    endif
!
!         -- CALCUL DES MATRICES TANGENTES MATERIELLES (DM,DF,DMF):
!         ---------------------------------------------------------
    if (matric) then
!           -- ON EXTRAIT DE DSIDEP LA SOUS-MATRICE INTERESSANTE D2D:
        d2d(1) = dsidep(1,1)
        d2d(2) = dsidep(1,2)
        d2d(3) = dsidep(1,4)/rac2
        d2d(4) = dsidep(2,1)
        d2d(5) = dsidep(2,2)
        d2d(6) = dsidep(2,4)/rac2
        d2d(7) = dsidep(4,1)/rac2
        d2d(8) = dsidep(4,2)/rac2
        d2d(9) = dsidep(4,4)/deux
        do 60,k = 1,9
        dm(k) = dm(k) + coef*hic/deux*poids*d2d(k)
        dmf(k) = dmf(k) + coef*hic/deux*poids*zic*d2d( k)
        df(k) = df(k) + coef*hic/deux*poids*zic*zic* d2d(k)
60      continue
    endif
70  continue
80  continue
!
!       -- CALCUL DE DIV(SIGMA) ET RECOPIE DE N ET M DANS 'PCONTPR':
!       ----------------------------------------------------------
!       BTSIG = BTSIG + BFT*M + BMT*N
    if (vecteu) then
        do 120,ino = 1,nnoel
        do 110,k = 1,3
        btsig(1,ino) = btsig(1,ino) + bm(k,2* (ino-1)+1)* n(k)*poids
        btsig(2,ino) = btsig(2,ino) + bm(k,2* (ino-1)+2)* n(k)*poids
        btsig(3,ino) = btsig(3,ino) + bf(k,3* (ino-1)+1)* m(k)*poids
        btsig(5,ino) = btsig(5,ino) + bf(k,3* (ino-1)+2)* m(k)*poids
        btsig(4,ino) = btsig(4,ino) - bf(k,3* (ino-1)+3)* m(k)*poids
110      continue
120      continue
    endif
!
!
!       -- CALCUL DE LA MATRICE TANGENTE :
!       ----------------------------------
!       KTANG = KTANG + BFT*DF*BF + BMT*DM*BM + BMT*DMF*BF
    if (matric) then
!         -- MEMBRANE :
!         -------------
        call utbtab('CUMU', 3, 2*nnoel, dm, bm,&
                    work, memb)
!
!         -- FLEXION :
!         ------------
        call utbtab('CUMU', 3, 3*nnoel, df, bf,&
                    work, flex)
!
!         -- COUPLAGE:
!         ------------
        call utctab('CUMU', 3, 3*nnoel, 2*nnoel, dmf,&
                    bf, bm, work, mefl)
    endif
!
!       -- FIN BOUCLE SUR LES POINTS DE GAUSS
    130 end do
!
!     -- ACCUMULATION DES SOUS MATRICES DANS KTAN :
!     -----------------------------------------------
    if (matric) then
        if (dkt) then
            call dxtloc(flex, memb, mefl, ctor, ktan)
        else
            call dxqloc(flex, memb, mefl, ctor, ktan)
        endif
    endif
end subroutine
