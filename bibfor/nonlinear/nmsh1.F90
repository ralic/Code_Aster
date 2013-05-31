subroutine nmsh1(fami, option, typmod, formal, ndim,&
                 nno, npg, iw, ivf, vff,&
                 idff, geomi, dff, compor, mate,&
                 lgpg, crit, angmas, instm, instp,&
                 deplm, depld, sigm, vim, sigp,&
                 vip, fint, matuu, codret)
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
! aslint: disable=W1306,W1504
    implicit none
    include 'jeveux.h'
    include 'asterc/r8vide.h'
    include 'asterfort/assert.h'
    include 'asterfort/cacina.h'
    include 'asterfort/cale.h'
    include 'asterfort/calet.h'
    include 'asterfort/calgf.h'
    include 'asterfort/calpf.h'
    include 'asterfort/codere.h'
    include 'asterfort/dfdmip.h'
    include 'asterfort/nmcomp.h'
    include 'asterfort/nmgeom.h'
    include 'asterfort/nmgpin.h'
    include 'asterfort/nmmalu.h'
    include 'asterfort/prep2.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vicin0.h'
    include 'asterfort/vicin2.h'
    include 'blas/daxpy.h'
    include 'blas/dcopy.h'
    include 'blas/dscal.h'
    integer :: ndim, nno, npg, mate, lgpg, codret, iw, idff
    character(len=8) :: typmod(*)
    character(len=*) :: fami
    character(len=16) :: option, compor(*), formal(*)
    real(kind=8) :: geomi(*), dff(nno, *), crit(*), instm, instp
    real(kind=8) :: vff(nno, npg)
    real(kind=8) :: angmas(3)
    real(kind=8) :: deplm(*), depld(*), sigm(2*ndim, npg)
    real(kind=8) :: vim(lgpg, npg), sigp(2*ndim, npg), vip(lgpg, npg)
    real(kind=8) :: matuu(*), fint(*)
!
! ----------------------------------------------------------------------
!
!     BUT:  CALCUL  DES OPTIONS RIGI_MECA_*, RAPH_MECA ET FULL_MECA_*
!           EN GRANDES DEFORMATIONS 2D (D_PLAN ET AXI) ET 3D
! ----------------------------------------------------------------------
! IN  OPTION  : OPTION DE CALCUL
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  IW      : PTR. POIDS DES POINTS DE GAUSS
! IN  VFF     : VALEUR  DES FONCTIONS DE FORME
! IN  IDFF    : PTR. DERIVEE DES FONCTIONS DE FORME ELEMENT DE REF.
! IN  GEOMI   : COORDONNEES DES NOEUDS (CONFIGURATION INITIALE)
! MEM DFF     : ESPACE MEMOIRE POUR LA DERIVEE DES FONCTIONS DE FORME
!               DIM :(NNO,3) EN 3D, (NNO,4) EN AXI, (NNO,2) EN D_PLAN
! IN  COMPOR  : COMPORTEMENT
! IN  MATE    : MATERIAU CODE
! IN  LGPG    : DIMENSION DU VECTEUR DES VAR. INTERNES POUR 1 PT GAUSS
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  ANGMAS  : LES TROIS ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
! IN  INSTM   : VALEUR DE L'INSTANT T-
! IN  INSTP   : VALEUR DE L'INSTANT T+
! IN  DEPLM   : DEPLACEMENT EN T-
! IN  DEPLD   : INCREMENT DE DEPLACEMENT ENTRE T- ET T+
! IN  SIGM    : CONTRAINTES DE CAUCHY EN T-
! IN  VIM     : VARIABLES INTERNES EN T-
! OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA_*)
! OUT VIP     : VARIABLES INTERNES    (RAPH_MECA ET FULL_MECA_*)
! OUT FINT    : FORCES INTERIEURES (RAPH_MECA ET FULL_MECA_*)
! OUT MATUU    : MATR. DE RIGIDITE NON SYM. (RIGI_MECA_* ET FULL_MECA_*)
! OUT IRET    : CODE RETOUR DE L'INTEGRATION DE LA LDC
!
!
    logical :: grand, axi, resi, rigi, lintbo
    integer :: lij(3, 3), vij(3, 3), ia, ja, na, g, kk, i, j
    integer :: nddl, ndu, vu(3, 27), ivf, n, kl, m, j1, kkd, ivash2
    integer :: cod(27), n6, nd, nbcin, numcin(2)
    common /tdim/  n6 , nd
    real(kind=8) :: geomm(3*27), geomp(3*27)
    real(kind=8) :: fm(3, 3), fmp(3, 3), fma(3, 3), fd(3, 3), fda(3, 3)
    real(kind=8) :: fdmt(3, 3)
    real(kind=8) :: fdm(3, 3), fmam(3, 3), fta(3, 3)
    real(kind=8) :: deplda(3*27), deplt(3*27)
    real(kind=8) :: sig(6), sigmam(6), sigg(6)
    real(kind=8) :: etdpnv(6), edpn1(3, 3), prodf(3, 3), etdpn1(3, 3)
    real(kind=8) :: jm, jp, r, poids
    real(kind=8) :: pff(6, nno, nno)
    real(kind=8) :: coef
    real(kind=8) :: dsidep(6, 6), tmp2
    real(kind=8) :: rac2, t1, t2, alpha
    real(kind=8) :: def(6, nno, 3), tmp1
    real(kind=8) :: tampon(10), taup(6)
    real(kind=8) :: tbid(6), rbid, r8bid, id(3, 3)
    real(kind=8) :: epsm(6), epsmm(6)
    real(kind=8) :: rp(3, 3), rpa(3, 3)
    real(kind=8) :: rpat(3, 3), etdm(3, 3), lambp(3, 3)
    real(kind=8) :: rpt(3, 3)
    parameter (grand = .true.)
    data    vij  / 1, 4, 5,&
     &               4, 2, 6,&
     &               5, 6, 3 /
    data id/1.d0,0.d0,0.d0, 0.d0,1.d0,0.d0, 0.d0,0.d0,1.d0/
! ---------------------------------------------------------------------
!
! -----------------------------DECLARATION-----------------------------
    if (formal(1) .ne. 'GDEF_HYPO_ELAS') then
        call assert(.false.)
    endif
!
    rbid = r8vide()
    rac2 = sqrt(2.d0)
    nddl = ndim*nno
    ndu = ndim
!     ALPHA : INDICE POUR CONF INTERMEDIAIRE COMPRIS ENTRE 0 ET 1
    alpha =1.d0/2.d0
    n6=6
    nd=3
    lintbo = .false.
!
!     AFFECTATION DES VARIABLES LOGIQUES  OPTIONS ET MODELISATION
    axi = typmod(1).eq.'AXIS'
    resi = option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL'
    rigi = option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL'
    call vicin0(compor, nbcin, numcin)
!
!--------------------------INITIALISATION------------------------
!
!     INTIALISATION DU VECTEUR TBID AVEC LA VALEUR RBID
    call r8inir(6, rbid, tbid, 1)
!
!     MISE A ZERO TAUP[6]=0, DSIDEP[54]=0, TAMPON[10]=0, COD[27]=0
    call r8inir(6, 0.d0, taup, 1)
    call r8inir(36, 0.d0, dsidep, 1)
    call r8inir(10, 0.d0, tampon, 1)
!
    do 9 i = 1, 27
        cod(i)=0
 9  end do
!
!    INITIALISATION TABLEAU VU CONTENANT LES INDICES POUR LE COUPLE(I,N)
!     si AXI='OUI' VU(1,N)=VU(3,N)
    call nmgpin(ndim, nno, axi, vu)
!
!-----------------------------TEST AVANT CALCUL---------------------
!
!     TEST SUR LE NOMBRE DE NOEUDS SI TEST NON VERIFIE MESSAGE ERREUR
    call assert(nno.le.27)
    if (typmod(1) .eq. 'C_PLAN') call u2mess('F', 'ALGORITH8_1')
    if (axi) ndu = 3
!
!------------------------------DEPLACEMENT ET GEOMETRIE-------------
!
!    DETERMINATION DES CONFIGURATIONS EN T- (GEOMM) ET T+ (GEOMP)
    call dcopy(nddl, geomi, 1, geomm, 1)
    call daxpy(nddl, 1.d0, deplm, 1, geomm,&
               1)
    call dcopy(nddl, geomm, 1, geomp, 1)
    if (resi) then
        call daxpy(nddl, 1.d0, depld, 1, geomp,&
                   1)
!        DEPLT : DEPLACEMENT TOTAL ENTRE CONF DE REF ET INSTANT T_N+1
        do 20 i = 1, nno*ndim
            deplt(i) = deplm(i) + depld(i)
20      continue
    else
        do 21 i = 1, nno*ndim
            deplt(i) = deplm(i)
            depld(i) = 0.d0
21      continue
    endif
!
!
!****************************BOUCLE SUR LES POINTS DE GAUSS************
!
! - CALCUL POUR CHAQUE POINT DE GAUSS
    do 10 g = 1, npg
!
!---CALCUL DE F_N, F_N+1 ET F_(N+ALPHA) PAR RAPPORT A GEOMI GEOM INITIAL
        call calgf(ndim, nno, axi, npg, geomi,&
                   g, iw, vff, idff, deplm,&
                   deplt, grand, alpha, r, poids,&
                   dff, fm, fmp, fma)
!
!----------------CALCUL DE f_(n+1) ET f_(n+alpha) PAR RAPPORT A GEOM T-
        call calpf(ndim, nno, axi, npg, geomm,&
                   g, iw, vff, idff, depld,&
                   grand, alpha, r, dff, fd,&
                   deplda, fda)
!
!--------------------------------CALCUL DE e_(n+1)--------------
        call cale(ndim, fd, id, fdm, fdmt,&
                  prodf, edpn1)
!
!--------------------------------CALCUL DE e~_(n+alpha)-----------
        call calet(ndim, fm, fma, fmp, edpn1,&
                   fmam, fta, etdpn1, jm, jp)
!------------------------CALCUL DE L,W ET DE R SI VMIS_CINE_LINE------
        call cacina(ndim, nno, npg, lgpg, axi,&
                    grand, compor, geomm, g, iw,&
                    vff, idff, fm, fma, depld,&
                    instm, instp, vim(1, g), rp, rpa,&
                    lambp)
!
!---------------------TRANSFORMATION DES ARG D ENTRE SUBROUTINE NMCOMP
!
        call nmgeom(ndim, nno, .false., grand, geomi,&
                    g, iw, ivf, idff, deplm,&
                    .true., r8bid, dff, fm, epsm,&
                    r8bid)
        call prep2(ndim, npg, g, rpa, etdpn1,&
                   sigm, jm, fda, rp, rpat,&
                   etdm, etdpnv, sigmam, rpt, epsm,&
                   epsmm)
!
!  **************  COMPORTEMENTS AVEC ECROUISSAGE CINEMATIQUE**********
!-----------------------TRANSFORMATION DE X : VARIABLE INTERNE---------
        if (nbcin .gt. 0) then
            call vicin2(ndim, g, npg, lgpg, vim,&
                        rpt, nbcin, numcin)
        endif
!
!************************APPEL A LA LOI DE COMPORTEMENT**********
        call nmcomp(fami, g, 1, 3, typmod,&
                    mate, compor, crit, instm, instp,&
                    6, epsmm, etdpnv, 6, sigmam,&
                    vim(1, g), option, angmas, 10, tampon,&
                    taup, vip(1, g), 36, dsidep, 1,&
                    rbid, cod(g))
!
!       TEST SUR LES CODES RETOUR DE LA LOI DE COMPORTEMENT
!
        if (cod(g) .eq. 1) then
            if (resi) then
                goto 9999
            endif
        endif
        if (cod(g) .eq. 4) lintbo= .true.
!
!        SUPPRESSION DES RACINES DE 2
        if (resi) call dscal(3, 1.d0/rac2, taup(4), 1)
!
!********************CONTRAINTE ET FORCES INTERIEURES******************
!
        if (resi) then
!
!         CONTRAINTE DE CAUCHY A PARTIR DE KIRCHHOFF
!         PASSAGE DE TAU_(n+1) A CONTRAINTE DE CAUCHY
            call dcopy(2*ndim, taup, 1, sigp(1, g), 1)
            coef=1.d0/jp
            call dscal(2*ndim, coef, sigp(1, g), 1)
!
            if (formal(1) .eq. 'GDEF_HYPO_ELAS') then
                ivash2=lgpg-9+1
                call daxpy(9, -1.d0, id, 1, lambp,&
                           1)
                call dcopy(9, lambp, 1, vip(ivash2, g), 1)
            endif
!         Pour le calcul des forces internes.
!         La configuration de calcul de LIJ est A VERIFIER !!!
!
            call dfdmip(ndim, nno, axi, geomp, g,&
                        iw, vff(1, g), idff, r, poids,&
                        dff)
            call nmmalu(nno, axi, r, vff(1, g), dff,&
                        lij)
!
!         VECTEUR FINT
            do 300 na = 1, nno
                do 310 ia = 1, ndu
!         ATTENTION IA=1,NDU
                    kk = vu(ia,na)
                    t1 = 0
                    do 320 ja = 1, ndu
                        t2 = taup(vij(ia,ja))
                        t1 = t1 + t2*dff(na,lij(ia,ja))
320                  continue
                    fint(kk) = fint(kk) + poids*t1
310              continue
300          continue
        endif
!
!
! *********************MATRICE TANGENTE(SYMETRIQUE)********************
!  REM : ON DUPLIQUE LES CAS 2D ET 3D POUR EVITER DE PERDRE TROP EN
!         TERME DE TEMPS DE CALCULS
!
        if (rigi) then
            call dfdmip(ndim, nno, axi, geomp, g,&
                        iw, vff(1, g), idff, r, poids,&
                        dff)
            call nmmalu(nno, axi, r, vff(1, g), dff,&
                        lij)
            do 125 n = 1, nno
                do 126 m = 1, n
                    pff(1,n,m) = dff(n,1)*dff(m,1)
                    pff(2,n,m) = dff(n,2)*dff(m,2)
                    pff(4,n,m) =(dff(n,1)*dff(m,2)+dff(n,2)*dff(m,1))/&
                    rac2
                    if (ndim .eq. 2) then
                        pff(3,n,m) = 0.d0
                        pff(5,n,m) = 0.d0
                        pff(6,n,m) = 0.d0
                    else
                        pff(3,n,m) = dff(n,3)*dff(m,3)
                        pff(5,n,m) = (dff(n,1)*dff(m,3)+dff(n,2)*dff( m,1) )/rac2
                        pff(6,n,m) = (dff(n,2)*dff(m,3)+dff(n,3)*dff( m,2) )/rac2
                    endif
126              continue
125          continue
!
            do 40 n = 1, nno
                do 30 i = 1, ndim
                    def(1,n,i) = id(i,1)*dff(n,1)
                    def(2,n,i) = id(i,2)*dff(n,2)
                    def(4,n,i) = (id(i,1)*dff(n,2) + id(i,2)*dff(n,1)) /rac2
                    if (ndim .eq. 2) then
                        def(3,n,i) = 0.d0
                        def(5,n,i) = 0.d0
                        def(6,n,i) = 0.d0
                    else
                        def(3,n,i) = id(i,3)*dff(n,3)
                        def(5,n,i) = (id(i,1)*dff(n,3) + id(i,3)*dff( n,1) )/rac2
                        def(6,n,i) = (id(i,2)*dff(n,3) + id(i,3)*dff( n,2) )/rac2
                    endif
! 5.2.5 - TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1
!
30              continue
40          continue
            if (axi) then
                do 50 n = 1, nno
                    def(3,n,1) = id(3,3)*zr(ivf+n+(g-1)*nno-1)/r
50              continue
            endif
!
            do 160 n = 1, nno
                do 150 i = 1, ndim
                    do 151,kl=1,2*ndim
                    sig(kl)=0.d0
                    sig(kl)=sig(kl)+def(1,n,i)*dsidep(1,kl)
                    sig(kl)=sig(kl)+def(2,n,i)*dsidep(2,kl)
                    sig(kl)=sig(kl)+def(3,n,i)*dsidep(3,kl)
                    sig(kl)=sig(kl)+def(4,n,i)*dsidep(4,kl)
                    if (ndim .eq. 3) then
                        sig(kl)=sig(kl)+def(5,n,i)*dsidep(5,kl)
                        sig(kl)=sig(kl)+def(6,n,i)*dsidep(6,kl)
                    endif
151                  continue
                    do 140 j = 1, ndim
                        do 130 m = 1, n
                            if (m .eq. n) then
                                j1 = i
                            else
                                j1 = ndim
                            endif
!
!              RIGIDITE GEOMETRIQUE
!
                            if (option(1:4) .eq. 'RIGI') then
                                sigg(1)=sigmam(1)
                                sigg(2)=sigmam(2)
                                sigg(3)=sigmam(3)
                                sigg(4)=sigmam(4)
                                if (ndim .eq. 3) then
                                    sigg(5)=sigmam(5)
                                    sigg(6)=sigmam(6)
                                endif
                            else
                                sigg(1)=sigp(1,g)
                                sigg(2)=sigp(2,g)
                                sigg(3)=sigp(3,g)
                                sigg(4)=sigp(4,g)
                                if (ndim .eq. 3) then
                                    sigg(5)=sigp(5,g)
                                    sigg(6)=sigp(6,g)
                                endif
                            endif
!
                            tmp1 = 0.d0
                            if (i .eq. j) then
                                tmp1 = pff(1,n,m)*sigg(1) + pff(2,n,m) *sigg(2) + pff(3,n,m)*sigg&
                                       &(3) + pff(4, n,m)*sigg(4)
                                if (ndim .eq. 3) then
                                    tmp1=tmp1+ pff(5,n,m)*sigg(5)&
                                    + pff(6,n,m)*sigg(6)
                                endif
! TERME DE CORRECTION AXISYMETRIQUE (NMGR2D)
                                if (axi .and. i .eq. 1) then
                                    tmp1=tmp1+zr(ivf+n+(g-1)*nno-1)*&
                                    zr(ivf+m+(g-1)*nno-1)/(r*r)*sigg(&
                                    3)
                                endif
                            endif
!
! 5.4.2    - RIGIDITE ELASTIQUE
!
                            tmp2=0.d0
                            tmp2=tmp2+sig(1)*def(1,m,j)
                            tmp2=tmp2+sig(2)*def(2,m,j)
                            tmp2=tmp2+sig(3)*def(3,m,j)
                            tmp2=tmp2+sig(4)*def(4,m,j)
                            if (ndim .eq. 3) then
                                tmp2=tmp2+sig(5)*def(5,m,j)
                                tmp2=tmp2+sig(6)*def(6,m,j)
                            endif
! 5.4.3    - STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                            if (j .le. j1) then
                                kkd = (ndim*(n-1)+i-1) * (ndim*(n-1)+ i) /2
                                kk = kkd + ndim*(m-1)+j
                                matuu(kk) = matuu(kk) + (tmp1+tmp2)* poids
                            endif
!
130                      continue
140                  continue
150              continue
160          continue
!
        endif
!
10  end do
    if (lintbo) cod(1) = 4
9999  continue
! - SYNTHESE DES CODES RETOURS
    call codere(cod, npg, codret)
!
end subroutine
