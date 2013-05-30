subroutine nmgpfi(fami, option, typmod, ndim, nno,&
                  npg, iw, vff, idff, geomi,&
                  dff, compor, mate, lgpg, crit,&
                  angmas, instm, instp, deplm, depld,&
                  sigm, vim, sigp, vip, fint,&
                  matr, codret)
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
! TOLE CRP_21
!
    implicit none
    include 'jeveux.h'
    include 'asterc/r8vide.h'
    include 'asterfort/assert.h'
    include 'asterfort/codere.h'
    include 'asterfort/crirup.h'
    include 'asterfort/dfdmip.h'
    include 'asterfort/nmcomp.h'
    include 'asterfort/nmepsi.h'
    include 'asterfort/nmgpin.h'
    include 'asterfort/nmmalu.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/u2mess.h'
    include 'blas/daxpy.h'
    include 'blas/dcopy.h'
    include 'blas/dscal.h'
    integer :: ndim, nno, npg, mate, lgpg, codret, iw, idff
    character(len=8) :: typmod(*)
    character(len=*) :: fami
    character(len=16) :: option, compor(*)
    real(kind=8) :: geomi(*), dff(nno, *), crit(*), instm, instp
    real(kind=8) :: vff(nno, npg)
    real(kind=8) :: angmas(3)
    real(kind=8) :: deplm(*), depld(*), sigm(2*ndim, npg)
    real(kind=8) :: vim(lgpg, npg), sigp(2*ndim, npg), vip(lgpg, npg)
    real(kind=8) :: matr(*), fint(*)
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
! OUT MATR    : MATR. DE RIGIDITE NON SYM. (RIGI_MECA_* ET FULL_MECA_*)
! OUT IRET    : CODE RETOUR DE L'INTEGRATION DE LA LDC
!
!
    logical :: grand, axi, resi, rigi
    integer :: lij(3, 3), vij(3, 3), ia, ja, na, ib, jb, nb, g, kk, os, ija, i
    integer :: nddl, ndu, vu(3, 27)
    integer :: cod(27)
    real(kind=8) :: tampon(10), wkout
    real(kind=8) :: geomm(3*27), geomp(3*27), r, w
    real(kind=8) :: jm, jd, jp, fm(3, 3), fd(3, 3), coef
    real(kind=8) :: sigmam(6), taup(6), dsidep(6, 3, 3)
    real(kind=8) :: rac2, rbid, tbid(6), t1, t2
!
    parameter (grand = .true.)
    data    vij  / 1, 4, 5,&
     &               4, 2, 6,&
     &               5, 6, 3 /
! ----------------------------------------------------------------------
!
! - INITIALISATION ET VERIFICATIONS
!
    rbid = r8vide()
    rac2 = sqrt(2.d0)
    call r8inir(6, rbid, tbid, 1)
!
    call assert(nno.le.27)
    if (typmod(1) .eq. 'C_PLAN') call u2mess('F', 'ALGORITH8_1')
!
    axi = typmod(1).eq.'AXIS'
    resi = option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL'
    rigi = option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL'
!
    nddl = ndim*nno
    ndu = ndim
    if (axi) ndu = 3
    call nmgpin(ndim, nno, axi, vu)
!
!    DETERMINATION DES CONFIGURATIONS EN T- (GEOMM) ET T+ (GEOMP)
    call dcopy(nddl, geomi, 1, geomm, 1)
    call daxpy(nddl, 1.d0, deplm, 1, geomm,&
               1)
    call dcopy(nddl, geomm, 1, geomp, 1)
    call daxpy(nddl, 1.d0, depld, 1, geomp,&
               1)
!
!    MISE A ZERO
!
    call r8inir(6, 0.d0, taup, 1)
    call r8inir(54, 0.d0, dsidep, 1)
    call r8inir(10, 0.d0, tampon, 1)
    do 9 i = 1, 27
        cod(i)=0
 9  end do
!
!
! - CALCUL POUR CHAQUE POINT DE GAUSS
    do 10 g = 1, npg
!
!      CALCUL DES DEFORMATIONS
        call dfdmip(ndim, nno, axi, geomi, g,&
                    iw, vff(1, g), idff, r, w,&
                    dff)
        call nmepsi(ndim, nno, axi, grand, vff(1, g),&
                    r, dff, deplm, fm, tbid)
        call dfdmip(ndim, nno, axi, geomm, g,&
                    iw, vff(1, g), idff, r, rbid,&
                    dff)
        call nmepsi(ndim, nno, axi, grand, vff(1, g),&
                    r, dff, depld, fd, tbid)
        call dfdmip(ndim, nno, axi, geomp, g,&
                    iw, vff(1, g), idff, r, rbid,&
                    dff)
        call nmmalu(nno, axi, r, vff(1, g), dff,&
                    lij)
!
        jm = fm(1,1)*(fm(2,2)*fm(3,3)-fm(2,3)*fm(3,2)) - fm(2,1)*(fm( 1,2)*fm(3,3)-fm(1,3)*fm(3,2&
             &)) + fm(3,1)*(fm(1,2)*fm(2,3)-fm(1, 3)*fm(2,2))
        jd = fd(1,1)*(fd(2,2)*fd(3,3)-fd(2,3)*fd(3,2)) - fd(2,1)*(fd( 1,2)*fd(3,3)-fd(1,3)*fd(3,2&
             &)) + fd(3,1)*(fd(1,2)*fd(2,3)-fd(1, 3)*fd(2,2))
        jp = jm*jd
!
!
!      PERTINENCE DES GRANDEURS
        if (jd .le. 1.d-2 .or. jd .gt. 1.d2) then
            codret = 1
            goto 9999
        endif
!
! -   APPEL A LA LOI DE COMPORTEMENT
!
!      POUR LES LOIS QUI NE RESPECTENT PAS ENCORE LA NOUVELLE INTERFACE
!      ET QUI ONT ENCORE BESOIN DES CONTRAINTES EN T-
        call r8inir(6, 0.d0, sigmam, 1)
        call dcopy(ndim*2, sigm(1, g), 1, sigmam, 1)
!
        cod(g) = 0
        call nmcomp(fami, g, 1, 3, typmod,&
                    mate, compor, crit, instm, instp,&
                    9, fm, fd, 6, sigmam,&
                    vim(1, g), option, angmas, 10, tampon,&
                    taup, vip( 1, g), 54, dsidep, 1,&
                    wkout, cod(g))
!
        if (cod(g) .eq. 1) then
            codret = 1
            if (.not. resi) call u2mess('F', 'ALGORITH11_88')
            goto 9999
        endif
!
!      SUPPRESSION DES RACINES DE 2
        if (resi) call dscal(3, 1/rac2, taup(4), 1)
!
!      MATRICE TANGENTE SANS LES RACINES DE 2
        if (rigi) then
            coef=1.d0/rac2
            call dscal(9, coef, dsidep(4, 1, 1), 6)
            call dscal(9, coef, dsidep(5, 1, 1), 6)
            call dscal(9, coef, dsidep(6, 1, 1), 6)
        endif
!
!
! - CONTRAINTE ET FORCES INTERIEURES
!
        if (resi) then
!
!        CONTRAINTE DE CAUCHY A PARTIR DE KIRCHHOFF
            call dcopy(2*ndim, taup, 1, sigp(1, g), 1)
            coef=1.d0/jp
            call dscal(2*ndim, coef, sigp(1, g), 1)
!
!        VECTEUR FINT
            do 300 na = 1, nno
                do 310 ia = 1, ndu
                    kk = vu(ia,na)
                    t1 = 0
                    do 320 ja = 1, ndu
                        t2 = taup(vij(ia,ja))
                        t1 = t1 + t2*dff(na,lij(ia,ja))
320                  continue
                    fint(kk) = fint(kk) + w*t1
310              continue
300          continue
        endif
!
!
! - MATRICE TANGENTE (NON SYMETRIQUE)
!  REM : ON DUPLIQUE LES CAS 2D ET 3D POUR EVITER DE PERDRE TROP EN
!         TERME DE TEMPS DE CALCULS
!
        if (rigi) then
!
!
            if (.not. resi) then
                call dcopy(2*ndim, sigm(1, g), 1, taup, 1)
                call dscal(2*ndim, jm, taup, 1)
            endif
!
            if (ndu .eq. 3) then
                do 500 na = 1, nno
                    do 510 ia = 1, 3
                        os = (vu(ia,na) - 1)*nddl
!
                        do 520 nb = 1, nno
                            do 530 ib = 1, 3
                                kk = os + vu(ib,nb)
                                t1 = 0.d0
                                do 550 ja = 1, 3
                                    do 560 jb = 1, 3
                                        ija = vij(ia,ja)
                                        t2 = dsidep(ija,ib,jb)
                                        t1 = t1 + dff( na, lij(ia, ja))* t2*dff(nb, lij(ib, jb) )
560                                  continue
550                              continue
!
!               RIGIDITE GEOMETRIQUE
                                do 570 jb = 1, 3
                                    t1 = t1 - dff(&
                                         na, lij(ia, ib))*dff( nb, lij(ib, jb)) *taup(vij(ia, jb)&
                                         )
570                              continue
                                matr(kk) = matr(kk) + w*t1
530                          continue
520                      continue
!
510                  continue
500              continue
!
            else if (ndu.eq.2) then
!
                do 600 na = 1, nno
                    do 610 ia = 1, 2
                        os = (vu(ia,na) - 1)*nddl
!
                        do 620 nb = 1, nno
                            do 630 ib = 1, 2
                                kk = os + vu(ib,nb)
                                t1 = 0.d0
                                do 650 ja = 1, 2
                                    do 660 jb = 1, 2
                                        ija = vij(ia,ja)
                                        t2 = dsidep(ija,ib,jb)
                                        t1 = t1 + dff( na, lij(ia, ja))* t2*dff(nb, lij(ib, jb) )
660                                  continue
650                              continue
!
!               RIGIDITE GEOMETRIQUE
                                do 670 jb = 1, 2
                                    t1 = t1 - dff(&
                                         na, lij(ia, ib))*dff( nb, lij(ib, jb)) *taup(vij(ia, jb)&
                                         )
670                              continue
                                matr(kk) = matr(kk) + w*t1
630                          continue
620                      continue
!
610                  continue
600              continue
!
            endif
!
        endif
!
10  end do
!
!
!     POST_ITER='CRIT_RUPT'
    if (crit(11) .gt. 0.d0) then
        ndim = 3
        call crirup(fami, mate, ndim, npg, lgpg,&
                    option, compor, sigp, vip, vim,&
                    instm, instp)
    endif
!
!
! - SYNTHESE DES CODES RETOURS
    call codere(cod, npg, codret)
!
9999  continue
end subroutine
