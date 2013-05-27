subroutine sh8for(xetemp, para, xidepm, sigma, fstab,&
                  xivect)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!               ELEMENT SHB8
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/chrp3d.h'
    include 'asterfort/houxgb.h'
    include 'asterfort/mulmat.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rloshb.h'
    include 'asterfort/shbbar.h'
    include 'asterfort/shbksi.h'
    include 'asterfort/shbrot.h'
    include 'asterfort/shcalb.h'
    include 'asterfort/shvrot.h'
    integer :: lag, irdc
    real(kind=8) :: fstab(12), para(*)
    real(kind=8) :: xe(24), xidepm(*), sigma(*)
    real(kind=8) :: xxg5(5), pxg5(5), xcoq(3, 4), bksip(3, 8, 5), b(3, 8)
    real(kind=8) :: xcent(3), ppp(3, 3)
    real(kind=8) :: xl(3, 4), xxx(3), yyy(3), xivect(24)
    real(kind=8) :: tmpke(24, 24), tmpke2(24, 24)
    real(kind=8) :: xxvb(3), hij(6)
    real(kind=8) :: gb(8, 4), gs(8, 4), xxgb(3, 4)
    real(kind=8) :: rr2(3, 3), lambda, xelocp(24)
    real(kind=8) :: udef(24), xxloc(24), xloc12(24), sigloc(6)
    real(kind=8) :: f(3, 8), sigmag(6), pqialf(3, 4)
    real(kind=8) :: qialfa(3, 4), fhg(3, 8), rr12(3, 3), rr1(3, 3), fhg24(24)
    real(kind=8) :: sitmp1(8, 8), sitmp2(8, 8), poids
    real(kind=8) :: fq(24), xetemp(*)
!
! INITIALISATIONS
!
! INFOS:
! XE EST RANGE COMME CA:
! (XNOEUD1 YNOEUD1 ZNOEUD1, XNOEUD2 YNOEUD2 ZNOEUD2,...)
! DANS SHB8_TEST_NUM: ATTENTION A LA NUMEROTATION DES NOEUDS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!      IF (NOMSHB.EQ.'SHB8') THEN
!
!-----------------------------------------------------------------------
    integer :: i, ia, ip, j, k, kk
    real(kind=8) :: ajac, aux, rbid, uns3, uns8, vol, xcooef
    real(kind=8) :: xmu, xnu, xyoung, zeta, zlamb
!-----------------------------------------------------------------------
    data gb/1.d0,1.d0,-1.d0,-1.d0,-1.d0,-1.d0,1.d0,&
     &    1.d0,1.d0,-1.d0,-1.d0,1.d0,-1.d0,1.d0,1.d0,&
     &   -1.d0,1.d0,-1.d0,1.d0,-1.d0,1.d0,-1.d0,1.d0,&
     &   -1.d0,-1.d0,1.d0,-1.d0,1.d0,1.d0,-1.d0,1.d0,-1.d0/
!
! ON DEFINI LES POINTS GAUSS ET LES POIDS
!
    xxg5(1) = -0.906179845938664D0
    xxg5(2) = -0.538469310105683D0
    xxg5(3) = 0.d0
    xxg5(4) = 0.538469310105683D0
    xxg5(5) = 0.906179845938664D0
!
    pxg5(1) = 0.236926885056189D0
    pxg5(2) = 0.478628670499366D0
    pxg5(3) = 0.568888888888889D0
    pxg5(4) = 0.478628670499366D0
    pxg5(5) = 0.236926885056189D0
!
    uns8 = 1.d0/8.d0
    uns3 = 1.d0/3.d0
!
! -----------------------------------------------------
! ON VERIFIE QUE LA CONNECTIVITE DONNE UN REPERE DIRECT
! SI CE N EST PAS LE CAS ON PERMUTE LES NOEUDS
! -----------------------------------------------------
!
!     ON FAIT UNE COPIE DE XETEMP DANS XE
    do 10 i = 1, 24
        xe(i) = xetemp(i)
10  continue
! TYPE DE LOI DE COMPORTEMENT:
!     IRDC = 1 : SHB8 TYPE PLEXUS
!     IRDC = 2 : C.P.
!     IRDC = 3 : 3D COMPLETE
!**         IRDC = OUT(1)
    irdc = nint(para(5))
    lag = nint(para(6))
    call r8inir(64, 0.d0, sitmp2, 1)
    do 470 j = 1, 8
        do 460 i = 1, 3
            f(i,j) = 0.d0
460      continue
470  continue
!
! CALCUL DE BKSIP(3,8,IP) DANS REPERE DE REFERENCE
!      BKSIP(1,*,IP) = VECTEUR BX AU POINT GAUSS IP
!      BKSIP(2,*,IP) = VECTEUR BY AU POINT GAUSS IP
!      BKSIP(3,*,IP) = VECTEUR BZ AU POINT GAUSS IP
!
    call shbksi(5, xxg5, bksip)
    do 540 ip = 1, 5
!
! RECHERCHE DE SIGMA DU POINT DE GAUSS GLOBAL
!
        do 480 i = 1, 6
!
! C'EST LES CONTRAINTES LOCALES POUR POUVOIR TRAITER LA PLASTICITE AVANT
! LES CONTRAINTES SONT PASSEES DANS LA CONFI.
! A LA FIN DU PAS DANS Q8PKCN
!
            sigloc(i) = sigma((ip-1)*6+i)
480      continue
        zeta = xxg5(ip)
        zlamb = 0.5d0* (1.d0-zeta)
        do 500 i = 1, 4
            do 490 j = 1, 3
                xcoq(j,i) = zlamb*xe((i-1)*3+j) + (1.d0-zlamb)*xe((i- 1+4)*3+j)
490          continue
500      continue
        call rloshb(xcoq, xcent, ppp, xl, xxx,&
                    yyy, rbid)
!
! PASSAGE DES CONTRAINTES AU REPERE GLOBAL
!
        call chrp3d(ppp, sigloc, sigmag, 1)
!
        call shcalb(bksip(1, 1, ip), xe, b, ajac)
!
! CALCUL DE BQ.SIGMA SI LAGRANGIEN TOTAL
!
        if (lag .eq. 1) then
            do 507 j = 1, 8
                do 506 i = 1, 8
                    sitmp1(i,j) = 0.d0
506              continue
507          continue
!
            do 509 j = 1, 8
                do 508 i = 1, 8
                    sitmp1(i,j) = sigmag(1)*b(1,i)*b(1,j) + sigmag(2)* b(2,i)*b(2,j) + sigmag(3)*&
                                  &b(3,i)*b(3,j) + sigmag( 4)*(b(1,i)*b(2,j)+b(2,i)*b(1,j)) + sig&
                                  &mag(6)* (b( 1,i)*b(3,j)+b(3,i)*b(1,j)) + sigmag(5)* (b(3,i)*b(&
                                  & 2,j)+b(2,i)*b(3,j))
508              continue
509          continue
!***            CALL SHASKS(SIGMAG,B,SITMP1)
            do 520 j = 1, 8
                do 510 i = 1, 8
                    sitmp2(i,j) = sitmp2(i,j) + 4.d0*ajac*pxg5(ip)* sitmp1(i,j)
510              continue
520          continue
        endif
!
! CALCUL DE B.SIGMA EN GLOBAL
!
        poids = 4.d0*ajac*pxg5(ip)
        do 530 k = 1, 8
            f(1,k) = f(1,k) + poids* (b(1,k)*sigmag(1)+b(2,k)*sigmag( 4)+ b(3,k)*sigmag(6))
            f(2,k) = f(2,k) + poids* (b(1,k)*sigmag(4)+b(2,k)*sigmag( 2)+ b(3,k)*sigmag(5))
            f(3,k) = f(3,k) + poids* (b(1,k)*sigmag(6)+b(2,k)*sigmag( 5)+ b(3,k)*sigmag(3))
530      continue
540  continue
!
! SI LAGRANGIEN TOTAL: RAJOUT DE FQ A F
!
    if (lag .eq. 1) then
        call r8inir(576, 0.d0, tmpke, 1)
        do 570 kk = 1, 3
            do 560 i = 1, 8
                do 550 j = 1, 8
                    tmpke(i+ (kk-1)*8,j+ (kk-1)*8) = sitmp2(i,j)
550              continue
560          continue
570      continue
        call r8inir(576, 0.d0, tmpke2, 1)
        do 590 j = 1, 8
            do 580 i = 1, 24
                tmpke2(i, (j-1)*3+1) = tmpke(i,j)
                tmpke2(i, (j-1)*3+2) = tmpke(i,j+8)
                tmpke2(i, (j-1)*3+3) = tmpke(i,j+16)
580          continue
590      continue
        call r8inir(576, 0.d0, tmpke, 1)
        do 610 i = 1, 8
            do 600 j = 1, 24
                tmpke((i-1)*3+1,j) = tmpke2(i,j)
                tmpke((i-1)*3+2,j) = tmpke2(i+8,j)
                tmpke((i-1)*3+3,j) = tmpke2(i+16,j)
600          continue
610      continue
!          DO 611 J = 1,24
!        WRITE(6,*) 'XIDEPM =',I,XIDEPM(I)
!  611     CONTINUE
        call mulmat(24, 24, 1, tmpke, xidepm,&
                    fq)
        do 620 k = 1, 8
            f(1,k) = f(1,k) + fq((k-1)*3+1)
            f(2,k) = f(2,k) + fq((k-1)*3+2)
            f(3,k) = f(3,k) + fq((k-1)*3+3)
620      continue
    endif
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! ON CALCULE LA STABILISATION POUR LE CALCUL DE B.SIGMA
! ON A BESOIN DU MATERIAU AUSSI!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
    xyoung = para(1)
    xnu = para(2)
    lambda = xyoung*xnu/ (1-xnu*xnu)
    xmu = 0.5d0*xyoung/ (1+xnu)
!
! DEPLACEMENT NODAL, REPERE GLOBAL
!
    do 630 i = 1, 24
        xxloc(i) = xe(i)
        xelocp(i) = xe(i) - xidepm(i)
        xloc12(i) = xe(i) - 0.5d0*xidepm(i)
630  continue
! ATTENTION, RR, MATRICE DU CORROTATIONNEL EST RANGEE PAR LIGNES!
!
    call shbrot(xxloc, rr2)
    call shbrot(xelocp, rr1)
    call shbrot(xloc12, rr12)
    call shvrot(rr2, xxloc, 1)
    call shvrot(rr1, xelocp, 1)
    call shvrot(rr12, xloc12, 1)
!
!  ON A BESOIN DES VECTEURS GAMMA(8, ALPHA=1 A 4) = GS(8,4)
!  ORIGINE DE XLOC12 PAS BONNE, MAIS PAS GRAVE :
!  DIFFERENTIELLES DANS B
!
    call shbbar(xloc12, b, vol)
!
! CALCUL DEPLACEMENT DEFORMANT DANS REPERE 1/2 PAS:
!
    do 640 i = 1, 24
        udef(i) = xxloc(i) - xelocp(i)
640  continue
!
! CALCUL DES FONCTIONS DE FORME  GS
! XXGB = X  * GB
!
    do 660 ia = 1, 4
        do 650 j = 1, 3
            xxgb(j,ia) = houxgb(xloc12(j),ia)
650      continue
660  continue
!
! GS = (BBB)  * XXGB
!
    do 680 j = 1, 4
        do 670 i = 1, 8
            gs(i,j) = 0.d0
670      continue
680  continue
    do 710 j = 1, 3
        do 700 ia = 1, 4
            do 690 i = 1, 8
                gs(i,ia) = gs(i,ia) + b(j,i)*xxgb(j,ia)
690          continue
700      continue
710  continue
!
! GS = GB - GS
!
    do 730 i = 1, 4
        do 720 j = 1, 8
            gs(j,i) = (gb(j,i)-gs(j,i))*uns8
720      continue
730  continue
!
! CALCUL DE XXVB = X * VB
!
    xxvb(1) = -xloc12(1) + xloc12(4) + xloc12(7) - xloc12(10) - xloc12(13) + xloc12(16) + xloc12(&
              &19) - xloc12(22)
    xxvb(2) = -xloc12(2) - xloc12(5) + xloc12(8) + xloc12(11) - xloc12(14) - xloc12(17) + xloc12(&
              &20) + xloc12(23)
    xxvb(3) = -xloc12(3) - xloc12(6) - xloc12(9) - xloc12(12) + xloc12(15) + xloc12(18) + xloc12(&
              &21) + xloc12(24)
!
! CALCUL DES RELATIONS CONTRAINTES ET DEFORMATIONS GENERALISEES
!
    hij(1) = uns3*xxvb(2)*xxvb(3)/xxvb(1)
    hij(2) = uns3*xxvb(1)*xxvb(3)/xxvb(2)
    hij(3) = uns3*xxvb(2)*xxvb(1)/xxvb(3)
    hij(4) = uns3*xxvb(3)
    hij(5) = uns3*xxvb(1)
    hij(6) = uns3*xxvb(2)
!
! CALCUL DES DEFORMATIONS GENERALISEES
!
    do 760 ia = 1, 4
        do 750 j = 1, 3
            aux = 0.d0
            do 740 i = 1, 8
                aux = aux + gs(i,ia)*udef((i-1)*3+j)
740          continue
            pqialf(j,ia) = aux
750      continue
760  continue
!
! CALCUL DES CONTRAINTES GENERALISEES
!
    do 780 i = 1, 4
        do 770 j = 1, 3
            qialfa(j,i) = fstab(j+ (i-1)*3)
770      continue
780  continue
!
    if (irdc .eq. 1) then
        qialfa(1,1) = qialfa(1,1)
        qialfa(2,2) = qialfa(2,2)
        qialfa(3,3) = qialfa(3,3)
        qialfa(1,2) = qialfa(1,2)
        qialfa(2,3) = qialfa(2,3) + ((lambda+2.d0*xmu)*hij(2))*pqialf( 2,3)
        qialfa(1,3) = qialfa(1,3) + ((lambda+2.d0*xmu)*hij(1))*pqialf( 1,3)
        qialfa(2,1) = qialfa(2,1)
        qialfa(3,2) = qialfa(3,2)
        qialfa(3,1) = qialfa(3,1)
        qialfa(1,4) = qialfa(1,4) + uns3* ((lambda+2*xmu)*hij(1))* pqialf(1,4)
        qialfa(2,4) = qialfa(2,4) + uns3* ((lambda+2*xmu)*hij(2))* pqialf(2,4)
! COMPORTEMENT SHB8 PLEXUS
        qialfa(3,4) = qialfa(3,4) + xmu*hij(1)*uns3*pqialf(3,4)
    endif
!
    if (irdc .eq. 2) then
        qialfa(1,1) = qialfa(1,1)
        qialfa(2,2) = qialfa(2,2)
        qialfa(3,3) = qialfa(3,3)
        qialfa(1,2) = qialfa(1,2)
        qialfa(2,3) = qialfa(2,3) + ((lambda+2.d0*xmu)*hij(2))*pqialf( 2,3)
        qialfa(1,3) = qialfa(1,3) + ((lambda+2.d0*xmu)*hij(1))*pqialf( 1,3)
        qialfa(2,1) = qialfa(2,1)
        qialfa(3,2) = qialfa(3,2)
        qialfa(3,1) = qialfa(3,1)
        qialfa(1,4) = qialfa(1,4) + uns3* ((lambda+2*xmu)*hij(1))* pqialf(1,4)
        qialfa(2,4) = qialfa(2,4) + uns3* ((lambda+2*xmu)*hij(2))* pqialf(2,4)
! COMPORTEMENT C.P.
        qialfa(3,4) = qialfa(3,4)
    endif
    if (irdc .eq. 3) then
! COMPORTEMENT LOI TRIDIM MMC 3D
        xcooef = xyoung/ ((1+xnu)* (1-2*xnu))
        qialfa(1,1) = qialfa(1,1)
        qialfa(2,2) = qialfa(2,2)
        qialfa(3,3) = qialfa(3,3)
        qialfa(1,2) = qialfa(1,2)
        qialfa(2,3) = qialfa(2,3) + ((xcooef* (1-xnu))*hij(2))*pqialf( 2,3)
        qialfa(1,3) = qialfa(1,3) + ((xcooef* (1-xnu))*hij(1))*pqialf( 1,3)
        qialfa(2,1) = qialfa(2,1)
        qialfa(3,2) = qialfa(3,2)
        qialfa(3,1) = qialfa(3,1)
        qialfa(1,4) = qialfa(1,4) + uns3* ((xcooef* (1-xnu))*hij(1))* pqialf(1,4)
        qialfa(2,4) = qialfa(2,4) + uns3* ((xcooef* (1-xnu))*hij(2))* pqialf(2,4)
        qialfa(3,4) = qialfa(3,4) + xcooef* (1-xnu)*hij(3)*uns3* pqialf(3,4)
    endif
! SAUVEGARDE DES FORCES DE STABILISATION
!
    do 800 i = 1, 4
        do 790 j = 1, 3
            fstab(j+ (i-1)*3) = qialfa(j,i)
790      continue
800  continue
!
! CALCUL DES FORCES DE HOURGLASS FHG
!
    do 820 i = 1, 8
        do 810 j = 1, 3
            fhg(j,i) = 0.d0
810      continue
820  continue
    do 850 j = 1, 3
        do 840 i = 1, 8
            do 830 ia = 1, 4
                fhg(j,i) = fhg(j,i) + qialfa(j,ia)*gs(i,ia)
830          continue
840      continue
850  continue
! ON REPASSE AU REPERE GLOBAL
    do 870 i = 1, 3
        do 860 j = 1, 8
            fhg24((j-1)*3+i) = fhg(i,j)
860      continue
870  continue
    call shvrot(rr12, fhg24, 2)
!
! RAJOUT DE LA STABILISATION AU B SIGMA DEJA CALCULE
!
    do 890 j = 1, 3
        do 880 i = 1, 8
            f(j,i) = f(j,i) + fhg24((i-1)*3+j)
880      continue
890  continue
!
! ATTENTION A L'ORDRE DE XIVECT
!
    do 910 i = 1, 3
        do 900 j = 1, 8
            xivect((j-1)*3+i) = f(i,j)
900      continue
910  continue
!
end subroutine
