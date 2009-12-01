      SUBROUTINE SH8FOR(XETEMP,PARA,XIDEPM,SIGMA,FSTAB,XIVECT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 24/03/2009   AUTEUR REZETTE C.REZETTE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY  
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY  
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR     
C (AT YOUR OPTION) ANY LATER VERSION.                                   
C                                                                       
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT   
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF            
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU      
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.                              
C                                                                       
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE     
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,         
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C
C               ELEMENT SHB8
C
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER LAG,IRDC
      REAL*8 EYG(5),ENU(5),FSTAB(12),PARA(*)
      REAL*8 XE(24),XIDEPM(*),SIGMA(*)
      REAL*8 XXG5(5),PXG5(5),XCOQ(3,4),BKSIP(3,8,5),B(3,8)
      REAL*8 XCENT(3),PPP(3,3)
      REAL*8 XL(3,4),XXX(3),YYY(3),XIVECT(24)
      REAL*8 TMPKE(24,24),TMPKE2(24,24)
      REAL*8 XXVB(3),HIJ(6)
      REAL*8 GB(8,4),GS(8,4),XXGB(3,4)
      REAL*8 RR2(3,3),LAMBDA,XELOCP(24)
      REAL*8 UDEF(24),XXLOC(24),XLOC12(24),SIGLOC(6)
      REAL*8 F(3,8),SIGMAG(6),PQIALF(3,4)
      REAL*8 QIALFA(3,4),FHG(3,8),RR12(3,3),RR1(3,3),FHG24(24)
      REAL*8 SITMP1(8,8),SITMP2(8,8),POIDS
      REAL*8 FQ(24),XETEMP(*)
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C------------FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C INITIALISATIONS
C
C INFOS:
C XE EST RANGE COMME CA:
C (XNOEUD1 YNOEUD1 ZNOEUD1, XNOEUD2 YNOEUD2 ZNOEUD2,...)
C DANS SHB8_TEST_NUM: ATTENTION A LA NUMEROTATION DES NOEUDS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      IF (NOMSHB.EQ.'SHB8') THEN
C
         DATA GB/1.D0,1.D0,-1.D0,-1.D0,-1.D0,-1.D0,1.D0,
     &    1.D0,1.D0,-1.D0,-1.D0,1.D0,-1.D0,1.D0,1.D0,
     &   -1.D0,1.D0,-1.D0,1.D0,-1.D0,1.D0,-1.D0,1.D0,
     &   -1.D0,-1.D0,1.D0,-1.D0,1.D0,1.D0,-1.D0,1.D0,-1.D0/
C
C ON DEFINI LES POINTS GAUSS ET LES POIDS
C
         XXG5(1) = -0.906179845938664D0
         XXG5(2) = -0.538469310105683D0
         XXG5(3) = 0.D0
         XXG5(4) = 0.538469310105683D0
         XXG5(5) = 0.906179845938664D0
C
         PXG5(1) = 0.236926885056189D0
         PXG5(2) = 0.478628670499366D0
         PXG5(3) = 0.568888888888889D0
         PXG5(4) = 0.478628670499366D0
         PXG5(5) = 0.236926885056189D0
C
         UNS8 = 1.D0/8.D0
         UNS3 = 1.D0/3.D0
C
C -----------------------------------------------------
C ON VERIFIE QUE LA CONNECTIVITE DONNE UN REPERE DIRECT
C SI CE N EST PAS LE CAS ON PERMUTE LES NOEUDS
C -----------------------------------------------------
C
C     ON FAIT UNE COPIE DE XETEMP DANS XE
         DO 10 I = 1,24
           XE(I) = XETEMP(I)
   10    CONTINUE
C TYPE DE LOI DE COMPORTEMENT:
C     IRDC = 1 : SHB8 TYPE PLEXUS
C     IRDC = 2 : C.P.
C     IRDC = 3 : 3D COMPLETE
C**         IRDC = OUT(1)
         IRDC = NINT(PARA(5))
         LAG = NINT(PARA(6))
         CALL R8INIR(64,0.D0,SITMP2,1)
         DO 470 J = 1,8
          DO 460 I = 1,3
            F(I,J) = 0.D0
  460     CONTINUE
  470    CONTINUE
C
C CALCUL DE BKSIP(3,8,IP) DANS REPERE DE REFERENCE
C      BKSIP(1,*,IP) = VECTEUR BX AU POINT GAUSS IP
C      BKSIP(2,*,IP) = VECTEUR BY AU POINT GAUSS IP
C      BKSIP(3,*,IP) = VECTEUR BZ AU POINT GAUSS IP
C
         CALL SHBKSI(5,XXG5,BKSIP)
         DO 540 IP = 1,5
C
C RECHERCHE DE SIGMA DU POINT DE GAUSS GLOBAL

          DO 480 I = 1,6
C
C C'EST LES CONTRAINTES LOCALES POUR POUVOIR TRAITER LA PLASTICITE AVANT
C LES CONTRAINTES SONT PASSEES DANS LA CONFI.
C A LA FIN DU PAS DANS Q8PKCN
C
            SIGLOC(I) = SIGMA((IP-1)*6+I)
  480     CONTINUE
          ZETA = XXG5(IP)
          ZLAMB = 0.5D0* (1.D0-ZETA)
          DO 500 I = 1,4
            DO 490 J = 1,3
              XCOQ(J,I) = ZLAMB*XE((I-1)*3+J) +
     &                    (1.D0-ZLAMB)*XE((I-1+4)*3+J)
  490       CONTINUE
  500     CONTINUE
          CALL RLOSHB(XCOQ,XCENT,PPP,XL,XXX,YYY,RBID)
C
C PASSAGE DES CONTRAINTES AU REPERE GLOBAL
C
          CALL CHRP3D(PPP,SIGLOC,SIGMAG,1)
C
          CALL SHCALB(BKSIP(1,1,IP),XE,B,AJAC)
C
C CALCUL DE BQ.SIGMA SI LAGRANGIEN TOTAL
C
          IF (LAG.EQ.1) THEN
            DO 507 J = 1,8
               DO 506 I = 1,8
                  SITMP1(I,J) = 0.D0
  506          CONTINUE
  507       CONTINUE
C
            DO 509 J = 1,8
               DO 508 I = 1,8
                  SITMP1(I,J) = SIGMAG(1)*B(1,I)*B(1,J) + 
     &             SIGMAG(2)*B(2,I)*B(2,J) +
     &             SIGMAG(3)*B(3,I)*B(3,J) + 
     &             SIGMAG(4)*(B(1,I)*B(2,J)+B(2,I)*B(1,J)) +
     &             SIGMAG(6)* (B(1,I)*B(3,J)+B(3,I)*B(1,J)) +
     &             SIGMAG(5)* (B(3,I)*B(2,J)+B(2,I)*B(3,J))
  508          CONTINUE
  509       CONTINUE
C***            CALL SHASKS(SIGMAG,B,SITMP1)
            DO 520 J = 1,8
              DO 510 I = 1,8
                SITMP2(I,J) = SITMP2(I,J) +
     &              4.D0*AJAC*PXG5(IP)*SITMP1(I,J)
  510         CONTINUE
  520       CONTINUE
          END IF
C
C CALCUL DE B.SIGMA EN GLOBAL
C
          POIDS = 4.D0*AJAC*PXG5(IP)
          DO 530 K = 1,8
            F(1,K) = F(1,K) + POIDS* (B(1,K)*SIGMAG(1)+B(2,K)*SIGMAG(4)+
     &               B(3,K)*SIGMAG(6))
            F(2,K) = F(2,K) + POIDS* (B(1,K)*SIGMAG(4)+B(2,K)*SIGMAG(2)+
     &               B(3,K)*SIGMAG(5))
            F(3,K) = F(3,K) + POIDS* (B(1,K)*SIGMAG(6)+B(2,K)*SIGMAG(5)+
     &               B(3,K)*SIGMAG(3))
  530     CONTINUE
  540    CONTINUE
C
C SI LAGRANGIEN TOTAL: RAJOUT DE FQ A F
C
         IF (LAG.EQ.1) THEN
          CALL R8INIR(576,0.D0,TMPKE,1)
          DO 570 KK = 1,3
            DO 560 I = 1,8
              DO 550 J = 1,8
                TMPKE(I+ (KK-1)*8,J+ (KK-1)*8) = SITMP2(I,J)
  550         CONTINUE
  560       CONTINUE
  570     CONTINUE
          CALL R8INIR(576,0.D0,TMPKE2,1)
          DO 590 J = 1,8
            DO 580 I = 1,24
              TMPKE2(I, (J-1)*3+1) = TMPKE(I,J)
              TMPKE2(I, (J-1)*3+2) = TMPKE(I,J+8)
              TMPKE2(I, (J-1)*3+3) = TMPKE(I,J+16)
  580       CONTINUE
  590     CONTINUE
          CALL R8INIR(576,0.D0,TMPKE,1)
          DO 610 I = 1,8
            DO 600 J = 1,24
              TMPKE((I-1)*3+1,J) = TMPKE2(I,J)
              TMPKE((I-1)*3+2,J) = TMPKE2(I+8,J)
              TMPKE((I-1)*3+3,J) = TMPKE2(I+16,J)
  600       CONTINUE
  610     CONTINUE
C          DO 611 J = 1,24
C	     WRITE(6,*) 'XIDEPM =',I,XIDEPM(I)
C  611     CONTINUE
          CALL MULMAT(24,24,1,TMPKE,XIDEPM,FQ)
          DO 620 K = 1,8
            F(1,K) = F(1,K) + FQ((K-1)*3+1)
            F(2,K) = F(2,K) + FQ((K-1)*3+2)
            F(3,K) = F(3,K) + FQ((K-1)*3+3)
  620     CONTINUE
         END IF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C ON CALCULE LA STABILISATION POUR LE CALCUL DE B.SIGMA
C ON A BESOIN DU MATERIAU AUSSI!
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
         XYOUNG = PARA(1)
         XNU = PARA(2)
         LAMBDA = XYOUNG*XNU/ (1-XNU*XNU)
         XMU = 0.5D0*XYOUNG/ (1+XNU)
C
C DEPLACEMENT NODAL, REPERE GLOBAL 
C
         DO 630 I = 1,24
          XXLOC(I) = XE(I)
          XELOCP(I) = XE(I) - XIDEPM(I)
          XLOC12(I) = XE(I) - 0.5D0*XIDEPM(I)
  630    CONTINUE
C ATTENTION, RR, MATRICE DU CORROTATIONNEL EST RANGEE PAR LIGNES!
C
         CALL SHBROT(XXLOC,RR2)
         CALL SHBROT(XELOCP,RR1)
         CALL SHBROT(XLOC12,RR12)
         CALL SHVROT(RR2,XXLOC,1)
         CALL SHVROT(RR1,XELOCP,1)
         CALL SHVROT(RR12,XLOC12,1)
C
C  ON A BESOIN DES VECTEURS GAMMA(8, ALPHA=1 A 4) = GS(8,4)
C  ORIGINE DE XLOC12 PAS BONNE, MAIS PAS GRAVE :
C  DIFFERENTIELLES DANS B
C
         CALL SHBBAR(XLOC12,B,VOL)
C
C CALCUL DEPLACEMENT DEFORMANT DANS REPERE 1/2 PAS:
C
         DO 640 I = 1,24
          UDEF(I) = XXLOC(I) - XELOCP(I)
  640    CONTINUE
C
C CALCUL DES FONCTIONS DE FORME  GS
C XXGB = X  * GB
C
         DO 660 IA = 1,4
          DO 650 J = 1,3
            XXGB(J,IA) = HOUXGB(XLOC12(J),IA)
  650     CONTINUE
  660    CONTINUE
C
C GS = (BBB)  * XXGB
C
         DO 680 J = 1,4
          DO 670 I = 1,8
            GS(I,J) = 0.D0
  670     CONTINUE
  680    CONTINUE
         DO 710 J = 1,3
          DO 700 IA = 1,4
            DO 690 I = 1,8
              GS(I,IA) = GS(I,IA) + B(J,I)*XXGB(J,IA)
  690       CONTINUE
  700     CONTINUE
  710    CONTINUE
C
C GS = GB - GS
C
         DO 730 I = 1,4
          DO 720 J = 1,8
            GS(J,I) = (GB(J,I)-GS(J,I))*UNS8
  720     CONTINUE
  730    CONTINUE
C
C CALCUL DE XXVB = X * VB
C
         XXVB(1) = -XLOC12(1) + XLOC12(4) + XLOC12(7) -
     &            XLOC12(10) - XLOC12(13) + XLOC12(16) +
     &            XLOC12(19) - XLOC12(22)
         XXVB(2) = -XLOC12(2) - XLOC12(5) + XLOC12(8) +
     &            XLOC12(11) - XLOC12(14) - XLOC12(17) +
     &            XLOC12(20) + XLOC12(23)
         XXVB(3) = -XLOC12(3) - XLOC12(6) - XLOC12(9) -
     &            XLOC12(12) + XLOC12(15) + XLOC12(18) +
     &            XLOC12(21) + XLOC12(24)
C
C CALCUL DES RELATIONS CONTRAINTES ET DEFORMATIONS GENERALISEES
C
         HIJ(1) = UNS3*XXVB(2)*XXVB(3)/XXVB(1)
         HIJ(2) = UNS3*XXVB(1)*XXVB(3)/XXVB(2)
         HIJ(3) = UNS3*XXVB(2)*XXVB(1)/XXVB(3)
         HIJ(4) = UNS3*XXVB(3)
         HIJ(5) = UNS3*XXVB(1)
         HIJ(6) = UNS3*XXVB(2)
C
C CALCUL DES DEFORMATIONS GENERALISEES
C
         DO 760 IA = 1,4
          DO 750 J = 1,3
            AUX = 0.D0
            DO 740 I = 1,8
              AUX = AUX + GS(I,IA)*UDEF((I-1)*3+J)
  740       CONTINUE
            PQIALF(J,IA) = AUX
  750     CONTINUE
  760    CONTINUE
C
C CALCUL DES CONTRAINTES GENERALISEES
C
         DO 780 I = 1,4
          DO 770 J = 1,3
            QIALFA(J,I) = FSTAB(J+ (I-1)*3)
  770     CONTINUE
  780    CONTINUE
C
         IF (IRDC.EQ.1) THEN
          QIALFA(1,1) = QIALFA(1,1)
          QIALFA(2,2) = QIALFA(2,2)
          QIALFA(3,3) = QIALFA(3,3)
          QIALFA(1,2) = QIALFA(1,2)
          QIALFA(2,3) = QIALFA(2,3) +
     &                   ((LAMBDA+2.D0*XMU)*HIJ(2))*PQIALF(2,3)
          QIALFA(1,3) = QIALFA(1,3) +
     &                   ((LAMBDA+2.D0*XMU)*HIJ(1))*PQIALF(1,3)
          QIALFA(2,1) = QIALFA(2,1)
          QIALFA(3,2) = QIALFA(3,2)
          QIALFA(3,1) = QIALFA(3,1)
          QIALFA(1,4) = QIALFA(1,4) +
     &                   UNS3* ((LAMBDA+2*XMU)*HIJ(1))*PQIALF(1,4)
          QIALFA(2,4) = QIALFA(2,4) +
     &                   UNS3* ((LAMBDA+2*XMU)*HIJ(2))*PQIALF(2,4)
C COMPORTEMENT SHB8 PLEXUS
          QIALFA(3,4) = QIALFA(3,4) + XMU*HIJ(1)*UNS3*PQIALF(3,4)
         END IF
C
         IF (IRDC.EQ.2) THEN
          QIALFA(1,1) = QIALFA(1,1)
          QIALFA(2,2) = QIALFA(2,2)
          QIALFA(3,3) = QIALFA(3,3)
          QIALFA(1,2) = QIALFA(1,2)
          QIALFA(2,3) = QIALFA(2,3) +
     &                   ((LAMBDA+2.D0*XMU)*HIJ(2))*PQIALF(2,3)
          QIALFA(1,3) = QIALFA(1,3) +
     &                   ((LAMBDA+2.D0*XMU)*HIJ(1))*PQIALF(1,3)
          QIALFA(2,1) = QIALFA(2,1)
          QIALFA(3,2) = QIALFA(3,2)
          QIALFA(3,1) = QIALFA(3,1)
          QIALFA(1,4) = QIALFA(1,4) +
     &                   UNS3* ((LAMBDA+2*XMU)*HIJ(1))*PQIALF(1,4)
          QIALFA(2,4) = QIALFA(2,4) +
     &                   UNS3* ((LAMBDA+2*XMU)*HIJ(2))*PQIALF(2,4)
C COMPORTEMENT C.P.
          QIALFA(3,4) = QIALFA(3,4)
         END IF
         IF (IRDC.EQ.3) THEN
C COMPORTEMENT LOI TRIDIM MMC 3D
          XCOOEF = XYOUNG/ ((1+XNU)* (1-2*XNU))
          QIALFA(1,1) = QIALFA(1,1)
          QIALFA(2,2) = QIALFA(2,2)
          QIALFA(3,3) = QIALFA(3,3)
          QIALFA(1,2) = QIALFA(1,2)
          QIALFA(2,3) = QIALFA(2,3) +
     &                   ((XCOOEF* (1-XNU))*HIJ(2))*PQIALF(2,3)
          QIALFA(1,3) = QIALFA(1,3) +
     &                   ((XCOOEF* (1-XNU))*HIJ(1))*PQIALF(1,3)
          QIALFA(2,1) = QIALFA(2,1)
          QIALFA(3,2) = QIALFA(3,2)
          QIALFA(3,1) = QIALFA(3,1)
          QIALFA(1,4) = QIALFA(1,4) +
     &                   UNS3* ((XCOOEF* (1-XNU))*HIJ(1))*PQIALF(1,4)
          QIALFA(2,4) = QIALFA(2,4) +
     &                   UNS3* ((XCOOEF* (1-XNU))*HIJ(2))*PQIALF(2,4)
          QIALFA(3,4) = QIALFA(3,4) +
     &                   XCOOEF* (1-XNU)*HIJ(3)*UNS3*PQIALF(3,4)
         END IF
C SAUVEGARDE DES FORCES DE STABILISATION
C
         DO 800 I = 1,4
          DO 790 J = 1,3
            FSTAB(J+ (I-1)*3) = QIALFA(J,I)
  790     CONTINUE
  800    CONTINUE
C
C CALCUL DES FORCES DE HOURGLASS FHG
C
         DO 820 I = 1,8
          DO 810 J = 1,3
            FHG(J,I) = 0.D0
  810     CONTINUE
  820    CONTINUE
         DO 850 J = 1,3
          DO 840 I = 1,8
            DO 830 IA = 1,4
              FHG(J,I) = FHG(J,I) + QIALFA(J,IA)*GS(I,IA)
  830       CONTINUE
  840     CONTINUE
  850    CONTINUE
C ON REPASSE AU REPERE GLOBAL
         DO 870 I = 1,3
          DO 860 J = 1,8
            FHG24((J-1)*3+I) = FHG(I,J)
  860     CONTINUE
  870    CONTINUE
         CALL SHVROT(RR12,FHG24,2)
C
C RAJOUT DE LA STABILISATION AU B SIGMA DEJA CALCULE
C
         DO 890 J = 1,3
          DO 880 I = 1,8
            F(J,I) = F(J,I) + FHG24((I-1)*3+J)
  880     CONTINUE
  890    CONTINUE
C
C ATTENTION A L'ORDRE DE XIVECT
C
         DO 910 I = 1,3
          DO 900 J = 1,8
            XIVECT((J-1)*3+I) = F(I,J)
  900     CONTINUE
  910    CONTINUE
C
      END
