      SUBROUTINE MEFROT(NDIM,SOM,VIT0,PROMAS,PROVIS,Z,RU,RINT,RE,
     &                  CP,CF,DH,VIT,RHO,VISC,
     &                  ITYPG,ZG,TG,DG,RUGG,AXG,XIG,AFLUID,PM,
     &                  CFG,VITG,RHOG,VISCG)
      IMPLICIT   NONE
C
      INTEGER       NDIM(14)
      REAL*8        SOM(9),VIT0,Z(*),RU,RINT(*),RE(*),CP(*),CF(*),DH
      REAL*8        VIT(*),RHO(*),VISC(*)
      CHARACTER*8   PROMAS,PROVIS
C
      INTEGER       ITYPG(*)
      REAL*8        ZG(*),TG(*),DG(*),RUGG(*),AXG(*),XIG(*)
      REAL*8        AFLUID,PM
      REAL*8        CFG(*),VITG(*),RHOG(*),VISCG(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 20/09/2000   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C TOLE CRP_21
C ----------------------------------------------------------------------
C     CALCUL DU DIAMETRE HYDRAULIQUE, ET DES NOMBRES DE REYNOLDS
C     OPERATEUR APPELANT : OP0144 , FLUST3, MEFIST
C ----------------------------------------------------------------------
C     OPTION DE CALCUL   : CALC_FLUI_STRU , CALCUL DES PARAMETRES DE
C     COUPLAGE FLUIDE-STRUCTURE POUR UNE CONFIGURATION DE TYPE "FAISCEAU
C     DE TUBES SOUS ECOULEMENT AXIAL"
C ----------------------------------------------------------------------
C IN  : NDIM   : TABLEAU DES DIMENSIONS
C IN  : SOM    : COORDONNEES DES SOMMETS DE L'ENCEINTE RECTANGULAIRE
C                OU XEXT,YEXT,REXT
C IN  : VIT0   : VITESSE MOYENNE D ECOULEMENT DU FLUIDE
C IN  : PROMAS : PROFIL DE MASSE VOLUMIQUE DU FLUIDE, DE TYPE FONCTION
C IN  : PROVIS : PROFIL DE VISCOSITE DU FLUIDE, DE TYPE FONCTION
C IN  : Z      : COORDONNEES 'Z'  DES DES POINTS DE DISCRETISATION DANS
C                LE REPERE AXIAL
C IN  : RU     : RUGOSITE DES CYLINDRES
C IN  : RINT   : RAYONS DES CYLINDRES
C OUT : RE     : NOMBRE DE REYNOLDS AUX POINTS DE DISCRETISATION
C OUT : CP     : COEFFICIENT DE PORTANCE CP DU FLUIDE AUTOUR D UN
C                CYLINDRE INCLINE, AUX POINTS DE DISCRETISATION
C OUT : CF     : COEFFICIENT DE TRAINEE VISQUEUSE DU FLUIDE LE LONG DES
C                PAROIS, AUX POINTS DE DISCRETISATION
C OUT : DH     : DIAMETRE HYDRAULIQUE
C OUT : VIT    : VITESSE D ECOULEMENT DU FLUIDE AUX POINTS DE
C                DISCRETISATION
C OUT : RHO    : MASSE VOLUMIQUE DU FLUIDE AUX POINTS DE DISCRETISATION
C IN  : VISC   : VISCOSITE DU FLUIDE AUX POINTS DE DISCRETISATION
C
C IN  : ITYPG  : VECTEUR DES TYPES DE GRILLES (2=EXTREMITE,1=MELANGE)
C IN  : ZG     : COORDONNEES 'Z' DES POSITIONS DES GRILLES DANS LE
C                REPERE AXIAL
C IN  : TG     : VECTEUR DES EPAISSEUR DES PLAQUETTES
C IN  : DG     : VECTEUR DES LONGUEURS DE PLAQUETTES
C IN  : RUGG   : VECTEUR DES RUGOSITES DES TYPES DE GRILLES
C OUT : AXG    : VECTEUR DES SECTIONS SOLIDE DES TYPES DE GRILLES
C OUT : XIG    : VECTEUR DES PERIMETRES MOUILLES DES TYPES DE GRILLES
C OUT : AFLUID : SECTION FLUIDE DE L'ECOULEMENT EN L'ABSENCE DE GRILLES
C OUT : PM     : PERIMETRE MOUILLE DE L'ECOULEMENT EN L'ABSENCE
C                DE GRILLES
C OUT : CFG    : VECTEUR DES COEEF DE FROTTEMENT DES TYPES DE GRILLES
C OUT : VITG   : VITESSE D'ECOULEMENT DU  FLUIDE AUX POINTS DE 
C               POSITIONNEMENT DES GRILLES
C OUT : RHOG   : MASSE VOLUMIQUE DU FLUIDE AUX MEMES POINTS
C OUT : VISCG  : VISCOSITE DU FLUIDE AUX MEMES POINTS 
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       I, J, K, IDHG, IENCEI, IER, IREG, NBCYL, NBGTOT,
     +              NBZ, NDIR, NTYPG
      REAL*8        ECART, XSOM(4), YSOM(4), PI, R8PI, REXT, RHO0
      CHARACTER*8   NOMPAR
      REAL*8        A,B,RELIM1,RELIM2,NBPLAQ
      REAL*8        X12,Y12,LONG12,X23,Y23,LONG23
C ----------------------------------------------------------------------
      CALL JEMARQ()
C
C --- LECTURE DES DIMENSIONS
      NBZ    = NDIM(1)
      NBCYL  = NDIM(3)
      IENCEI = NDIM(6)
      NDIR   = NDIM(10)
      NTYPG  = NDIM(13)
      NBGTOT = NDIM(14)
C
C --- CREATION DES OBJETS DE TRAVAIL
      IF(NTYPG.NE.0) THEN
        CALL WKVECT('&&MEFROT.TEMP.DHG','V V R',NTYPG ,IDHG)
        CALL WKVECT('&&MEFROT.TEMP.REG','V V R',NBGTOT,IREG)
      ENDIF
C
      PI = R8PI()
C
      IF(IENCEI.EQ.1) REXT = SOM(3)
C
C --- PARAMETRE POUR L APPEL DES FONCTIONS, PROFIL DE MASSE VOLUMIQUE ET
C --- PROFIL DE VISCOSITE
      IF(NDIR.EQ.1) THEN
          NOMPAR = 'X'
      ELSE IF(NDIR.EQ.2) THEN
          NOMPAR = 'Y'
      ELSE IF(NDIR.EQ.3) THEN
          NOMPAR = 'Z'
      ENDIF
C
C --- PROFIL DE MASSE VOLUMIQUE ET DE VISCOSITE AUX POINTS DE
C --- DISCRETISATION
      DO 10 I = 1,NBZ
          CALL FOINTE('F ',PROMAS,1,NOMPAR,Z(I), RHO(I),IER)
          CALL FOINTE('F ',PROVIS,1,NOMPAR,Z(I),VISC(I),IER)
  10  CONTINUE
C
C --- CALCUL DE LA MASSE VOLUMIQUE DE REFERENCE
      RHO0 = RHO(1)
      RHO(0) = RHO0
C
C --- PROFIL DE VITESSE AUX POINTS DE DISCRETISATION
C
      DO 30 I = 1,NBZ
          VIT(I) = RHO0 * VIT0 / RHO(I)
  30  CONTINUE
      VIT(0) = VIT0
C
C
C --- CALCUL DU DIAMETRE HYDRAULIQUE DH
C --- DU SYSTEME CYLINDRES+ENCEINTE
C
      A = 0.D0
      B = 0.D0
C
      DO 40 I = 1,NBCYL
         A = A+RINT(I)*RINT(I)
         B = B+RINT(I)
  40  CONTINUE
C
C --- ENCEINTE CIRCULAIRE
      IF (IENCEI.EQ.1) THEN
C
         REXT = SOM(3)
         DH = 2.D0*(REXT*REXT-A)/(REXT+B)
C
         IF(NTYPG.NE.0) THEN
           AFLUID = PI*REXT*REXT-PI*A
           PM = 2.D0*(PI*REXT+PI*B)
         ENDIF
C
C --- ENCEINTE RECTANGULAIRE
      ELSE IF (IENCEI.EQ.2) THEN
C
         DO 50 I = 1,4
             XSOM(I) = SOM(2*I-1)
             YSOM(I) = SOM(2*I)
  50     CONTINUE
C
         X12 = XSOM(2)-XSOM(1)
         Y12 = YSOM(2)-YSOM(1)
         LONG12 = (X12*X12+Y12*Y12)
         LONG12 = SQRT(LONG12)
C
         X23 = XSOM(3)-XSOM(2)
         Y23 = YSOM(3)-YSOM(2)
         LONG23 = (X23*X23+Y23*Y23)
         LONG23 = SQRT(LONG23)
C
         DH = 2.D0*(LONG12*LONG23-PI*A)/
     &           (LONG12+LONG23+PI*B)
C
         IF(NTYPG.NE.0) THEN
           AFLUID = LONG12*LONG23-PI*A
           PM = 2.D0*(LONG12+LONG23+PI*B)
         ENDIF
C
      ELSE IF (IENCEI.EQ.0) THEN
         DH = 100.D0
      ENDIF
C
      DO 60 I = 1,NBZ
C
C --- CALCUL DU NOMBRE DE REYNOLDS BASE SUR LE DIAMETRE
C --- HYDRAULIQUE DH ET LA VITESSE AXIALE DU FLUIDE
C
         RE(I) = DH*ABS(VIT(I))/VISC(I)
C
C ---    NOMBRES DE REYNOLDS DE TRANSITION
C
         RELIM1 = 23.D0*DH/RU
         RELIM2 = 560.D0*DH/RU
C
         IF (VIT(I).NE.0.D0) THEN
C
C ---       CALCUL DU COEFFICIENT DE PORTANCE CP
C ---       DU FLUIDE AUTOUR D UN CYLINDRE INCLINE
C ---       --------------------------------------
            CP(I) = 0.080D0
C
C ---       CALCUL DU COEFFICIENT DE TRAINEE VISQUEUSE
C ---       CF DU FLUIDE LE LONG DES PAROIS
C ---       ------------------------------------------
C
C ---       REGIME LAMINAIRE
C
            IF (RE(I).LT.2000.D0) THEN
               CF(I) = 16.D0*PI/RE(I)
            ENDIF
C
C ---       REGIME CRITIQUE
C
            IF ((RE(I).GE.2000.D0).AND.(RE(I).LE.4000.D0)) THEN
               CF(I) = 1.2D-04*PI*(RE(I)**0.55D0)
            ENDIF
C
C ---       REGIME HYDRAULIQUEMENT LISSE
C
            IF ((RE(I).GT.4000.D0).AND.(RE(I).LT.RELIM1)) THEN
               CF(I) = 1.D0*PI/4.D0/(1.8D0*LOG10(RE(I))-1.64D0)
     &                           /(1.8D0*LOG10(RE(I))-1.64D0)
            ENDIF
C
C ---       TRANSITION TURBULENTE
C
            IF ((RE(I).GE.RELIM1).AND.(RE(I).LE.RELIM2)) THEN
               CF(I) = 0.1D0*PI*((1.46D0*RU/DH+
     &                          100.D0/RE(I))**0.25D0)/4.D0
            ENDIF
C
C ---       REGIME QUADRATIQUE TURBULENT
C
            IF (RE(I).GT.RELIM2) THEN
               CF(I) = PI/(2.D0*LOG10(3.7D0*DH/RU))
     &                 /(2.D0*LOG10(3.7D0*DH/RU))
     &                 /4.D0
            ENDIF
C
C
         ELSE
            CF(I) = 0.D0
            CP(I) = 0.D0
         ENDIF
  60  CONTINUE
C
C
C     ----------------------------------------------
C     CALCUL DU PROFIL DE MASSE VOLUMIQUE AU NIVEAU 
C     DES GRILLES PAR INTERPOLATION LINEAIRE
C     ----------------------------------------------
C
      IF (NTYPG.NE.0) THEN
C
         DO 18 I=2,NBZ
            DO 19 J=1,NBGTOT
               ECART=(Z(I)-ZG(J))*(Z(I-1)-ZG(J))
C
               IF (ECART.LE.0.D0) THEN
                  RHOG(J)=( RHO(I-1)*(Z(I)-ZG(J))+
     &                      RHO(I)*(ZG(J)-Z(I-1)) )/
     &                         (Z(I)-Z(I-1))
C
C    ----------------------------------------
C      CALCUL DU PROFIL DE VISCOSITE CINEMATIQUE AU NIVEAU 
C     DES GRILLES PAR INTERPOLATION LINEAIRE
C     ---------------------------------------
C
                  VISCG(J)=( VISC(I-1)*(Z(I)-ZG(J))+
     &                      VISC(I)*(ZG(J)-Z(I-1)) )/
     &                         (Z(I)-Z(I-1))      
               END IF
19          CONTINUE
18       CONTINUE
C
C     ---------------------------------------------------------
C     CALCUL DU PROFIL AXIAL DE VITESSE AU NIVEAU DES GRILLES
C     (CONSERVATION DU DEBIT MASSIQUE) 
C     ---------------------------------------------------------
C
         DO 15 J=1,NBGTOT
            VITG(J)=0.D0
15       CONTINUE
C
         NBPLAQ=2*(SQRT(DBLE(NBCYL))+1)
         DO 85 K=1,NTYPG
            AXG(K)=NBPLAQ*TG(K)*DG(K)-(0.5D0*NBPLAQ*TG(K))*
     &                                (0.5D0*NBPLAQ*TG(K))
            XIG(K)=4*DG(K)+SQRT(DBLE(NBCYL))*4*
     &                     (DG(K)-0.5D0*NBPLAQ*TG(K))
85       CONTINUE
C
         DO 11 J=1,NBGTOT
            DO 84 K=1,NTYPG
               IF (ITYPG(J).EQ.K) THEN
                VITG(J)=1.D0/(1.D0-(AXG(K)/AFLUID))*
     &                                (1.D0/RHOG(J))*RHO0*VIT0
                END IF
84          CONTINUE  
11       CONTINUE
C
C     ------------------------------------------------------------------
C     CALCUL DU PROFIL DU NOMBRE DE REYNOLDS STATIONNAIRE
C     AU NIVEAU DES GRILLES
C     ------------------------------------------------------------------
C 
         DO 86 K=1,NTYPG
            ZR(IDHG+K-1)=4.D0*(AFLUID-AXG(K))/(PM+XIG(K))
86       CONTINUE
C
         DO 81 I=1,NBZ
            DO 82 J=1,NBGTOT
               ECART=(Z(I)-ZG(J))*(Z(I-1)-ZG(J))
C
               IF (ECART.LE.0.D0) THEN
               DO 87 K=1,NTYPG
                  IF (ITYPG(J).EQ.K) THEN
                  ZR(IREG+J-1)=ZR(IDHG+K-1)*ABS(VITG(J))/VISCG(J)
                  END IF
87             CONTINUE
               END IF
82          CONTINUE
81       CONTINUE
C
C     ----------------------------------------------------------------
C     CALCUL DU PROFIL DU COEFFICIENT DE FROTTEMENT CFG
C     AU NIVEAU DES GRILLES
C     ----------------------------------------------------------------
C
         DO 13 J=1,NBGTOT
            CFG(J)=0.D0
13       CONTINUE
C
         DO 14 J=1,NBGTOT
C    
            IF (VITG(J).NE.0.D0) THEN
C
C              ----------------
C              REGIME LAMINAIRE
C              ----------------
               IF (ZR(IREG+J-1).LT.2000.D0) THEN
                  CFG(J)=16.D0*PI/ZR(IREG+J-1)
               END IF
C
C              ---------------
C              REGIME CRITIQUE
C              ---------------
               IF ((ZR(IREG+J-1).GE.2000.D0).AND.
     &             (ZR(IREG+J-1).LE.4000.D0)) THEN
                  CFG(J)=1.2D-04*PI*(ZR(IREG+J-1)**0.55D0)
               END IF
C      
C     REDEFINITION DES COEFFICIENTS DE TRANSITION DANS LE CAS
C     DES GRILLES
C     ------------------------------------------------------------------
C
               DO 89 K=1,NTYPG
                  IF (ITYPG(J).EQ.K) THEN
                     RELIM1=23.D0*ZR(IDHG+K-1)/RUGG(K)
                     RELIM2=560.D0*ZR(IDHG+K-1)/RUGG(K)
                  END IF
89             CONTINUE
C
C              ----------------------------
C              REGIME HYDRAULIQUEMENT LISSE
C              ----------------------------
               IF ((ZR(IREG+J-1).GT.4000.D0).AND.
     &             (ZR(IREG+J-1).LT.RELIM1)) THEN
                  CFG(J)=1.D0*PI/4.D0/(1.8D0*LOG10(ZR(IREG+J-1))-1.64D0)
     &                              /(1.8D0*LOG10(ZR(IREG+J-1))-1.64D0)
               END IF
C
C              ---------------------
C              TRANSITION TURBULENTE
C              ---------------------
               IF ((ZR(IREG+J-1).GE.RELIM1).AND.
     &             (ZR(IREG+J-1).LE.RELIM2)) THEN
                  K = ITYPG(J)
                  CFG(J)=0.1D0*PI*((1.46D0*RUGG(K)/ZR(IDHG+K-1)+
     &                             100.D0/ZR(IREG+J-1))**0.25D0)/4.D0
               END IF
C
C              -----------------------------
C              REGIME QUADRATIQUE TURBULENT
C              -----------------------------
               IF (ZR(IREG+J-1).GT.RELIM2) THEN
                  K = ITYPG(J)
                  CFG(J)=PI/(2.D0*LOG10(3.7D0*ZR(IDHG+K-1)/RUGG(K)))
     &                    /(2.D0*LOG10(3.7D0*ZR(IDHG+K-1)/RUGG(K)))
     &                    /4.D0
               END IF
C
            ELSE
               CFG(J)=0.D0
            END IF 
C
14       CONTINUE
C
      ENDIF
C
      IF ( NTYPG .NE. 0 ) THEN
         CALL JEDETR ( '&&MEFROT.TEMP.DHG' )
         CALL JEDETR ( '&&MEFROT.TEMP.REG' )
      ENDIF
      CALL JEDEMA()
      END
