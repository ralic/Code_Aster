      SUBROUTINE TE0146(OPTION,NOMTE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     OPTION,NOMTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/05/2002   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C              SEE THE FILE "LICENSE.TERMS" FOR INFORMATION ON USAGE AND
C              REDISTRIBUTION OF THIS FILE.
C ======================================================================
C     CALCUL FORCES ELEMENTAIRES LINEIQUES POUR LES ELEMENTS DE POUTRE
C     D'EULER ET DE TIMOSHENKO
C TOLE CRP_6
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C        'FC1D1D_MECA'       : FORCES LINEIQUES (COMP)
C        'FR1D1D_MECA'       : FORCES LINEIQUES (REEL)
C        'FF1D1D_MECA'       : FORCES LINEIQUES (FONCTION)
C        'SR1D1D_MECA'       : FORCES LINEIQUES SUIVEUSES (FONCTION)
C        'CHAR_MECA_PESA_R'  : CHARGES DE PESANTEUR
C IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
C        'MECA_POU_D_E' : POUTRE DROITE D  EULER      (SECTION VARIABLE)
C        'MECA_POU_D_T' : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
C        'MECA_POU_C_T' : POUTRE COURBE DE TIMOSHENKO(SECTION CONSTANTE)
C        'MECA_POU_D_EM': POUTRE DROITE MULTIFIBRE D EULER (SECT. CONST)
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      REAL*8       PGL(3,3), FE(14), FR(14), FI(14), FGR(14), FGI(14)
      REAL*8       FER(12), FEI(12), XL, RAD, ANGARC, ANGS2, ANG,A,A2
      REAL*8       PGL1(3,3), PGL2(3,3),CARSEC(6)
      CHARACTER*16 CH16
      INTEGER NNO,NC,NC1,NNOC,NCC,LX,LRCOU,LORIEN,LSECT,ITYPE,LSECT2
      INTEGER LVECT,I ,NBFIB,NCARFI,JACF,JNF
C     ------------------------------------------------------------------
      NNO = 2
      NC  = 6
      NC1 = 6
      IF (NOMTE .EQ. 'MECA_POU_D_TG') NC1 = 7
      NNOC = 1
      NCC  = 6
C
C     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
      CALL JEVECH ('PGEOMER','L',LX)
      LX = LX - 1
      XL = SQRT( (ZR(LX+4)-ZR(LX+1))**2
     +           + (ZR(LX+5)-ZR(LX+2))**2 + (ZR(LX+6)-ZR(LX+3))**2 )
      IF( XL .EQ. 0.D0 ) THEN
         CH16 = ' ?????????'
         CALL UTMESS('F','ELEMENTS DE POUTRE (TE0146)',
     +                  'NOEUDS CONFONDUS POUR UN ELEMENT: '//CH16(:8))
      ENDIF
      IF (NOMTE .EQ. 'MECA_POU_C_T') THEN
         CALL JEVECH ('PCAARPO', 'L',LRCOU)
         RAD    = ZR(LRCOU)
         ANGARC = ZR(LRCOU+1)
         ANGS2  = TRIGOM('ASIN', XL / ( 2.D0 * RAD ) )
         ANG    = ANGS2 * 2.D0
         XL     = RAD * ANG
      ENDIF

C     --- RECUPERATION DES ORIENTATIONS ALPHA,BETA,GAMMA ---
      CALL JEVECH ('PCAORIE', 'L',LORIEN)
C
C     --- MATRICE DE PASSAGE DU REPERE GLOBAL AU REPERE LOCAL: PGL ---
      IF (NOMTE .EQ. 'MECA_POU_C_T') THEN
         CALL MATRO2 ( ZR(LORIEN) , ANGARC , ANGS2 , PGL1, PGL2 )
      ELSE
         CALL MATROT ( ZR(LORIEN) , PGL )
      ENDIF


      IF(NOMTE.EQ.'MECA_POU_D_EM')THEN
C ----- POUTRES MULTIFIBRES
C     --- RECUPERATION DES CARACTERISTIQUES DES FIBRES :
        CALL JEVECH('PNBSP_I','L',JNF)
        NBFIB = ZI(JNF)
        CALL JEVECH('PFIBRES','L',JACF)
        NCARFI = 3
        CALL PMFITG(NBFIB,NCARFI,ZR(JACF),CARSEC)
        A=CARSEC(1)
        A2=CARSEC(1)
        ITYPE=0
      ELSE


C       --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
        CALL JEVECH ('PCAGNPO', 'L',LSECT)
        LSECT = LSECT-1
        ITYPE =  NINT(ZR(LSECT+23))
        A     =  ZR(LSECT+1)
        LSECT2 = LSECT + 11
        A2     = ZR(LSECT2+1)
      END IF
C
C     --- PASSAGE DU REPERE LOCAL AU REPERE GLOBAL ---
      IF ( OPTION .EQ. 'CHAR_MECA_FC1D1D' ) THEN
         CALL PTFOCP(ITYPE,OPTION,NOMTE,A,A2,XL,RAD,
     +               ANGS2,1,NNO,NC,PGL,PGL1,PGL2, FR, FI )
         CALL JEVECH ('PVECTUC','E',LVECT)
         IF ( NOMTE .EQ. 'MECA_POU_C_T' ) THEN
            CALL UTPVLG ( NNOC, NCC, PGL1, FR, FGR )
            CALL UTPVLG ( NNOC, NCC, PGL2, FR(7), FGR(7) )
            CALL UTPVLG ( NNOC, NCC, PGL1, FI, FGI )
            CALL UTPVLG ( NNOC, NCC, PGL2, FI(7), FGI(7) )
         ELSE
            CALL UTPVLG ( NNO, NC, PGL, FR, FGR )
            CALL UTPVLG ( NNO, NC, PGL, FI, FGI )
         ENDIF
         DO 10 I = 1,12
            ZC(LVECT+I-1) = DCMPLX(FGR(I),FGI(I))
 10      CONTINUE
      ELSE
         CALL PTFORP(ITYPE,OPTION,NOMTE,A,A2,XL,RAD,
     +               ANGS2,1,NNO,NC,PGL,PGL1,PGL2, FER, FEI )
         DO 20 I = 1,NC
            FE(I)     = FER(I)
            FE(I+NC1) = FER(I+NC)
 20      CONTINUE
         IF (NC1.EQ.7) THEN
            FE(7) = 0.D0
            FE(14) = 0.D0
         ENDIF
         CALL JEVECH ('PVECTUR','E',LVECT)
         IF ( NOMTE .EQ. 'MECA_POU_C_T' ) THEN
            CALL UTPVLG ( NNOC, NCC, PGL1, FE, ZR(LVECT) )
            CALL UTPVLG ( NNOC, NCC, PGL2, FE(7), ZR(LVECT+6) )
         ELSE
            CALL UTPVLG ( NNO, NC1, PGL, FE, ZR(LVECT) )
         ENDIF
      ENDIF
C
      END
