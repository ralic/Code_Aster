      SUBROUTINE OP0056(IER)
      IMPLICIT   NONE
      INTEGER IER
C.......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 16/06/2004   AUTEUR DURAND C.DURAND 
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
C TOLE  CRP_20

C     MULTI_COUCHE : COMMANDE DEFI_COQU_MULT
C     ROUTINE ASSOCIE A LA COMMANDE DECRIVANT UN MATERIAU MULTICOUCHE
C     COUCHE PAR COUCHE (EPAISSEUR,TYPE DU MATERIAU,PREMIERE DIRECTION
C                        D'ORTHOTROPIE)
C.......................................................................
C .                                                                    .
C .  - FONCTION REALISEE : REMPLISSAGE DE L'OBJET MULTIC//'.MATER.MECA'.
C .                     CONTENANT LE COMPORTEMENT HOMOGENEISEE ET LES
C .                        CARACTERISTIQUES MECANIQUE OU THERMIQUE     .
C .                        COUCHE PAR COUCHE DANS LE REPERE DE         .
C .                        REFERENCE                                   .
C .  - ARGUMENTS :                                                     .
C .                                                                    .
C .      SORTIE :     IER <-- CODE D'ERREUR                            .
C .                                                                    .
C .  - ROUTINES APPELEES :                                             .
C .    GETRES  GETFAC  GETVR8  GETVID  GETVIS  WKVECT  RCVALE          .
C .    JEDETR                                                          .
C .                                                                    .
C .  - OBJETS CREES :                                               .
C .    MULTIC//'.ELAS_COQMU.VALR'   MULTIC//'.THER_COQMU.VALR'
C .    MULTIC//'.ELAS_COQMU.VALC'   MULTIC//'.THER_COQMU.VALC'
C .    MULTIC//'.ELAS_COQMU.VALK'   MULTIC//'.THER_COQMU.VALK'
C .    MULTIC//'.MATERIAU.NOMRC'

C      REFERENCE : DHATT - BATOZ VOL 2 PP 238 - 243
C.......................................................................

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

      INTEGER IFR,IUNIFI,NBCOU,ICOU,N,IADR,JEPOR,JRELA,K,LONOBJ,JMATE,
     &        JOBME,JOBMC,I,NIMPR,IMPR,NBRES,J,NOBJ,JOBTH,JOBTC,NBAD,
     &        IRET,NV,N1
      REAL*8 LAML,LAMT,LAMN,CP,QT(31),VALRES(9)
      REAL*8 EPAIS,ORIEN,EPTOT,EPI,ORDI,RHOHOM,R8DGRD
      REAL*8 EL,ET,NULT,GLT,QLL,QTT,QLT,GLN,GTN
      REAL*8 Q11,Q12,Q13,Q22,Q23,Q33,QM(56)
      REAL*8 M11,M12,M13,M21,M22,M23,M31,M32,M33
      REAL*8 DL,DT,RHO,G11,G22,G12
      REAL*8 C,S,C2,S2,C3,S3,C4,S4,D11,D22,D12
      REAL*8 HF(3,3),HFINV(3,3),HI(3,3),AI(3,3)
      REAL*8 DA1I(2,2),DA2I(2,4),IDI(2,2),EZD(2,2)
      REAL*8 XAB(2,2),C11I(2,2),C11(2,2),HTI(2,2),DET
      REAL*8 C11I1(2,2),C11I2(2,2),C11I3(2,2),C11I4(2,2)
      REAL*8 X1,X2,X3,X4,XT,XC,YT,YC,SLT
      REAL*8 HTIINV(2,2),C11INV(2,2)
      REAL*8 HC11,HC22,HC12,K11,K22,K12
      REAL*8 A11,A12,A13,A22,A23,A33
      REAL*8 B11,B12,B13,B22,B23,B33
      REAL*8 L11,L22,L12
      REAL*8 RZKP,RZKM,DIF3,DIF4,DIF5
      REAL*8 R8BID,H,H2,H3,H4,R8MAEM,ZERO,EPSI,R8MIEM,R8VIDE
      CHARACTER*2 BL2,CODRET(9),VAL
      CHARACTER*3 NUM
      CHARACTER*8 K8B,MULTIC,MATER,NOMRES(9)
      CHARACTER*16 TYPE,NOMCMD,FICHIE
      LOGICAL ELAS,THER,FAUX,ULEXIS
      PARAMETER (NV=83)

      CALL JEMARQ()
      CALL INFMAJ()

      BL2 = '  '
      ZERO = 0.D0
      FAUX = .FALSE.
      IER = 0
      EPSI=R8MIEM()

      CALL GETRES(MULTIC,TYPE,NOMCMD)
      CALL GETFAC('COUCHE',NBCOU)

C     --- VERIFICATION DES MATERIAUX ---

      ELAS = .FALSE.
      THER = .FALSE.
      DO 20 ICOU = 1,NBCOU
        CALL GETVID('COUCHE','MATER',ICOU,1,1,MATER,N)
        CALL JEVEUO(MATER//'.MATERIAU.NOMRC ','L',IADR)
        CALL JELIRA(MATER//'.MATERIAU.NOMRC ','LONMAX',NBAD,K8B)
        DO 10 I = 1,NBAD
          IF (ZK16(IADR+I-1).EQ.'ELAS_ORTH       ') THEN
            ELAS = .TRUE.
          ELSE IF (ZK16(IADR+I-1).EQ.'THER_ORTH       ') THEN
            THER = .TRUE.
          ELSE
            CALL UTDEBM('F',NOMCMD,'MATERIAU NON VALIDE')
            CALL UTIMPK('L',' MATERIAU : ',1,MATER)
            CALL UTFINM
          END IF
   10   CONTINUE
   20 CONTINUE
      IF (THER .AND. ELAS) THEN
        CALL UTDEBM('F',NOMCMD,'MATERIAUX NON VALIDES')
        CALL UTIMPK('L','ON NE PEUT AVOIR A LA FOIS ',1,'MECANIQUE')
        CALL UTIMPK('S',' ET ',1,'THERMIQUE')
        CALL UTFINM
      END IF

      NIMPR = 0
      CALL GETFAC('IMPRESSION',IMPR)
      IF (IMPR.NE.0) THEN
         NIMPR = 1
         IFR    = 0
         FICHIE = ' '
         CALL GETVIS ( 'IMPRESSION', 'UNITE'  , 1,1,1, IFR   , N1 )
         CALL GETVTX ( 'IMPRESSION', 'FICHIER', 1,1,1, FICHIE, N1 )
         IF ( N1 .NE. 0 ) THEN
            CALL UTMESS('A',NOMCMD,
     +               'LE MOT CLE "FICHIER" EST APPELE A DISPARAITRE.'//
     +               ' UTILISER LE MOT CLE "UNITE"')
            IFR = IUNIFI( FICHIE )
         ENDIF
         IF ( .NOT. ULEXIS( IFR ) ) THEN
            CALL ULOPEN ( IFR, ' ', FICHIE, 'NEW', 'O' )
         ENDIF
      END IF

C     - PHASE D'EXECUTION, REMPLISSAGE DES OBJETS JEVEUX ------

      IF (ELAS) THEN

C     ------ CARACTERISTIQUES MECANIQUES ------

        CALL WKVECT('&&OP0056.EPOR','V V R',3*NBCOU,JEPOR)
        CALL WKVECT(MULTIC//'.MATERIAU.NOMRC ','G V K16',NBCOU+2,JRELA)
        ZK16(JRELA) = 'ELAS_COQMU      '
        LONOBJ = 56 + NV*NBCOU
        CALL WKVECT(MULTIC//'.ELAS_COQMU.VALK','G V K8',2*LONOBJ,JMATE)
        CALL JEECRA(MULTIC//'.ELAS_COQMU.VALK','LONUTI',LONOBJ,' ')
        CALL WKVECT(MULTIC//'.ELAS_COQMU.VALR','G V R',LONOBJ,JOBME)
        CALL JEECRA(MULTIC//'.ELAS_COQMU.VALR','LONUTI',LONOBJ,' ')
        CALL WKVECT(MULTIC//'.ELAS_COQMU.VALC','G V C',LONOBJ,JOBMC)
        CALL JEECRA(MULTIC//'.ELAS_COQMU.VALC','LONUTI',0,' ')
        EPTOT = 0.D0
        DO 30 I = 1,56
          CALL CODENT(I,'G',NUM)
          ZK8(JMATE+I-1) = 'HOM_'//NUM
   30   CONTINUE
        DO 50 ICOU = 1,NBCOU
          CALL GETVR8('COUCHE','EPAIS',ICOU,1,1,EPAIS,N)
          CALL GETVID('COUCHE','MATER',ICOU,1,1,MATER,N)
          CALL GETVR8('COUCHE','ORIENTATION',ICOU,1,1,ORIEN,N)
          ZK16(JRELA+1+ICOU) = MATER
          CALL CODENT(ICOU,'G',NUM)
          DO 40 I = 1,NV
            CALL CODENT(I,'G',VAL)
            ZK8(JMATE+56+NV* (ICOU-1)+I-1) = 'C'//NUM//'_V'//VAL
   40     CONTINUE
          ZR(JEPOR-1+3*ICOU-2) = EPAIS
          ZR(JEPOR-1+3*ICOU-1) = ORIEN
          EPTOT = EPTOT + EPAIS
   50   CONTINUE
        EPAIS = -0.5D0*EPTOT
        RHOHOM = 0.D0
        DO 60 I = 1,56
          QM(I) = 0.D0
   60   CONTINUE
        IRET = 0
        DO 70 I = 1,NBCOU
          NBRES = 9
          NOMRES(1) = 'E_L'
          NOMRES(2) = 'E_T'
          NOMRES(3) = 'NU_LT'
          NOMRES(4) = 'G_LT'
          NOMRES(5) = 'ALPHA_L'
          NOMRES(6) = 'ALPHA_T'
          NOMRES(7) = 'RHO'
          NOMRES(8) = 'G_LN'
          NOMRES(9) = 'G_TN'
C         EN PRINCIPE G_LN = G_LT CF BATOZ
          K8B = ' '
          CALL RCVALE(ZK16(JRELA+I+1) (1:8),'ELAS_ORTH',0,K8B,R8BID,6,
     &                NOMRES,VALRES,CODRET,'F ')
          CALL RCVALE(ZK16(JRELA+I+1) (1:8),'ELAS_ORTH',0,K8B,R8BID,3,
     &                NOMRES(7),VALRES(7),CODRET(7),'  ')
          EL = VALRES(1)
          ET = VALRES(2)
          NULT = VALRES(3)
          GLT = VALRES(4)
          DL = VALRES(5)
          DT = VALRES(6)
          IF (CODRET(7).NE.'OK') THEN
            IRET = IRET + 1
            RHO = 0.D0
          ELSE
            RHO = VALRES(7)
          END IF
C           --- GLN NON DEFINI ==> GLN = GLT (ISOTROPIE TRANSVERSE) ---
          IF (CODRET(8).NE.'OK') THEN
            GLN = GLT
          ELSE
            GLN = VALRES(8)
          END IF
C           --- GTN NON DEFINI ==> GRANDE RIGIDITE --------------------
          IF (CODRET(9).NE.'OK') THEN
            GTN = 1.D+6*GLT
          ELSE
            GTN = VALRES(9)
          END IF
          IF (EL.LT.EPSI) THEN
            QLL = 0.D0
            QTT = ET
          ELSE
            QLL = EL/ (1.D0-NULT*NULT*ET/EL)
            QTT = QLL*ET/EL
          END IF
          QLT = QTT*NULT
          ORIEN = ZR(JEPOR-1+3*I-1)
          C = COS(ORIEN*R8DGRD())
          S = SIN(ORIEN*R8DGRD())
          C2 = C*C
          S2 = S*S
          C3 = C**3
          S3 = S**3
          C4 = C**4
          S4 = S**4
          EPI = ZR(JEPOR-1+3*I-2)
          EPAIS = EPAIS + EPI
C         COTE MOYENNE DE LA COUCHE I
          ZR(JEPOR-1+3*I) = EPAIS - 0.5D0*EPI
          ORDI = ZR(JEPOR-1+3*I)
          RHOHOM = RHOHOM + RHO*EPI/EPTOT
C          QIJ : MATRICE DE COMPORTEMENT H DANS LE REPERE UTILISATEUR
          Q11 = C4*QLL + S4*QTT + 2.D0*C2*S2*QLT + 4.D0*C2*S2*GLT
          Q22 = S4*QLL + C4*QTT + 2.D0*C2*S2*QLT + 4.D0*C2*S2*GLT
          Q12 = C2*S2* (QLL+QTT-4.D0*GLT) + (S4+C4)*QLT
          Q13 = C3*S*QLL - C*S3*QTT + (C*S3-C3*S)* (QLT+2.D0*GLT)
          Q23 = C*S3*QLL - C3*S*QTT - (C*S3-C3*S)* (QLT+2.D0*GLT)
          Q33 = C2*S2* (QLL+QTT-2.D0*QLT) + (S4+C4-2.D0*C2*S2)*GLT
C         COEF DE DILATATION THERMIQUE REPERE UTILISATEUR
          D11 = C2*DL + S2*DT
          D22 = S2*DL + C2*DT
          D12 = 2.D0*C*S* (DL-DT)
C          MATRICE DE COMPORTEMENT HTAU DANS REPERE UTILISATEUR
          G11 = C2*GLN + S2*GTN
          G22 = S2*GLN + C2*GTN
          G12 = C*S* (GLN-GTN)
C          T1T*HL*ALPHA
          M11 = C2*QLL*DL + S2*QLT*DT
          M12 = C2*QLT*DL + S2*QTT*DT
          M13 = 0.D0
          M21 = S2*QLL*DL + C2*QLT*DT
          M22 = S2*QLT*DL + C2*QTT*DT
          M23 = 0.D0
          M31 = C*S* (QLL*DL-QLT*DT)
          M32 = C*S* (QLT*DL-QTT*DT)
          M33 = 0.D0

C       LECTURE DES CRITERES
          NBRES = 5
          NOMRES(1) = 'XT'
          NOMRES(2) = 'XC'
          NOMRES(3) = 'YT'
          NOMRES(4) = 'YC'
          NOMRES(5) = 'S_LT'
          K8B = ' '
          CALL RCVALE(ZK16(JRELA+I+1) (1:8),'ELAS_ORTH',0,K8B,R8BID,5,
     &                NOMRES,VALRES,CODRET,'F ')
          IF (CODRET(1).NE.'OK') THEN
             XT=R8VIDE()
          ELSE
             XT = VALRES(1)
          ENDIF
          IF (CODRET(2).NE.'OK') THEN
             XC=R8VIDE()
          ELSE
             XC = VALRES(2)
          ENDIF
          IF (CODRET(3).NE.'OK') THEN
             YT=R8VIDE()
          ELSE
             YT = VALRES(3)
          ENDIF
          IF (CODRET(4).NE.'OK') THEN
             YC=R8VIDE()
          ELSE
             YC = VALRES(4)
          ENDIF
          IF (CODRET(5).NE.'OK') THEN
             SLT=R8VIDE()
          ELSE
             SLT = VALRES(5)
          ENDIF
          
C         STOCKAGE DANS MULTIC//'.ELAS_COQMU.VALR'

C         EPAISSEUR DE LA COUCHE I
          ZR(JOBME+56+ (I-1)*NV) = EPI
C         ANGLE ENTRE L'AXE L DE LA COUCHE I ET X DU REPERE UTILISATEUR
          ZR(JOBME+56+ (I-1)*NV+1) = ORIEN
C         COTE MOYENNE DE LA COUCHE I
          ZR(JOBME+56+ (I-1)*NV+2) = ORDI
C          QIJ : MATRICE DE COMPORTEMENT H DANS LE REPERE UTILISATEUR
          ZR(JOBME+56+ (I-1)*NV+3) = Q11
          ZR(JOBME+56+ (I-1)*NV+4) = Q12
          ZR(JOBME+56+ (I-1)*NV+5) = Q13
          ZR(JOBME+56+ (I-1)*NV+6) = Q22
          ZR(JOBME+56+ (I-1)*NV+7) = Q23
          ZR(JOBME+56+ (I-1)*NV+8) = Q33
C         COEF DE DILATATION THERMIQUE REPERE UTILISATEUR
          ZR(JOBME+56+ (I-1)*NV+9) = D11
          ZR(JOBME+56+ (I-1)*NV+10) = D22
          ZR(JOBME+56+ (I-1)*NV+11) = D12
C          MATRICE DE COMPORTEMENT HTAU DANS REPERE UTILISATEUR
          ZR(JOBME+56+ (I-1)*NV+12) = G11
          ZR(JOBME+56+ (I-1)*NV+13) = G22
          ZR(JOBME+56+ (I-1)*NV+14) = G12

C         HM = SOMME(I=1,NCOU)(EPI*H)
          QM(1) = QM(1) + Q11*EPI
          QM(2) = QM(2) + Q12*EPI
          QM(3) = QM(3) + Q13*EPI
          QM(4) = QM(4) + Q22*EPI
          QM(5) = QM(5) + Q23*EPI
          QM(6) = QM(6) + Q33*EPI
C         HMF = SOMME(I=1,NCOU)(EPI*ORDI*H)
          QM(7) = QM(7) + Q11*EPI*ORDI
          QM(8) = QM(8) + Q12*EPI*ORDI
          QM(9) = QM(9) + Q13*EPI*ORDI
          QM(10) = QM(10) + Q22*EPI*ORDI
          QM(11) = QM(11) + Q23*EPI*ORDI
          QM(12) = QM(12) + Q33*EPI*ORDI
C         HF = SOMME(I=1,NCOU)(ZI+1**3-ZI**3)*H/3)
          QM(13) = QM(13) + Q11*EPI* (ORDI**2+EPI**2/12.D0)
          QM(14) = QM(14) + Q12*EPI* (ORDI**2+EPI**2/12.D0)
          QM(15) = QM(15) + Q13*EPI* (ORDI**2+EPI**2/12.D0)
          QM(16) = QM(16) + Q22*EPI* (ORDI**2+EPI**2/12.D0)
          QM(17) = QM(17) + Q23*EPI* (ORDI**2+EPI**2/12.D0)
          QM(18) = QM(18) + Q33*EPI* (ORDI**2+EPI**2/12.D0)

          QM(30) = QM(30) + M11*EPI
          QM(31) = QM(31) + M12*EPI
          QM(32) = QM(32) + M13*EPI
          QM(33) = QM(33) + M21*EPI
          QM(34) = QM(34) + M22*EPI
          QM(35) = QM(35) + M23*EPI
          QM(36) = QM(36) + M31*EPI
          QM(37) = QM(37) + M32*EPI
          QM(38) = QM(38) + M33*EPI

          QM(39) = QM(39) + M11*EPI*ORDI
          QM(40) = QM(40) + M12*EPI*ORDI
          QM(41) = QM(41) + M13*EPI*ORDI
          QM(42) = QM(42) + M21*EPI*ORDI
          QM(43) = QM(43) + M22*EPI*ORDI
          QM(44) = QM(44) + M23*EPI*ORDI
          QM(45) = QM(45) + M31*EPI*ORDI
          QM(46) = QM(46) + M32*EPI*ORDI
          QM(47) = QM(47) + M33*EPI*ORDI
          QM(48) = QM(48) + M11*EPI* (ORDI**2+EPI**2/12.D0)
          QM(49) = QM(49) + M12*EPI* (ORDI**2+EPI**2/12.D0)
          QM(50) = QM(50) + M13*EPI* (ORDI**2+EPI**2/12.D0)
          QM(51) = QM(51) + M21*EPI* (ORDI**2+EPI**2/12.D0)
          QM(52) = QM(52) + M22*EPI* (ORDI**2+EPI**2/12.D0)
          QM(53) = QM(53) + M23*EPI* (ORDI**2+EPI**2/12.D0)
          QM(54) = QM(54) + M31*EPI* (ORDI**2+EPI**2/12.D0)
          QM(55) = QM(55) + M32*EPI* (ORDI**2+EPI**2/12.D0)
          QM(56) = QM(56) + M33*EPI* (ORDI**2+EPI**2/12.D0)

C         ON STOCKE LES VALEURS PRECEDENTES PAR COUCHE (NON CUMULEES)
C         (EPI*H)     CF HM
          ZR(JOBME+56+ (I-1)*NV+27) = Q11*EPI
          ZR(JOBME+56+ (I-1)*NV+28) = Q12*EPI
          ZR(JOBME+56+ (I-1)*NV+29) = Q13*EPI
          ZR(JOBME+56+ (I-1)*NV+30) = Q22*EPI
          ZR(JOBME+56+ (I-1)*NV+31) = Q23*EPI
          ZR(JOBME+56+ (I-1)*NV+32) = Q33*EPI
C         (EPI*ORDI*H)   CF HMF
          ZR(JOBME+56+ (I-1)*NV+33) = Q11*EPI*ORDI
          ZR(JOBME+56+ (I-1)*NV+34) = Q12*EPI*ORDI
          ZR(JOBME+56+ (I-1)*NV+35) = Q13*EPI*ORDI
          ZR(JOBME+56+ (I-1)*NV+36) = Q22*EPI*ORDI
          ZR(JOBME+56+ (I-1)*NV+37) = Q23*EPI*ORDI
          ZR(JOBME+56+ (I-1)*NV+38) = Q33*EPI*ORDI
C         (ZI+1**3-ZI**3)*H/3)    CF HF
          ZR(JOBME+56+ (I-1)*NV+39) = Q11*EPI* (ORDI**2+EPI**2/12.D0)
          ZR(JOBME+56+ (I-1)*NV+40) = Q12*EPI* (ORDI**2+EPI**2/12.D0)
          ZR(JOBME+56+ (I-1)*NV+41) = Q13*EPI* (ORDI**2+EPI**2/12.D0)
          ZR(JOBME+56+ (I-1)*NV+42) = Q22*EPI* (ORDI**2+EPI**2/12.D0)
          ZR(JOBME+56+ (I-1)*NV+43) = Q23*EPI* (ORDI**2+EPI**2/12.D0)
          ZR(JOBME+56+ (I-1)*NV+44) = Q33*EPI* (ORDI**2+EPI**2/12.D0)
C         DILATATION THERMIQUE
          ZR(JOBME+56+ (I-1)*NV+45) = M11*EPI
          ZR(JOBME+56+ (I-1)*NV+46) = M12*EPI
          ZR(JOBME+56+ (I-1)*NV+47) = M13*EPI
          ZR(JOBME+56+ (I-1)*NV+48) = M21*EPI
          ZR(JOBME+56+ (I-1)*NV+49) = M22*EPI
          ZR(JOBME+56+ (I-1)*NV+50) = M23*EPI
          ZR(JOBME+56+ (I-1)*NV+51) = M31*EPI
          ZR(JOBME+56+ (I-1)*NV+52) = M32*EPI
          ZR(JOBME+56+ (I-1)*NV+53) = M33*EPI
          ZR(JOBME+56+ (I-1)*NV+54) = M11*EPI*ORDI
          ZR(JOBME+56+ (I-1)*NV+55) = M12*EPI*ORDI
          ZR(JOBME+56+ (I-1)*NV+56) = M13*EPI*ORDI
          ZR(JOBME+56+ (I-1)*NV+57) = M21*EPI*ORDI
          ZR(JOBME+56+ (I-1)*NV+58) = M22*EPI*ORDI
          ZR(JOBME+56+ (I-1)*NV+59) = M23*EPI*ORDI
          ZR(JOBME+56+ (I-1)*NV+60) = M31*EPI*ORDI
          ZR(JOBME+56+ (I-1)*NV+61) = M32*EPI*ORDI
          ZR(JOBME+56+ (I-1)*NV+62) = M33*EPI*ORDI
          ZR(JOBME+56+ (I-1)*NV+63) = M11*EPI* (ORDI**2+EPI**2/12.D0)
          ZR(JOBME+56+ (I-1)*NV+64) = M12*EPI* (ORDI**2+EPI**2/12.D0)
          ZR(JOBME+56+ (I-1)*NV+65) = M13*EPI* (ORDI**2+EPI**2/12.D0)
          ZR(JOBME+56+ (I-1)*NV+66) = M21*EPI* (ORDI**2+EPI**2/12.D0)
          ZR(JOBME+56+ (I-1)*NV+67) = M22*EPI* (ORDI**2+EPI**2/12.D0)
          ZR(JOBME+56+ (I-1)*NV+68) = M23*EPI* (ORDI**2+EPI**2/12.D0)
          ZR(JOBME+56+ (I-1)*NV+69) = M31*EPI* (ORDI**2+EPI**2/12.D0)
          ZR(JOBME+56+ (I-1)*NV+70) = M32*EPI* (ORDI**2+EPI**2/12.D0)
          ZR(JOBME+56+ (I-1)*NV+71) = M33*EPI* (ORDI**2+EPI**2/12.D0)

C        STOCKAGE DES CRITERES
          ZR(JOBME+56 -1 + (I-1)*NV+79) = XT
          ZR(JOBME+56 -1 + (I-1)*NV+80) = XC
          ZR(JOBME+56 -1 + (I-1)*NV+81) = YT
          ZR(JOBME+56 -1 + (I-1)*NV+82) = YC
          ZR(JOBME+56 -1 + (I-1)*NV+83) = SLT
   70   CONTINUE
        IF (IRET.EQ.0) THEN
        ELSE IF (IRET.NE.NBCOU) THEN
          CALL UTMESS('F',NOMCMD,'LE PARAMETRE "RHO" N''EST PAS '//
     &                'DEFINI POUR TOUTES LES COUCHES.')
        END IF
        QM(19) = EPTOT
        IF (IRET.EQ.NBCOU) THEN
          QM(20) = R8MAEM()
        ELSE
          QM(20) = RHOHOM
        END IF
C        -- INVERSION DE HF -- POUR CALCULER HC PAGE 242
        HF(1,1) = QM(13)
        HF(1,2) = QM(14)
        HF(1,3) = QM(15)
        HF(2,2) = QM(16)
        HF(2,3) = QM(17)
        HF(3,3) = QM(18)
        HF(2,1) = HF(1,2)
        HF(3,1) = HF(1,3)
        HF(3,2) = HF(2,3)
        DO 90 K = 1,3
          DO 80 J = 1,3
            HFINV(K,J) = 0.D0
   80     CONTINUE
   90   CONTINUE
        DO 100 I = 1,3
          HFINV(I,I) = 1.D0
  100   CONTINUE
        CALL MGAUSS(HF,HFINV,3,3,3,ZERO,FAUX)
        DO 120 K = 1,2
          DO 110 J = 1,2
            DA1I(K,J) = 0.D0
            C11(K,J) = 0.D0
            IDI(K,J) = 0.D0
            EZD(K,J) = 0.D0
  110     CONTINUE
  120   CONTINUE
        HC11 = 0.D0
        HC22 = 0.D0
        HC12 = 0.D0
        DO 220 ICOU = 1,NBCOU
          EPI = ZR(JEPOR+3*ICOU-3)
          ORDI = ZR(JEPOR+3*ICOU-1)
C          QIJ : MATRICE DE COMPORTEMENT HI DANS LE REPERE UTILISATEUR
          HI(1,1) = ZR(JOBME+56+ (ICOU-1)*NV+3)
          HI(1,2) = ZR(JOBME+56+ (ICOU-1)*NV+4)
          HI(1,3) = ZR(JOBME+56+ (ICOU-1)*NV+5)
          HI(2,2) = ZR(JOBME+56+ (ICOU-1)*NV+6)
          HI(2,3) = ZR(JOBME+56+ (ICOU-1)*NV+7)
          HI(3,3) = ZR(JOBME+56+ (ICOU-1)*NV+8)
          HI(2,1) = HI(1,2)
          HI(3,1) = HI(1,3)
          HI(3,2) = HI(2,3)
          DO 140 K = 1,3
            DO 130 J = 1,3
              AI(K,J) = 0.D0
  130       CONTINUE
  140     CONTINUE
          DO 170 I = 1,3
            DO 160 J = 1,3
              DO 150 K = 1,3
                AI(I,J) = AI(I,J) + HI(I,K)*HFINV(K,J)
  150         CONTINUE
  160       CONTINUE
  170     CONTINUE
C         TERMES DE LA MATRICE INTERVENANT DANS D1(Z)
C      CF. DHAT-BATOZ VOL 2 PAGE 243
C      DAI1 MATRICE (2,2) CONSTANTE PAR COUCHE
C      TERME 1,1 : A11+A33 TERME 1,2 : A13+A32
C      TERME 2,1 : A31+A23 TERME 2,2 : A22+A33
          DA1I(1,1) = AI(1,1) + AI(3,3)
          DA1I(1,2) = AI(1,3) + AI(3,2)
          DA1I(2,1) = AI(3,1) + AI(2,3)
          DA1I(2,2) = AI(2,2) + AI(3,3)
          
          ZR(JOBME+56+ (ICOU-1)*NV+15) = AI(1,1)
          ZR(JOBME+56+ (ICOU-1)*NV+16) = AI(2,1)
          ZR(JOBME+56+ (ICOU-1)*NV+17) = AI(3,1)
          ZR(JOBME+56+ (ICOU-1)*NV+18) = AI(1,2)
          ZR(JOBME+56+ (ICOU-1)*NV+19) = AI(2,2)
          ZR(JOBME+56+ (ICOU-1)*NV+20) = AI(3,2)
          ZR(JOBME+56+ (ICOU-1)*NV+21) = AI(1,3)
          ZR(JOBME+56+ (ICOU-1)*NV+22) = AI(2,3)
          ZR(JOBME+56+ (ICOU-1)*NV+23) = AI(3,3)

C          TERMES CONSTANT DANS D1(Z)
          DO 190 K = 1,2
            DO 180 J = 1,2
              IDI(K,J) = EZD(K,J) - ((ORDI-EPI/2.D0)**2)*DA1I(K,J)/2.D0
              EZD(K,J) = EZD(K,J) + EPI*ORDI*DA1I(K,J)
  180       CONTINUE
  190     CONTINUE
C          MATRICE DE COMPORTEMENT HTAU DANS REPERE UTILISATEUR
          HTI(1,1) = ZR(JOBME+56+ (ICOU-1)*NV+12)
          HTI(2,2) = ZR(JOBME+56+ (ICOU-1)*NV+13)
          HTI(1,2) = ZR(JOBME+56+ (ICOU-1)*NV+14)
          HTI(2,1) = HTI(1,2)
C           -- INVERSION DE HTI ---
          DET = HTI(1,1)*HTI(2,2) - HTI(1,2)*HTI(2,1)
          IF (ABS(DET).LT.EPSI) THEN
            C11I(1,1) = 0.D0
            C11I(1,2) = 0.D0
            C11I(2,1) = 0.D0
            C11I(2,2) = 0.D0
            HTIINV(1,1) = 0.D0
            HTIINV(2,2) = 0.D0
            HTIINV(1,2) = 0.D0
            HTIINV(2,1) = 0.D0
          ELSE
            HTIINV(1,1) = HTI(2,2)/DET
            HTIINV(2,2) = HTI(1,1)/DET
            HTIINV(1,2) = -HTI(1,2)/DET
            HTIINV(2,1) = -HTI(2,1)/DET
          END IF
C         CALCUL DE C11
          CALL UTBTAB('ZERO',2,2,HTIINV,IDI,XAB,C11I1)
          CALL UTCTAB('ZERO',2,2,2,HTIINV,DA1I,IDI,XAB,C11I2)
          CALL UTCTAB('ZERO',2,2,2,HTIINV,IDI,DA1I,XAB,C11I3)
          CALL UTBTAB('ZERO',2,2,HTIINV,DA1I,XAB,C11I4)
          X1 = EPI/4.D0
          X2 = (3.D0*ORDI*ORDI*EPI+EPI**3/4.D0)/24.D0
          X3 = X2
          X4 = (5.D0*ORDI**4*EPI+2.5D0*ORDI*ORDI*EPI**3+EPI**5/16.D0)/
     &         80.D0
          DO 210 K = 1,2
            DO 200 J = 1,2
              C11I(K,J) = X1*C11I1(K,J) + X2*C11I2(K,J) +
     &                    X3*C11I3(K,J) + X4*C11I4(K,J)
              C11(K,J) = C11(K,J) + C11I(K,J)
  200       CONTINUE
  210     CONTINUE
          HC11 = HC11 + EPI*HTI(1,1)
          HC22 = HC22 + EPI*HTI(2,2)
          HC12 = HC12 + EPI*HTI(1,2)
C        -- INVERSION DE C11 LOCALE --- HC = C11INV LOCALE
          DET = C11I(1,1)*C11I(2,2) - C11I(1,2)*C11I(2,1)
          IF (ABS(DET).LT.EPSI) THEN
            C11INV(1,1) = 0.D0
            C11INV(2,2) = 0.D0
            C11INV(1,2) = 0.D0
            C11INV(2,1) = 0.D0
          ELSE
            C11INV(1,1) = C11I(2,2)/DET
            C11INV(2,2) = C11I(1,1)/DET
            C11INV(1,2) = -C11I(1,2)/DET
            C11INV(2,1) = -C11I(2,1)/DET
          END IF
          ZR(JOBME+56+ (ICOU-1)*NV+72) = C11I(1,1)
          ZR(JOBME+56+ (ICOU-1)*NV+73) = C11I(2,2)
          ZR(JOBME+56+ (ICOU-1)*NV+74) = C11I(1,2)
          ZR(JOBME+56+ (ICOU-1)*NV+75) = C11INV(1,1)
          ZR(JOBME+56+ (ICOU-1)*NV+76) = C11INV(2,2)
          ZR(JOBME+56+ (ICOU-1)*NV+77) = C11INV(1,2)
  220   CONTINUE
C FIN BOUCLE SUR LES COUCHES
C        -- INVERSION DE C11 INTEGREE SUR L'EPAISSEUR
C        -- INVERSION DE C11 GLOBALE --- HC = C11INV GLOBALE
        DET = C11(1,1)*C11(2,2) - C11(1,2)*C11(2,1)
        IF (ABS(DET).LT.EPSI) THEN
          C11INV(1,1) = 0.D0
          C11INV(2,2) = 0.D0
          C11INV(1,2) = 0.D0
          C11INV(2,1) = 0.D0
        ELSE
          C11INV(1,1) = C11(2,2)/DET
          C11INV(2,2) = C11(1,1)/DET
          C11INV(1,2) = -C11(1,2)/DET
          C11INV(2,1) = -C11(2,1)/DET
        END IF
C       COEFFICENTS DE CISAILLEMENT MOYENS
        IF (ABS(HC11).LT.EPSI) THEN
          K11 = 0.D0
        ELSE
          K11 = C11INV(1,1)/HC11
        END IF
        IF (ABS(HC22).LT.EPSI) THEN
          K22 = 0.D0
        ELSE
          K22 = C11INV(2,2)/HC22
        END IF
        IF (ABS(HC12).LT.EPSI) THEN
          K12 = 0.D0
        ELSE
          K12 = C11INV(1,2)/HC12
        END IF
        QM(21) = C11(1,1)
        QM(22) = C11(2,2)
        QM(23) = C11(1,2)
        QM(24) = C11INV(1,1)
        QM(25) = C11INV(2,2)
        QM(26) = C11INV(1,2)
        QM(27) = K11
        QM(28) = K22
        QM(29) = K12
        IF (NIMPR.GT.0) THEN
          WRITE (IFR,1000)
          WRITE (IFR,*) 'COEFFICIENTS HOMOGENEISES MECANIQUES :'
          WRITE (IFR,*) 'BLOCS R,Q,P :'
          WRITE (IFR,*) '   QM(1-18)=   :'
          WRITE (IFR,'(5(2X,E13.6))') (QM(I),I=1,18)
          WRITE (IFR,*) 'EPAISSEUR - MASSE VOL. :'

          WRITE (IFR,'(5(2X,E13.6))') QM(19),QM(20)
          WRITE (IFR,*) 'CISAILLEMENT - DCI11, DCI22, DCI12 :'

          WRITE (IFR,'(5(2X,E13.6))') QM(21),QM(22),QM(23)
          WRITE (IFR,*) 'CISAILLEMENT - DC11, DC22, DC12 :'

          WRITE (IFR,'(5(2X,E13.6))') QM(24),QM(25),QM(26)
          WRITE (IFR,*) 'CISAILLEMENT - K11, K22, K12 :'

          WRITE (IFR,'(5(2X,E13.6))') QM(27),QM(28),QM(29)
          WRITE (IFR,1000)
        END IF
        DO 230 I = 1,56
          ZR(JOBME-1+I) = QM(I)
          ZC(JOBMC-1+I) = 0.D0
  230   CONTINUE
        DO 240 I = 57,56 + NV*NBCOU
          ZC(JOBMC-1+I) = 0.D0
  240   CONTINUE
        CALL JEDETR('&&OP0056.EPOR')
      END IF

      IF (THER) THEN

C     ------ CARACTERISTIQUES THERMIQUES ------

        CALL WKVECT('&&OP0056.EPOR','V V R',3*NBCOU,JEPOR)
        CALL JEEXIN(MULTIC//'.MATERIAU.NOMRC ',NOBJ)
        IF (NOBJ.EQ.0) THEN
          CALL WKVECT(MULTIC//'.MATERIAU.NOMRC ','G V K16',NBCOU+2,
     &                JRELA)
        END IF
        ZK16(JRELA) = 'THER_COQMU      '
        LONOBJ = 31 + 3*NBCOU
        CALL WKVECT(MULTIC//'.THER_COQMU.VALK','G V K8',2*LONOBJ,JMATE)
        CALL JEECRA(MULTIC//'.THER_COQMU.VALK','LONUTI',LONOBJ,' ')
        CALL WKVECT(MULTIC//'.THER_COQMU.VALR','G V R',LONOBJ,JOBTH)
        CALL JEECRA(MULTIC//'.THER_COQMU.VALR','LONUTI',LONOBJ,' ')
        CALL WKVECT(MULTIC//'.THER_COQMU.VALC','G V C',LONOBJ,JOBTC)
        CALL JEECRA(MULTIC//'.THER_COQMU.VALC','LONUTI',0,' ')
        EPTOT = 0.D0
        DO 250 I = 1,31
          CALL CODENT(I,'G',NUM)
          ZK8(JMATE+I-1) = 'HOM_'//NUM
  250   CONTINUE
        DO 270 ICOU = 1,NBCOU
          CALL GETVR8('COUCHE','EPAIS',ICOU,1,1,EPAIS,N)
          CALL GETVID('COUCHE','MATER',ICOU,1,1,MATER,N)
          CALL GETVR8('COUCHE','ORIENTATION',ICOU,1,1,ORIEN,N)
          ZK16(JRELA+1+ICOU) = MATER
          CALL CODENT(ICOU,'G',NUM)
          DO 260 I = 1,3
            CALL CODENT(I,'G',VAL)
            ZK8(JMATE+31+3* (ICOU-1)+I-1) = 'C'//NUM//'_V'//VAL
  260     CONTINUE
          ZR(JEPOR-1+3*ICOU-2) = EPAIS
          ZR(JEPOR-1+3*ICOU-1) = ORIEN
          EPTOT = EPTOT + EPAIS
  270   CONTINUE

        EPAIS = -0.5D0*EPTOT
        H = 0.5D0*EPTOT
        H2 = H*H
        H3 = H*H2
        H4 = H*H3
        DO 280 I = 1,31
          QT(I) = 0.D0
  280   CONTINUE
        DO 290 I = 1,NBCOU
          NBRES = 4
          NOMRES(1) = 'LAMBDA_L'
          NOMRES(2) = 'LAMBDA_T'
          NOMRES(3) = 'LAMBDA_N'
          NOMRES(4) = 'RHO_CP'
          K8B = ' '
          CALL RCVALE(ZK16(JRELA+I+1) (1:8),'THER_ORTH',0,K8B,R8BID,
     &                NBRES,NOMRES,VALRES,CODRET,BL2)
          LAML = VALRES(1)
          LAMT = VALRES(2)
          LAMN = VALRES(3)
          CP = VALRES(4)
          C = COS(ZR(JEPOR+3*I-2)*R8DGRD())
          S = SIN(ZR(JEPOR+3*I-2)*R8DGRD())
          C2 = C*C
          S2 = S*S
          EPAIS = EPAIS + ZR(JEPOR+3*I-3)
          ZR(JEPOR+3*I-1) = EPAIS - 0.5D0*ZR(JEPOR+3*I-3)
          EPI = ZR(JEPOR+3*I-3)
          ORDI = ZR(JEPOR+3*I-1)
          ZR(JOBTH+31+3* (I-1)) = EPI
          ZR(JOBTH+31+3* (I-1)+1) = ZR(JEPOR+3*I-2)
          ZR(JOBTH+31+3* (I-1)+2) = ORDI
          RZKP = ORDI + EPI/2
          RZKM = ORDI - EPI/2
          DIF3 = (RZKP**3-RZKM**3)/3.D0
          DIF4 = (RZKP**4-RZKM**4)/4.D0
          DIF5 = (RZKP**5-RZKM**5)/5.D0
          A11 = EPI + DIF5/H4 - 2.D0*DIF3/H2
          A12 = -EPI/ (2.D0*H) + DIF3/ (2.D0*H2) + DIF4/ (2.D0*H3) -
     &          DIF5/ (2.D0*H4)
          A13 = EPI*ORDI/ (2.D0*H) + DIF3/ (2.D0*H2) - DIF4/ (2.D0*H3) -
     &          DIF5/ (2.D0*H4)
          A22 = DIF3/ (4.D0*H2) + DIF5/ (4.D0*H4) - DIF4/ (2.D0*H3)
          A23 = -DIF3/ (4.D0*H2) + DIF5/ (4.D0*H4)
          A33 = DIF3/ (4.D0*H2) + DIF5/ (4.D0*H4) + DIF4/ (2.D0*H3)
          B11 = 4.D0*DIF3/H4
          B12 = EPI*ORDI/H3 - 2.D0*DIF3/H4
          B13 = -EPI*ORDI/H3 - 2.D0*DIF3/H4
          B22 = EPI/ (4.D0*H2) + DIF3/H4 - EPI*ORDI/H3
          B23 = DIF3/H4 - EPI/ (4.D0*H2)
          B33 = EPI/ (4.D0*H2) + DIF3/H4 + EPI*ORDI/H3
          L11 = C2*LAML + S2*LAMT
          L22 = S2*LAML + C2*LAMT
          L12 = C*S* (LAML-LAMT)
          QT(1) = QT(1) + A11*L11
          QT(2) = QT(2) + A11*L22
          QT(3) = QT(3) + A11*L12
          QT(4) = QT(4) + A12*L11
          QT(5) = QT(5) + A12*L22
          QT(6) = QT(6) + A12*L12
          QT(7) = QT(7) + A13*L11
          QT(8) = QT(8) + A13*L22
          QT(9) = QT(9) + A13*L12
          QT(10) = QT(10) + A22*L11
          QT(11) = QT(11) + A22*L22
          QT(12) = QT(12) + A22*L12
          QT(13) = QT(13) + A23*L11
          QT(14) = QT(14) + A23*L22
          QT(15) = QT(15) + A23*L12
          QT(16) = QT(16) + A33*L11
          QT(17) = QT(17) + A33*L22
          QT(18) = QT(18) + A33*L12
          QT(19) = QT(19) + B11*LAMN
          QT(20) = QT(20) + B12*LAMN
          QT(21) = QT(21) + B13*LAMN
          QT(22) = QT(22) + B22*LAMN
          QT(23) = QT(23) + B23*LAMN
          QT(24) = QT(24) + B33*LAMN
          QT(25) = QT(25) + A11*CP
          QT(26) = QT(26) + A12*CP
          QT(27) = QT(27) + A13*CP
          QT(28) = QT(28) + A22*CP
          QT(29) = QT(29) + A23*CP
          QT(30) = QT(30) + A33*CP
  290   CONTINUE
        QT(31) = EPTOT
        IF (NIMPR.GT.0) THEN
          WRITE (IFR,1000)
          WRITE (IFR,*) 'COEFFICIENTS HOMOGENEISES THERMIQUES :'
          WRITE (IFR,*) 'BLOCS A,B,C - EPAISSEUR :'
          WRITE (IFR,*) '   QT(1-31)=   :'
          WRITE (IFR,'(5(2X,E13.6))') QT
          WRITE (IFR,1000)
        END IF
        DO 300 I = 1,31
          ZR(JOBTH-1+I) = QT(I)
          ZR(JOBTC-1+I) = 0.D0
  300   CONTINUE
        CALL JEDETR('&&OP0056.EPOR')
      END IF

 1000 FORMAT (/,80 ('-'))

      CALL JEDEMA()
      END
