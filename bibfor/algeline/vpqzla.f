      SUBROUTINE VPQZLA      
     &  (TYPEQZ, QRN, IQRN, LQRN, QRAR, QRAI, QRBA, QRVL,
     &   LVEC, KQRN, LVALPR, NCONV, OMECOR, KTYP, KQRNR,
     &   NEQACT, ILSCAL, IRSCAL, OPTIOF, TYPRES, OMEMIN, OMEMAX, OMESHI,
     &   DDLEXC, NFREQ, LMASSE, LRAIDE, LAMOR, NUMEDD, SIGMA)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 21/04/2008   AUTEUR BOITEAU O.BOITEAU 
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
C TOLE CRP_20
C TOLE CRP_21
C TOLE CRP_4
C     SUBROUTINE ASTER ORCHESTRANT LA METHODE QZ (VERSION LAPACK).
C     EN GENERALISEE: MATRICE K ET M SYMETRIQUES AVEC K E R/C ET M E R
C       EVENTUELLEMENT SPD SI (PAS DE BLOCAGE DE LAGRANGE, PAS DE
C       FLAMBEMENT, K E R)
C-----------------------------------------------------------------------
C CE PROGRAMME EFFECUE LES TACHES SUIVANTES:
C
C    - PASSAGE DES MATRICES EN STOCKAGE MORSE A DES MATRICES PLEINES
C    (NECESSAIRE POUR UTILISER LAPACK)
C    - APPEL AUX ROUTINES LAPACK DE RESOLUTION DE PB GENERALISES
C       SPD REEL OU NON SYMETRIQUE REEL OU COMPLEXE: 
C          BETA * A * X - ALPHA * B * X = 0
C       ON PEUT ALORS CALCULER LES VALEURS PROPRES DU PROBLEME 
C       GENERALISE ENFAISANT LE RAPPORT ALPHA/BETA SI BETA EST NON NUL.
C    - TRI DES VALEURS PROPRES: SEUL LES MODES PROPRES CORRESPONDANT 
C    AUX DDL ACTIFS SONT CONSERVES. LES MODES PROVENANT DES LAGRANGES
C    OU DES DDL BLOQUES DONNENT DES VALEURS NULLES POUR BETA.
C
C   --------------------------------------------------------------------
C     PARAMETRES D'APPELS:
C (A/B SIGNIFIE SD DE TYPE A SI K EST REELLE, TYPE B SI K EST COMPLEXE
C  AVEC A ET B SOIT R, SOIT C)
C
C IN  TYPEQZ : K  : TYPE DE METHODE : QR, QZ_SIMPLE OU QZ_EQUI.
C IN  QRN    : IS : DIMENSION DU SYSTEME
C IN  IQRN   : IS : ADRESSE JEVEUX DE LA MATRICE A PLEINE (R/C)
C IN  LQRN   : IS : ADRESSE JEVEUX DE LA MATRICE B PLEINE (R/C)
C IN  IADIA  : IS : .SMDI POUR LA STRUCTURE DE DONNEES NUME_DDL
C IN  IHCOL  : IS : .SMHC POUR LA STRUCTURE DE DONNEES NUME_DDL
C IN  QRAR   : IS : ADRESSE JEVEUX DE RE(ALPHA) (SI K E R)
C                   OU ALPHA E C (SI K E C)
C IN  QRAI   : IS : ADRESSE JEVEUX DE IM(ALPHA)
C                   SI K E R, SINON VIDE
C IN  QRBA   : IS : ADRESSE JEVEUX DE BETA (R/C)
C IN  QRVL   : IS : ADRESSE JEVEUX D'UN VECTEUR AUX POUR LAPACK (R/C)
C OUT LVEC   : IS : ADRESSE JEVEUX  MATRICE DES VECTEURS PROPRES (R/C)
C IN  KQRN   : IS : ADRESSE JEVEUX D'UN VECTEUR AUX POUR LAPACK (R/C)
C OUT LVALPR : IS : ADRESSE JEVEUX DU VECTEUR DES VALEURS PROPRES (R/C)
C                   EN QUADRATIQUE, ELEMENT DE C
C OUT NCONV  : IS : NOMBRE DE MODES CONVERGES RETENUS APRES TRI
C IN  OMECOR : R8 : SEUIL DE MODE RIGIDE
C IN  KTYP   : K1 : TYPE DE LA MATRICE DE RAIDEUR
C IN  KQRNR  : IS : ADRESSE JEVEUX VECTEUR AUX LAPACK SI K E C (R)
C IN  NEQACT : IS : NOMBRE DE DDL ACTIFS
C IN  ILSCAL/IRSCAL : IS : ADRESSE JEVEUX VECTEURS AUX POUR QZ_EQUI
C IN  OPTIOF : K16: OPTION DEMANDEE (BANDE, PLUS_PETITE,CENTRE,TOUT) 
C IN  TYPRES : K16: TYPE DE SD_RESULTAT
C IN  OMEMIN/OMEMAX: R8 : FREQS MIN ET MAX DE LA BANDE RECHERCHEE
C IN  OMESHI : R8 : VALEUR  RETENUE DU SHIFT PAR VPFOPR
C IN  DDLEXC : IS : DDLEXC(1..QRN) VECTEUR POSITION DES DDLS BLOQUES.
C IN  NFREQ  : IS : NBRE DE MODES DEMANDES SI OPTIOF=CENTRE OU 
C                   PLUS_PETITE
C IN  LMASSE : IS : DESCRIPTEUR DE LA MATRICE DE MASSE M (R)
C IN  LRAIDE : IS : DESCRIPTEUR DE LA MATRICE DE RAIDEUR K (R/C)
C IN  LAMOR  : IS : DESCRIPTEUR DE LA MATRICE DE D'AMORTISSEMENT ET/OU
C                   D'EFFET GYROSCOPIQUE (R)
C IN  NUMEDD : K19: NOM DU NUME_DDL
C IN  SIGMA  : C16: VALEUR DU SHIFT EN GENERALISE COMPLEXE
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8  ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

C DECLARATION PARAMETRES D'APPELS
      INTEGER      QRN,IQRN,LQRN,QRAR,QRAI,
     &             QRBA,QRVL,LVEC,KQRN,LVALPR,NCONV,KQRNR,NEQACT,
     &             ILSCAL,IRSCAL,DDLEXC(QRN),NFREQ,LMASSE,LRAIDE,LAMOR
      CHARACTER*1  KTYP
      CHARACTER*16 TYPEQZ,OPTIOF,TYPRES
      CHARACTER*19 NUMEDD
      REAL*8       OMECOR,OMEMIN,OMEMAX,OMESHI
      COMPLEX*16   SIGMA
C-----------------------------------------------------------------------
C DECLARATION VARIABLES LOCALES

      INTEGER      I,J,K,DECAL,IDEB,IFIN,QRLWOR,KQRN2,IAUXH,VALI(3),IFM,
     &             NIV,IRET,IVALR,IVALM,IADIA,IHCOL,IVP1,IVP2,IVALA,J2,
     &             IAUXH2,QRNS2,LVEC2,LVEC3,LVEC4,LVEC5,IMULT
      INTEGER*4    QRN4,LDVL4,LDVR4,QRLWO4,QRINFO,ILO,IHI,IWORK
      REAL*8       R8MIEM,R8MAEM,R8PREM,ABNRM,BBNRM,RCONDE,RCONDV,BAUX, 
     &             RAUXI,AAUX,VALR(4),RAUX,ANORM,BNORM,PREC2,VPINF,PREC,
     &             VPMAX,VPCOUR,ALPHA,FREQOM,DDOT,PREC3,RUN,RZERO,RAUXR,
     &             RAUXM,RAUXA
      COMPLEX*16   CUN,CZERO,CAUXM,CAUXR,CAUXA
      CHARACTER*24 NOMRAI,NOMMAS,NOMAMO
      CHARACTER*32 JEXNUM
      LOGICAL      BWORK,LKR,LTEST,LC

C-----------------------------------------------------------------------

      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)
      CALL MATFPE(-1)

C------      
C------
C INITS
C------
C------
C ---- PARAMETRES POUR LAPACK
      QRN4 = QRN
      QRLWO4 = -1
C ---- ON CHERCHE LES VECTEURS PROPRES A DROITE (ON EST EN SYMETRIQUE)
      LDVL4 = 1
      LDVR4 = QRN
C ---- METTRE LTEST=.TRUE. SI ON VEUX FAIRE DES TESTS UNITAIRES SUR LES
C ---- SOLVEURS LAPACK.
      LTEST=.FALSE.
C      LTEST=.TRUE.
      IF (LAMOR.NE.0) THEN
C ---- QRN DOIT ETRE PAIRE EN QUADRATIQUE
        QRNS2=QRN/2
        LC=.TRUE.
        IF ((QRNS2*2).NE.QRN) CALL ASSERT(.FALSE.)
        IMULT=2
      ELSE
        LC=.FALSE.
        IMULT=1
      ENDIF
      CUN=(1.D0,0.D0)
      CZERO=(0.D0,0.D0)
      RUN=1.D0
      RZERO=0.D0
      IF (KTYP.EQ.'R') THEN
        LKR=.TRUE.
      ELSE IF (KTYP.EQ.'C') THEN
        LKR=.FALSE.
      ELSE
C ---- OPTION ILLICITE
        CALL ASSERT(.FALSE.)      
      ENDIF

C--------------------------------------------------
C--------------------------------------------------
C CONVERSION DES MATRICES MORSE EN MATRICES PLEINES 
C--------------------------------------------------
C--------------------------------------------------      

      IDEB = 1
C ---- MATRICES K ET M REELLES SYMETRIQUES
      NOMRAI=ZK24(ZI(LRAIDE+1))
      NOMMAS=ZK24(ZI(LMASSE+1))
      CALL JEVEUO(JEXNUM(NOMRAI(1:19)//'.VALM',1),'L',IVALR)
      CALL JEVEUO(JEXNUM(NOMMAS(1:19)//'.VALM',1),'L',IVALM)
      IF (LC) THEN
        NOMAMO=ZK24(ZI(LAMOR+1))
        CALL JEVEUO(JEXNUM(NOMAMO(1:19)//'.VALM',1),'L',IVALA)
      ENDIF
      CALL JEVEUO(NUMEDD(1:14)//'.SMOS.SMHC','L',IHCOL)
      CALL JEVEUO(NUMEDD(1:14)//'.SMOS.SMDI','L',IADIA)
      
C ---- PB MODAL GENERALISE
      IF (.NOT.LC) THEN
C ---- MATRICES K ET M REELLES SYMETRIQUES
        IF (LKR) THEN
          DO 31 J = 1,QRN
            IFIN = ZI(IADIA-1+J)
            DO 30 I = IDEB,IFIN
              IAUXH=ZI(IHCOL-1+I)
              RAUXR=ZR(IVALR-1+I)
              RAUXM=ZR(IVALM-1+I)
C ------ MATRICE A ET B TRIANGULAIRE SUP
              ZR(IQRN-1+(J-1)*QRN+IAUXH) = RAUXR
              ZR(LQRN-1+(J-1)*QRN+IAUXH) = RAUXM
C ------ MATRICE A ET B TRIANGULAIRE INF
              ZR(IQRN-1+QRN*(IAUXH-1)+J) = RAUXR
              ZR(LQRN-1+QRN*(IAUXH-1)+J) = RAUXM
   30       CONTINUE
            IDEB = IFIN+1
   31     CONTINUE
C ---- MATRICES K COMPLEXE ET M REELLE SYMETRIQUES
        ELSE
          DO 33 J = 1,QRN
            IFIN = ZI(IADIA-1+J)
            DO 32 I = IDEB,IFIN
              IAUXH=ZI(IHCOL-1+I)
              CAUXR=ZC(IVALR-1+I)
              CAUXM=ZR(IVALM-1+I)*CUN
              ZC(IQRN-1+(J-1)*QRN+IAUXH) = CAUXR
              ZC(LQRN-1+(J-1)*QRN+IAUXH) = CAUXM
              ZC(IQRN-1+QRN*(IAUXH-1)+J) = CAUXR
              ZC(LQRN-1+QRN*(IAUXH-1)+J) = CAUXM
   32       CONTINUE
            IDEB = IFIN+1
   33     CONTINUE
        ENDIF
      ELSE
C ---- PB MODAL QUADRATIQUE
C ---- MATRICES K ET M REELLES SYMETRIQUES
        IF (LKR) THEN
          DO 35 J = 1,QRNS2
            IFIN = ZI(IADIA-1+J)
            J2=J+QRNS2
            DO 34 I = IDEB,IFIN
              IAUXH=ZI(IHCOL-1+I)
              IAUXH2=IAUXH+QRNS2
              RAUXR=ZR(IVALR-1+I)
              RAUXM=-ZR(IVALM-1+I)
              RAUXA=-ZR(IVALA-1+I)
C POUR TEST
C CAS BASIQUES
C              IF (IAUXH.EQ.J) THEN
C               RAUXR=J*RUN
C               RAUXM=-(-RUN)
C               RAUXA=-(RUN)
C             ELSE
C               RAUXR=RZERO
C               RAUXM=RZERO
C               RAUXA=RZERO
C             ENDIF
C ON SIMULE LE PB GENERALISE
C              RAUXA=RAUXM
C              RAUXM=RZERO
C FIN TEST

C ------ MATRICE A TRIANGULAIRE SUP PUIS INF
              ZR(IQRN-1+(J-1)*QRN+IAUXH)   =  RAUXR
              ZR(IQRN-1+(J2-1)*QRN+IAUXH2) =  RAUXM           
              ZR(IQRN-1+QRN*(IAUXH-1)+J)   =  RAUXR
              ZR(IQRN-1+QRN*(IAUXH2-1)+J2) =  RAUXM
C ------ IDEM MATRICE B
              ZR(LQRN-1+(J-1)*QRN+IAUXH)   =  RAUXA
              ZR(LQRN-1+(J-1)*QRN+IAUXH2)  =  RAUXM
              ZR(LQRN-1+(J2-1)*QRN+IAUXH)  =  RAUXM           
              ZR(LQRN-1+QRN*(IAUXH-1)+J)   =  RAUXA
              ZR(LQRN-1+QRN*(IAUXH2-1)+J)  =  RAUXM
              ZR(LQRN-1+QRN*(IAUXH-1)+J2)  =  RAUXM
   34       CONTINUE
            IDEB = IFIN+1
   35     CONTINUE
C ---- MATRICES K COMPLEXE ET M REELLE SYMETRIQUES
        ELSE
          DO 37 J = 1,QRNS2
            IFIN = ZI(IADIA-1+J)
            DO 36 I = IDEB,IFIN
              IAUXH=ZI(IHCOL-1+I)
              CAUXR=ZC(IVALR-1+I)
              CAUXM=-ZR(IVALM-1+I)*CUN
              CAUXA=-ZR(IVALA-1+I)*CUN
              ZC(IQRN-1+(J-1)*QRN+IAUXH)   =  CAUXR
              ZC(IQRN-1+(J2-1)*QRN+IAUXH2) =  CAUXM           
              ZC(IQRN-1+QRN*(IAUXH-1)+J)   =  CAUXR
              ZC(IQRN-1+QRN*(IAUXH2-1)+J2) =  CAUXM
              ZC(LQRN-1+(J-1)*QRN+IAUXH)   =  CAUXA
              ZC(LQRN-1+(J-1)*QRN+IAUXH2)  =  CAUXM
              ZC(LQRN-1+(J2-1)*QRN+IAUXH)  =  CAUXM           
              ZC(LQRN-1+QRN*(IAUXH-1)+J)   =  CAUXA
              ZC(LQRN-1+QRN*(IAUXH2-1)+J)  =  CAUXM
              ZC(LQRN-1+QRN*(IAUXH-1)+J2)  =  CAUXM
   36       CONTINUE
            IDEB = IFIN+1
   37     CONTINUE
        ENDIF
      ENDIF
C ---- TESTS UNITAIRES SI LTEST=.TRUE.
      IF (LTEST) THEN
        IF (LKR) THEN
          DO 61 I=1,QRN
          DO 60 J=1,QRN
            ZR(IQRN-1+(J-1)*QRN+I)=RZERO
            ZR(LQRN-1+(J-1)*QRN+I)=RZERO
   60     CONTINUE
   61     CONTINUE
          DO 62 I=1,QRN
            ZR(IQRN-1+(I-1)*QRN+I)=I*RUN
            ZR(LQRN-1+(I-1)*QRN+I)=RUN
   62     CONTINUE
        ELSE      
          DO 64 I=1,QRN
          DO 63 J=1,QRN
            ZC(IQRN-1+(J-1)*QRN+I)=CZERO
            ZC(LQRN-1+(J-1)*QRN+I)=CZERO
   63     CONTINUE
   64     CONTINUE
          DO 65 I=1,QRN
            ZC(IQRN-1+(I-1)*QRN+I)=I*CUN
            ZC(LQRN-1+(I-1)*QRN+I)=CUN
   65     CONTINUE
        ENDIF
      ENDIF      
C ---- CALCUL DE LA NORME INFINIE (ET L1) DE A ET B (SYMETRIQUES)
      ANORM=0.D0
      BNORM=0.D0
      DO 44 I = 1,QRN
        AAUX=0.D0
        BAUX=0.D0
        IF (LKR) THEN
          DO 41 J = 1,QRN
            AAUX=AAUX+ABS(ZR(IQRN-1+(I-1)*QRN+J))
            BAUX=BAUX+ABS(ZR(LQRN-1+(I-1)*QRN+J))
   41     CONTINUE
        ELSE
          DO 42 J = 1,QRN
            AAUX=AAUX+ABS(ZC(IQRN-1+(I-1)*QRN+J))
            BAUX=BAUX+ABS(ZC(LQRN-1+(I-1)*QRN+J))
   42     CONTINUE
        ENDIF
        ANORM=MAX(ANORM,AAUX)
        BNORM=MAX(BNORM,BAUX)
   44 CONTINUE
C ---- ERREUR DONNEES OU CALCUL
      IF (ANORM*BNORM.EQ.0.D0) CALL ASSERT(.FALSE.)
      IF (NIV.GE.2) THEN
        WRITE(IFM,45)ANORM,BNORM
        WRITE(IFM,*)
   45   FORMAT('METHODE QZ, NORME A/B: ',1PD10.2,' / ',1PD10.2)
      ENDIF

C-------------------------------------------------------------------
C-------------------------------------------------------------------
C RESOLUTION LAPACK PROPREMENT DITE, EN 2 PASSES
C 1ERE PASSE: POUR ESTIMER L'ESPACE DE TRAVAIL OPTIMAL EN TEMPS
C 2ND PASSE : RESOLUTION VALEURS PROPRES ET VECTEURS PROPRES A DROITE
C-------------------------------------------------------------------
C-------------------------------------------------------------------

C ---- ADRESSE VECTEURS PROPRES
       IF (LKR.AND.LC) THEN
         CALL WKVECT('&&VPQZLA.VP2','V V R',QRN*QRN,LVEC2)
         LVEC3=LVEC2
       ELSE
         LVEC3=LVEC
       ENDIF    
C ---- QZ EXPERT (EQUILIBRAGE)
      IF (TYPEQZ(1:7).EQ.'QZ_EQUI') THEN
        IF (LKR) THEN
C RECHERCHE DE LA TAILLE OPTIMALE POUR L'ESPACE DE TRAVAIL
          CALL DGGEVX('B','N','V','N',QRN4,ZR(IQRN),
     &      QRN4,ZR(LQRN),QRN4,ZR(QRAR),
     &      ZR(QRAI),ZR(QRBA),ZR(QRVL),LDVL4,ZR(LVEC3),LDVR4,
     &      ILO,IHI,ZR(ILSCAL),ZR(IRSCAL),ABNRM,BBNRM,RCONDE,
     &      RCONDV,ZR(KQRN),QRLWO4,IWORK,BWORK,QRINFO)
C CREATION DU VECTEUR DE TRAVAIL OPTIMALE, DESTRUCTION DU PRECEDENT
C ET RESOLUTION
          IF (QRINFO.EQ.0) THEN
            QRLWO4 = INT(ZR(KQRN))
            QRLWOR = INT(ZR(KQRN))
            CALL WKVECT('&&VPQZLA.QR.WORK2','V V R',QRLWOR,KQRN2)
            CALL DGGEVX('B','N','V','N',QRN4,ZR(IQRN),
     &        QRN4,ZR(LQRN),QRN4,ZR(QRAR),
     &        ZR(QRAI),ZR(QRBA),ZR(QRVL),LDVL4,ZR(LVEC3),LDVR4,
     &        ILO,IHI,ZR(ILSCAL),ZR(IRSCAL),ABNRM,BBNRM,RCONDE,
     &        RCONDV,ZR(KQRN2),QRLWO4,IWORK,BWORK,QRINFO)
          ENDIF
        ELSE
          CALL ZGGEVX('B','N','V','N',QRN4,ZC(IQRN),
     &      QRN4,ZC(LQRN),QRN4,ZC(QRAR),
     &      ZC(QRBA),ZC(QRVL),LDVL4,ZC(LVEC3),LDVR4,
     &      ILO, IHI,ZR(ILSCAL),ZR(IRSCAL),ABNRM,BBNRM,RCONDE,
     &      RCONDV,ZC(KQRN),QRLWO4,ZR(KQRNR),IWORK,BWORK,QRINFO)
          IF (QRINFO.EQ.0) THEN
            QRLWO4 = INT(DBLE(ZC(KQRN)))
            QRLWOR = INT(DBLE(ZC(KQRN)))
            CALL WKVECT('&&VPQZLA.QR.WORK2','V V C',QRLWOR,KQRN2)
            CALL ZGGEVX('B','N','V','N',QRN4,ZC(IQRN),
     &        QRN4,ZC(LQRN),QRN4,ZC(QRAR),
     &        ZC(QRBA),ZC(QRVL),LDVL4,ZC(LVEC3),LDVR4,
     &        ILO,IHI,ZR(ILSCAL),ZR(IRSCAL),ABNRM,BBNRM,RCONDE,
     &        RCONDV,ZC(KQRN2),QRLWO4,ZR(KQRNR),IWORK,BWORK,QRINFO)
          ENDIF
        ENDIF
               
C ----  QZ SIMPLE 
      ELSEIF (TYPEQZ(1:9).EQ.'QZ_SIMPLE') THEN
        IF (LKR) THEN
          CALL DGGEV('N','V',QRN4,ZR(IQRN),QRN4,ZR(LQRN),QRN4,ZR(QRAR),
     &      ZR(QRAI),ZR(QRBA),ZR(QRVL),LDVL4,ZR(LVEC3),LDVR4,
     &      ZR(KQRN),QRLWO4,QRINFO)
          IF (QRINFO.EQ.0) THEN
            QRLWO4 = INT(ZR(KQRN))
            QRLWOR = INT(ZR(KQRN))
            CALL WKVECT('&&VPQZLA.QR.WORK2','V V R',QRLWOR,KQRN2)
            CALL DGGEV('N','V',QRN4,ZR(IQRN),QRN4,ZR(LQRN),QRN4,
     &        ZR(QRAR),ZR(QRAI),ZR(QRBA),ZR(QRVL),LDVL4,ZR(LVEC3),LDVR4,
     &        ZR(KQRN2),QRLWO4,QRINFO)
          ENDIF
        ELSE
          CALL ZGGEV('N','V',QRN4,ZC(IQRN),QRN4,ZC(LQRN),QRN4,ZC(QRAR),
     &      ZC(QRBA),ZC(QRVL),LDVL4,ZC(LVEC3),LDVR4,ZC(KQRN),QRLWO4,
     &      ZR(KQRNR),QRINFO)          
          IF (QRINFO.EQ.0) THEN
            QRLWO4 = INT(DBLE(ZC(KQRN)))
            QRLWOR = INT(DBLE(ZC(KQRN)))
            CALL WKVECT('&&VPQZLA.QR.WORK2','V V C',QRLWOR,KQRN2)
            CALL ZGGEV('N','V',QRN4,ZC(IQRN),QRN4,ZC(LQRN),QRN4,
     &        ZC(QRAR),ZC(QRBA),ZC(QRVL),LDVL4,ZC(LVEC3),LDVR4,
     &        ZC(KQRN2),QRLWO4,ZR(KQRNR),QRINFO)
          ENDIF
        ENDIF          
 
C ----  QR
      ELSEIF (TYPEQZ(1:5).EQ.'QZ_QR') THEN
C ---- CONFIGURATION ILLICITE
        IF (LC) CALL ASSERT(.FALSE.)
        IF (LKR) THEN
          CALL DSYGV(1,'V','U',QRN4,ZR(IQRN),QRN4,ZR(LQRN),QRN4,
     &               ZR(LVALPR),ZR(KQRN),QRLWO4,QRINFO)
          IF (QRINFO.EQ.0) THEN
            QRLWO4 = INT(ZR(KQRN))
            QRLWOR = INT(ZR(KQRN))
            CALL WKVECT('&&VPQZLA.QR.WORK2','V V R',QRLWOR,KQRN2)
            CALL DSYGV(1,'V','U',QRN4,ZR(IQRN),QRN4,ZR(LQRN),QRN4,
     &                 ZR(LVALPR),ZR(KQRN2),QRLWO4,QRINFO)
          ENDIF
        ELSE
C ---- CONFIGURATION ILLICITE
          CALL ASSERT(.FALSE.)
        ENDIF
      ELSE
C ---- OPTION INVALIDE
        CALL ASSERT(.FALSE.)
      ENDIF

C-------------------------------------      
C ------------------------------------
C TRAITEMENT  DES ERREURS DANS LAPACK
C ------------------------------------
C-------------------------------------
      IF (QRINFO.NE.0)
     &  CALL U2MESI('F','ALGELINE5_59', 1 ,QRINFO)
      CALL JEEXIN('&&VPQZLA.QR.WORK2',IRET)
      IF (IRET.NE.0) CALL JEDETR('&&VPQZLA.QR.WORK2')

C POUR DEBUG
C      DO I=1,QRN
C        IF (LKR) THEN
C          WRITE(IFM,*)'I/ALPHA/BETA ',I,ZR(QRAR+I-1),ZR(QRBA+I-1)
C        ELSE
C          WRITE(IFM,*)'I/ALPHA/BETA ',I,ZC(QRAR+I-1),ZC(QRBA+I-1)
C        ENDIF
C      ENDDO

C-----------------------------------      
C ----------------------------------
C POST-TRAITEMENTS ET VERIFICATIONS
C ----------------------------------
C-----------------------------------
      
C ---- INITS.
      DECAL = 0
C     PRECISION MACHINE COMME DANS ARPACK
      PREC=(R8PREM()*0.5D0)**(2.0D+0/3.0D+0)
      PREC2=R8MAEM()*0.5D0
      PREC3=-2.D0*OMECOR
      ALPHA = 0.717D0
      
      IF (LKR.AND.LC)
     &  CALL WKVECT('&&VPQZLA.VP4','V V C',QRN*QRN,LVEC4)
C ---- SI SYSTEME NON SYM REEL, TRAITEMENT DES PARTIES IMAGINAIRES
C ---- NEGATIVES
      IF ((TYPEQZ(1:5).NE.'QZ_QR').AND.(LKR).AND.(.NOT.LC)) THEN
        DO 50 I = 1,QRN
          RAUX=ABS(ZR(QRAI-1+I))
          IF (RAUX.GT.OMECOR) THEN
            VALI(1)=I
            VALR(1)=ZR(QRAR-1+I)
            VALR(2)=ZR(QRAI-1+I)
            CALL U2MESG('A','ALGELINE5_51',0,' ',1,VALI,2,VALR)
          ENDIF
          IF ((RAUX.NE.0.D0).AND.(NIV.GE.2)) THEN
            WRITE(IFM,*)'<VPQZLA> LA VALEUR PROPRE NUMERO ',I
            WRITE(IFM,*)'A UNE PARTIE IMAGINAIRE NON NULLE'
            WRITE(IFM,*)'RE(VP) = ',ZR(QRAR-1+I)
            WRITE(IFM,*)'IM(VP) = ',ZR(QRAI-1+I)
            WRITE(IFM,*)'--> CE PHENOMENE NUMERIQUE EST FREQUENT'
            WRITE(IFM,*)'--> SUR LES PREMIERES VALEURS PROPRES'
            WRITE(IFM,*)'--> LORSQUE LE SPECTRE RECHERCHE EST'
            WRITE(IFM,*)'--> TRES ETENDU (EN PULSATION) '
          ENDIF
   50   CONTINUE
      ENDIF

C---------------------------------------------------------
C ----  ON TESTE LES MODES VALIDES
C ----  1/ ADEQUATION /ALPHA/, /BETA/ VS //A// ET //B//
C ----  2/ ADEQUATION /BETA/ PROCHE DE ZERO ET DDL BLOQUE
C---------------------------------------------------------

      IF ((TYPEQZ(1:5).NE.'QZ_QR').AND.(.NOT.LC)) THEN
        IF (LKR) THEN
C ---- GENERALISE NON SYM REEL
          DO 55 I = 1,QRN
            IF (ABS(ZR(QRBA-1+I)).GT.PREC) THEN
              RAUX=SQRT(ZR(QRAR-1+I)**2+ZR(QRAI-1+I)**2)
              RAUXI=ABS(ZR(QRBA-1+I))
              IF ((RAUX.GT.(ANORM+PREC)).OR.(RAUXI.GT.(BNORM+PREC))) 
     &        THEN
                VALI(1)=I
                VALR(1)=RAUX
                VALR(2)=ANORM
                VALR(3)=RAUXI
                VALR(4)=BNORM
                CALL U2MESG('F','ALGELINE5_61',0,' ',1,VALI,4,VALR)
              ENDIF
              ZR(LVALPR-1+I-DECAL) = ZR(QRAR-1+I)/ZR(QRBA-1+I)
              CALL DCOPY(QRN4,ZR(LVEC3+(I-1)*QRN),1,
     &                        ZR(LVEC+(I-1-DECAL)*QRN),1)
            ELSE
              DECAL = DECAL+1
              IF (NIV.GE.2) THEN
                WRITE(IFM,*)'<VPQZLA> ON SAUTE LA VALEUR PROPRE N ',I
                WRITE(IFM,*)'ALPHA/BETA = ',ZR(QRAR-1+I),ZR(QRBA-1+I)
                WRITE(IFM,*)'--> ELLE CORRESPOND SOIT A UN LAGRANGE,'
     &                        //'SOIT A UN DDL PHYSIQUE BLOQUE'
              ENDIF
            ENDIF
   55     CONTINUE
        ELSE
C ---- GENERALISE NON SYM COMPLEXE
          DO 56 I = 1,QRN
            IF (ABS(ZC(QRBA-1+I)).GT.PREC) THEN
              RAUX=ABS(ZC(QRAR-1+I))
              RAUXI=ABS(ZC(QRBA-1+I))
              IF ((RAUX.GT.(ANORM+PREC)).OR.(RAUXI.GT.(BNORM+PREC)))
     &        THEN
                VALI(1)=I
                VALR(1)=RAUX
                VALR(2)=ANORM
                VALR(3)=RAUXI
                VALR(4)=BNORM
                CALL U2MESG('F','ALGELINE5_61',0,' ',1,VALI,4,VALR)
              ENDIF
              ZC(LVALPR-1+I-DECAL) = ZC(QRAR-1+I)/ZC(QRBA-1+I)
              CALL ZCOPY(QRN4,ZC(LVEC3+(I-1)*QRN),1,
     &          ZC(LVEC+(I-1-DECAL)*QRN),1) 
            ELSE
              DECAL = DECAL+1
              IF (NIV.GE.2) THEN
                WRITE(IFM,*)'<VPQZLA> ON SAUTE LA VALEUR PROPRE N ',I
                WRITE(IFM,*)'ALPHA/BETA = ',ZC(QRAR-1+I),ZC(QRBA-1+I)
                WRITE(IFM,*)'--> ELLE CORRESPOND SOIT A UN LAGRANGE,'
     &                        //'SOIT A UN DDL PHYSIQUE BLOQUE'
              ENDIF
            ENDIF
   56     CONTINUE
        ENDIF
      ELSE IF ((TYPEQZ(1:5).NE.'QZ_QR').AND.(LC)) THEN
        IF (LKR) THEN
C ---- QUADRATIQUE NON SYM REEL
          DO 155 I = 2,QRN,2
            IF (ABS(ZR(QRBA-1+I)).GT.PREC) THEN
              RAUX=SQRT(ZR(QRAR-1+I)**2+ZR(QRAI-1+I)**2)
              RAUXI=ABS(ZR(QRBA-1+I))
              IF ((RAUX.GT.(ANORM+PREC)).OR.(RAUXI.GT.(BNORM+PREC))) 
     &        THEN
                VALI(1)=I
                VALR(1)=RAUX
                VALR(2)=ANORM
                VALR(3)=RAUXI
                VALR(4)=BNORM
                CALL U2MESG('F','ALGELINE5_61',0,' ',1,VALI,4,VALR)
              ENDIF
              ZC(LVALPR+I-2-DECAL) = DCMPLX(
     &          ZR(QRAR-1+I)/ZR(QRBA-1+I),ZR(QRAI-1+I)/ZR(QRBA-1+I))
              ZC(LVALPR+I-1-DECAL) = DCMPLX(
     &          ZR(QRAR-1+I)/ZR(QRBA-1+I),-ZR(QRAI-1+I)/ZR(QRBA-1+I))
              DO 156 J=1,QRN
                ZC(LVEC4+(I-2-DECAL)*QRN+J-1)=DCMPLX(
     &             ZR(LVEC3+(I-2)*QRN+J-1),ZR(LVEC3+(I-1)*QRN+J-1))
                ZC(LVEC4+(I-1-DECAL)*QRN+J-1)=DCMPLX(
     &             ZR(LVEC3+(I-2)*QRN+J-1),-ZR(LVEC3+(I-1)*QRN+J-1))
  156         CONTINUE 
            ELSE
              DECAL = DECAL+2
              IF (NIV.GE.2) THEN
                WRITE(IFM,*)'<VPQZLA> ON SAUTE LA VALEUR PROPRE N ',I
                WRITE(IFM,*)'ALPHAR/ALPHAI/BETA = ',ZR(QRAR-1+I),
     &                      ZR(QRAI-1+I),ZR(QRBA-1+I)
                WRITE(IFM,*)'--> ELLE CORRESPOND SOIT A UN LAGRANGE,'
     &                        //'SOIT A UN DDL PHYSIQUE BLOQUE'
              ENDIF
            ENDIF
  155     CONTINUE
        ELSE
C ---- QUADRATIQUE NON SYM COMPLEXE
          DO 160 I = 1,QRN
            IF (ABS(ZC(QRBA-1+I)).GT.PREC) THEN
              RAUX=ABS(ZC(QRAR-1+I))
              RAUXI=ABS(ZC(QRBA-1+I))
              IF ((RAUX.GT.(ANORM+PREC)).OR.(RAUXI.GT.(BNORM+PREC)))
     &        THEN
                VALI(1)=I
                VALR(1)=RAUX
                VALR(2)=ANORM
                VALR(3)=RAUXI
                VALR(4)=BNORM
                CALL U2MESG('F','ALGELINE5_61',0,' ',1,VALI,4,VALR)
              ENDIF
              ZC(LVALPR-1+I-DECAL) = ZC(QRAR-1+I)/ZC(QRBA-1+I)
              CALL ZCOPY(QRN4,ZC(LVEC3+(I-1)*QRN),1,
     &                        ZC(LVEC+(I-1-DECAL)*QRN),1) 
            ELSE
              DECAL = DECAL+1
              IF (NIV.GE.2) THEN
                WRITE(IFM,*)'<VPQZLA> ON SAUTE LA VALEUR PROPRE N ',I
                WRITE(IFM,*)'ALPHA/BETA = ',ZC(QRAR-1+I),ZC(QRBA-1+I)
                WRITE(IFM,*)'--> ELLE CORRESPOND SOIT A UN LAGRANGE,'
     &                        //'SOIT A UN DDL PHYSIQUE BLOQUE'
              ENDIF
            ENDIF
  160     CONTINUE
        ENDIF
      ELSEIF (TYPEQZ(1:5).EQ.'QZ_QR') THEN
C     --- POST-TRAITEMENT POUR QR ---
        DO 57 I = 1,QRN
           IF ((ZR(LVALPR+I-1).LT.PREC3).OR.(ZR(LVALPR+I-1).GT.PREC2)) 
     &     THEN
             DECAL = DECAL+1
             IF (NIV.GE.2) THEN
                WRITE(IFM,*)'<VPQZLA> ON SAUTE LA VALEUR PROPRE N ',I
                WRITE(IFM,*)'LAMBDA = ',ZR(LVALPR+I-1)
                WRITE(IFM,*)'--> ELLE CORRESPOND SOIT A UN LAGRANGE,'
     &                        //'SOIT A UN DDL PHYSIQUE BLOQUE'
             ENDIF
           ELSE
             ZR(LVALPR-1+I-DECAL) = ZR(LVALPR-1+I)
             CALL DCOPY(QRN4,ZR(IQRN+(I-1)*QRN),1,
     &                       ZR(LVEC+(I-1-DECAL)*QRN),1)
           ENDIF
   57   CONTINUE
      ENDIF
      
C ----  NBRE DE MODES RETENUS
      NCONV = QRN-DECAL

C POUR DEBUG
C      DO I=1,NCONV
C        IF (LKR.AND..NOT.LC) THEN
C          WRITE(IFM,*)'I/LAMBDA ',I,FREQOM(ZR(LVALPR-1+I))
C        ELSE
C          WRITE(IFM,*)'I/LAMBDA ',I,ZC(LVALPR-1+I)
C        ENDIF
C      ENDDO
C RESULTAT DU TEST UNITAIRE
      IF (LTEST) THEN
        WRITE(IFM,*)'*******RESULTATS DU TEST UNITAIRE VPQZLA *********'
        WRITE(IFM,*)' --> ON DOIT TROUVER LAMBDA(I)=I'
        DO 66 I=1,NCONV
          IF (LKR.AND..NOT.LC) THEN
            WRITE(IFM,*)'I/LAMBDA(I) ',I,ZR(LVALPR-1+I)
          ELSE
            WRITE(IFM,*)'I/LAMBDA(I) ',I,ZC(LVALPR-1+I)
          ENDIF
   66   CONTINUE
      ENDIF

C-------------------------------------      
C ----  ON TESTE LES MODES VALIDES
C ----  1/ NBRE TOTAL DE MODES TROUVES
C-------------------------------------     
      IF ((NCONV/IMULT).NE.NEQACT) THEN
        VALI(1)=NCONV/IMULT
        VALI(2)=NEQACT
        CALL U2MESG('F','ALGELINE5_62',0,' ',2,VALI,0,VALR)
      ENDIF

C------------------------------------------------------------------
C -----------------------------------------------------------------
C SELECTION ET TRI DES MODES SUIVANT LES DESIRATAS DES UTILISATEURS
C -----------------------------------------------------------------
C------------------------------------------------------------------

C ---- INITS
      IF (LKR.AND..NOT.LC) THEN
        CALL WKVECT('&&VPQZLA.QR.VPGSKP1','V V R',QRN,IVP1)
      ELSE
        CALL WKVECT('&&VPQZLA.QR.VPGSKP1','V V C',QRN,IVP1)
      ENDIF
      CALL WKVECT('&&VPQZLA.QR.VPGSKP2','V V R',NCONV,IVP2)
      IF (LKR.AND..NOT.LC) THEN
C ---- GENERALISE SPD OU NON SYM REEL
        CALL VPORDO(0,0,NCONV,ZR(LVALPR),ZR(LVEC),QRN)
        VPINF = OMEMIN-PREC
        VPMAX = OMEMAX+PREC
        J=0
        IF (OPTIOF(1:5).EQ.'BANDE') THEN
          DO 80 I = 1, NCONV
            VPCOUR = ZR(LVALPR-1+I)
            IF ((VPCOUR.GE.VPINF).AND.(VPCOUR.LE.VPMAX)) THEN
              J = J+1
              ZR(LVALPR-1+J) = VPCOUR
              CALL DCOPY(QRN4,ZR(LVEC+(I-1)*QRN),1,
     &                        ZR(LVEC+(J-1)*QRN),1)
            ENDIF
   80     CONTINUE
          NCONV = J
          IF (NCONV.NE.NFREQ) THEN
            VALI(1)=NCONV
            VALI(2)=NFREQ
            CALL U2MESG('F','ALGELINE5_63',0,' ',2,VALI,0,VALR)
          ENDIF
          DO 81 I = 1, NCONV
            ZR(LVALPR-1+I) = ZR(LVALPR-1+I) - OMESHI
   81     CONTINUE
          CALL VPORDO(1,0,NCONV,ZR(LVALPR),ZR(LVEC),QRN)
        ELSE
          DO 82 I = 1, NCONV
            ZR(LVALPR-1+I) = ZR(LVALPR-1+I) - OMESHI
   82     CONTINUE
          CALL VPORDO(1,0,NCONV,ZR(LVALPR),ZR(LVEC),QRN)
          NCONV=NFREQ
        ENDIF
        CALL VPGSKP(QRN,NCONV,ZR(LVEC),ALPHA,LMASSE,2,ZR(IVP1),
     &              DDLEXC,ZR(IVP2))
      ELSE IF (.NOT.LKR.AND..NOT.LC) THEN
C ---- GENERALISE NON SYM COMPLEXE
C DECALAGE DU SHIFT HOMOGENE A CE QUI EST FAIT POUR SORENSEN
C STRATEGIE BIZARRE A REVOIR (CF VPSORC, VPFOPC, RECTFC, VPBOSC)
        CALL VPORDC(1,0,NCONV,ZC(LVALPR),ZC(LVEC),QRN)
        NCONV=NFREQ
        CALL VPGSKC(QRN,NCONV,ZC(LVEC),ALPHA,LMASSE,1,ZC(IVP1),DDLEXC,
     &              ZR(IVP2))
        DO 83 I = 1, NCONV
          ZC(LVALPR-1+I) = ZC(LVALPR-1+I) + SIGMA
   83   CONTINUE
      ELSE IF (LKR.AND.LC) THEN
C ---- QUADRATIQUE NON SYM REEL
        CALL JEDETR('&&VPQZLA.VP2')
        CALL VPORDC(1,0,NCONV,ZC(LVALPR),ZC(LVEC4),QRN)
        CALL WPGSKP(QRNS2,NCONV,ZC(LVEC4),ALPHA,LMASSE,LAMOR,2,
     &              ZC(IVP1),DDLEXC,ZR(IVP2))
        DO 89 I=1,NCONV
          DO 88 J=1,QRNS2
C ---- REMPLISSAGE DU VECT PAR LA PARTIE BASSE DE VECTA
           ZC(LVEC+(I-1)*QRN+J-1)=ZC(LVEC4+(I-1)*QRN+QRNS2+J-1)
   88     CONTINUE
   89   CONTINUE
        CALL JEDETR('&&VPQZLA.VP4')
      ELSE IF (.NOT.LKR.AND.LC) THEN
C ---- QUADRATIQUE NON SYM COMPLEXE     
      ENDIF
      CALL JEDETR('&&VPQZLA.QR.VPGSKP1')
      CALL JEDETR('&&VPQZLA.QR.VPGSKP2')
            
      CALL MATFPE(1)      
      CALL JEDEMA()      
      END
