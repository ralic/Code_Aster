      SUBROUTINE GLRCMM (MATR,MATI,EP,SURFGP,P,SIG,EPST,DEPS
     &                 ,DSIG,ECR,TSHEAR,DELAS,DSIDEP)

      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 25/09/2006   AUTEUR MARKOVIC D.MARKOVIC 
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
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C     CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL,VECTEU,MATRIC,TEMPNO
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C 

      REAL*8   MATR(*)
      INTEGER  MATI(*)
      INTEGER  TSHEAR
      REAL*8   EP, SURFGP, P(3,3), SIG(*), DEPS(*), EPST(*)
      REAL*8   DSIG(*)
      REAL*8   ECR(*)

      REAL*8  VGLOB(3), VLOC(3), Q(2,2), TQ(2,2), ALPHOR,
     &      GG(2),DTLOC(2),DTORTH(2),DGAMOR(2),AUX,
     &      MAXMP(2),MINMP(2),NMIN(2),NMAX(2)
     
      REAL*8 NMIN0,NMIN1,NMIN2,NMAX0,NMAX1,NMAX2,MP1N0,MP2N0
      INTEGER I,IER, J

      INTEGER         MP1F(2),MP2F(2),DMP1F(2),DMP2F(2)
     &               ,DDMP1F(2),DDMP2F(2)
      REAL*8           NORMN,NORMM,DSIDEP(6,*),DELAS(6,*)          
      CHARACTER*24    CGLR
      INTEGER       IMP1,IMP2,IDMP1,IDMP2,IDDMP1,IDDMP2,INN,INM
      INTEGER       IMP1MX,IMP2MI  
      
C----------------------

      CALL JEMARQ()
      
C----- INITIALISATION DES OBJETS "GLRC"      
      
      CGLR = '&&GLRC.MP1FONC'
      CALL JEEXIN(CGLR,IMP1)      
      IF (IMP1 .EQ. 0) THEN
        CALL WKVECT(CGLR,'V V I',2,IMP1)      
      ELSE
        CALL JEVEUO(CGLR,'E',IMP1)      
      ENDIF
      
      CGLR = '&&GLRC.MP2FONC'
      CALL JEEXIN(CGLR,IMP2)      
      IF (IMP2 .EQ. 0) THEN
        CALL WKVECT(CGLR,'V V I',2,IMP2)      
      ELSE
        CALL JEVEUO(CGLR,'E',IMP2)      
      ENDIF
      
      CGLR = '&&GLRC.DMP1FONC'
      CALL JEEXIN(CGLR,IDMP1)      
      IF (IDMP1 .EQ. 0) THEN
        CALL WKVECT(CGLR,'V V I',2,IDMP1)      
      ELSE
        CALL JEVEUO(CGLR,'E',IDMP1)      
      ENDIF
      
      CGLR = '&&GLRC.DMP2FONC'
      CALL JEEXIN(CGLR,IDMP2)      
      IF (IDMP2 .EQ. 0) THEN
        CALL WKVECT(CGLR,'V V I',2,IDMP2)      
      ELSE
        CALL JEVEUO(CGLR,'E',IDMP2)      
      ENDIF
      
      CGLR = '&&GLRC.DDMP1FONC'
      CALL JEEXIN(CGLR,IDDMP1)      
      IF (IDDMP1 .EQ. 0) THEN
        CALL WKVECT(CGLR,'V V I',2,IDDMP1)      
      ELSE
        CALL JEVEUO(CGLR,'E',IDDMP1)      
      ENDIF
      
      CGLR = '&&GLRC.DDMP2FONC'
      CALL JEEXIN(CGLR,IDDMP2)      
      IF (IDDMP2 .EQ. 0) THEN
        CALL WKVECT(CGLR,'V V I',2,IDDMP2)      
      ELSE
        CALL JEVEUO(CGLR,'E',IDDMP2)      
      ENDIF      
      
      CGLR = '&&GLRC.NORMN'
      CALL JEEXIN(CGLR,INN)      
      IF (INN .EQ. 0) THEN
        CALL WKVECT(CGLR,'V V R',1,INN)      
C         CALL JECREO(CGLR,'V V R')      
C         CALL JEVEUO(CGLR,'E',INN)      
       ELSE
        CALL JEVEUO(CGLR,'E',INN)      
      ENDIF
      
      
      CGLR = '&&GLRC.NORMM'
      CALL JEEXIN(CGLR,INM)      
      IF (INM .EQ. 0) THEN
        CALL WKVECT(CGLR,'V V R',1,INM)      
C         CALL JECREO(CGLR,'V V R')      
C         CALL JEVEUO(CGLR,'E',INM)      
      ELSE
        CALL JEVEUO(CGLR,'E',INM)      
      ENDIF 
      
      CGLR = '&&GLRC.MP1'
      CALL JEEXIN(CGLR,IMP1MX)      
      IF (IMP1MX .EQ. 0) THEN
        CALL WKVECT(CGLR,'V V R',2,IMP1MX)      
      ELSE
        CALL JEVEUO(CGLR,'E',IMP1MX)      
      ENDIF
      
      CGLR = '&&GLRC.MP2'
      CALL JEEXIN(CGLR,IMP2MI)      
      IF (IMP2MI .EQ. 0) THEN
        CALL WKVECT(CGLR,'V V R',2,IMP2MI)      
      ELSE
        CALL JEVEUO(CGLR,'E',IMP2MI)      
      ENDIF
      
           
C ---   TRANSFORMATION DES DONNEES      
      
       DO 5, I = 1,2
        MP1F(I)   = MATI(I+1)
        MP2F(I)   = MATI(I+3)
        DMP1F(I)  = MATI(I+5)
        DMP2F(I)  = MATI(I+7)
        DDMP1F(I) = MATI(I+9)
        DDMP2F(I) = MATI(I+11)
 5    CONTINUE
      
      IF (ECR(12).LT.5.D0) THEN
        DO 10, I = 1,3
          VGLOB(I) = ECR(10 + I)
 10     CONTINUE
        CALL MATMUL(P,VGLOB,3,3,1,VLOC)

        ALPHOR = ATAN2(VLOC(2),VLOC(1))
        
        ECR(11) = ALPHOR
        ECR(12) = 10.D0
        ECR(13) = 10.D0
      ELSE
        ALPHOR = ECR(11)
      ENDIF

      Q(1,1) = COS(ALPHOR)
      Q(2,1) = SIN(ALPHOR)
      Q(1,2) = -Q(2,1)
      Q(2,2) = Q(1,1)

       DO 40, I = 1,2
         ZI(IMP1-1   + I) = MP1F(I) 
         ZI(IMP2-1   + I) = MP2F(I)
         ZI(IDMP1-1  + I) = DMP1F(I)
         ZI(IDMP2-1  + I) = DMP2F(I)
         ZI(IDDMP1-1 + I) = DDMP1F(I)
         ZI(IDDMP2-1 + I) = DDMP2F(I)
 40    CONTINUE

        DO 50, I=1,2
            CALL CAFONC(MP1F(I),0.D0,MP1N0) 
            CALL CAFONC(MP2F(I),0.D0,MP2N0) 

            CALL MMFONC(MP1F(I),AUX,MAXMP(I))
            CALL MMFONC(MP2F(I),MINMP(I),AUX)
            
            ZR(IMP1MX-1 + I) = MAXMP(I)
            ZR(IMP2MI-1 + I) = MINMP(I)


            IF ( (MP1N0  .LT.  0.D0).OR.(MP2N0  .GT.  0.D0)
     &          .OR.     (MAXMP(I)-MINMP(I) .LT. 0.D0)   ) THEN
              CALL UTMESS('A','GLRC','BAD DEFINITION OF MP1 AND MP2')
            ENDIF
 50     CONTINUE
        NORMM=0.5D0*MAX(MAXMP(1)-MINMP(1),MAXMP(2)-MINMP(2))

        DO 60, I=1,2
          IF(MP1F(I) .GT. 0) THEN
            NMIN1 = -1.0D20
            NMAX1 =  1.0D20
            NMIN2 = -1.0D20
            NMAX2 =  1.0D20
            NMAX0=MIN(NMAX1,NMAX2)
            NMIN0=MAX(NMIN1,NMIN2)   

            CALL INTERF(MP1F(I),MP2F(I),NORMM,NMIN0,NMIN(I))

            CALL INTERF(MP1F(I),MP2F(I),NORMM, NMAX0,NMAX(I))

          ELSE
            NMAX(I)=0.D0
            NMIN(I)=0.D0
          ENDIF
60      CONTINUE

        NORMN=0.5D0*MAX(ABS(NMAX(1)-NMIN(1)),ABS(NMAX(2)-NMIN(2)))

C ---   CALCUL
        CALL GLRCDD(MATR,MATI,EP,SURFGP,Q,SIG,EPST,DEPS,DSIG
     &             ,ECR,DELAS,DSIDEP)

      ZR(INN) = NORMN
      ZR(INM) = NORMM
      
      CALL JEDEMA()
      
      END 
