      SUBROUTINE FETREO(REORTH,ALPHAN,NBI,IRG,ITER,NBREOR,IRP,K24FIR,
     &                  K24DDR,IPSRO,GS,IGSMKP,RMIN,IRH,INFOFE,IFM,
     &                  NBPROC,RANG,K24IRP)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/07/2005   AUTEUR BOITEAU O.BOITEAU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C    - FONCTION REALISEE:  CALCUL NOUVELLE DIRECTION DE DESCENTE
C                          REORTHOGONALISEE
C
C      IN :  REORTH :  LOG : FLAG INDIQUANT LA REOTHOGONALISATION
C     OUT :  ALPHAN :  IN  : NUME DU PARAM DE DESCENTE DE L'ITER SUIV
C      IN :   NBI   :  IN  : NOMBRE DE NOEUDS D'INTERFACE
C      IN :   IRG   :  IN  : ADRESSE JEVEUX GK+1
C      IN :   ITER  :  IN  : NUMERO D'ITERATION
C      IN :  NBREOR :  IN  : NBRE DE DD REORTHOGONALISEES
C      IN :   IRP   :  IN   : ADRESSE JEVEUX PK+1
C      IN : K24FIR/K24DDR/IPSRO   : K24/IN : ADRESSE OU OBJETS JEVEUX 
C                             CONTENANT FI*PK, PK ET PK.FIPK
C      IN : GS/IGSMKP: LOG  : FLAG DETERMINANT LA METHODE DE REORTHO
C      IN :  RMIN   :  R8  : PLUS PETITE VALEUR REELLE DISCERNABLE
C      IN :   IRH   :  IN  : ADRESSE JEVEUX HK+1
C     IN RANG  : IN  : RANG DU PROCESSEUR
C     IN NBPROC: IN  : NOMBRE DE PROCESSEURS
C     IN K24IRP : K24 : NOM DE L'OBJET JEVEUX A DISTRIBUER EN PARALLELE
C----------------------------------------------------------------------
C RESPONSABLE BOITEAU O.BOITEAU
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INTEGER      NBI,ITER,NBREOR,IRP,IRG,IPSRO,IRH,RANG,
     &             NBPROC
      REAL*8       ALPHAN
      LOGICAL      REORTH,GS,IGSMKP
      CHARACTER*24 INFOFE,K24IRP,K24FIR,K24DDR

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
      
C DECLARATION VARIABLES LOCALES
      REAL*8       DDOT,RAUX,NORMAV,NORMAP,RGSKP2,BETAN,BETAD,BETA,RMIN,
     &             RBID
      INTEGER      IAUX1,IAUX2,IAUX3,I,J,ITER1,NBI1,IAD,IFM,NIV,IBID,
     &             NIVMPI,IDDFRO,IDDRO
      CHARACTER*24 K24B
      LOGICAL      LPARA
            
C INITS DIVERSES
      IF (NBPROC.EQ.1) THEN
        LPARA=.FALSE.
      ELSE
        LPARA=.TRUE.
      ENDIF
      IF (INFOFE(10:10).EQ.'T') THEN
        NIVMPI=2
      ELSE
        NIVMPI=1
      ENDIF
      
      IF (RANG.EQ.0) THEN

        ITER1=ITER+1
        NBI1=NBI-1
        CALL JEVEUO(K24DDR,'L',IDDRO)
        CALL JEVEUO(K24FIR,'L',IDDFRO)
      
C PARAM EN DUR DE L'IGSM DE KAHN-PARLETT POUR REORTHOGONALISER LES DD
        RGSKP2=0.717D0**2
C ---------------------------------------------------
C ----  PHASE DE REORTHOGONALISATION DES DD VIA UN GS, UN GSM OU UN
C ----  IGSM DE TYPE KAHN-PARLETT
C ---------------------------------------------------
        IF (REORTH) THEN
      
          IF (INFOFE(1:1).EQ.'T') THEN
            IF (GS) THEN
            WRITE(IFM,*)'<FETI/FETREO',RANG,'> REORTHO DE TYPE GS'
            ELSE IF (IGSMKP) THEN
            WRITE(IFM,*)'<FETI/FETREO',RANG,'> REORTHO DE TYPE IGSMKP'
            ELSE
            WRITE(IFM,*)'<FETI/FETREO',RANG,'> REORTHO DE TYPE GSM'
            ENDIF
          ENDIF      
          IF (ITER.LT.NBREOR) THEN
            IAUX2=1
            IAUX3=ITER1
           ELSE
            IAUX2=0
            IAUX3=NBREOR
          ENDIF
C -----------------------------
C CALCUL NOUVELLE DIRECTION DE DESCENTE ORTHOGONALISEE (ETAPE 1)
C (ZR(IRP)) PK+1 = HK+1 (EQUIVAUT A GK+1 SI SANS PRECOND)
C -----------------------------
          CALL DCOPY(NBI,ZR(IRH),1,ZR(IRP),1)

C -----------------------------
C BOUCLE ET TEST IGSM DE KAHN-PARLETT
C -----------------------------
           DO 60 I=IAUX2,IAUX3
         
C --------------
C CALCUL DE BETAKI=-(HK+1.(FI*PK))/(PK.(FI*PK)) (ETAPE 2.1)
C -------------- 
            IAUX1=IDDFRO+I*NBI
            IF (GS) THEN            
              RAUX=-DDOT(NBI,ZR(IRH),1,ZR(IAUX1),1)/ZR(IPSRO+I)
            ELSE
              RAUX=-DDOT(NBI,ZR(IRP),1,ZR(IAUX1),1)/ZR(IPSRO+I)
            ENDIF     
            IAUX1=IDDRO+I*NBI
            IF (IGSMKP) NORMAV=DDOT(NBI,ZR(IRP),1,ZR(IRP),1)

C --------------
C CALCUL NOUVELLE DIRECTION DE DESCENTE ORTHOGONALISEE (ETAPE 2.2)
C (ZR(IRP)) PK+1_PRIM = PK+1 + BETAKI * PI
C --------------
            CALL DAXPY(NBI,RAUX,ZR(IAUX1),1,ZR(IRP),1)

C --------------
C PREMIER TEST (ETAPE 2.3) SI IGSM DE TYPE KAHN-PARLETT
C --------------
            IF (IGSMKP) THEN
              NORMAP=DDOT(NBI,ZR(IRP),1,ZR(IRP),1)
              IF (NORMAP.LT.(RGSKP2 * NORMAV)) THEN
C --------------
C CALCUL DE BETAKI_PRIM=-(PK+1_PRIM.(FI*PK))/(PK.(FI*PK)) (ETAPE 3.1)
C --------------
                IAUX1=IDDFRO+I*NBI
                RAUX=-DDOT(NBI,ZR(IRP),1,ZR(IAUX1),1)/ZR(IPSRO+I)
                IAUX1=IDDRO+I*NBI  
C --------------
C CALCUL NOUVELLE DIRECTION DE DESCENTE ORTHOGONALISEE (ETAPE 3.2)
C (ZR(IRP)) PK+1_SEC = PK+1_PRIM + BETAKI_PRIM * PI
C --------------
                CALL DAXPY(NBI,RAUX,ZR(IAUX1),1,ZR(IRP),1)
C --------------
C SECOND TEST (ETAPE 3.3)
C --------------
                NORMAV=DDOT(NBI,ZR(IRP),1,ZR(IRP),1)
                IF (NORMAV.LT.(RGSKP2 * NORMAP)) THEN
                  DO 58 J=0,NBI1
                    ZR(IRP+J)=0.D0
   58             CONTINUE
                  GOTO 61                               
                ENDIF
              ENDIF
            ENDIF
   60     CONTINUE
   
C SORTIE PREVUE POUR LE TEST 3.3
   61     CONTINUE 

C CALCUL DE ALPHAN = GK+1.PK+1  
          ALPHAN=DDOT(NBI,ZR(IRG),1,ZR(IRP),1)
                
        ELSE
      
C MONITORING
          IF (INFOFE(1:1).EQ.'T')
     &    WRITE(IFM,*)'<FETI/FETREO',RANG,'> SANS REORTHOGONALISATION'
C ---------------------------------------------------
C ----  PAS DE REORTHOGONALISATION (ELLE EST IMPLICITE)
C ---------------------------------------------------
C ON REORTHOGONALISE SEULEMENT PAR RAPPORT A LA DERNIERE DD
C CALCUL DE BETAK = HK+1.GK+1/HK.GK = BETANK/BETADK     
          BETAN=DDOT(NBI,ZR(IRG),1,ZR(IRH),1)
          BETAD=ALPHAN
          IF (ABS(BETAD).LT.RMIN) THEN
            BETAD=RMIN
            CALL UTMESS('A','FETREO','PB DIVISION PAR ZERO'//
     &                      'DANS LA CONSTRUCTION DU BETA !')      
          ENDIF   
          BETA=BETAN/BETAD
          ALPHAN=BETAN

C -----------------------------
C CALCUL NOUVELLE DIRECTION DE DESCENTE NON ORTHOGONALISEE
C (ZR(IRP)) PK+1 = HK+1 + BETAK+1 * PK
C -----------------------------
          DO 90 I=0,NBI1
            ZR(IRP+I)=ZR(IRH+I)+BETA*ZR(IRP+I)
   90     CONTINUE
   
        ENDIF
C FIN DU SI RANG
      ENDIF
C EN PARALLELE, ENVOI DE VO A TOUS LES PROC POUR PREPARER LE CALCUL
C SUIVANT, C'EST A DIRE Z = FI * V
      IF (LPARA)
     &  CALL FETMPI(9,NBI,IFM,NIVMPI,IBID,IBID,K24IRP,K24B,K24B,RBID)
      END      
