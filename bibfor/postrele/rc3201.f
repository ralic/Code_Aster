      SUBROUTINE RC3201 ( LPMPB, LSN, LSNET, LFATIG, LROCHT, LIEU, IG, 
     &                    IOCS, SEISME, NPASS, MATER, SNMAX, SNEMAX,  
     &                    SPMAX, KEMAX, SPMECM, SPTHEM, SAMAX, UTOT, SM,
     &                    SIGPM, RESUAS, RESUSS, RESUCA, RESUCS, FACTUS,
     &                    PMMAX, PBMAX, PMBMAX )
      IMPLICIT   NONE
      INTEGER             IG, IOCS, NPASS
      REAL*8              SNMAX, SNEMAX, SPMAX, KEMAX, SAMAX, UTOT, SM,
     &                    SIGPM, RESUAS(*), RESUSS(*), RESUCA(*),
     &                    RESUCS(*), FACTUS(*), PMMAX, PBMAX, PMBMAX
      LOGICAL             LPMPB, LSN, LSNET, LFATIG, LROCHT, SEISME
      CHARACTER*4         LIEU
      CHARACTER*8         MATER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 21/10/2008   AUTEUR VIVAN L.VIVAN 
C TOLE CRP_20 CRP_21
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE  CRP_20
C     ------------------------------------------------------------------
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
C     CALCUL DES AMPLITUDES DE CONTRAINTES
C     CALCUL DU FACTEUR D'USAGE
C     ON TRAITE LES SITUATIONS COMBINABLES DANS LEUR GROUPE

C     Soit 2 états stabilisés I et J appartenant aux situations P et Q

C     on calcule le SALT(I,J) = 0,5*(EC/E)*Ke(Sn(P,Q))*Sp(I,J)

C     avec Sn(P,Q) = Max( Sn(I,J) )
C          Sn(I,J) = Max( Max(Sn(I,J,ThP)), Max(Sn(I,J,ThQ)) )

C     avec Sp(I,J) = Max( Max(Sp(I,J,ThP)), Max(Sp(I,J,ThQ)) )

C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*32 JEXNOM,JEXNUM,JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------

      INTEGER NBSIGR,NBSIG2,JNSG,IS1,IOC1,IS2,IOC2,INDS,JCOMBI,JPRESA,
     &        JPRESB,JNBOCC,IFM,NIV,I1,I2,JMSA,JMSAS,JMSAB,NDIM,JNOC,
     &        NSCY,NS,JMSN,JNSITU,NSITUP,NSITUQ,INDI,JIST,I, ICAS, 
     &        ICSS, NBSITU, I3, I4
      REAL*8 PPI,PPJ,PQI,PQJ,SALTIJ,SALIJS,UG,SN,SP(2),SMM,SNS,SPS(2),
     &       MPI(12),MPJ(12),MQI(12),MQJ(12),MSE(12),
     &       MATPI(8),MATPJ(8),MATQI(8),MAT1(8),MAT2(8),
     &       MATQJ(8),SALTSE, SNET,SNETS, R8VIDE,VALE(2),
     &       SP12MA(2),SP1(2),SP2(2),SP3(2),SP4(2)
      REAL*8 TYPEKE,SPMECA,SPTHER,SPMECS,SPTHES,SPTHEM,SPMECM,SIMPIJ
      REAL*8 KEMECA,KETHER,KEMECS,KETHES,PM,PB,PMPB,PMS,PBS,PMPBS
      CHARACTER*8 K8B
CCC
      CHARACTER*2  CODRET
      LOGICAL     ENDUR
      INTEGER     NOCC
      REAL*8      NADM
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)
      CALL JEVEUO('&&RC3200.SITU_NUMERO'    ,'L',JNSITU)
      CALL JEVEUO('&&RC3200.SITU_COMBINABLE','L',JCOMBI)
      CALL JEVEUO('&&RC3200.SITU_PRES_A'    ,'L',JPRESA)
      CALL JEVEUO('&&RC3200.SITU_PRES_B'    ,'L',JPRESB)
      CALL JEVEUO('&&RC3200.SITU_NB_OCCUR'  ,'L',JNBOCC)

      CALL JELIRA('&&RC3200.SITU_PRES_A','LONUTI',NBSITU,K8B)
      CALL JELIRA(JEXNUM('&&RC3200.LES_GROUPES',IG),'LONUTI',NBSIGR,K8B)
      CALL JEVEUO(JEXNUM('&&RC3200.LES_GROUPES',IG),'L',JNSG)

      DO 11 IS1 = 1,NBSIGR
        DO 12 IS2 = 1,10
          RESUAS(10*(IS1-1)+IS2) = R8VIDE()
          RESUSS(10*(IS1-1)+IS2) = R8VIDE()
 12     CONTINUE
 11   CONTINUE

      IF (IOCS.EQ.0) THEN
        NBSIG2 = NBSIGR
      ELSE
        NBSIG2 = NBSIGR - 1
      END IF
      NDIM = NBSIG2*NBSIG2
      CALL WKVECT('&&RC3201.NB_OCCURR',   'V V I',NBSIG2,JNOC)
      CALL WKVECT('&&RC3201.IMPR_SITU',   'V V I',NBSIG2,JIST)
      CALL WKVECT('&&RC3201.MATRICE_SN',  'V V R',NDIM,  JMSN)
      CALL WKVECT('&&RC3201.MATRICE_SALT','V V R',NDIM,  JMSA)
      IF (SEISME) THEN
        CALL WKVECT('&&RC3201.MATRICE_SALT_B','V V R',NDIM,JMSAB)
        CALL WKVECT('&&RC3201.MATRICE_SALT_S','V V R',NDIM,JMSAS)
      END IF
C
      SP12MA(1)=0.D0
      SP12MA(2)=0.D0
      NS = 0
      NSCY = 0
      IF (SEISME) THEN
        DO 16 IS1 = 1,NBSIGR
           IOC1 = ZI(JNSG+IS1-1)-NBSITU
           IF (IOC1.EQ.IOCS) GO TO 18
 16     CONTINUE
        CALL U2MESS('F','POSTRCCM_30')
 18     CONTINUE
        NS   = ZI(JNBOCC+2*(NBSITU+IOCS)-2)
        NSCY = ZI(JNBOCC+2*(NBSITU+IOCS)-1)
        PPI = 0.D0
        NSITUP = ZI(JNSITU+ZI(JNSG+IS1-1)-1)
        CALL RCMO02('A',NSITUP,MSE)
        CALL RCMA02('A',IOCS,MATPI)
        NSITUQ = 0
        PM = 0.D0
        PB = 0.D0
        PMPB = 0.D0
        SN = 0.D0
        SNET = 0.D0
        SP(1) = 0.D0
        SP(2) = 0.D0
        SPMECA = 0.D0
        SPTHER = 0.D0
        TYPEKE = MATPI(8)
        IF ( LPMPB ) THEN
          CALL RC32PM(LIEU,SEISME,PPI,MSE,MSE,PM,PB,PMPB)
          RESUAS(10*(IS1-1)+1) = PM
          RESUAS(10*(IS1-1)+2) = PB
          RESUAS(10*(IS1-1)+3) = PMPB
          PMMAX  = MAX (  PMMAX , PM  )
          PBMAX  = MAX (  PBMAX , PB  )
          PMBMAX = MAX ( PMBMAX , PMPB )
        ENDIF
        IF ( LSN ) THEN
          CALL RC32SN('SN_SITU',LIEU,NSITUP,PPI,MSE,NSITUQ,PPI,MSE,
     +                                                   SEISME,MSE,SN)
          RESUAS(10*(IS1-1)+4) = SN
          SNMAX  = MAX( SNMAX , SN )
        ENDIF
        IF ( LSN .AND. LSNET ) THEN
          CALL RC32SN('SN*_SITU',LIEU,NSITUP,PPI,MSE,NSITUQ,PPI,MSE,
     +                                                 SEISME,MSE,SNET)
          RESUAS(10*(IS1-1)+5) = SNET
          SNEMAX  = MAX( SNEMAX , SNET )
        ENDIF
        IF ( LFATIG ) THEN
          CALL RC32SP('SP_SITU',LIEU,NSITUP,PPI,MSE,NSITUQ,PPI,MSE,
     +                SEISME,MSE,SP,TYPEKE,SPMECA,SPTHER)
          CALL RC32SA('SITU',MATER,MATPI,MATPI,SN,SP,TYPEKE,SPMECA,
     &                SPTHER,KEMECA,KETHER,SALTSE,SM)
          RESUAS(10*(IS1-1)+6) = SP(1)
          RESUAS(10*(IS1-1)+7) = KEMECA
          RESUAS(10*(IS1-1)+8) = KETHER
          RESUAS(10*(IS1-1)+9) = SALTSE
        ENDIF
        IF (NIV.GE.2) THEN
          IF ( LPMPB ) THEN
            WRITE (IFM,*) '  SEISME,   PM = ',PM
            WRITE (IFM,*) '            PB = ',PB
            WRITE (IFM,*) '          PMPB = ',PMPB
          ENDIF
          IF ( LSN )    WRITE (IFM,*) '  SEISME,   SN = ',SN
          IF ( LSN.AND.LSNET )  WRITE (IFM,*) '  SEISME,  SN* = ',SNET
          IF ( LFATIG ) WRITE (IFM,*) '  SEISME,   SP = ',SP(1)
          IF (TYPEKE.GT.0.D0 .AND. LFATIG ) THEN
            WRITE (IFM,*) '            SPMECA = ',SPMECA
            WRITE (IFM,*) '            SPTHER = ',SPTHER
            WRITE (IFM,*) '            KEMECA = ',KEMECA
            WRITE (IFM,*) '            KETHER = ',KETHER
          END IF
          IF ( LFATIG ) WRITE (IFM,*) '          SALT = ',SALTSE
        END IF
      ELSE
        DO 30 I = 1 , 12
           MSE(I) = 0.D0
 30     CONTINUE
      END IF

C --- SITUATION P :

      ICAS = 0
      ICSS = 0
      I1 = 0
      DO 20 IS1 = 1,NBSIGR
        IOC1 = ZI(JNSG+IS1-1)
        IF (IOC1.GT.NBSITU) GO TO 20
        IF (.NOT.ZL(JCOMBI+IOC1-1)) GO TO 20

        I1 = I1 + 1
        ZI(JNOC-1+I1) = ZI(JNBOCC+2*IOC1-2)
        ZI(JIST-1+I1) = ZI(JNSITU+ZI(JNSG+IS1-1)-1)

        NSITUP = ZI(JNSITU+IOC1-1)
        NSITUQ = 0
        PPI = ZR(JPRESA+IOC1-1)
        CALL RCMO02('A',NSITUP,MPI)
        CALL RCMA02('A',IOC1,MATPI)
        TYPEKE = MATPI(8)
        PPJ = ZR(JPRESB+IOC1-1)
        CALL RCMO02('B',NSITUP,MPJ)
        CALL RCMA02('B',IOC1,MATPJ)
        PMS = 0.D0
        PM  = 0.D0
        PBS = 0.D0
        PB  = 0.D0
        PMPBS = 0.D0
        PMPB  = 0.D0
        SNS   = 0.D0
        SNETS = 0.D0
        SN    = 0.D0
        SNET  = 0.D0
        SPS(1) = 0.D0
        SPS(2) = 0.D0
        SP(1)  = 0.D0
        SP(2)  = 0.D0
        SPMECA = 0.D0
        SPTHER = 0.D0
        SPMECS = 0.D0
        SPTHES = 0.D0
        INDI = NBSIG2*(I1-1) + (I1-1)
C
        IF ( LPMPB ) THEN
          CALL RC32PM(LIEU,.FALSE.,PPI,MPI,MSE,PM,PB,PMPB)
          CALL RC32PM(LIEU,.FALSE.,PPJ,MPJ,MSE,PM,PB,PMPB)
          RESUSS(10*(IS1-1)+1) = PM
          RESUSS(10*(IS1-1)+2) = PB
          RESUSS(10*(IS1-1)+3) = PMPB
          PMMAX  = MAX (  PMMAX , PM  )
          PBMAX  = MAX (  PBMAX , PB  )
          PMBMAX = MAX ( PMBMAX , PMPB )
        ENDIF
        IF ( LSN ) THEN
          CALL RC32SN('SN_SITU',LIEU,NSITUP,PPI,MPI,NSITUQ,PPJ,MPJ,
     +                                                  .FALSE.,MSE,SN)
          RESUSS(10*(IS1-1)+4) = SN
          SNMAX = MAX(SNMAX,SN)
        ENDIF
        IF ( LSN .AND. LSNET ) THEN
          CALL RC32SN('SN*_SITU',LIEU,NSITUP,PPI,MPI,NSITUQ,PPJ,MPJ,
     +                                                .FALSE.,MSE,SNET)
          RESUSS(10*(IS1-1)+5) = SNET
          SNEMAX = MAX(SNEMAX,SNET)
        ENDIF
        IF ( LROCHT ) THEN
           CALL RC32RT ( LIEU, PPI, PPJ, SIMPIJ )
           SIGPM = MAX ( SIGPM, SIMPIJ )
        ENDIF
        IF (SEISME) THEN
          IF ( LPMPB ) THEN
            CALL RC32PM(LIEU,SEISME,PPI,MPI,MSE,PMS,PBS,PMPBS)
            CALL RC32PM(LIEU,SEISME,PPJ,MPJ,MSE,PMS,PBS,PMPBS)
            RESUAS(10*(IS1-1)+1) = PMS
            RESUAS(10*(IS1-1)+2) = PBS
            RESUAS(10*(IS1-1)+3) = PMPBS
            PMMAX  = MAX (  PMMAX , PMS  )
            PBMAX  = MAX (  PBMAX , PBS  )
            PMBMAX = MAX ( PMBMAX , PMPBS )
          ENDIF
          IF ( LSN ) THEN
            CALL RC32SN('SN_SITU',LIEU,NSITUP,PPI,MPI,NSITUQ,PPJ,MPJ,
     +                                                  SEISME,MSE,SNS)
            RESUAS(10*(IS1-1)+4) = SNS
            SNMAX = MAX(SNMAX,SNS)
          ENDIF
          IF ( LSN .AND. LSNET ) THEN
            CALL RC32SN('SN*_SITU',LIEU,NSITUP,PPI,MPI,NSITUQ,PPJ,MPJ,
     +                                                SEISME,MSE,SNETS)
            RESUAS(10*(IS1-1)+5) = SNETS
            SNEMAX = MAX(SNEMAX,SNETS)
          ENDIF
        END IF
        IF (NIV.GE.2) THEN
          IF ( LPMPB .AND. LSN ) THEN
            WRITE (IFM,1010) NSITUP, SN, PM, PB, PMPB
          ELSEIF ( LPMPB ) THEN
            WRITE (IFM,1012) NSITUP, PM, PB, PMPB
          ELSEIF ( LSN ) THEN
            WRITE (IFM,1014) NSITUP, SN
          END IF
          IF ( LSN .AND. LSNET ) THEN
            WRITE (IFM,1016) NSITUP, SNET
          END IF
        END IF
C
        IF ( (LPMPB .OR. LSN .OR. LSNET) .AND. .NOT.LFATIG ) GOTO 20
C
        NOCC = ZI(JNBOCC+2*IOC1-2)
        SPS(1) = 0.D0
        SPS(2) = 0.D0
        SP(1)  = 0.D0
        SP(2)  = 0.D0
        SPMECA = 0.D0
        SPTHER = 0.D0
        SPMECS = 0.D0
        SPTHES = 0.D0
        CALL RC32SP('SP_SITU',LIEU,NSITUP,PPI,MPI,NSITUQ,PPJ,MPJ,
     +              .FALSE.,MSE,SP,TYPEKE,SPMECA,SPTHER)
        CALL RC32SA ( 'SITU',MATER, MATPI, MATPJ, SN, SP, TYPEKE,
     &                SPMECA, SPTHER,KEMECA,KETHER,SALTIJ, SMM )
        RESUSS(10*(IS1-1)+6) = SP(1)
        RESUSS(10*(IS1-1)+7) = KEMECA
        RESUSS(10*(IS1-1)+8) = KETHER
        RESUSS(10*(IS1-1)+9) = SALTIJ
        KEMAX = MAX( KEMAX , KEMECA )
        IF (NIV.GE.2)  WRITE (IFM,2050) NSITUP, SALTIJ

        ZR(JMSA-1+INDI+1) = SALTIJ
        ZR(JMSA-1+INDI+1) = SALTIJ
        IF (SALTIJ.GT.SAMAX) THEN
          SAMAX = SALTIJ
          SM = SMM
        END IF
        IF (SEISME) THEN
          ZR(JMSAB-1+INDI+1) = SALTIJ
          ZR(JMSAB-1+INDI+1) = SALTIJ
          CALL RC32SP('SP_SITU',LIEU,NSITUP,PPI,MPI,NSITUQ,PPJ,MPJ,
     +                SEISME,MSE,SPS,TYPEKE,SPMECS,SPTHES)
          CALL RC32SA('SITU',MATER,MATPI,MATPJ,SNS,SPS,TYPEKE,
     &                SPMECS,SPTHES,KEMECS,KETHES,SALIJS,SMM)
          RESUAS(10*(IS1-1)+6) = SPS(1)
          RESUAS(10*(IS1-1)+7) = KEMECS
          RESUAS(10*(IS1-1)+8) = KETHES
          RESUAS(10*(IS1-1)+9) = SALIJS
          KEMAX = MAX( KEMAX , KEMECA )
          ZR(JMSAS-1+INDI+1) = SALIJS
          ZR(JMSAS-1+INDI+1) = SALIJS
        END IF

        SPMAX = MAX(SPMAX,SPS(1),SP(1))
        SPMECM = MAX(SPMECM,SPMECS,SPMECA)
        SPTHEM = MAX(SPTHEM,SPTHES,SPTHER)
        IF (NIV.GE.2) WRITE (IFM,1032) NSITUP, SP(1)
        IF (TYPEKE.GT.0.D0) THEN
          IF (NIV.GE.2) THEN
            WRITE (IFM,1132) SPMECA,SPTHER,KEMECA,KETHER
          END IF
        END IF

        CALL LIMEND( MATER, SALTIJ, 'WOHLER', ENDUR )
        IF (ENDUR) THEN
          UG=0.D0
        ELSE
          CALL RCVALE ( MATER, 'FATIGUE', 1, 'SIGM    ', SALTIJ, 1,
     +                  'WOHLER  ', NADM, CODRET, 'F ' )
          IF ( NADM .LT. 0 ) THEN
            VALE(1) = SALTIJ
            VALE(2) = NADM
            CALL U2MESG('A', 'POSTRCCM_32',0,' ',0,0,2,VALE)
          ENDIF
          UG = DBLE( NOCC ) / NADM
        ENDIF
        RESUAS(10*(IS1-1)+10) = UG
        RESUSS(10*(IS1-1)+10) = UG
        IF (NIV.GE.2)  WRITE (IFM,2060) NSITUP, UG

C ----- SITUATION Q :
C       -------------
        I2 = I1
        DO 10 IS2 = IS1 + 1,NBSIGR
          IOC2 = ZI(JNSG+IS2-1)
          IF (.NOT.ZL(JCOMBI+IOC2-1)) GO TO 10
          IF (IOC2.GT.NBSITU) GO TO 10
          I2 = I2 + 1

          NSITUQ = ZI(JNSITU+IOC2-1)

          PQI = ZR(JPRESA+IOC2-1)
          CALL RCMO02('A',NSITUQ,MQI)
          CALL RCMA02('A',IOC2,MATQI)
          TYPEKE = MATPI(8)

          PQJ = ZR(JPRESB+IOC2-1)
          CALL RCMO02('B',NSITUQ,MQJ)
          CALL RCMA02('B',IOC2,MATQJ)

          IF ( LROCHT ) THEN
             CALL RC32RT ( LIEU, PQI, PQJ, SIMPIJ )
             SIGPM = MAX ( SIGPM, SIMPIJ )      
             CALL RC32RT ( LIEU, PPI, PQI, SIMPIJ )
             SIGPM = MAX ( SIGPM, SIMPIJ )      
             CALL RC32RT ( LIEU, PPI, PQJ, SIMPIJ )
             SIGPM = MAX ( SIGPM, SIMPIJ )      
             CALL RC32RT ( LIEU, PPJ, PQI, SIMPIJ )
             SIGPM = MAX ( SIGPM, SIMPIJ )      
             CALL RC32RT ( LIEU, PPJ, PQJ, SIMPIJ )
             SIGPM = MAX ( SIGPM, SIMPIJ )      
          ENDIF

C ------- CALCUL DU SN(P,Q), ON A 4 COMBINAISONS
          SNS = 0.D0
          SN  = 0.D0
          CALL RC32SN('SN_COMB',LIEU,NSITUP,PPI,MPI,NSITUQ,PQI,MQI,
     +                                                  .FALSE.,MSE,SN)
          CALL RC32SN('SN_COMB',LIEU,NSITUP,PPI,MPI,NSITUQ,PQJ,MQJ,
     +                                                  .FALSE.,MSE,SN)
          CALL RC32SN('SN_COMB',LIEU,NSITUP,PPJ,MPJ,NSITUQ,PQJ,MQJ,
     +                                                  .FALSE.,MSE,SN)
          CALL RC32SN('SN_COMB',LIEU,NSITUP,PPJ,MPJ,NSITUQ,PQI,MQI,
     +                                                  .FALSE.,MSE,SN)
          ICSS = ICSS + 1
          RESUCS(ICSS) = SN
          SNMAX = MAX(SNMAX,SN)
          IF (SEISME) THEN
            CALL RC32SN('SN_COMB',LIEU,NSITUP,PPI,MPI,NSITUQ,PQI,MQI,
     +                                                  SEISME,MSE,SNS)
            CALL RC32SN('SN_COMB',LIEU,NSITUP,PPI,MPI,NSITUQ,PQJ,MQJ,
     +                                                  SEISME,MSE,SNS)
            CALL RC32SN('SN_COMB',LIEU,NSITUP,PPJ,MPJ,NSITUQ,PQJ,MQJ,
     +                                                  SEISME,MSE,SNS)
            CALL RC32SN('SN_COMB',LIEU,NSITUP,PPJ,MPJ,NSITUQ,PQI,MQI,
     +                                                  SEISME,MSE,SNS)
            ICAS = ICAS + 1
            RESUCA(ICAS) = SNS
            SNMAX = MAX(SNMAX,SNS)
          END IF
          IF (NIV.GE.2) WRITE (IFM,1020) NSITUP,NSITUQ,SN
          INDS = NBSIG2*(I1-1) + (I2-1)
          INDI = NBSIG2*(I2-1) + (I1-1)

C ------- 1/ CALCUL DU SALT(I,I) A PARTIR DU SN(P,Q) ET SP(I,I)

          SPS(1) = 0.D0
          SPS(2) = 0.D0
          SP(1)  = 0.D0
          SP(2)  = 0.D0
          SPMECA = 0.D0
          SPTHER = 0.D0
          SPMECS = 0.D0
          SPTHES = 0.D0
          CALL RC32SP('SP_COMB',LIEU,NSITUP,PPI,MPI,NSITUQ,PQI,MQI,
     +                .FALSE.,MSE,SP1,TYPEKE,SPMECA,SPTHER)
     
          CALL RC32SP('SP_COMB',LIEU,NSITUP,PPI,MPI,NSITUQ,PQJ,MQJ,
     +                .FALSE.,MSE,SP2,TYPEKE,SPMECA,SPTHER)
          DO 21 I3 = 1,2
            IF (SP2(I3).GT.SP1(I3)) THEN
              SP12MA(I3) = SP2(I3)
              DO 121 I4=1,8
                MAT1(I4) = MATPI(I4)
                MAT2(I4) = MATQJ(I4)
  121         CONTINUE
            ELSE
              SP12MA(I3) = SP1(I3)
              DO 221 I4=1,8
                MAT1(I4) = MATPI(I4)
                MAT2(I4) = MATQI(I4)
  221         CONTINUE
            ENDIF
  21      CONTINUE  
     
          CALL RC32SP('SP_COMB',LIEU,NSITUP,PPJ,MPJ,NSITUQ,PQI,MQI,
     +                .FALSE.,MSE,SP3,TYPEKE,SPMECA,SPTHER)
          DO 22 I3 = 1,2
            IF (SP3(I3).GT.SP12MA(I3)) THEN
              SP12MA(I3) = SP3(I3)
              DO 122 I4=1,8
                MAT1(I4) = MATPJ(I4)
                MAT2(I4) = MATQI(I4)
  122         CONTINUE
            ENDIF
  22      CONTINUE  

          CALL RC32SP('SP_COMB',LIEU,NSITUP,PPJ,MPJ,NSITUQ,PQJ,MQJ,
     +                .FALSE.,MSE,SP4,TYPEKE,SPMECA,SPTHER)
          DO 23 I3 = 1,2
            IF (SP4(I3).GT.SP12MA(I3)) THEN
              SP12MA(I3) = SP4(I3)
              DO 123 I4=1,8
                MAT1(I4) = MATPJ(I4)
                MAT2(I4) = MATQJ(I4)
  123         CONTINUE
            ENDIF
  23      CONTINUE  
          CALL RC32SA('COMB',MATER,MAT1,MAT2,SN,SP12MA,TYPEKE,
     &                SPMECA,SPTHER,KEMECA,KETHER,SALTIJ,SMM)
          ICSS = ICSS + 1
          RESUCS(ICSS) = SP12MA(1)
          ICSS = ICSS + 1
          RESUCS(ICSS) = SP12MA(2)
          ICSS = ICSS + 1
          RESUCS(ICSS) = SALTIJ
          KEMAX = MAX( KEMAX , KEMECA )
          ZR(JMSA-1+INDS+1) = SALTIJ
          ZR(JMSA-1+INDI+1) = SALTIJ
          IF (SALTIJ.GT.SAMAX) THEN
            SAMAX = SALTIJ
            SM = SMM
          END IF
          IF (SEISME) THEN
            ZR(JMSAB-1+INDS+1) = SALTIJ
            ZR(JMSAB-1+INDI+1) = SALTIJ
            CALL RC32SP('SP_COMB',LIEU,NSITUP,PPI,MPI,NSITUQ,PQI,MQI,
     +                  SEISME,MSE,SPS,TYPEKE,SPMECS,SPTHES)
            CALL RC32SA('COMB',MATER,MATPI,MATQI,SNS,SPS,TYPEKE,
     &                  SPMECS,SPTHES,KEMECS,KETHES,SALIJS,SMM)
            ICAS = ICAS + 1
            RESUCA(ICAS) = SPS(1)
            ICAS = ICAS + 1
            RESUCA(ICAS) = SPS(2)
            ICAS = ICAS + 1
            RESUCA(ICAS) = SALIJS
            KEMAX = MAX( KEMAX , KEMECA )
            ZR(JMSAS-1+INDS+1) = SALIJS
            ZR(JMSAS-1+INDI+1) = SALIJS
          END IF
          SPMAX = MAX(SPMAX,SPS(1),SP12MA(1),SPS(2),SP12MA(2))
          SPMECM = MAX(SPMECM,SPMECS,SPMECA)
          SPTHEM = MAX(SPTHEM,SPTHES,SPTHER)
          IF (NIV.GE.2) WRITE (IFM,1031) SP12MA(1), SP12MA(2)
          IF (TYPEKE.GT.0.D0) THEN
            IF (NIV.GE.2) THEN
              WRITE (IFM,1131) SPMECA,SPTHER,KEMECA,KETHER
            END IF
          END IF
   10   CONTINUE
   20 CONTINUE

C --- CALCUL DU FACTEUR D'USAGE

      IF ( LFATIG ) THEN
        IF (SEISME) THEN
          CALL RC32FS ( NBSIG2, ZI(JNOC), ZI(JIST),ZR(JMSAS),
     &                  ZR(JMSAB), SALTSE, NS, NSCY, MATER, UG )
          UTOT = UTOT + UG
        END IF
        IF ( NPASS .EQ. 0 ) THEN
           CALL RC32FU ( NBSIG2, ZI(JNOC), ZI(JIST),
     &                   ZR(JMSA), MATER, UG, FACTUS )
        ELSE
           CALL RC32FP ( NBSIG2, ZI(JNOC), ZI(JIST), ZI(JNSG),
     &                   ZR(JMSA), MATER, UG, FACTUS )
        ENDIF
        UTOT = UTOT + UG
      END IF
C
      IF (SEISME) THEN
        CALL JEDETR('&&RC3201.MATRICE_SALT_B')
        CALL JEDETR('&&RC3201.MATRICE_SALT_S')
      END IF
      CALL JEDETR('&&RC3201.MATRICE_SALT')
      CALL JEDETR('&&RC3201.MATRICE_SN')
      CALL JEDETR('&&RC3201.NB_OCCURR')
      CALL JEDETR('&&RC3201.IMPR_SITU')

 1032 FORMAT (1P,' SITUATION ',I4,' SP =',E12.5)
 2050 FORMAT (1P,' SITUATION ',I4,' SALT =',E12.5)
 2060 FORMAT (1P,' SITUATION ',I4,' FACT_USAGE =',E12.5)

 1010 FORMAT (1P,' SITUATION ',I4,' SN =',E12.5,' PM =',E12.5,
     +                            ' PB =',E12.5,' PMPB =',E12.5)
 1012 FORMAT (1P,' SITUATION ',I4,' PM =',E12.5,
     +                            ' PB =',E12.5,' PMPB =',E12.5)
 1014 FORMAT (1P,' SITUATION ',I4,' SN =',E12.5 )
 1016 FORMAT (1P,' SITUATION ',I4,' SN* =',E12.5 )
 1020 FORMAT (1P,' COMBINAISON DES SITUATIONS ',I4,3X,I4,'  SN =',E12.5)
 1031 FORMAT (1P,41X,'SP1 MIN =',E12.5,2X,'SP2 MAX =',E12.5)
 1131 FORMAT (1P,40X,' SPMECA=',E12.5,' SPTHER=',E12.5,
     &                                ' KEMECA=',E12.5,' KETHER=',E12.5)
 1132 FORMAT (1P,40X,' SPMECA=',E12.5,' SPTHER=',E12.5,
     &                                ' KEMECA=',E12.5,' KETHER=',E12.5)
      CALL JEDEMA()
      END
