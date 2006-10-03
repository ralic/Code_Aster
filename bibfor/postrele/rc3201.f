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
C MODIF POSTRELE  DATE 03/10/2006   AUTEUR CIBHHLV L.VIVAN 
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
     &        NSCY,NS,JMSN,JNSITU,NSITUP,NSITUQ,INDI,JIST,I, ICAS, ICSS
      REAL*8 PPI,PPJ,PQI,PQJ,SALTIJ,SALIJS,UG,SN,SP,SMM,SNS,SPS,MPI(12),
     &       MPJ(12),MQI(12),MQJ(12),MSE(12),MATPI(8),MATPJ(8),MATQI(8),
     &       MATQJ(8),SALTSE, SNET,SNETS, R8VIDE
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

      CALL JELIRA(JEXNUM('&&RC3200.LES_GROUPES',IG),'LONMAX',NBSIGR,K8B)
      CALL JEVEUO(JEXNUM('&&RC3200.LES_GROUPES',IG),'L',JNSG)
      IF (NIV.GE.2) THEN
        WRITE (IFM,1000) IG,NBSIGR
        WRITE (IFM,1002) (ZI(JNSITU+ZI(JNSG+I1-1)-1),I1=1,NBSIGR)
      END IF

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
      NDIM = 2*NBSIG2
      CALL WKVECT('&&RC3201.NB_OCCURR','V V I',NDIM,JNOC)
      CALL WKVECT('&&RC3201.IMPR_SITU','V V I',NDIM,JIST)
      NDIM = NBSIG2*NBSIG2
      CALL WKVECT('&&RC3201.MATRICE_SN','V V R',NDIM,JMSN)
      NDIM = 4*NBSIG2*NBSIG2
      CALL WKVECT('&&RC3201.MATRICE_SALT','V V R',NDIM,JMSA)
      IF (SEISME) THEN
        CALL WKVECT('&&RC3201.MATRICE_SALT_B','V V R',NDIM,JMSAB)
        CALL WKVECT('&&RC3201.MATRICE_SALT_S','V V R',NDIM,JMSAS)
      END IF
C
      NS = 0
      NSCY = 0
      IF (SEISME) THEN
        DO 16 IS1 = 1,NBSIGR
           IOC1 = ZI(JNSG+IS1-1)
           IF (IOC1.EQ.IOCS) GO TO 18
 16     CONTINUE
        CALL U2MESS('F','POSTRELE_30')
 18     CONTINUE
        NS = ZI(JNBOCC+2*IOCS-2)
        NSCY = ZI(JNBOCC+2*IOCS-1)
        PPI = ZR(JPRESA+IOCS-1)
        NSITUP = ZI(JNSITU+IOCS-1)
        CALL RCMO02('A',NSITUP,MSE)
        CALL RCMA02('A',IOCS,MATPI)
        NSITUQ = 0
        PM = 0.D0
        PB = 0.D0
        PMPB = 0.D0
        SN = 0.D0
        SNET = 0.D0
        SP = 0.D0
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
          CALL RC32SA(MATER,MATPI,MATPI,SN,SP,TYPEKE,SPMECA,SPTHER,
     &                KEMECA,KETHER,SALTSE,SM)
          RESUAS(10*(IS1-1)+6) = SP
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
          IF ( LFATIG ) WRITE (IFM,*) '  SEISME,   SP = ',SP
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
        IF (.NOT.ZL(JCOMBI+IOC1-1)) GO TO 20
        IF (IOC1.EQ.IOCS) GO TO 20

        I1 = I1 + 1
        ZI(JNOC-1+2* (I1-1)+1) = ZI(JNBOCC+2*IOC1-2)
        ZI(JNOC-1+2* (I1-1)+2) = ZI(JNBOCC+2*IOC1-2)
        ZI(JIST-1+2* (I1-1)+1) = ZI(JNSITU+ZI(JNSG+IS1-1)-1)
        ZI(JIST-1+2* (I1-1)+2) = ZI(JNSITU+ZI(JNSG+IS1-1)-1)

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
        SPS   = 0.D0
        SP    = 0.D0
        SPMECA = 0.D0
        SPTHER = 0.D0
        SPMECS = 0.D0
        SPTHES = 0.D0
        INDI = 4*NBSIG2* (I1-1) + 4* (I1-1)

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
        SPS = 0.D0
        SP  = 0.D0
        SPMECA = 0.D0
        SPTHER = 0.D0
        SPMECS = 0.D0
        SPTHES = 0.D0
        CALL RC32SP('SP_SITU',LIEU,NSITUP,PPI,MPI,NSITUQ,PPJ,MPJ,
     +              .FALSE.,MSE,SP,TYPEKE,SPMECA,SPTHER)
        CALL RC32SA ( MATER, MATPI, MATPJ, SN, SP, TYPEKE,
     &                SPMECA, SPTHER,KEMECA,KETHER,SALTIJ, SMM )
        RESUSS(10*(IS1-1)+6) = SP
        RESUSS(10*(IS1-1)+7) = KEMECA
        RESUSS(10*(IS1-1)+8) = KETHER
        RESUSS(10*(IS1-1)+9) = SALTIJ
        KEMAX = MAX( KEMAX , KEMECA )
        IF (NIV.GE.2)  WRITE (IFM,2050) NSITUP, SALTIJ

        ZR(JMSA-1+INDI+3) = SALTIJ
        IF (SALTIJ.GT.SAMAX) THEN
          SAMAX = SALTIJ
          SM = SMM
        END IF
        IF (SEISME) THEN
          ZR(JMSAB-1+INDI+3) = SALTIJ
          CALL RC32SP('SP_SITU',LIEU,NSITUP,PPI,MPI,NSITUQ,PPJ,MPJ,
     +                SEISME,MSE,SPS,TYPEKE,SPMECS,SPTHES)
          CALL RC32SA(MATER,MATPI,MATPJ,SNS,SPS,TYPEKE,SPMECS,SPTHES,
     &                KEMECS,KETHES,SALIJS,SMM)
          RESUAS(10*(IS1-1)+6) = SPS
          RESUAS(10*(IS1-1)+7) = KEMECS
          RESUAS(10*(IS1-1)+8) = KETHES
          RESUAS(10*(IS1-1)+9) = SALIJS
          KEMAX = MAX( KEMAX , KEMECA )
          ZR(JMSAS-1+INDI+3) = SALIJS
        END IF

        SPMAX = MAX(SPMAX,SPS,SP)
        SPMECM = MAX(SPMECM,SPMECS,SPMECA)
        SPTHEM = MAX(SPTHEM,SPTHES,SPTHER)
        IF (NIV.GE.2) WRITE (IFM,1032) SP
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
            CALL UTDEBM ('A','WOHLER','NOMBRE DE CYCLES ADMISSIBLES'//
     +                       ' NEGATIF, VERIFIER LA COURBE DE WOHLER')
            CALL UTIMPR ('L','   CONTRAINTE CALCULEE = ',1,SALTIJ)
            CALL UTIMPR ('L','   NADM = ',1,NADM)
            CALL UTFINM ()
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
          IF (IOC2.EQ.IOCS) GO TO 10
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
          INDS = 4*NBSIG2* (I1-1) + 4* (I2-1)
          INDI = 4*NBSIG2* (I2-1) + 4* (I1-1)

C ------- 1/ CALCUL DU SALT(I,I) A PARTIR DU SN(P,Q) ET SP(I,I)

          SPS = 0.D0
          SP = 0.D0
          SPMECA = 0.D0
          SPTHER = 0.D0
          SPMECS = 0.D0
          SPTHES = 0.D0
          CALL RC32SP('SP_COMB',LIEU,NSITUP,PPI,MPI,NSITUQ,PQI,MQI,
     +                .FALSE.,MSE,SP,TYPEKE,SPMECA,SPTHER)
          CALL RC32SA(MATER,MATPI,MATQI,SN,SP,TYPEKE,SPMECA,SPTHER,
     &                KEMECA,KETHER,SALTIJ,SMM)
          ICSS = ICSS + 1
          RESUCS(ICSS) = SP
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
            CALL RC32SA(MATER,MATPI,MATQI,SNS,SPS,TYPEKE,SPMECS,SPTHES,
     &                  KEMECS,KETHES,SALIJS,SMM)
            ICAS = ICAS + 1
            RESUCA(ICAS) = SPS
            ICAS = ICAS + 1
            RESUCA(ICAS) = SALIJS
            KEMAX = MAX( KEMAX , KEMECA )
            ZR(JMSAS-1+INDS+1) = SALIJS
            ZR(JMSAS-1+INDI+1) = SALIJS
          END IF
          SPMAX = MAX(SPMAX,SPS,SP)
          SPMECM = MAX(SPMECM,SPMECS,SPMECA)
          SPTHEM = MAX(SPTHEM,SPTHES,SPTHER)
          IF (NIV.GE.2) WRITE (IFM,1031) SP
          IF (TYPEKE.GT.0.D0) THEN
            IF (NIV.GE.2) THEN
              WRITE (IFM,1131) SPMECA,SPTHER,KEMECA,KETHER
            END IF
          END IF

C ------- 2/ CALCUL DU SALT(I,J) A PARTIR DU SN(P,Q) ET SP(I,J)

          SPS = 0.D0
          SP = 0.D0
          SPMECA = 0.D0
          SPTHER = 0.D0
          SPMECS = 0.D0
          SPTHES = 0.D0
          CALL RC32SP('SP_COMB',LIEU,NSITUP,PPI,MPI,NSITUQ,PQJ,MQJ,
     +                .FALSE.,MSE,SP,TYPEKE,SPMECA,SPTHER)
          CALL RC32SA(MATER,MATPI,MATQJ,SN,SP,TYPEKE,SPMECA,SPTHER,
     &                KEMECA,KETHER,SALTIJ,SMM)
          ICSS = ICSS + 1
          RESUCS(ICSS) = SP
          ICSS = ICSS + 1
          RESUCS(ICSS) = SALTIJ
          KEMAX = MAX( KEMAX , KEMECA )
          ZR(JMSA-1+INDS+3) = SALTIJ
          ZR(JMSA-1+INDI+2) = SALTIJ
          IF (SALTIJ.GT.SAMAX) THEN
            SAMAX = SALTIJ
            SM = SMM
          END IF
          IF (SEISME) THEN
            ZR(JMSAB-1+INDS+3) = SALTIJ
            ZR(JMSAB-1+INDI+2) = SALTIJ
            CALL RC32SP('SP_COMB',LIEU,NSITUP,PPI,MPI,NSITUQ,PQJ,MQJ,
     +                  SEISME,MSE,SPS,TYPEKE,SPMECS,SPTHES)
            CALL RC32SA(MATER,MATPI,MATQJ,SNS,SPS,TYPEKE,SPMECS,SPTHES,
     &                  KEMECS,KETHES,SALIJS,SMM)
            ICAS = ICAS + 1
            RESUCA(ICAS) = SPS
            ICAS = ICAS + 1
            RESUCA(ICAS) = SALIJS
            KEMAX = MAX( KEMAX , KEMECA )
            ZR(JMSAS-1+INDS+3) = SALIJS
            ZR(JMSAS-1+INDI+2) = SALIJS
          END IF
          SPMAX  = MAX(SPMAX,SPS,SP)
          SPMECM = MAX(SPMECM,SPMECS,SPMECA)
          SPTHEM = MAX(SPTHEM,SPTHES,SPTHER)
          IF (NIV.GE.2) WRITE (IFM,1032) SP
          IF (TYPEKE.GT.0.D0) THEN
            IF (NIV.GE.2) THEN
              WRITE (IFM,1132) SPMECA,SPTHER,KEMECA,KETHER
            END IF
          END IF

C ------- 3/ CALCUL DU SALT(J,I) A PARTIR DU SN(P,Q) ET SP(J,I)

          SPS = 0.D0
          SP = 0.D0
          SPMECA = 0.D0
          SPTHER = 0.D0
          SPMECS = 0.D0
          SPTHES = 0.D0
          CALL RC32SP('SP_COMB',LIEU,NSITUP,PPJ,MPJ,NSITUQ,PQI,MQI,
     +                .FALSE.,MSE,SP,TYPEKE,SPMECA,SPTHER)
          CALL RC32SA(MATER,MATPJ,MATQI,SN,SP,TYPEKE,SPMECA,SPTHER,
     &                KEMECA,KETHER,SALTIJ,SMM)
          ICSS = ICSS + 1
          RESUCS(ICSS) = SP
          ICSS = ICSS + 1
          RESUCS(ICSS) = SALTIJ
          KEMAX = MAX( KEMAX , KEMECA )
          ZR(JMSA-1+INDS+2) = SALTIJ
          ZR(JMSA-1+INDI+3) = SALTIJ
          IF (SALTIJ.GT.SAMAX) THEN
            SAMAX = SALTIJ
            SM = SMM
          END IF
          IF (SEISME) THEN
            ZR(JMSAB-1+INDS+2) = SALTIJ
            ZR(JMSAB-1+INDI+3) = SALTIJ
            CALL RC32SP('SP_COMB',LIEU,NSITUP,PPJ,MPJ,NSITUQ,PQI,MQI,
     +                  SEISME,MSE,SPS,TYPEKE,SPMECS,SPTHES)
            CALL RC32SA(MATER,MATPJ,MATQI,SNS,SPS,TYPEKE,SPMECS,SPTHES,
     &                  KEMECS,KETHES,SALIJS,SMM)
            ICAS = ICAS + 1
            RESUCA(ICAS) = SPS
            ICAS = ICAS + 1
            RESUCA(ICAS) = SALIJS
            KEMAX = MAX( KEMAX , KEMECA )
            ZR(JMSAS-1+INDS+2) = SALIJS
            ZR(JMSAS-1+INDI+3) = SALIJS
          END IF
          SPMAX = MAX(SPMAX,SPS,SP)
          SPMECM = MAX(SPMECM,SPMECS,SPMECA)
          SPTHEM = MAX(SPTHEM,SPTHES,SPTHER)
          IF (NIV.GE.2) WRITE (IFM,1034) SP
          IF (TYPEKE.GT.0.D0) THEN
            IF (NIV.GE.2) THEN
              WRITE (IFM,1134) SPMECA,SPTHER,KEMECA,KETHER
            END IF
          END IF

C ------- 4/ CALCUL DU SALT(J,J) A PARTIR DU SN(P,Q) ET SP(J,J)

          SPS = 0.D0
          SP = 0.D0
          SPMECA = 0.D0
          SPTHER = 0.D0
          SPMECS = 0.D0
          SPTHES = 0.D0
          CALL RC32SP('SP_COMB',LIEU,NSITUP,PPJ,MPJ,NSITUQ,PQJ,MQJ,
     +                .FALSE.,MSE,SP,TYPEKE,SPMECA,SPTHER)
          CALL RC32SA(MATER,MATPJ,MATQJ,SN,SP,TYPEKE,SPMECA,SPTHER,
     &                KEMECA,KETHER,SALTIJ,SMM)
          ICSS = ICSS + 1
          RESUCS(ICSS) = SP
          ICSS = ICSS + 1
          RESUCS(ICSS) = SALTIJ
          KEMAX = MAX( KEMAX , KEMECA )
          ZR(JMSA-1+INDS+4) = SALTIJ
          ZR(JMSA-1+INDI+4) = SALTIJ
          IF (SALTIJ.GT.SAMAX) THEN
            SAMAX = SALTIJ
            SM = SMM
          END IF
          IF (SEISME) THEN
            ZR(JMSAB-1+INDS+4) = SALTIJ
            ZR(JMSAB-1+INDI+4) = SALTIJ
            CALL RC32SP('SP_COMB',LIEU,NSITUP,PPJ,MPJ,NSITUQ,PQJ,MQJ,
     +                  SEISME,MSE,SPS,TYPEKE,SPMECS,SPTHES)
            CALL RC32SA(MATER,MATPJ,MATQJ,SNS,SPS,TYPEKE,SPMECS,SPTHES,
     &                  KEMECS,KETHES,SALIJS,SMM)
            ICAS = ICAS + 1
            RESUCA(ICAS) = SPS
            ICAS = ICAS + 1
            RESUCA(ICAS) = SALIJS
            KEMAX = MAX( KEMAX , KEMECA )
            ZR(JMSAS-1+INDS+4) = SALIJS
            ZR(JMSAS-1+INDI+4) = SALIJS
          END IF
          SPMAX = MAX(SPMAX,SPS,SP)
          SPMECM = MAX(SPMECM,SPMECS,SPMECA)
          SPTHEM = MAX(SPTHEM,SPTHES,SPTHER)
          IF (NIV.GE.2) WRITE (IFM,1033) SP
          IF (TYPEKE.GT.0.D0) THEN
            IF (NIV.GE.2) THEN
              WRITE (IFM,1133) SPMECA,SPTHER,KEMECA,KETHER
            END IF
          END IF
   10   CONTINUE
   20 CONTINUE

C --- CALCUL DU FACTEUR D'USAGE

      IF ( LFATIG ) THEN
        IF (SEISME) THEN
          CALL RC32FS(NBSIG2,ZI(JNOC),ZI(JIST),NBSIG2,ZI(JNOC),ZI(JIST),
     &                ZR(JMSAS),ZR(JMSAB),SALTSE,NS,NSCY,MATER,UG)
          UTOT = UTOT + UG
        END IF
        CALL RC32FU(NBSIG2,ZI(JNOC),ZI(JIST),NBSIG2,ZI(JNOC),ZI(JIST),
     &              ZR(JMSA),NPASS,MATER,UG,FACTUS)
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

 2050 FORMAT (1P,' SITUATION ',I4,' SALT =',E12.5)
 2060 FORMAT (1P,' SITUATION ',I4,' FACT_USAGE =',E12.5)

 1000 FORMAT ('=> GROUPE: ',I4,' , NOMBRE DE SITUATIONS: ',I4)
 1002 FORMAT ('=> LISTE DES NUMEROS DE SITUATION: ',100 (I4,1X))
 1010 FORMAT (1P,' SITUATION ',I4,' SN =',E12.5,' PM =',E12.5,
     +                            ' PB =',E12.5,' PMPB =',E12.5)
 1012 FORMAT (1P,' SITUATION ',I4,' PM =',E12.5,
     +                            ' PB =',E12.5,' PMPB =',E12.5)
 1014 FORMAT (1P,' SITUATION ',I4,' SN =',E12.5 )
 1016 FORMAT (1P,' SITUATION ',I4,' SN* =',E12.5 )
 1020 FORMAT (1P,' COMBINAISON DES SITUATIONS ',I4,3X,I4,'  SN =',E12.5)
 1031 FORMAT (1P,26X,'ETAT_A ETAT_A ',' SP =',E12.5)
 1032 FORMAT (1P,26X,'ETAT_B ETAT_A ',' SP =',E12.5)
 1033 FORMAT (1P,26X,'ETAT_B ETAT_B ',' SP =',E12.5)
 1034 FORMAT (1P,26X,'ETAT_A ETAT_B ',' SP =',E12.5)
 1131 FORMAT (1P,26X,'ETAT_A ETAT_A ',' SPMECA=',E12.5,' SPTHER=',E12.5,
     &                                ' KEMECA=',E12.5,' KETHER=',E12.5)
 1132 FORMAT (1P,26X,'ETAT_B ETAT_A ',' SPMECA=',E12.5,' SPTHER=',E12.5,
     &                                ' KEMECA=',E12.5,' KETHER=',E12.5)
 1133 FORMAT (1P,26X,'ETAT_B ETAT_B ',' SPMECA=',E12.5,' SPTHER=',E12.5,
     &                                ' KEMECA=',E12.5,' KETHER=',E12.5)
 1134 FORMAT (1P,26X,'ETAT_A ETAT_B ',' SPMECA=',E12.5,' SPTHER=',E12.5,
     &                                ' KEMECA=',E12.5,' KETHER=',E12.5)
      CALL JEDEMA()
      END
