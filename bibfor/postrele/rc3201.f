      SUBROUTINE RC3201 ( LPMPB, LSN, LSNET, LFATIG, LROCHT, LIEU, IG,
     &                    IOCS, SEISME, NPASS, MATER, SNMAX, SNEMAX,
     &                    SPMAX, KEMAX, SPMECM, SPTHEM, SAMAX, UTOT, SM,
     &                    SIGPM, RESUAS, RESUSS, RESUCA, RESUCS, FACTUS,
     &                    PMMAX, PBMAX, PMBMAX )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      INTEGER             IG, IOCS, NPASS
      REAL*8              SNMAX, SNEMAX, SPMAX, KEMAX, SAMAX, UTOT, SM,
     &                    SIGPM, RESUAS(*), RESUSS(*), RESUCA(*),
     &                    RESUCS(*), FACTUS(*), PMMAX, PBMAX, PMBMAX
      LOGICAL             LPMPB,LSN, LSNET,LFATIG, LROCHT, SEISME,LBID
      CHARACTER*4         LIEU
      CHARACTER*8         MATER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C TOLE CRP_20 CRP_21
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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

      INTEGER NBSIGR,NBSIG2,JNSG,IS1,IOC1,IS2,IOC2,INDS,JCOMBI,JPRESA,
     &        JPRESB,JNBOCC,IFM,NIV,I1,I2,NDIM,JNOC,
     &        NSCY,NS,JMSN,JNSITU,NSITUP,NSITUQ,INDI,JIST,I, ICAS,
     &        ICSS, NBSITU, I4,
     &        JMFU,JMFUB,JMFUS,NBTHEP,NBTHEQ
      REAL*8 PPI,PPJ,PQI,PQJ,SALTIJ(2),SALIJS(2),UG,SN,SP(2),
     &     SMM,SNS,SPS(2),SPP,SQQ(2),SQQS(2),
     &     MPI(12),MPJ(12),MQI(12),MQJ(12),MSE(12),SIJ0(12),
     &     MATPI(8),MATPJ(8),MATQI(8),MAT1(8),MAT2(8),
     &     MATQJ(8),SALTSE(2), SNET,SNETS, R8VIDE,VALE(2),
     &     SP12MA(2),SP2(2),FUIJ(2),FUSE(2),SPMEPS,
     &     SP2S(2),SPPS,TYPEKE,SPMES2(2),SPMEQS(2),
     &     SPMECA(2),SPTHER(2),SPMECS(2),SPTHES(2),SPTHEM,SPMECM,
     &     SIMPIJ,KEMECA,KETHER,KEMECS,KETHES,PM,PB,PMPB,PMS,PBS,PMPBS,
     &     SPMEC2(2),SPMECP,SPMECQ(2),SPTHE2(2),SPTHEP(2),SPTHEQ(2)
      CHARACTER*8 K8B,KNUMES, KBID
CCC
      INTEGER ICODRE
      LOGICAL     ENDUR,CMAX,MECA
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

      DO 13 IS1 = 1,12
          SIJ0(IS1) = 0.D0
 13   CONTINUE

      IF (IOCS.EQ.0) THEN
        NBSIG2 = NBSIGR
      ELSE
        NBSIG2 = NBSIGR - 1
      END IF
      NDIM = NBSIG2*NBSIG2
      CALL WKVECT('&&RC3201.NB_OCCURR',   'V V I',NBSIG2,JNOC)
      CALL WKVECT('&&RC3201.IMPR_SITU',   'V V I',NBSIG2,JIST)
      CALL WKVECT('&&RC3201.MATRICE_SN',  'V V R',NDIM,  JMSN)
      CALL WKVECT('&&RC3201.MATRICE_FU','V V R',NDIM,  JMFU)
      IF (SEISME) THEN
        CALL WKVECT('&&RC3201.MATRICE_FU_B','V V R',NDIM,  JMFUB)
        CALL WKVECT('&&RC3201.MATRICE_FU_S','V V R',NDIM,  JMFUS)
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
        CALL RCMO02('S',NSITUP,MSE)
        CALL RCMA02('A',IOCS,MATPI)
        NSITUQ = 0
        PM = 0.D0
        PB = 0.D0
        PMPB = 0.D0
        SN = 0.D0
        SNET = 0.D0
        SP(1) = 0.D0
        SP(2) = 0.D0
        TYPEKE = MATPI(8)
        IF ( LPMPB ) THEN
          CALL RC32PM(LIEU,SEISME,PPI,SIJ0,MSE,PM,PB,PMPB)
          RESUAS(10*(IS1-1)+1) = PM
          RESUAS(10*(IS1-1)+2) = PB
          RESUAS(10*(IS1-1)+3) = PMPB
          PMMAX  = MAX (  PMMAX , PM  )
          PBMAX  = MAX (  PBMAX , PB  )
          PMBMAX = MAX ( PMBMAX , PMPB )
        ENDIF
        IF ( LSN ) THEN
          CALL RC32SN('SN_SITU',LIEU,NSITUP,PPI,SIJ0,NSITUQ,PPI,SIJ0,
     &                                                   SEISME,MSE,SN)
          RESUAS(10*(IS1-1)+4) = SN
          SNMAX  = MAX( SNMAX , SN )
        ENDIF
        IF ( LSN .AND. LSNET ) THEN
          CALL RC32SN('SN*_SITU',LIEU,NSITUP,PPI,SIJ0,NSITUQ,PPI,SIJ0,
     &                                                 SEISME,MSE,SNET)
          RESUAS(10*(IS1-1)+5) = SNET
          SNEMAX  = MAX( SNEMAX , SNET )
        ENDIF
        IF ( LFATIG ) THEN
          CALL RC32SP('SP_SITU',LIEU,NSITUP,PPI,SIJ0,NSITUQ,PPI,SIJ0,
     &                SEISME,MSE,SP,TYPEKE,SPMECA,SPTHEP)
          CALL RC32SA('SITU',MATER,MATPI,MATPI,SN,SP,TYPEKE,SPMECA,
     &                SPTHEP,KEMECA,KETHER,SALTSE,SM,FUSE)
          RESUAS(10*(IS1-1)+6) = SP(1)
          RESUAS(10*(IS1-1)+7) = KEMECA
          RESUAS(10*(IS1-1)+8) = KETHER
          RESUAS(10*(IS1-1)+9) = SALTSE(1)
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
            WRITE (IFM,*) '            SPMECA = ',SPMECA(1)
            WRITE (IFM,*) '            SPTHER = ',SPTHEP
            WRITE (IFM,*) '            KEMECA = ',KEMECA
            WRITE (IFM,*) '            KETHER = ',KETHER
          END IF
          IF ( LFATIG ) WRITE (IFM,*) '          SALT = ',SALTSE(1)
          IF ( LFATIG ) WRITE (IFM,*) '          FU = ',FUSE
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
        ZI(JIST-1+I1) = ZI(JNSITU+IOC1-1)

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
        SPMECA(1)  = 0.D0
        SPMECS(1)  = 0.D0
        SPTHES(1)  = 0.D0
        SPTHER(1)  = 0.D0
        SPS(1)  = 0.D0
        SPS(2)  = 0.D0
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
     &                                                  .FALSE.,MSE,SN)
          RESUSS(10*(IS1-1)+4) = SN
          SNMAX = MAX(SNMAX,SN)
        ENDIF
        IF ( LSN .AND. LSNET ) THEN
          CALL RC32SN('SN*_SITU',LIEU,NSITUP,PPI,MPI,NSITUQ,PPJ,MPJ,
     &                                                .FALSE.,MSE,SNET)
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
     &                                                  SEISME,MSE,SNS)
            RESUAS(10*(IS1-1)+4) = SNS
            SNMAX = MAX(SNMAX,SNS)
          ENDIF
          IF ( LSN .AND. LSNET ) THEN
            CALL RC32SN('SN*_SITU',LIEU,NSITUP,PPI,MPI,NSITUQ,PPJ,MPJ,
     &                                                SEISME,MSE,SNETS)
            RESUAS(10*(IS1-1)+5) = SNETS
            SNEMAX = MAX(SNEMAX,SNETS)
          ENDIF
        END IF
        IF (NIV.GE.2) THEN
          IF ( LPMPB )  WRITE (IFM,1012) NSITUP, PM, PB, PMPB
          IF ( LSN ) THEN
            IF ( LSNET ) THEN
              IF (SEISME) THEN
                WRITE (IFM,1017) NSITUP, SNET, SNETS
              ELSE
                WRITE (IFM,1016) NSITUP, SNET
              END IF
            ENDIF
            IF (SEISME) THEN
              WRITE (IFM,1015) NSITUP, SN, SNS
            ELSE
              WRITE (IFM,1014) NSITUP, SN
            END IF
          END IF
        END IF
C
        IF ( (LPMPB .OR. LSN .OR. LSNET) .AND. .NOT.LFATIG ) GOTO 20
C
        NOCC = ZI(JNBOCC+2*IOC1-2)
        CALL RC32SP('SP_SITU',LIEU,NSITUP,PPI,MPI,NSITUQ,PPJ,MPJ,
     &              .FALSE.,MSE,SP,TYPEKE,SPMECA,SPTHEP)
        SPMECP = SPMECA(1)
        CALL RC32SA ( 'SITU',MATER, MATPI, MATPJ, SN, SP, TYPEKE,
     &                SPMECA, SPTHEP,KEMECA,KETHER,SALTIJ,SMM,FUIJ )
        RESUSS(10*(IS1-1)+6) = SP(1)
        RESUSS(10*(IS1-1)+7) = KEMECA
        RESUSS(10*(IS1-1)+8) = KETHER
        RESUSS(10*(IS1-1)+9) = SALTIJ(1)
        KEMAX = MAX( KEMAX , KEMECA )

        ZR(JMFU-1+INDI+1) = FUIJ(1)
        IF (SALTIJ(1).GT.SAMAX) THEN
          SAMAX = SALTIJ(1)
          SM = SMM
        END IF
        IF (SEISME) THEN
          ZR(JMFUB-1+INDI+1) = FUIJ(1)
          CALL RC32SP('SP_SITU',LIEU,NSITUP,PPI,MPI,NSITUQ,PPJ,MPJ,
     &                SEISME,MSE,SPS,TYPEKE,SPMECS,SPTHES)
          CALL RC32SA('SITU',MATER,MATPI,MATPJ,SNS,SPS,TYPEKE,
     &                SPMECS,SPTHES,KEMECS,KETHES,SALIJS,SMM,FUIJ)
          RESUAS(10*(IS1-1)+6) = SPS(1)
          RESUAS(10*(IS1-1)+7) = KEMECS
          RESUAS(10*(IS1-1)+8) = KETHES
          RESUAS(10*(IS1-1)+9) = SALIJS(1)
          KEMAX = MAX( KEMAX , KEMECA )
          ZR(JMFUS-1+INDI+1) = FUIJ(1)
          SPMEPS = SPMECS(1)
        END IF

        SPMAX = MAX(SPMAX,SPS(1),SP(1))
        SPMECM = MAX(SPMECM,SPMECS(1),SPMECA(1))
        SPTHEM = MAX(SPTHEM,SPTHES(1),SPTHER(1))
        IF (NIV.GE.2) THEN
          WRITE (IFM,1018) NSITUP, SP(1)
          IF (SEISME) WRITE (IFM,1019) NSITUP, SPS(1)
          IF (TYPEKE.GT.0.D0) THEN
            WRITE (IFM,1050) NSITUP,SPMECA(1),SPTHEP(1),KEMECA,KETHER
            IF (SEISME) WRITE (IFM,1051) NSITUP,SPMECS(1),KEMECS
          END IF
          WRITE (IFM,1060) NSITUP, SALTIJ(1),ZR(JMFU-1+INDI+1)
          IF (SEISME) WRITE (IFM,1061) NSITUP, SALIJS(1),
     &                                        ZR(JMFUS-1+INDI+1)
        END IF

        CALL LIMEND( MATER, SALTIJ(1), 'WOHLER',KBID, ENDUR )
        IF (ENDUR) THEN
          UG=0.D0
        ELSE
          CALL RCVALE ( MATER, 'FATIGUE', 1, 'SIGM    ', SALTIJ(1), 1,
     &                  'WOHLER  ', NADM, ICODRE, 2)
          IF ( NADM .LT. 0 ) THEN
            VALE(1) = SALTIJ(1)
            VALE(2) = NADM
            CALL U2MESG('A', 'POSTRCCM_32',0,' ',0,0,2,VALE)
          ENDIF
          UG = DBLE( NOCC ) / NADM
        ENDIF
        RESUAS(10*(IS1-1)+10) = UG
        RESUSS(10*(IS1-1)+10) = UG
C        IF (NIV.GE.2)  WRITE (IFM,1061) NSITUP, UG

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

C ------- CALCUL DU SN(P,Q), ON A 4 COMBINAISONS + SN(P,P) et SN(Q,Q)
          CALL RC32SN('SN_COMB',LIEU,NSITUP,PPI,MPI,NSITUQ,PQI,MQI,
     &                                                  .FALSE.,MSE,SN)
          CALL RC32SN('SN_COMB',LIEU,NSITUP,PPI,MPI,NSITUQ,PQJ,MQJ,
     &                                                  .FALSE.,MSE,SN)
          CALL RC32SN('SN_COMB',LIEU,NSITUP,PPJ,MPJ,NSITUQ,PQJ,MQJ,
     &                                                  .FALSE.,MSE,SN)
          CALL RC32SN('SN_COMB',LIEU,NSITUP,PPJ,MPJ,NSITUQ,PQI,MQI,
     &                                                  .FALSE.,MSE,SN)
          CALL RC32SN('SN_SITU',LIEU,NSITUQ,PQI,MQI,NSITUQ,PQJ,MQJ,
     &                                                  .FALSE.,MSE,SN)
          ICSS = ICSS + 1
          RESUCS(ICSS) = SN
          SNMAX = MAX(SNMAX,SN)
          IF (SEISME) THEN
            CALL RC32SN('SN_COMB',LIEU,NSITUP,PPI,MPI,NSITUQ,PQI,MQI,
     &                                                  SEISME,MSE,SNS)
            CALL RC32SN('SN_COMB',LIEU,NSITUP,PPI,MPI,NSITUQ,PQJ,MQJ,
     &                                                  SEISME,MSE,SNS)
            CALL RC32SN('SN_COMB',LIEU,NSITUP,PPJ,MPJ,NSITUQ,PQJ,MQJ,
     &                                                  SEISME,MSE,SNS)
            CALL RC32SN('SN_COMB',LIEU,NSITUP,PPJ,MPJ,NSITUQ,PQI,MQI,
     &                                                  SEISME,MSE,SNS)
            CALL RC32SN('SN_SITU',LIEU,NSITUQ,PQI,MQI,NSITUQ,PQJ,MQJ,
     &                                                  SEISME,MSE,SNS)
            ICAS = ICAS + 1
            RESUCA(ICAS) = SNS
            SNMAX = MAX(SNMAX,SNS)
          END IF
          IF (NIV.GE.2) WRITE (IFM,1110) NSITUP,NSITUQ,SN
          IF ((NIV.GE.2) .AND. SEISME) WRITE (IFM,1111) SNS
          INDS = NBSIG2*(I1-1) + (I2-1)
          INDI = NBSIG2*(I2-1) + (I1-1)

C ------- 1/ CALCUL DU SALT(I,I) A PARTIR DU SN(P,Q) ET SP(P,Q)

C NOMBRE DE PAS DE TEMPS POUR DISTINGUER LE CAS MECANIQUE PUR
          KNUMES = 'S       '
          CALL CODENT(NSITUP,'D0',KNUMES(2:8))
          CALL JELIRA(JEXNOM('&&RC3200.SITU_THERMIQUE',KNUMES),'LONUTI',
     &                NBTHEP,K8B)
          KNUMES = 'S       '
          CALL CODENT(NSITUQ,'D0',KNUMES(2:8))
          CALL JELIRA(JEXNOM('&&RC3200.SITU_THERMIQUE',KNUMES),'LONUTI',
     &                NBTHEQ,K8B)

          MECA = .FALSE.
          IF ((NBTHEP+NBTHEQ) .EQ. 0) MECA = .TRUE.


C - PREMIERE COMBINAISON : PI - QI
          CALL RC32SP('SP_COMB',LIEU,NSITUP,PPI,MPI,NSITUQ,PQI,MQI,
     &                .FALSE.,MSE,SP12MA,TYPEKE,SPMECA,SPTHER)

          DO 119 I4=1,8
             MAT1(I4) = MATPI(I4)
             MAT2(I4) = MATQI(I4)
  119     CONTINUE

          IF (SEISME) THEN
            CALL RC32SP('SP_COMB',LIEU,NSITUP,PPI,MPI,NSITUQ,PQI,MQI,
     &                  SEISME,MSE,SPS,TYPEKE,SPMECS,SPTHES)
          END IF

C - DEUXIEME COMBINAISON : PI - QJ
          CALL RC32SP('SP_COMB',LIEU,NSITUP,PPI,MPI,NSITUQ,PQJ,MQJ,
     &                .FALSE.,MSE,SP2,TYPEKE,SPMEC2,SPTHE2)

          IF (TYPEKE.GT.0.D0) THEN
            CALL RC32MS(.TRUE.,SPMECA,SPMEC2,LBID)
            CALL RC32MS(.TRUE.,SPTHER,SPTHE2,LBID)
          ENDIF

          IF (SEISME) THEN
            CALL RC32SP('SP_COMB',LIEU,NSITUP,PPI,MPI,NSITUQ,PQJ,MQJ,
     &                  SEISME,MSE,SP2S,TYPEKE,SPMES2,SPTHES)
            CALL RC32MS(MECA,SPS,SP2S,CMAX)
            IF (TYPEKE.GT.0.D0) THEN
              CALL RC32MS(.TRUE.,SPMECS,SPMES2,LBID)
            ENDIF
          END IF

          CALL RC32MS(MECA,SP12MA,SP2,CMAX)

          IF (CMAX) THEN
            DO 120 I4=1,8
              MAT1(I4) = MATPI(I4)
              MAT2(I4) = MATQJ(I4)
  120       CONTINUE
          ENDIF

C - TROISIEME COMBINAISON : PJ - QI
          CALL RC32SP('SP_COMB',LIEU,NSITUP,PPJ,MPJ,NSITUQ,PQI,MQI,
     &                .FALSE.,MSE,SP2,TYPEKE,SPMEC2,SPTHE2)

          IF (TYPEKE.GT.0.D0) THEN
            CALL RC32MS(.TRUE.,SPMECA,SPMEC2,LBID)
            CALL RC32MS(.TRUE.,SPTHER,SPTHE2,LBID)
          ENDIF

          IF (SEISME) THEN
            CALL RC32SP('SP_COMB',LIEU,NSITUP,PPJ,MPJ,NSITUQ,PQI,MQI,
     &                  SEISME,MSE,SP2S,TYPEKE,SPMES2,SPTHES)
            CALL RC32MS(MECA,SPS,SP2S,CMAX)
            IF (TYPEKE.GT.0.D0) THEN
              CALL RC32MS(.TRUE.,SPMECS,SPMES2,LBID)
            ENDIF
          END IF

          CALL RC32MS(MECA,SP12MA,SP2,CMAX)

          IF (CMAX) THEN
            DO 121 I4=1,8
              MAT1(I4) = MATPJ(I4)
              MAT2(I4) = MATQI(I4)
  121       CONTINUE
          ENDIF

C - QUATRIEME COMBINAISON : PJ - QJ
          CALL RC32SP('SP_COMB',LIEU,NSITUP,PPJ,MPJ,NSITUQ,PQJ,MQJ,
     &                .FALSE.,MSE,SP2,TYPEKE,SPMEC2,SPTHE2)

          IF (TYPEKE.GT.0.D0) THEN
            CALL RC32MS(.TRUE.,SPMECA,SPMEC2,LBID)
            CALL RC32MS(.TRUE.,SPTHER,SPTHE2,LBID)
          ENDIF

          IF (SEISME) THEN
            CALL RC32SP('SP_COMB',LIEU,NSITUP,PPJ,MPJ,NSITUQ,PQJ,MQJ,
     &                  SEISME,MSE,SP2S,TYPEKE,SPMES2,SPTHES)
            CALL RC32MS(MECA,SPS,SP2S,CMAX)
            IF (TYPEKE.GT.0.D0) THEN
              CALL RC32MS(.TRUE.,SPMECS,SPMES2,LBID)
            ENDIF
          END IF

          CALL RC32MS(MECA,SP12MA,SP2,CMAX)

          IF (CMAX) THEN
            DO 122 I4=1,8
              MAT1(I4) = MATPJ(I4)
              MAT2(I4) = MATQI(I4)
  122       CONTINUE
          ENDIF

C -  CINQUIEME COMBINAISON : QI - QJ
         CALL RC32SP('SP_SITU',LIEU,NSITUQ,PQI,MQI,0,PQJ,MQJ,
     &              .FALSE.,MSE,SQQ,TYPEKE,SPMECQ,SPTHEQ)
         SPP = RESUSS(10*(IS1-1)+6)
         IF (SQQ(1).GE.SP12MA(1)) THEN
           SP12MA(1) = SQQ(1)
           SP12MA(2) = SPP
           DO 124 I4=1,8
             MAT1(1) = MATQI(1)
             MAT2(1) = MATQJ(1)
  124      CONTINUE
         END IF

         IF (TYPEKE.GT.0.D0) THEN
           IF (SPMECQ(1).GE.SPMECA(1)) THEN
             SPMECA(1) = SPMECQ(1)
             SPMECA(2) = SPMECP
           ENDIF
           IF (SPTHEQ(1).GE.SPTHER(1)) THEN
             SPTHER(1) = SPTHEQ(1)
             SPTHER(2) = SPTHEP(1)
           ENDIF
         ENDIF

         IF (SEISME) THEN
           CALL RC32SP('SP_SITU',LIEU,NSITUQ,PQI,MQI,NSITUQ,PQJ,MQJ,
     &                SEISME,MSE,SQQS,TYPEKE,SPMEQS,SPTHES)
           IF (SQQS(1).GE.SPS(1)) THEN
             SPS(1) = SQQS(1)
             SPPS = RESUAS(10*(IS1-1)+6)
             SPS(2) = SPPS
           END IF
           IF (TYPEKE.GT.0.D0) THEN
             IF (SPMEQS(1).GE.SPMECS(1)) THEN
             SPMECS(1) = SPMEQS(1)
             SPMECS(2) = SPMEPS
             ENDIF
           ENDIF
         END IF

C - SIXIEME COMBINAISON : PI - PJ
         SPP = RESUSS(10*(IS1-1)+6)
         IF (SPP.GE.SP12MA(1)) THEN
           SP12MA(1) = SPP
           SP12MA(2) = SQQ(1)
           DO 123 I4=1,8
             MAT1(1) = MATPI(1)
             MAT2(1) = MATPJ(1)
  123      CONTINUE
         ENDIF

         IF (TYPEKE.GT.0.D0) THEN
           IF (SPMECP.GE.SPMECA(1)) THEN
             SPMECA(1) = SPMECP
             SPMECA(2) = SPMECQ(1)
           ENDIF
           IF (SPTHEP(1).GE.SPTHER(1)) THEN
             SPTHER(1) = SPTHEP(1)
             SPTHER(2) = SPTHEQ(1)
           ENDIF
         ENDIF

         IF (SEISME) THEN
           SPPS = RESUAS(10*(IS1-1)+6)
           IF (SPPS.GE.SPS(1)) THEN
             SPS(1) = SPPS
             SPS(2) = SQQS(1)
           END IF
           IF (TYPEKE.GT.0.D0) THEN
             IF (SPMEPS.GE.SPMECS(1)) THEN
             SPMECS(1) = SPMEPS
             SPMECS(2) = SPMEQS(1)
             ENDIF
           ENDIF
         END IF


C - CALCUL DE SALT ASSOCIE A SP1 ET SP2
         CALL RC32SA('COMB',MATER,MAT1,MAT2,SN,SP12MA,TYPEKE,
     &                SPMECA,SPTHER,KEMECA,KETHER,SALTIJ,SMM,FUIJ)
          ICSS = ICSS + 1
          RESUCS(ICSS) = SP12MA(1)
          ICSS = ICSS + 1
          RESUCS(ICSS) = SP12MA(2)
          ICSS = ICSS + 1
          RESUCS(ICSS) = SALTIJ(1)
          ICSS = ICSS + 1
          RESUCS(ICSS) = SALTIJ(2)
          KEMAX = MAX( KEMAX , KEMECA )
          ZR(JMFU-1+INDI+1) = FUIJ(1)+FUIJ(2)
          ZR(JMFU-1+INDS+1) = FUIJ(1)+FUIJ(2)
          IF (SALTIJ(1).GT.SAMAX) THEN
            SAMAX = SALTIJ(1)
            SM = SMM
          ELSEIF (SALTIJ(2).GT.SAMAX) THEN
            SAMAX = SALTIJ(2)
            SM = SMM
          END IF
          IF (SEISME) THEN
            ZR(JMFUB-1+INDS+1) = FUIJ(1)+FUIJ(2)
            ZR(JMFUB-1+INDI+1) = FUIJ(1)+FUIJ(2)
C ON PREND SPTHES = SPTHER
            CALL RC32SA('COMB',MATER,MAT1,MAT2,SNS,SPS,TYPEKE,
     &                  SPMECS,SPTHER,KEMECS,KETHES,SALIJS,SMM,FUIJ)
            ICAS = ICAS + 1
            RESUCA(ICAS) = SPS(1)
            ICAS = ICAS + 1
            RESUCA(ICAS) = SPS(2)
            ICAS = ICAS + 1
            RESUCA(ICAS) = SALIJS(1)
            ICAS = ICAS + 1
            RESUCA(ICAS) = SALIJS(2)
            KEMAX = MAX( KEMAX , KEMECA )
            ZR(JMFUS-1+INDS+1) = FUIJ(1)+FUIJ(2)
            ZR(JMFUS-1+INDI+1) = FUIJ(1)+FUIJ(2)
          END IF
          SPMAX = MAX(SPMAX,SPS(1),SP12MA(1),SPS(2),SP12MA(2))
          SPMECM = MAX(SPMECM,SPMECS(1),SPMECA(1))
          SPTHEM = MAX(SPTHEM,SPTHES(1),SPTHER(1))
          IF (NIV.GE.2) THEN
            WRITE (IFM,1121) SP12MA(1), SP12MA(2)
            IF (SEISME) WRITE (IFM,1122) SPS(1), SPS(2)
            IF (TYPEKE.GT.0.D0)  THEN
              WRITE (IFM,1131) SPMECA(1),SPMECA(2),KEMECA
              WRITE (IFM,1132) SPTHER(1),SPTHER(2),KETHER
              IF (SEISME) WRITE (IFM,1133) SPMECS(1),SPMECS(2),KEMECS
            END IF
            WRITE (IFM,1231) SALTIJ(1), SALTIJ(2)
            IF (SEISME) WRITE (IFM,1232) SALIJS(1), SALIJS(2)
            WRITE (IFM,1331) FUIJ(1), FUIJ(2)
          ENDIF
   10   CONTINUE
   20 CONTINUE

C --- CALCUL DU FACTEUR D'USAGE

      IF ( LFATIG ) THEN
        IF (SEISME) THEN
          CALL RC32FS ( NBSIG2, ZI(JNOC), ZI(JIST),ZR(JMFUS),
     &                  ZR(JMFUB), FUSE(1), NS, NSCY, UG )
          UTOT = UTOT + UG
        END IF
        IF ( NPASS .EQ. 0 ) THEN
           CALL RC32FU ( NBSIG2, ZI(JNOC), ZI(JIST),
     &                   ZR(JMFU), UG, FACTUS )
        ELSE
           CALL RC32FP ( NBSIG2, ZI(JNOC), ZI(JIST), ZI(JNSG),
     &                   ZR(JMFU), UG, FACTUS )
        ENDIF
        UTOT = UTOT + UG
      END IF
C
      IF (SEISME) THEN
        CALL JEDETR('&&RC3201.MATRICE_FU_B')
        CALL JEDETR('&&RC3201.MATRICE_FU_S')
      END IF
      CALL JEDETR('&&RC3201.MATRICE_FU')
      CALL JEDETR('&&RC3201.MATRICE_SN')
      CALL JEDETR('&&RC3201.NB_OCCURR')
      CALL JEDETR('&&RC3201.IMPR_SITU')


 1012 FORMAT (1P,' SITUATION ',I4,' PM =',E12.5,
     &                            ' PB =',E12.5,' PMPB =',E12.5)
 1014 FORMAT (1P,' SITUATION ',I4,' SN =',E12.5 )
 1015 FORMAT (1P,' SITUATION ',I4,' SN =',E12.5 ,
     &    ' SN AVEC SEISME =',E12.5 )
 1016 FORMAT (1P,' SITUATION ',I4,' SN* =',E12.5 )
 1017 FORMAT (1P,' SITUATION ',I4,' SN* =',E12.5,'
     &       SN* AVEC SEISME =',E12.5 )
 1018 FORMAT (1P,' SITUATION ',I4,' SP =',E12.5)
 1019 FORMAT (1P,' SITUATION ',I4,' AVEC SEISME : SP =',E12.5)
 1050 FORMAT (1P,' SITUATION ',I4,' SPMECA=',E12.5,' SPTHER=',E12.5,
     &                                ' KEMECA=',E12.5,' KETHER=',E12.5)
 1051 FORMAT (1P,' SITUATION ',I4,' AVEC SEISME : SPMECA =',E12.5,
     &                                ' KEMECA=',E12.5)
 1060 FORMAT (1P,' SITUATION ',I4,' SALT =',E12.5,' FACT_USAGE =',E12.5)
 1061 FORMAT (1P,' SITUATION ',I4,' AVEC SEISME : SALT =',E12.5,
     &                                   ' FACT_USAGE =',E12.5)
C
 1110 FORMAT (1P,' COMBINAISON DES SITUATIONS ',I4,3X,I4,'  SN =',E12.5)
 1111 FORMAT (1P,41X,'AVEC SEISME : SN =',E12.5)
 1121 FORMAT (1P,41X,'SP1 =',E12.5,2X,'SP2 =',E12.5)
 1122 FORMAT (1P,41X,'AVEC SEISME : SP1 =',E12.5,2X,'SP2 =',E12.5)
 1131 FORMAT (1P,41X,'SPMECA1=',E12.5,' SPMECA2=',E12.5,
     &                                ' KEMECA=',E12.5)
 1132 FORMAT (1P,41X,'SPTHER1=',E12.5,' SPTHER2=',E12.5,
     &                                ' KETHER=',E12.5)
 1133 FORMAT (1P,41X,'AVEC SEISME : SPMECA1=',E12.5,' SPMECA2=',E12.5,
     &                                ' KEMECA=',E12.5)
 1231 FORMAT (1P,41X,'SALT1 =',E12.5,2X,'SALT2 =',E12.5)
 1232 FORMAT (1P,41X,'AVEC SEISME : SALT1 =',E12.5,2X,'SALT2 =',E12.5)
 1331 FORMAT (1P,41X,'FU1 =',E12.5,2X,'FU2 =',E12.5)
      CALL JEDEMA()
      END
