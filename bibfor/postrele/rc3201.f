      SUBROUTINE RC3201 ( LIEU, IG, IOCS, SEISME, NPASS, MATER, 
     +                    SNMAX, SPMAX, SAMAX, UTOT, SM )
      IMPLICIT   NONE
      INTEGER             IG, IOCS, NPASS
      REAL*8              SNMAX, SPMAX, SAMAX, UTOT, SM
      LOGICAL             SEISME
      CHARACTER*4         LIEU
      CHARACTER*8         MATER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 16/10/2002   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
C     CALCUL DES AMPLITUDES DE CONTRAINTES
C     CALCUL DU FACTEUR D'USAGE 
C     ON TRAITE LES SITUATIONS COMBINABLES DANS LEUR GROUPE
C
C     Soit 2 états stabilisés I et J appartenant aux situations P et Q
C
C     on calcule le SALT(I,J) = 0,5*(EC/E)*Ke*Sn(P,Q)*Sp(I,J)
C
C     avec Sn(P,Q) = Max( Sn(I,J) )
C          Sn(I,J) = Max( Max(Sn(I,J,ThP)), Max(Sn(I,J,ThQ)) )
C
C     avec Sp(I,J) = Max( Max(Sp(I,J,ThP)), Max(Sp(I,J,ThQ)) )
C
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32     JEXNOM, JEXNUM, JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      NBSIGR, NBSIG2, JNSG, IS1, IOC1, IS2, IOC2, IND, 
     +             JCOMBI, JPRESA, JPRESB, JNBOCC, IFM, NIV, I1, I2,
     +             JMSA, JMSAS, JMSAB, NDIM, JNOC, NSCY, NS, JMSN,
     +             JNSITU, NSITUP, NSITUQ
      REAL*8       PPI, PPJ, PQI, PQJ, SALTIJ, SALIJS, UG, SN, SP, SMM,
     +             SNS, SPS, MPI(6), MPJ(6), MQI(6), MQJ(6), MSE(6),
     +             MATPI(7), MATPJ(7),MATQI(7), MATQJ(7), SALTSE
      CHARACTER*8  K8B
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL INFNIV ( IFM, NIV )
C
      CALL JEVEUO ( '&&RC3200.SITU_NUMERO'    , 'L', JNSITU )
      CALL JEVEUO ( '&&RC3200.SITU_COMBINABLE', 'L', JCOMBI )
      CALL JEVEUO ( '&&RC3200.SITU_PRES_A'    , 'L', JPRESA )
      CALL JEVEUO ( '&&RC3200.SITU_PRES_B'    , 'L', JPRESB )
      CALL JEVEUO ( '&&RC3200.SITU_NB_OCCUR'  , 'L', JNBOCC )
C
      CALL JELIRA(JEXNUM('&&RC3200.LES_GROUPES',IG),'LONMAX',NBSIGR,K8B)
      CALL JEVEUO(JEXNUM('&&RC3200.LES_GROUPES',IG),'L',JNSG)
      IF ( NIV .GE. 2 ) THEN
        WRITE(IFM,*)'=> GROUPE ', IG,' NB SITUATIONS ', NBSIGR
      ENDIF
C
      IF ( IOCS .EQ. 0 )  THEN
         NBSIG2 = NBSIGR
      ELSE
         NBSIG2 = NBSIGR - 1
      ENDIF
      NDIM = 2*NBSIG2
      CALL WKVECT ('&&RC3201.NB_OCCURR'   ,'V V I',NDIM, JNOC )
      NDIM = NBSIG2*NBSIG2
      CALL WKVECT ('&&RC3201.MATRICE_SN'  ,'V V R',NDIM, JMSN )
      NDIM = 4*NBSIG2*NBSIG2
      CALL WKVECT ('&&RC3201.MATRICE_SALT','V V R',NDIM, JMSA )
      IF ( SEISME ) THEN
         CALL WKVECT ('&&RC3201.MATRICE_SALT_B','V V R',NDIM, JMSAB )
         CALL WKVECT ('&&RC3201.MATRICE_SALT_S','V V R',NDIM, JMSAS )
      ENDIF
C
      NS = 0
      NSCY = 0
      IF ( SEISME ) THEN
         NS   = ZI(JNBOCC+2*IOCS-2)
         NSCY = ZI(JNBOCC+2*IOCS-1)
         PPI  = ZR(JPRESA+IOCS-1)
         NSITUP = ZI(JNSITU+IOCS-1)
         CALL RCMO02 ( 'A', NSITUP, MSE )
         CALL RCMA02 ( 'A', IOCS, MATPI )
         SN = 0.D0
         SP = 0.D0
         CALL RC32SN ( LIEU, SEISME, NSITUP,PPI,MSE, NSITUP,PPI,MSE,
     +                 MSE, SN )
         CALL RC32SP ( LIEU, SEISME, NSITUP,PPI,MSE, NSITUP,PPI,MSE,
     +                 MSE, SP )
         CALL RC32SA ( MATER, MATPI, MATPI, SN, SP, SALTSE, SM )
         IF ( NIV .GE. 2 ) THEN
            WRITE(IFM,*)'  SEISME,   SN = ', SN
            WRITE(IFM,*)'            SP = ', SP
            WRITE(IFM,*)'          SALT = ', SALTSE
         ENDIF
      ELSE
         MSE(1) = 0.D0
         MSE(2) = 0.D0
         MSE(3) = 0.D0
         MSE(4) = 0.D0
         MSE(5) = 0.D0
         MSE(6) = 0.D0
      ENDIF
C 
C
C --- SITUATION P :
C     -------------
C
      I1 = 0
      DO 110 IS1 = 1 , NBSIGR
        IOC1 = ZI(JNSG+IS1-1)
        IF ( .NOT. ZL(JCOMBI+IOC1-1) )  GOTO 110
        IF ( IOC1.EQ.IOCS )  GOTO 110
C
        NSITUP = ZI(JNSITU+IOC1-1)
C
        I1 = I1 + 1
        ZI(JNOC-1+2*(I1-1)+1) = ZI(JNBOCC+2*IOC1-2)
        ZI(JNOC-1+2*(I1-1)+2) = ZI(JNBOCC+2*IOC1-2)
C
        PPI = ZR(JPRESA+IOC1-1)
        CALL RCMO02 ( 'A', NSITUP, MPI )
        CALL RCMA02 ( 'A', IOC1, MATPI )
C
        PPJ = ZR(JPRESB+IOC1-1)
        CALL RCMO02 ( 'B', NSITUP, MPJ )
        CALL RCMA02 ( 'B', IOC1, MATPJ )
C
        NSITUQ = 0
C
        SNS = 0.D0
        SN  = 0.D0
        SPS = 0.D0
        SP  = 0.D0
        IND = 4*NBSIG2*(I1-1)+4*(I1-1)
C
        CALL RC32SN ( LIEU, .FALSE., NSITUP,PPI,MPI, NSITUQ,PPJ,MPJ,
     +                MSE, SN )
        CALL RC32SP ( LIEU, .FALSE., NSITUP,PPI,MPI, NSITUQ,PPJ,MPJ,
     +                MSE, SP )
        CALL RC32SA ( MATER, MATPI, MATPJ, SN, SP, SALTIJ, SMM )
        ZR(JMSA-1+IND+3) = SALTIJ
        IF ( SALTIJ .GT. SAMAX ) THEN
           SAMAX = SALTIJ
           SM = SMM
        ENDIF
        IF ( SEISME ) THEN
          ZR(JMSAB-1+IND+3) = SALTIJ
          CALL RC32SN ( LIEU, SEISME, NSITUP,PPI,MPI, NSITUQ,PPJ,MPJ,
     +                  MSE, SNS )
          CALL RC32SP ( LIEU, SEISME, NSITUP,PPI,MPI, NSITUQ,PPJ,MPJ,
     +                  MSE, SPS )
          CALL RC32SA ( MATER, MATPI, MATPJ, SNS, SPS, SALIJS, SMM )
          ZR(JMSAS-1+IND+3) = SALIJS
        ENDIF
C
        SNMAX = MAX ( SNMAX , SNS , SN )
        SPMAX = MAX ( SNMAX , SPS , SP )
        IF ( NIV .GE. 2 )  WRITE(IFM,1010) IS1, SN, SP
C
C ----- SITUATION Q :
C       -------------
C
        I2 = I1
        DO 120 IS2 = IS1+1 , NBSIGR
          IOC2 = ZI(JNSG+IS2-1)
          IF ( .NOT. ZL(JCOMBI+IOC2-1) )  GOTO 120
          IF ( IOC2.EQ.IOCS )  GOTO 120
C
          NSITUQ = ZI(JNSITU+IOC2-1)
C
          I2 = I2 + 1
          ZI(JNOC-1+2*(I2-1)+1) = ZI(JNBOCC+2*IOC2-2)
          ZI(JNOC-1+2*(I2-1)+2) = ZI(JNBOCC+2*IOC2-2)
C
          PQI = ZR(JPRESA+IOC2-1)
          CALL RCMO02 ( 'A', NSITUQ, MQI )
          CALL RCMA02 ( 'A', IOC2, MATQI )
C
          PQJ = ZR(JPRESB+IOC2-1)
          CALL RCMO02 ( 'B', NSITUQ, MQJ )
          CALL RCMA02 ( 'B', IOC2, MATQJ )
C
C ------- CALCUL DU SN(P,Q), ON A 4 COMBINAISONS
C
          SNS = 0.D0
          SN  = 0.D0
C
          CALL RC32SN ( LIEU,.FALSE., NSITUP,PPI,MPI, NSITUQ,PQI,MQI,
     +                  MSE, SN )
          CALL RC32SN ( LIEU,.FALSE., NSITUP,PPI,MPI, NSITUQ,PQJ,MQJ,
     +                  MSE, SN )
          CALL RC32SN ( LIEU,.FALSE., NSITUP,PPJ,MPJ, NSITUQ,PQJ,MQJ,
     +                  MSE, SN )
          CALL RC32SN ( LIEU,.FALSE., NSITUP,PPJ,MPJ, NSITUQ,PQI,MQI,
     +                  MSE, SN )
          IF ( SEISME ) THEN
            CALL RC32SN ( LIEU, SEISME, NSITUP,PPI,MPI, NSITUQ,PQI,MQI,
     +                    MSE, SNS )
            CALL RC32SN ( LIEU, SEISME, NSITUP,PPI,MPI, NSITUQ,PQJ,MQJ,
     +                    MSE, SNS )
            CALL RC32SN ( LIEU, SEISME, NSITUP,PPJ,MPJ, NSITUQ,PQJ,MQJ,
     +                    MSE, SNS )
            CALL RC32SN ( LIEU, SEISME, NSITUP,PPJ,MPJ, NSITUQ,PQI,MQI,
     +                    MSE, SNS )
          ENDIF
C
          SNMAX = MAX ( SNMAX , SNS , SN )
          IF ( NIV .GE. 2 ) WRITE(IFM,1020) IS1, IS2, SN
C
C ------- 1/ CALCUL DU SALT(I,I) A PARTIR DU SN(P,Q) ET SP(I,I)
C
          SPS = 0.D0
          SP  = 0.D0
          IND = 4*NBSIG2*(I1-1)+4*(I2-1)
C
          CALL RC32SP ( LIEU,.FALSE., NSITUP,PPI,MPI, NSITUQ,PQI,MQI,
     +                  MSE, SP )
          CALL RC32SA ( MATER, MATPI, MATQI, SN, SP, SALTIJ, SMM )
          ZR(JMSA-1+IND+1) = SALTIJ
          IF ( SALTIJ .GT. SAMAX ) THEN
            SAMAX = SALTIJ
            SM = SMM
          ENDIF
          IF ( SEISME ) THEN
            ZR(JMSAB-1+IND+1) = SALTIJ
            CALL RC32SP ( LIEU, SEISME, NSITUP,PPI,MPI, NSITUQ,PQI,MQI,
     +                    MSE, SPS )
            CALL RC32SA ( MATER, MATPI, MATQI, SNS, SPS, SALIJS, SMM )
            ZR(JMSAS-1+IND+1) = SALIJS
          ENDIF
C
          SPMAX = MAX ( SPMAX , SPS , SP )
          IF ( NIV .GE. 2 )  WRITE(IFM,1031) SP
C
C ------- 2/ CALCUL DU SALT(I,J) A PARTIR DU SN(P,Q) ET SP(I,J)
C
          SPS = 0.D0
          SP  = 0.D0
          IND = 4*NBSIG2*(I1-1)+4*(I2-1)
C
          CALL RC32SP ( LIEU, .FALSE., NSITUP,PPI,MPI, NSITUQ,PQJ,MQJ,
     +                  MSE, SP )
          CALL RC32SA ( MATER, MATPI, MATQJ, SN, SP, SALTIJ, SMM )
          ZR(JMSA-1+IND+3) = SALTIJ
          IF ( SALTIJ .GT. SAMAX ) THEN
            SAMAX = SALTIJ
            SM = SMM
          ENDIF
          IF ( SEISME ) THEN
            ZR(JMSAB-1+IND+3) = SALTIJ
            CALL RC32SP ( LIEU, SEISME, NSITUP,PPI,MPI, NSITUQ,PQJ,MQJ,
     +                    MSE, SPS )
            CALL RC32SA ( MATER, MATPI, MATQJ, SNS, SPS, SALIJS, SMM )
            ZR(JMSAS-1+IND+3) = SALIJS
          ENDIF
C
          SPMAX = MAX ( SPMAX , SPS , SP )
          IF ( NIV .GE. 2 )  WRITE(IFM,1032) SP
C
C ------- 3/ CALCUL DU SALT(J,I) A PARTIR DU SN(P,Q) ET SP(J,I)
C
          SPS = 0.D0
          SP  = 0.D0
          IND = 4*NBSIG2*(I1-1)+4*(I2-1)
C
          CALL RC32SP ( LIEU, .FALSE., NSITUP,PPJ,MPJ, NSITUQ,PQI,MQI,
     +                  MSE, SP )
          CALL RC32SA ( MATER, MATPJ, MATQI, SN, SP, SALTIJ, SMM )
          ZR(JMSA-1+IND+2) = SALTIJ
          IF ( SALTIJ .GT. SAMAX ) THEN
            SAMAX = SALTIJ
            SM = SMM
          ENDIF
          IF ( SEISME ) THEN
            ZR(JMSAB-1+IND+2) = SALTIJ
            CALL RC32SP ( LIEU, SEISME, NSITUP,PPJ,MPJ, NSITUQ,PQI,MQI,
     +                    MSE, SPS )
            CALL RC32SA ( MATER, MATPJ, MATQI, SNS, SPS, SALIJS, SMM )
            ZR(JMSAS-1+IND+2) = SALIJS
          ENDIF
C
          SPMAX = MAX ( SPMAX , SPS , SP )
          IF ( NIV .GE. 2 )  WRITE(IFM,1034)  SP
C
C ------- 4/ CALCUL DU SALT(J,J) A PARTIR DU SN(P,Q) ET SP(J,J)
C
          SPS = 0.D0
          SP  = 0.D0
          IND = 4*NBSIG2*(I1-1)+4*(I2-1)
C
          CALL RC32SP ( LIEU, .FALSE., NSITUP,PPJ,MPJ, NSITUQ,PQJ,MQJ,
     +                  MSE, SP )
          CALL RC32SA ( MATER, MATPJ, MATQJ, SN, SP, SALTIJ, SMM )
          ZR(JMSA-1+IND+4) = SALTIJ
          IF ( SALTIJ .GT. SAMAX ) THEN
            SAMAX = SALTIJ
            SM = SMM
          ENDIF
          IF ( SEISME ) THEN
            ZR(JMSAB-1+IND+4) = SALTIJ
            CALL RC32SP ( LIEU, SEISME, NSITUP,PPJ,MPJ, NSITUQ,PQJ,MQJ,
     +                    MSE, SPS )
            CALL RC32SA ( MATER, MATPJ, MATQJ, SNS, SPS, SALIJS, SMM )
            ZR(JMSAS-1+IND+4) = SALIJS
          ENDIF
C
          SPMAX = MAX ( SPMAX , SPS , SP )
          IF ( NIV .GE. 2 )  WRITE(IFM,1033)  SP
C
 120      CONTINUE
C
 110    CONTINUE
C
C --- CALCUL DU FACTEUR D'USAGE
C
      IF ( SEISME ) THEN
         CALL RC32FS ( NBSIG2, ZI(JNOC), NBSIG2, ZI(JNOC), ZR(JMSAS),
     +                         ZR(JMSAB), SALTSE, NS, NSCY, MATER, UG )
         UTOT = UTOT + UG
      ENDIF
      CALL RC36FU ( NBSIG2, ZI(JNOC), NBSIG2, ZI(JNOC),
     +                                     ZR(JMSA), NPASS, MATER, UG )
C
      UTOT = UTOT + UG
C
      IF ( SEISME ) THEN
         CALL JEDETR ( '&&RC3201.MATRICE_SALT_B' )
         CALL JEDETR ( '&&RC3201.MATRICE_SALT_S' )
      ENDIF
      CALL JEDETR ( '&&RC3201.MATRICE_SALT' )
      CALL JEDETR ( '&&RC3201.MATRICE_SN'   )
      CALL JEDETR ( '&&RC3201.NB_OCCURR'    )
C
 1010 FORMAT(1P,' COMBINAISON P ',I4,' SN =',E12.5,' SP =',E12.5)
 1020 FORMAT(1P,' COMBINAISON P Q ',I4,I4,' SN =',E12.5)
 1031 FORMAT(1P,'                 I I ',' SP =',E12.5)
 1032 FORMAT(1P,'                 I J ',' SP =',E12.5)
 1033 FORMAT(1P,'                 J J ',' SP =',E12.5)
 1034 FORMAT(1P,'                 J I ',' SP =',E12.5)
C
      CALL JEDEMA( )
      END
