      SUBROUTINE RC3203 ( LIEU, IG1, IG2, NPASS, MATER,
     +        SNMAX, SPMAX,  SPMECM,SPTHEM, SAMAX, UTOT, SM )
      IMPLICIT   NONE
      INTEGER             IG1, IG2, NPASS
      REAL*8              SNMAX, SPMAX, SAMAX, UTOT, SM, SPMECM,SPTHEM
      CHARACTER*4         LIEU
      CHARACTER*8         MATER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 21/03/2005   AUTEUR CIBHHLV L.VIVAN 
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
C
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
C
C     CALCUL DES AMPLITUDES DE CONTRAINTES
C     CALCUL DU FACTEUR D'USAGE 
C     TRAITEMENT D'UNE SITUATION DE PASSAGE DU GROUPE IG1 AU GROUPE IG2
C
C     Soit 2 états stabilisés I et J appartenant respectivement aux 
C     situations P du groupe IG1 et Q du groupe IG2 :
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
      INTEGER      NBSIG1, NBSIG2, JNSG1, JNSG2, IS1, IOC1, IS2, IOC2, 
     +             JCOMBI, JPRESA, JPRESB, JNBOCC, JMSA, NDIM, INDI,
     +             JNOC1, JNOC2, IFM, NIV, JNSITU, NSITUP, NSITUQ, 
     +             JIST1, JIST2, I1, I2
      REAL*8       PPI, PPJ, PQI, PQJ, SALTIJ, UG, SN, SP, SMM, 
     +             MPI(6), MPJ(6), MQI(6), MQJ(6), MSE(6),
     +             MATPI(8), MATPJ(8),MATQI(8), MATQJ(8)
      REAL*8       TYPEKE,SPMECA,SPTHER
      REAL*8 KEMECA,KETHER,KEMECS,KETHES
      LOGICAL      SEISME
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
      CALL JELIRA(JEXNUM('&&RC3200.LES_GROUPES',IG1),'LONMAX',
     +                                                       NBSIG1,K8B)
      CALL JEVEUO(JEXNUM('&&RC3200.LES_GROUPES',IG1),'L',JNSG1)
      CALL JELIRA(JEXNUM('&&RC3200.LES_GROUPES',IG2),'LONMAX',
     +                                                       NBSIG2,K8B)
      CALL JEVEUO(JEXNUM('&&RC3200.LES_GROUPES',IG2),'L',JNSG2)
      IF ( NIV .GE. 2 ) THEN
        WRITE(IFM,*)'=> SITUATION DE PASSAGE :'
        WRITE(IFM,*)'   GROUPE ', IG1,' NB SITUATIONS ', NBSIG1
        WRITE(IFM,*)'   GROUPE ', IG2,' NB SITUATIONS ', NBSIG2
      ENDIF
C
      CALL WKVECT ('&&RC3203.NB_OCCURR_1' ,'V V I',2*NBSIG1, JNOC1 )
      CALL WKVECT ('&&RC3203.NB_OCCURR_2' ,'V V I',2*NBSIG2, JNOC2 )
      CALL WKVECT ('&&RC3203.IMPR_SITU_1' ,'V V I',2*NBSIG1, JIST1 )
      CALL WKVECT ('&&RC3203.IMPR_SITU_2' ,'V V I',2*NBSIG2, JIST2 )
      NDIM = 4*NBSIG1*NBSIG2
      CALL WKVECT ('&&RC3203.MATRICE_SALT','V V R',NDIM, JMSA )
C 
      SEISME = .FALSE.
      MSE(1) = 0.D0
      MSE(2) = 0.D0
      MSE(3) = 0.D0
      MSE(4) = 0.D0
      MSE(5) = 0.D0
      MSE(6) = 0.D0
C
C --- SITUATION P :
C     -------------
C
      I1 = 0
      DO 110 IS1 = 1 , NBSIG1
        IOC1 = ZI(JNSG1+IS1-1)
        IF ( .NOT. ZL(JCOMBI+IOC1-1) )  GOTO 110
        I1 = I1 + 1
C
        NSITUP = ZI(JNSITU+IOC1-1)
C
        ZI(JNOC1-1+2*(I1-1)+1) = ZI(JNBOCC+2*IOC1-2)
        ZI(JNOC1-1+2*(I1-1)+2) = ZI(JNBOCC+2*IOC1-2)
        ZI(JIST1-1+2*(I1-1)+1) = IOC1
        ZI(JIST1-1+2*(I1-1)+2) = IOC1
C
        PPI = ZR(JPRESA+IOC1-1)
        CALL RCMO02 ( 'A', NSITUP, MPI )
        CALL RCMA02 ( 'A', IOC1, MATPI )
C
        PPJ = ZR(JPRESB+IOC1-1)
        CALL RCMO02 ( 'B', NSITUP, MPJ )
        CALL RCMA02 ( 'B', IOC1, MATPJ )
        
        TYPEKE=MATPI(8)
C
C ----- SITUATION Q :
C       -------------
C
        I2 = 0
        DO 120 IS2 = 1 , NBSIG2
          IOC2 = ZI(JNSG1+IS2-1)
          IF ( .NOT. ZL(JCOMBI+IOC2-1) )  GOTO 120
          I2 = I2 + 1
C
          NSITUQ = ZI(JNSITU+IOC2-1)
C
          ZI(JNOC2-1+2*(I2-1)+1) = ZI(JNBOCC+2*IOC2-2)
          ZI(JNOC2-1+2*(I2-1)+2) = ZI(JNBOCC+2*IOC2-2)
          ZI(JIST2-1+2*(I2-1)+1) = IOC2
          ZI(JIST2-1+2*(I2-1)+2) = IOC2
C
          PQI = ZR(JPRESA+IOC2-1)
          CALL RCMO02 ( 'A', NSITUQ, MQI )
          CALL RCMA02 ( 'A', IOC2, MATQI )
C
          PQJ = ZR(JPRESB+IOC2-1)
          CALL RCMO02 ( 'B', NSITUQ, MQJ )
          CALL RCMA02 ( 'B', IOC2, MATQJ )
C
C --------- CALCUL DU SN(P,Q), ON A 4 COMBINAISONS
C
          SN = 0.D0
          SPMECA = 0.D0
          SPTHER = 0.D0
C
          CALL RC32SN ( 'SN_COMB', LIEU, NSITUP, PPI, MPI, NSITUQ, PQI,
     +                  MQI, SEISME, MSE, SN )
C
          CALL RC32SN ( 'SN_COMB', LIEU, NSITUP, PPI, MPI, NSITUQ, PQJ,
     +                  MQJ, SEISME, MSE, SN )
C
          CALL RC32SN ( 'SN_COMB', LIEU, NSITUP, PPJ, MPJ, NSITUQ, PQJ,
     +                  MQJ, SEISME, MSE, SN )
C
          CALL RC32SN ( 'SN_COMB', LIEU, NSITUP, PPJ, MPJ, NSITUQ, PQI,
     +                  MQI, SEISME, MSE, SN )
C
          SNMAX = MAX ( SNMAX , SN )
          IF ( NIV .GE. 2 ) WRITE(IFM,1020) IOC1, IOC2, SN
          INDI = 4*NBSIG2*(I1-1)+4*(I2-1)
C
C ------- 1/ CALCUL DU SALT(I,I) A PARTIR DU SN(P,Q) ET SP(I,I)
C
          SP = 0.D0
C
          CALL RC32SP ( 'SP_COMB',LIEU, NSITUP,PPI,MPI, NSITUQ,PQI,MQI,
     +                  SEISME, MSE, SP,TYPEKE, SPMECA, SPTHER )
          CALL RC32SA ( MATER, MATPI, MATQI, SN, SP,
     +               TYPEKE, SPMECA, SPTHER,KEMECA,KETHER,SALTIJ,SMM )
          ZR(JMSA-1+INDI+1) = SALTIJ
C
          SPMAX = MAX ( SPMAX , SP )
          IF ( SALTIJ .GT. SAMAX ) THEN
            SAMAX = SALTIJ
            SM = SMM
          ENDIF
          IF ( NIV .GE. 2 )  WRITE(IFM,1031) SP
          IF (TYPEKE.GT.0.D0) THEN
             SPMECM = MAX ( SPMECM , SPMECA )
             SPTHEM = MAX ( SPTHEM , SPTHER )
             IF ( NIV .GE. 2 )  THEN
               WRITE(IFM,1131) SPMECA,SPTHER,KEMECA,KETHER
             ENDIF
          ENDIF
C
C ------- 2/ CALCUL DU SALT(I,J) A PARTIR DU SN(P,Q) ET SP(I,J)
C
          SP = 0.D0
C
          CALL RC32SP ( 'SP_COMB',LIEU, NSITUP,PPI,MPI, NSITUQ,PQJ,MQJ,
     +                  SEISME,MSE, SP ,TYPEKE, SPMECA, SPTHER)
          CALL RC32SA ( MATER, MATPI, MATQJ, SN, SP,
     +                  TYPEKE, SPMECA, SPTHER,KEMECA,KETHER,SALTIJ,SMM)
          ZR(JMSA-1+INDI+3) = SALTIJ
C
          SPMAX = MAX ( SPMAX , SP )
          IF ( SALTIJ .GT. SAMAX ) THEN
            SAMAX = SALTIJ
            SM = SMM
          ENDIF
          IF ( NIV .GE. 2 )  WRITE(IFM,1032) SP
          IF (TYPEKE.GT.0.D0) THEN
             SPMECM = MAX ( SPMECM , SPMECA )
             SPTHEM = MAX ( SPTHEM , SPTHER )
             IF ( NIV .GE. 2 )  THEN
               WRITE(IFM,1132) SPMECA,SPTHER,KEMECA,KETHER
             ENDIF
          ENDIF
C
C ------- 3/ CALCUL DU SALT(J,I) A PARTIR DU SN(P,Q) ET SP(J,I)
C
          SP = 0.D0
C
          CALL RC32SP ( 'SP_COMB',LIEU, NSITUP,PPJ,MPJ, NSITUQ,PQI,MQI,
     +                  SEISME,MSE, SP,TYPEKE, SPMECA, SPTHER )
          CALL RC32SA ( MATER, MATPJ, MATQI, SN, SP,
     +                  TYPEKE, SPMECA, SPTHER,KEMECA,KETHER,SALTIJ,SMM)
          ZR(JMSA-1+INDI+2) = SALTIJ
C
          SPMAX = MAX ( SPMAX , SP )
          IF ( SALTIJ .GT. SAMAX ) THEN
            SAMAX = SALTIJ
            SM = SMM
          ENDIF
          IF ( NIV .GE. 2 )  WRITE(IFM,1034)  SP
          IF (TYPEKE.GT.0.D0) THEN
             SPMECM = MAX ( SPMECM , SPMECA )
             SPTHEM = MAX ( SPTHEM , SPTHER )
             IF ( NIV .GE. 2 )  THEN
               WRITE(IFM,1134) SPMECA,SPTHER,KEMECA,KETHER
             ENDIF
          ENDIF
C
C ------- 4/ CALCUL DU SALT(J,J) A PARTIR DU SN(P,Q) ET SP(J,J)
C
          SP = 0.D0
C
          CALL RC32SP ( 'SP_COMB',LIEU, NSITUP,PPJ,MPJ, NSITUQ,PQJ,MQJ,
     +                  SEISME,MSE, SP,TYPEKE, SPMECA, SPTHER )
          CALL RC32SA ( MATER, MATPJ, MATQJ, SN, SP,
     +                 TYPEKE, SPMECA, SPTHER,KEMECA,KETHER,SALTIJ,SMM )
          ZR(JMSA-1+INDI+4) = SALTIJ
C
          IF ( SALTIJ .GT. SAMAX ) THEN
            SAMAX = SALTIJ
            SM = SMM
          ENDIF
          IF ( NIV .GE. 2 )  WRITE(IFM,1033)  SP
          IF (TYPEKE.GT.0.D0) THEN
             SPMECM = MAX ( SPMECM , SPMECA )
             SPTHEM = MAX ( SPTHEM , SPTHER )
             IF ( NIV .GE. 2 )  THEN
               WRITE(IFM,1133) SPMECA,SPTHER,KEMECA,KETHER
             ENDIF
          ENDIF
C
 120    CONTINUE
C
 110  CONTINUE
C
C --- CALCUL DU FACTEUR D'USAGE
C
      CALL RC36FU ( NBSIG1, ZI(JNOC1), ZI(JIST1), NBSIG2, ZI(JNOC2),
     +                         ZI(JIST2), ZR(JMSA), NPASS, MATER, UG )
C
      UTOT = UTOT + UG
C
      CALL JEDETR ( '&&RC3203.MATRICE_SALT' )
      CALL JEDETR ( '&&RC3203.NB_OCCURR_1'  )
      CALL JEDETR ( '&&RC3203.NB_OCCURR_2'  )
      CALL JEDETR ( '&&RC3203.IMPR_SITU_1'  )
      CALL JEDETR ( '&&RC3203.IMPR_SITU_2'  )
C
 1020 FORMAT(1P,' COMBINAISON DES SITUATIONS ',I4,3X,I4,'  SN =',E12.5)
 1031 FORMAT(1P,26X,'ETAT_A ETAT_A ',' SP =',E12.5)
 1032 FORMAT(1P,26X,'ETAT_B ETAT_A ',' SP =',E12.5)
 1033 FORMAT(1P,26X,'ETAT_B ETAT_B ',' SP =',E12.5)
 1034 FORMAT(1P,26X,'ETAT_A ETAT_B ',' SP =',E12.5)
C
 1131 FORMAT(1P,26X,'ETAT_A ETAT_A ',' SPMECA=',E12.5,' SPTHER=',E12.5,
     &                               ' KEMECA=',E12.5,' KETHER=',E12.5)
 1132 FORMAT(1P,26X,'ETAT_B ETAT_A ',' SPMECA=',E12.5,' SPTHER=',E12.5,
     &                               ' KEMECA=',E12.5,' KETHER=',E12.5)
 1133 FORMAT(1P,26X,'ETAT_B ETAT_B ',' SPMECA=',E12.5,' SPTHER=',E12.5,
     &                               ' KEMECA=',E12.5,' KETHER=',E12.5)
 1134 FORMAT(1P,26X,'ETAT_A ETAT_B ',' SPMECA=',E12.5,' SPTHER=',E12.5,
     &                               ' KEMECA=',E12.5,' KETHER=',E12.5)
C
      CALL JEDEMA( )
      END
