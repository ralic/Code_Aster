      SUBROUTINE RC3603 ( IG1, IG2, NPASS, IMA, IPT, NBM, ADRM, C, K, 
     +                    CARA, NOMMAT, SNMAX, SAMAX, UTOT, SM )
      IMPLICIT   NONE
      INTEGER             IG1, IG2, NPASS, IMA, IPT, NBM, ADRM(*)
      REAL*8              C(*), K(*), CARA(*), SNMAX, SAMAX, UTOT, SM
      CHARACTER*8         NOMMAT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 11/09/2002   AUTEUR VABHHTS J.PELLET 
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
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
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
     +             JCOMBI, JPRESA, JPRESB, JMOMEA, JMOMEB, JNBOCC,
     +             NBTH1, JTH1, NBTH2, JTH2, JCHMAT, JMSA, NDIM, IND,
     +             JNOC1, JNOC2, IFM, NIV
      REAL*8       PPI, PPJ, PQI, PQJ, SALTIJ, UG, SN, SP, SMM, 
     +             MPI(3), MPJ(3), MQI(3), MQJ(3), MSE(3),
     +             MATPI(13), MATPJ(13),MATQI(13), MATQJ(13)
      CHARACTER*8  K8B
      CHARACTER*24 MOMEPI, MOMEPJ, MOMEQI, MOMEQJ,
     +             MATEPI, MATEPJ, MATEQI, MATEQJ
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL INFNIV ( IFM, NIV )
C
      CALL JEVEUO ( '&&RC3600.SITU_COMBINABLE', 'L', JCOMBI )
      CALL JEVEUO ( '&&RC3600.SITU_PRES_A'    , 'L', JPRESA )
      CALL JEVEUO ( '&&RC3600.SITU_PRES_B'    , 'L', JPRESB )
      CALL JEVEUO ( '&&RC3600.SITU_MOMENT_A'  , 'L', JMOMEA )
      CALL JEVEUO ( '&&RC3600.SITU_MOMENT_B'  , 'L', JMOMEB )
      CALL JEVEUO ( '&&RC3600.SITU_NB_OCCUR'  , 'L', JNBOCC )
C
      CALL JEVEUO ( '&&RC3600.MATERIAU'       , 'L', JCHMAT )
C
C
      CALL JELIRA(JEXNUM('&&RC3600.LES_GROUPES',IG1),'LONMAX',
     +                                                       NBSIG1,K8B)
      CALL JEVEUO(JEXNUM('&&RC3600.LES_GROUPES',IG1),'L',JNSG1)
      CALL JELIRA(JEXNUM('&&RC3600.LES_GROUPES',IG2),'LONMAX',
     +                                                       NBSIG2,K8B)
      CALL JEVEUO(JEXNUM('&&RC3600.LES_GROUPES',IG2),'L',JNSG2)
      IF ( NIV .GE. 2 ) THEN
        WRITE(IFM,*)'=> SITUATION DE PASSAGE :'
        WRITE(IFM,*)'   GROUPE ', IG1,' NB SITUATIONS ', NBSIG1
        WRITE(IFM,*)'   GROUPE ', IG2,' NB SITUATIONS ', NBSIG2
      ENDIF
C
      CALL WKVECT ('&&RC3603.NB_OCCURR_1' ,'V V I',2*NBSIG1, JNOC1 )
      CALL WKVECT ('&&RC3603.NB_OCCURR_2' ,'V V I',2*NBSIG2, JNOC2 )
      NDIM = 4*NBSIG1*NBSIG2
      CALL WKVECT ('&&RC3603.MATRICE_SALT','V V R',NDIM, JMSA )
C 
      MSE(1) = 0.D0
      MSE(2) = 0.D0
      MSE(3) = 0.D0
C
C --- SITUATION P :
C     -------------
C
      DO 110 IS1 = 1 , NBSIG1
        IOC1 = ZI(JNSG1+IS1-1)
        IF ( .NOT. ZL(JCOMBI+IOC1-1) )  GOTO 110
C
        ZI(JNOC1-1+2*(IS1-1)+1) = ZI(JNBOCC+2*IOC1-2)
        ZI(JNOC1-1+2*(IS1-1)+2) = ZI(JNBOCC+2*IOC1-2)
C
        PPI = ZR(JPRESA+IOC1-1)
        MOMEPI = ZK24(JMOMEA+IOC1-1)
        CALL RCMO01 ( MOMEPI, IMA, IPT, MPI )
        MATEPI = ZK24(JCHMAT+2*IOC1-1)
        CALL RCMA01 ( MATEPI, IMA, IPT, NBM, ADRM, MATPI )
C
        PPJ = ZR(JPRESB+IOC1-1)
        MOMEPJ = ZK24(JMOMEB+IOC1-1)
        CALL RCMO01 ( MOMEPJ, IMA, IPT, MPJ )
        MATEPJ = ZK24(JCHMAT+2*IOC1-2)
        CALL RCMA01 ( MATEPJ, IMA, IPT, NBM, ADRM, MATPJ )
C
        CALL JELIRA ( JEXNUM('&&RC3600.SITU_THERMIQUE',IOC1),
     +                                          'LONUTI', NBTH1, K8B )
        IF ( NBTH1 .NE. 0 ) THEN
          CALL JEVEUO(JEXNUM('&&RC3600.SITU_THERMIQUE',IOC1),'L',JTH1)
        ELSE
          JTH1 = 1
        ENDIF
C
C ----- SITUATION Q :
C       -------------
C
        DO 120 IS2 = 1 , NBSIG2
          IOC2 = ZI(JNSG1+IS2-1)
          IF ( .NOT. ZL(JCOMBI+IOC2-1) )  GOTO 120
C
          ZI(JNOC2-1+2*(IS2-1)+1) = ZI(JNBOCC+2*IOC2-2)
          ZI(JNOC2-1+2*(IS2-1)+2) = ZI(JNBOCC+2*IOC2-2)
C
          PQI = ZR(JPRESA+IOC2-1)
          MOMEQI = ZK24(JMOMEA+IOC2-1)
          CALL RCMO01 ( MOMEQI, IMA, IPT, MQI )
          MATEQI = ZK24(JCHMAT+2*IOC2-1)
          CALL RCMA01 ( MATEQI, IMA, IPT, NBM, ADRM, MATQI )
C
          PQJ = ZR(JPRESB+IOC2-1)
          MOMEQJ = ZK24(JMOMEB+IOC2-1)
          CALL RCMO01 ( MOMEQJ, IMA, IPT, MQJ )
          MATEQJ = ZK24(JCHMAT+2*IOC2-2)
          CALL RCMA01 ( MATEQJ, IMA, IPT, NBM, ADRM, MATQJ )
C
          CALL JELIRA ( JEXNUM('&&RC3600.SITU_THERMIQUE',IOC2),
     +                                 'LONUTI', NBTH2, K8B )
          IF ( NBTH2 .NE. 0 ) THEN
            CALL JEVEUO ( JEXNUM('&&RC3600.SITU_THERMIQUE',IOC2),
     +                                          'L', JTH2 )
          ELSE
            JTH2 = 1
          ENDIF
C
C --------- CALCUL DU SN(P,Q), ON A 4 COMBINAISONS
C
          SN = 0.D0
C
          CALL RC36SN ( NBM, ADRM, IPT, C, CARA, MATPI, PPI, MPI,
     +                  MATQI, PQI, MQI, MSE, NBTH1, ZI(JTH1),  
     +                  NBTH2, ZI(JTH2),IOC1,IOC2, SN )
C
          CALL RC36SN ( NBM, ADRM, IPT, C, CARA, MATPI, PPI, MPI,
     +                  MATQJ, PQJ, MQJ, MSE, NBTH1, ZI(JTH1),  
     +                        NBTH2, ZI(JTH2),IOC1,IOC2,  SN )
C
          CALL RC36SN ( NBM, ADRM, IPT, C, CARA, MATPJ, PPJ, MPJ,
     +                  MATQJ, PQJ, MQJ, MSE, NBTH1, ZI(JTH1),  
     +                           NBTH2, ZI(JTH2),IOC1,IOC2,  SN )
C
          CALL RC36SN ( NBM, ADRM, IPT, C, CARA, MATPJ, PPJ, MPJ,
     +                  MATQI, PQI, MQI, MSE, NBTH1, ZI(JTH1),  
     +                      NBTH2, ZI(JTH2),IOC1,IOC2,  SN )
C
          IF ( NIV .GE. 2 ) WRITE(IFM,1020) IS1, IS2, SN
C
          SNMAX = MAX ( SNMAX , SN )
C
C ------- 1/ CALCUL DU SALT(I,I) A PARTIR DU SN(P,Q) ET SP(I,I)
C
          SP = 0.D0
C
          CALL RC36SP ( NBM, ADRM, IPT, C, K, CARA, 
     +                  MATPI, PPI, MPI, MATQI, PQI, MQI, MSE, 
     +      NBTH1, ZI(JTH1), NBTH2, ZI(JTH2),IOC1,IOC2,  SP )
C
          IF ( NIV .GE. 2 )  WRITE(IFM,1031) SP
C
          CALL RC36SA ( NOMMAT, MATPI, MATQI, SN, SP, SALTIJ, SMM )
C
          IND = 4*NBSIG2*(IS1-1)+4*(IS2-1)
          ZR(JMSA-1+IND+1) = SALTIJ
          IF ( SALTIJ .GT. SAMAX ) THEN
            SAMAX = SALTIJ
            SM = SMM
          ENDIF
C
C ------- 2/ CALCUL DU SALT(I,J) A PARTIR DU SN(P,Q) ET SP(I,J)
C
          SP = 0.D0
C
          CALL RC36SP ( NBM, ADRM, IPT, C, K, CARA, 
     +                  MATPI, PPI, MPI, MATQJ, PQJ, MQJ, MSE, 
     +          NBTH1, ZI(JTH1), NBTH2, ZI(JTH2),IOC1,IOC2,  SP )
C
          IF ( NIV .GE. 2 )  WRITE(IFM,1032) SP
C
          CALL RC36SA ( NOMMAT, MATPI, MATQJ, SN, SP, SALTIJ, SMM )
C
          IND = 4*NBSIG2*(IS1-1)+4*(IS2-1)
          ZR(JMSA-1+IND+2) = SALTIJ
          IF ( SALTIJ .GT. SAMAX ) THEN
            SAMAX = SALTIJ
            SM = SMM
          ENDIF
C
C ------- 3/ CALCUL DU SALT(J,I) A PARTIR DU SN(P,Q) ET SP(J,I)
C
          SP = 0.D0
C
          CALL RC36SP ( NBM, ADRM, IPT, C, K, CARA, 
     +                  MATPJ, PPJ, MPJ, MATQI, PQI, MQI, MSE, 
     +        NBTH1, ZI(JTH1), NBTH2, ZI(JTH2),IOC1,IOC2,  SP )
C
          IF ( NIV .GE. 2 )  WRITE(IFM,1034)  SP
C
          CALL RC36SA ( NOMMAT, MATPJ, MATQI, SN, SP, SALTIJ, SMM )
C
          IND = 4*NBSIG2*(IS1-1)+4*(IS2-1)
          ZR(JMSA-1+IND+3) = SALTIJ
          IF ( SALTIJ .GT. SAMAX ) THEN
            SAMAX = SALTIJ
            SM = SMM
          ENDIF
C
C ------- 4/ CALCUL DU SALT(J,J) A PARTIR DU SN(P,Q) ET SP(J,J)
C
          SP = 0.D0
C
          CALL RC36SP ( NBM, ADRM, IPT, C, K, CARA, 
     +                  MATPJ, PPJ, MPJ, MATQJ, PQJ, MQJ, MSE, 
     +         NBTH1, ZI(JTH1), NBTH2, ZI(JTH2),IOC1,IOC2,  SP )
C
          IF ( NIV .GE. 2 )  WRITE(IFM,1033)  SP
C
          CALL RC36SA ( NOMMAT, MATPJ, MATQJ, SN, SP, SALTIJ, SMM )
C
          IND = 4*NBSIG2*(IS1-1)+4*(IS2-1)
          ZR(JMSA-1+IND+4) = SALTIJ
          IF ( SALTIJ .GT. SAMAX ) THEN
            SAMAX = SALTIJ
            SM = SMM
          ENDIF
C
 120    CONTINUE
C
 110  CONTINUE
C
C --- CALCUL DU FACTEUR D'USAGE
C
      CALL RC36FU ( NBSIG1, ZI(JNOC1), NBSIG2, ZI(JNOC2),
     +                          ZR(JMSA), NPASS, NOMMAT, UG )
C
      UTOT = UTOT + UG
C
      CALL JEDETR ( '&&RC3603.MATRICE_SALT' )
      CALL JEDETR ( '&&RC3603.NB_OCCURR_1'  )
      CALL JEDETR ( '&&RC3603.NB_OCCURR_2'  )
C
 1020 FORMAT(1P,' COMBINAISON P Q ',I4,I4,' SN =',E12.5)
 1031 FORMAT(1P,'                 I I ',' SP =',E12.5)
 1032 FORMAT(1P,'                 I J ',' SP =',E12.5)
 1033 FORMAT(1P,'                 J J ',' SP =',E12.5)
 1034 FORMAT(1P,'                 J I ',' SP =',E12.5)
C
      CALL JEDEMA( )
      END
