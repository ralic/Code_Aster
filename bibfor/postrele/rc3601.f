      SUBROUTINE RC3601 ( IG, IOCS, SEISME, NPASS, IMA, IPT, NBM, ADRM,
     +                    C, K, CARA, NOMMAT, SNMAX, SAMAX, UTOT, SM )
      IMPLICIT   NONE
      INTEGER             IG, IOCS, NPASS, IMA, IPT, NBM, ADRM(*)
      REAL*8              C(*), K(*), CARA(*), SNMAX, SAMAX, UTOT, SM
      LOGICAL             SEISME
      CHARACTER*8         NOMMAT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 22/10/2002   AUTEUR JMBHH01 J.M.PROIX 
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
      INTEGER      NBSIGR, JNSG, IS1, IOC1, IS2, IOC2, IND, IFM, NIV,
     +             JCOMBI, JPRESA, JPRESB, JMOMEA, JMOMEB, JNBOCC,
     +             NBTH1, JTH1, NBTH2, JTH2, JCHMAT, JMSA, NDIM, JNOC,
     +             NSCY, NS, JMSN, NBSIG2, I1, I2
      REAL*8       PPI, PPJ, PQI, PQJ, SALTIJ, UG, SN, SP, SMM,
     +             MPI(3), MPJ(3), MQI(3), MQJ(3), MSE(3),
     +             MATPI(13), MATPJ(13),MATQI(13), MATQJ(13), MATSE(13)
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
      CALL JELIRA(JEXNUM('&&RC3600.LES_GROUPES',IG),'LONMAX',NBSIGR,K8B)
      CALL JEVEUO(JEXNUM('&&RC3600.LES_GROUPES',IG),'L',JNSG)
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
      CALL WKVECT ('&&RC3601.NB_OCCURR'   ,'V V I',NDIM, JNOC )
      NDIM = NBSIG2*NBSIG2
      CALL WKVECT ('&&RC3601.MATRICE_SN'  ,'V V R',NDIM, JMSN )
      NDIM = 4*NBSIG2*NBSIG2
      CALL WKVECT ('&&RC3601.MATRICE_SALT','V V R',NDIM, JMSA )
C
      NS = 0
      IF ( SEISME ) THEN
         MOMEPI = ZK24(JMOMEA+IOCS-1)
         CALL RCMO01 ( MOMEPI, IMA, IPT, MSE )
         MSE(1) = 2*MSE(1)
         MSE(2) = 2*MSE(2)
         MSE(3) = 2*MSE(3)
         MATEPI = ZK24(JCHMAT+2*IOCS-1)
         CALL RCMA01 ( MATEPI, IMA, IPT, NBM, ADRM, MATSE )
         NS   = ZI(JNBOCC+2*IOCS-2)
         NSCY = ZI(JNBOCC+2*IOCS-1)
      ELSE
         MSE(1) = 0.D0
         MSE(2) = 0.D0
         MSE(3) = 0.D0
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
        I1 = I1 + 1
        ZI(JNOC-1+2*(I1-1)+1) = ZI(JNBOCC+2*IOC1-2)
        ZI(JNOC-1+2*(I1-1)+2) = ZI(JNBOCC+2*IOC1-2)
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
        NBTH2 = 0
        JTH2  = 1
        IOC2=0
C
        SN = 0.D0
        CALL RC36SN ( NBM, ADRM, IPT, C, CARA, MATPI, PPI, 
     +                     MPI, MATPJ, PPJ, MPJ, MSE, NBTH1, ZI(JTH1),
     +                       NBTH2, ZI(JTH2),IOC1,IOC2, SN )
        ZR(JMSN-1+NBSIG2*(I1-1)+1) = SN
        SNMAX = MAX ( SNMAX , SN )
C
        SP = 0.D0
        CALL RC36SP ( NBM, ADRM, IPT, C, K, CARA, MATPI, PPI,
     +                     MPI, MATPJ, PPJ, MPJ, MSE, NBTH1, ZI(JTH1),
     +                       NBTH2, ZI(JTH2), IOC1,IOC2, SP )
C
        IF ( NIV .GE. 2 )  WRITE(IFM,1010) IS1, SN, SP
C
        CALL RC36SA ( NOMMAT, MATPI, MATPJ, SN, SP, SALTIJ, SMM )
C
        IND = 4*NBSIG2*(I1-1)+4*(I1-1)
        ZR(JMSA-1+IND+3) = SALTIJ
        IF ( SALTIJ .GT. SAMAX ) THEN
           SAMAX = SALTIJ
           SM = SMM
        ENDIF
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
          I2 = I2 + 1
          ZI(JNOC-1+2*(I2-1)+1) = ZI(JNBOCC+2*IOC2-2)
          ZI(JNOC-1+2*(I2-1)+2) = ZI(JNBOCC+2*IOC2-2)
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
C ------- CALCUL DU SN(P,Q), ON A 4 COMBINAISONS
C
          SN = 0.D0
C
          CALL RC36SN ( NBM, ADRM, IPT, C, CARA, MATPI, PPI,
     +                  MPI, MATQI, PQI, MQI, MSE, NBTH1, ZI(JTH1),  
     +                               NBTH2, ZI(JTH2),IOC1,IOC2,SN )
C
          CALL RC36SN ( NBM, ADRM, IPT, C, CARA, MATPI, PPI,
     +                  MPI, MATQJ, PQJ, MQJ, MSE, NBTH1, ZI(JTH1),  
     +                               NBTH2, ZI(JTH2), IOC1,IOC2,SN )
C
          CALL RC36SN ( NBM, ADRM, IPT, C, CARA, MATPJ, PPJ,
     +                  MPJ, MATQJ, PQJ, MQJ, MSE, NBTH1, ZI(JTH1),  
     +                               NBTH2, ZI(JTH2), IOC1,IOC2,SN )
C
          CALL RC36SN ( NBM, ADRM, IPT, C, CARA, MATPJ, PPJ,
     +                  MPJ, MATQI, PQI, MQI, MSE, NBTH1, ZI(JTH1),  
     +                               NBTH2, ZI(JTH2), IOC1,IOC2,SN )
C
          ZR(JMSN-1+NBSIG2*(I1-1)+I2) = SN
          ZR(JMSN-1+NBSIG2*(I2-1)+I1) = SN
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
     +                  NBTH1, ZI(JTH1), NBTH2, ZI(JTH2),IOC1,IOC2,SP )
C
          IF ( NIV .GE. 2 )  WRITE(IFM,1031) SP
C
          CALL RC36SA ( NOMMAT, MATPI, MATQI, SN, SP, SALTIJ, SMM )
C
          IND = 4*NBSIG2*(I1-1)+4*(I2-1)
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
     +                  NBTH1, ZI(JTH1), NBTH2, ZI(JTH2),IOC1,IOC2,SP )
C
          IF ( NIV .GE. 2 )  WRITE(IFM,1032) SP
C
          CALL RC36SA ( NOMMAT, MATPI, MATQJ, SN, SP, SALTIJ, SMM )
C
          IND = 4*NBSIG2*(I1-1)+4*(I2-1)
C          ZR(JMSA-1+IND+2) = SALTIJ
          ZR(JMSA-1+IND+3) = SALTIJ
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
     +                  NBTH1, ZI(JTH1), NBTH2, ZI(JTH2),IOC1,IOC2,SP )
C
          IF ( NIV .GE. 2 )  WRITE(IFM,1034)  SP
C
          CALL RC36SA ( NOMMAT, MATPJ, MATQI, SN, SP, SALTIJ, SMM )
C
          IND = 4*NBSIG2*(I1-1)+4*(I2-1)
C          ZR(JMSA-1+IND+3) = SALTIJ
          ZR(JMSA-1+IND+2) = SALTIJ
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
     +                  NBTH1, ZI(JTH1), NBTH2, ZI(JTH2),IOC1,IOC2,SP )
C
          IF ( NIV .GE. 2 )  WRITE(IFM,1033)  SP
C
          CALL RC36SA ( NOMMAT, MATPJ, MATQJ, SN, SP, SALTIJ, SMM )
C
          IND = 4*NBSIG2*(I1-1)+4*(I2-1)
          ZR(JMSA-1+IND+4) = SALTIJ
          IF ( SALTIJ .GT. SAMAX ) THEN
            SAMAX = SALTIJ
            SM = SMM
          ENDIF
C
 120      CONTINUE
C
 110    CONTINUE
C
C --- CALCUL DU FACTEUR D'USAGE
C
      IF ( SEISME ) THEN
         CALL RC36FS ( NBSIG2, ZI(JNOC), NBSIG2, ZI(JNOC), ZR(JMSA),
     +         NS, NSCY, MATSE, MSE, ZR(JMSN), NOMMAT, C, K, CARA, UG )
      ELSE
         CALL RC36FU ( NBSIG2, ZI(JNOC), NBSIG2, ZI(JNOC),
     +                                   ZR(JMSA), NPASS, NOMMAT, UG )
      ENDIF
C
      UTOT = UTOT + UG
C
      CALL JEDETR ( '&&RC3601.MATRICE_SALT' )
      CALL JEDETR ( '&&RC3601.MATRICE_SN'   )
      CALL JEDETR ( '&&RC3601.NB_OCCURR'    )
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
