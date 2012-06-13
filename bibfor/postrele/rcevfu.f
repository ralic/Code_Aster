      SUBROUTINE RCEVFU ( CNOC, CFAT, FUT )
      IMPLICIT     NONE
      INCLUDE 'jeveux.h'
      REAL*8       FUT
      CHARACTER*24 CNOC, CFAT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     OPERATEUR POST_RCCM, TYPE_RESU_MECA='EVOLUTION'
C     CALCUL DU FACTEUR D'USAGE TOTAL (FUT)
C
C     ------------------------------------------------------------------
C
      INTEGER      NBINST, JNOCR, JFU, I1, I2, IND, NOC1, NOC2,
     +             I1M, I2M, NOC1M, NOC2M, NBCYCL
      INTEGER      JNOCK, JNOCL, JFUKL, INDI, INDS, K, L, IFM, NIV
      REAL*8       FUM, FUKL,R8PREM
      LOGICAL      ENCORE
      CHARACTER*8  K8B
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL INFNIV ( IFM, NIV )
C
      CALL JELIRA ( CNOC, 'LONMAX', NBINST, K8B )
      CALL JEVEUO ( CNOC, 'L', JNOCR )
      CALL JEVEUO ( CFAT, 'L', JFU )
C
      FUT = 0.D0
C
      CALL WKVECT ( '&&RCEVFU.NB_OCC_K', 'V V I', NBINST, JNOCK )
      CALL WKVECT ( '&&RCEVFU.NB_OCC_L', 'V V I', NBINST, JNOCL )
      DO 10 I1 = 1 , NBINST
         ZI(JNOCK+I1-1) = ZI(JNOCR+I1-1)
         ZI(JNOCL+I1-1) = ZI(JNOCR+I1-1)
 10   CONTINUE
C
      CALL WKVECT ( '&&RCEVFU.MATR_FU', 'V V R', NBINST*NBINST, JFUKL )
      IND = 0
      DO 20 I1 = 1 , NBINST
         INDI = NBINST*(I1-1) + I1
         IND = IND + 1
         ZR(JFUKL-1+INDI) = ZR(JFU-1+5*(IND-1)+4)
         DO 22 I2 = I1+1 , NBINST
            INDS = NBINST*(I1-1) + I2
            INDI = NBINST*(I2-1) + I1
            IND = IND + 1
            ZR(JFUKL-1+INDS) = ZR(JFU-1+5*(IND-1)+4)
            ZR(JFUKL-1+INDI) = ZR(JFU-1+5*(IND-1)+4)
 22      CONTINUE
 20   CONTINUE
C
      IFM = 6
      IND = 0
C
 100  CONTINUE
      IND = IND + 1
C
      IF ( NIV .EQ. 2 ) THEN
        IF ( IND .EQ. 1 ) THEN
          WRITE(IFM,*) 'MATRICE FACTEUR D''USAGE INITIALE'
        ELSE
          WRITE(IFM,*) 'MATRICE FACTEUR D''USAGE MODIFIEE'
        ENDIF
        WRITE(IFM,1010) ( ZI(JNOCL+L-1), L=1,NBINST )
        DO 700 K = 1 , NBINST
          I1 = NBINST*(K-1)
          WRITE(IFM,1000) ZI(JNOCK+K-1), (ZR(JFUKL-1+I1+L), L=1,NBINST)
 700    CONTINUE
      ENDIF
C
      FUM = 0.D0
C
      DO 110 I1 = 1 , NBINST
         NOC1 = ZI(JNOCK-1+I1)
         IF ( NOC1 .EQ. 0 ) GOTO 110
         K = NBINST*(I1-1)
C
         DO 112 I2 = 1 , NBINST
            NOC2 = ZI(JNOCL-1+I2)
            IF ( NOC2 .EQ. 0 ) GOTO 112
            L = I2
C
            FUKL = ZR(JFUKL-1+K+L)
            IF ( FUKL .LT. R8PREM() ) GOTO 112
            IF ( FUKL .GT. FUM ) THEN
               NOC1M = NOC1
               NOC2M = NOC2
               I1M = I1
               I2M = I2
               FUM = FUKL
            ENDIF
C
 112    CONTINUE
C
 110  CONTINUE
      NBCYCL = MIN( NOC1M , NOC2M )
C
      IF ( FUM .LT. R8PREM()  ) GOTO 999
      IF ( NIV .EQ. 2 ) THEN
         WRITE(IFM,1020)'=> FACTEUR D''USAGE MAXI: ',FUM,I1M,I2M
         WRITE(IFM,1030)'   NB_OCCUR = ', NBCYCL
      ENDIF
C
C --- ON CUMULE
C
      FUT = FUT + FUM*DBLE(NBCYCL)
C
C --- ON MET A ZERO LES FACTEURS D'USAGE INCRIMINES
C
      IF ( NOC1M .EQ. NOC2M ) THEN
         ZI(JNOCL-1+I1M) = 0
         ZI(JNOCL-1+I2M) = 0
         ZI(JNOCK-1+I1M) = 0
         ZI(JNOCK-1+I2M) = 0
         DO 120 K = 1 , NBINST
            ZR(JFUKL-1+(K-1)*NBINST+I2M) = 0.D0
            ZR(JFUKL-1+(I2M-1)*NBINST+K) = 0.D0
            ZR(JFUKL-1+(K-1)*NBINST+I1M) = 0.D0
            ZR(JFUKL-1+(I1M-1)*NBINST+K) = 0.D0
 120     CONTINUE
      ELSEIF ( NOC1M .LT. NOC2M ) THEN
         ZI(JNOCL-1+I2M) = ZI(JNOCL-1+I2M) - NOC1M
         ZI(JNOCK-1+I2M) = ZI(JNOCK-1+I2M) - NOC1M
         ZI(JNOCK-1+I1M) = 0
         ZI(JNOCL-1+I1M) = 0
         DO 122 K = 1 , NBINST
            ZR(JFUKL-1+(I1M-1)*NBINST+K) = 0.D0
            ZR(JFUKL-1+(K-1)*NBINST+I1M) = 0.D0
 122     CONTINUE
      ELSE
         ZI(JNOCL-1+I2M) = 0
         ZI(JNOCK-1+I2M) = 0
         ZI(JNOCL-1+I1M) = ZI(JNOCL-1+I1M) - NOC2M
         ZI(JNOCK-1+I1M) = ZI(JNOCK-1+I1M) - NOC2M
         DO 124 K = 1 , NBINST
            ZR(JFUKL-1+(K-1)*NBINST+I2M) = 0.D0
            ZR(JFUKL-1+(I2M-1)*NBINST+K) = 0.D0
 124     CONTINUE
      ENDIF
C
C --- EXISTE-T-IL DES ETATS TELS QUE NB_OCCUR > 0
C
      ENCORE = .FALSE.
      DO 200 I1 = 1, NBINST
         IF ( ZI(JNOCK-1+I1) .GT. 0 ) THEN
           ENCORE = .TRUE.
        ENDIF
 200   CONTINUE
      IF ( ENCORE )  GOTO 100 
C
 999  CONTINUE
C
      IF ( NIV .EQ. 2 )
     +   WRITE(IFM,*)'-->> FACTEUR D''USAGE CUMULE = ', FUT
C
      CALL JEDETR ( '&&RCEVFU.NB_OCC_K' )
      CALL JEDETR ( '&&RCEVFU.NB_OCC_L' )
      CALL JEDETR ( '&&RCEVFU.MATR_FU'  )
C
 1000 FORMAT(1P,I10,'|',40(E10.3,'|'))
 1010 FORMAT(1P,'   NB_OCCUR ','|',40(I10,'|'))
 1020 FORMAT(1P,A28,E12.5,', LIGNE:',I4,', COLONNE:',I4)
 1030 FORMAT(1P,A15,I8)
C
      CALL JEDEMA()
      END
