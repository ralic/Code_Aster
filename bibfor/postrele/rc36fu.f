      SUBROUTINE RC36FU ( NBSIG1, NOC1, NBSIG2, NOC2, SALTIJ, NPASS,
     +                    NOMMAT, UG )    
      IMPLICIT   NONE
      INTEGER             NBSIG1, NOC1(*), NBSIG2, NOC2(*), NPASS
      REAL*8              SALTIJ(*), UG
      CHARACTER*(*)       NOMMAT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 01/10/2002   AUTEUR CIBHHLV L.VIVAN 
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
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
C
C     CALCUL DU FACTEUR D'USAGE 
C
C     ------------------------------------------------------------------
      INTEGER      IS1, IS2, I, K, L, NK, NL, N0, I1, I2, I3, IFM, NIV
      REAL*8       SALT, SALTM, NADM, UKL
      LOGICAL      TROUVE
      CHARACTER*2  CODRET
C     ------------------------------------------------------------------
C
      CALL INFNIV ( IFM, NIV )
C
      IF ( NIV .GE. 2 ) THEN
        WRITE(IFM,*) 'MATRICE SALT INITIALE'
        WRITE(IFM,1010) ( NOC2(2*(L-1)+1),NOC2(2*(L-1)+2),L=1,NBSIG2 )
        DO 100 K = 1 , NBSIG1
          I1 = 4*NBSIG2*(K-1)
          WRITE(IFM,1000) NOC1(2*(K-1)+1),
     +       (SALTIJ(I1+4*(L-1)+1),SALTIJ(I1+4*(L-1)+3), L=1,NBSIG2)
          WRITE(IFM,1000) NOC1(2*(K-1)+2),
     +       (SALTIJ(I1+4*(L-1)+2),SALTIJ(I1+4*(L-1)+4), L=1,NBSIG2)
 100    CONTINUE
      ENDIF
C
      UG = 0.D0
C
 10   CONTINUE
      SALTM = 0.D0
      TROUVE = .FALSE.
C
      DO 20 K = 1 , NBSIG1
C
         I1 = 4*NBSIG2*(K-1)
C
         DO 22 L = 1 , NBSIG2
C
            I2 = 4*(L-1)
C
            DO 24 I = 1, 4
               SALT = SALTIJ(I1+I2+I)
               IF ( SALT .GT. SALTM ) THEN
                  TROUVE = .TRUE.
                  SALTM  = SALT
                  I3  = I
                  IS1 = K
                  IS2 = L
                  IF ( I3.EQ.1 .OR. I3.EQ.2 ) THEN
                     NL = NOC2(2*(IS2-1)+1)
                  ELSEIF ( I3.EQ.3 .OR. I3.EQ.4 ) THEN
                     NL = NOC2(2*(IS2-1)+2)
                  ENDIF
                  IF ( I3.EQ.1 .OR. I3.EQ.3 ) THEN
                     NK = NOC1(2*(IS1-1)+1)
                  ELSEIF ( I3.EQ.2 .OR. I3.EQ.4 ) THEN
                     NK = NOC1(2*(IS1-1)+2)
                  ENDIF
               ENDIF
 24         CONTINUE
C
 22      CONTINUE
C
 20   CONTINUE
C
      IF ( TROUVE ) THEN
         IF ( NPASS .EQ. 0 ) THEN
            N0 = MIN ( NK , NL )
         ELSE
            N0 = MIN ( NK , NL, NPASS )
         ENDIF
         CALL RCVALE ( NOMMAT, 'FATIGUE', 1, 'SIGM', SALTM, 1,
     +                      'WOHLER', NADM, CODRET, 'F ' )
C
         UKL = DBLE( N0 ) / NADM
C
         IF ( I3.EQ.1 .OR. I3.EQ.2 ) THEN
            NOC2(2*(IS2-1)+1) = NL - N0
         ELSEIF ( I3.EQ.3 .OR. I3.EQ.4 ) THEN
            NOC2(2*(IS2-1)+2) = NL - N0
         ENDIF
         IF ( I3.EQ.1 .OR. I3.EQ.3 ) THEN
            NOC1(2*(IS1-1)+1) = NK - N0
         ELSEIF ( I3.EQ.2 .OR. I3.EQ.4 ) THEN
            NOC1(2*(IS1-1)+2) = NK - N0
         ENDIF
C
         IF ( NIV .GE. 2 ) THEN
           WRITE(IFM,1040)'=> SALT MAXI = ', SALTM, IS1, IS2, I3
           WRITE(IFM,1030)'          N0 = ', N0
           WRITE(IFM,1020)'        NADM = ', NADM
           WRITE(IFM,1020)'         UKL = ', UKL
         ENDIF
C
         IF ( (NOC2(2*(IS2-1)+1) .EQ. 0) .OR.
     +        (NOC2(2*(IS2-1)+2) .EQ. 0) ) THEN
            DO 30 L = 1 , NBSIG1
               I1 = 4*NBSIG2*(L-1)
               IF ( I3.EQ.1 .OR. I3.EQ.2 ) THEN
                  SALTIJ(I1+4*(IS2-1)+1) = 0.D0
                  SALTIJ(I1+4*(IS2-1)+2) = 0.D0
               ELSEIF ( I3.EQ.3 .OR. I3.EQ.4 ) THEN
                  SALTIJ(I1+4*(IS2-1)+3) = 0.D0
                  SALTIJ(I1+4*(IS2-1)+4) = 0.D0
               ENDIF
 30         CONTINUE
            I1 = 4*NBSIG2*(IS2-1)
            DO 32 L = 1 , NBSIG2
               IF ( I3.EQ.1 .OR. I3.EQ.2 ) THEN
                  SALTIJ(I1+4*(L-1)+1) = 0.D0
                  SALTIJ(I1+4*(L-1)+3) = 0.D0
               ELSEIF ( I3.EQ.3 .OR. I3.EQ.4 ) THEN
                  SALTIJ(I1+4*(L-1)+2) = 0.D0
                  SALTIJ(I1+4*(L-1)+4) = 0.D0
               ENDIF
 32         CONTINUE
         ENDIF
C
         IF ( (NOC1(2*(IS1-1)+1) .EQ. 0) .OR.
     +        (NOC1(2*(IS1-1)+2) .EQ. 0) ) THEN
            DO 40 L = 1 , NBSIG1
               I1 = 4*NBSIG2*(L-1)
               IF ( I3.EQ.1 .OR. I3.EQ.2 ) THEN
                  SALTIJ(I1+4*(IS1-1)+1) = 0.D0
                  SALTIJ(I1+4*(IS1-1)+2) = 0.D0
               ELSEIF ( I3.EQ.3 .OR. I3.EQ.4 ) THEN
                  SALTIJ(I1+4*(IS1-1)+3) = 0.D0
                  SALTIJ(I1+4*(IS1-1)+4) = 0.D0
               ENDIF
 40         CONTINUE
            I1 = 4*NBSIG2*(IS1-1)
            DO 42 L = 1 , NBSIG2
               IF ( I3.EQ.1 .OR. I3.EQ.3) THEN
                  SALTIJ(I1+4*(L-1)+1) = 0.D0
                  SALTIJ(I1+4*(L-1)+3) = 0.D0
               ELSEIF ( I3.EQ.2 .OR. I3.EQ.4 ) THEN
                  SALTIJ(I1+4*(L-1)+2) = 0.D0
                  SALTIJ(I1+4*(L-1)+4) = 0.D0
               ENDIF
 42         CONTINUE
         ENDIF
C
         IF ( NIV .GE. 2 ) THEN
           WRITE(IFM,*) 'MATRICE SALT MODIFIEE'
         WRITE(IFM,1010) ( NOC2(2*(L-1)+1),NOC2(2*(L-1)+2),L=1,NBSIG2 )
           DO 110 K = 1 , NBSIG1
             I1 = 4*NBSIG2*(K-1)
             WRITE(IFM,1000) NOC1(2*(K-1)+1),
     +          (SALTIJ(I1+4*(L-1)+1),SALTIJ(I1+4*(L-1)+3), L=1,NBSIG2)
             WRITE(IFM,1000) NOC1(2*(K-1)+2),
     +          (SALTIJ(I1+4*(L-1)+2),SALTIJ(I1+4*(L-1)+4), L=1,NBSIG2)
 110       CONTINUE
         ENDIF
C
         UG = UG + UKL 
         GOTO 10
C
      ENDIF
C
 1000 FORMAT(1P,I9,'|',40(E9.2,1X,E9.2,'|'))
 1010 FORMAT(1P,9X,'|',40(I9,1X,I9'|'))
 1040 FORMAT(1P,A15,E12.5,' LIGNE ',I4,' COLONNE ',I4,' INDICE ',I4)
 1030 FORMAT(1P,A15,I12)
 1020 FORMAT(1P,A15,E12.5)
C
      END
