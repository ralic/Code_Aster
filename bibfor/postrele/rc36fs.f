      SUBROUTINE RC36FS ( NBSIG1, NOC1, NBSIG2, NOC2, SALTIJ, NS, NSCY,
     +                    MATSE, MSE, SN, NOMMAT, C, K, CARA, UG )
      IMPLICIT   NONE
      INTEGER             NBSIG1, NOC1(*), NBSIG2, NOC2(*), NS, NSCY
      REAL*8              SALTIJ(*), MATSE(*), MSE(*), SN(*), C(*), 
     +                    K(*), CARA(*), UG
      CHARACTER*8         NOMMAT
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
      INTEGER      IS1, IS2, IS3, I, I1, I2, IND1, IND2, IFM, L,  
     +             NIV, NS2, ICMP, ICOMP
      REAL*8       SALT, SALTM, NADM, U1KL, U2KL, SP, SNKL, SALTKL,
     +             MIJ, SM
      LOGICAL      TROUVE
      CHARACTER*2  CODRET
C     ------------------------------------------------------------------
C
      CALL INFNIV ( IFM, NIV )
C
      IF ( NIV .GE. 2 ) THEN
        WRITE(IFM,*) 'MATRICE SALT INITIALE (SEISME)'
        WRITE(IFM,1010) ( NOC2(2*(L-1)+1),NOC2(2*(L-1)+2),L=1,NBSIG2 )
        DO 100 I = 1 , NBSIG1
          I1 = 4*NBSIG2*(I-1)
          WRITE(IFM,1000) NOC1(2*(I-1)+1),
     +       (SALTIJ(I1+4*(L-1)+1),SALTIJ(I1+4*(L-1)+3), L=1,NBSIG2)
          WRITE(IFM,1000) NOC1(2*(I-1)+2),
     +       (SALTIJ(I1+4*(L-1)+2),SALTIJ(I1+4*(L-1)+4), L=1,NBSIG2)
 100    CONTINUE
      ENDIF
C
      UG = 0.D0
      NS2 = NS / 2
      ICOMP = 0
C
      MIJ = 0.D0
      DO 50 ICMP = 1 , 3
         MIJ = MIJ + MSE(ICMP)**2
 50   CONTINUE
      MIJ = SQRT( MIJ )
C
      SP = K(2)*C(2)*CARA(2)*MIJ / 4 / CARA(1)
C
 10   CONTINUE
      SALTM = 0.D0
      TROUVE = .FALSE.
      ICOMP = ICOMP + 1
      IF ( ICOMP .GT. NS2 ) GOTO 9999
C
      DO 20 I1 = 1 , NBSIG1
C
         IND1 = 4*NBSIG2*(I1-1)
C
         DO 22 I2 = 1 , NBSIG2
C
            IND2 = 4*(I2-1)
C
            DO 24 I = 1, 4
               SALT = SALTIJ(IND1+IND2+I)
               IF ( SALT .GT. SALTM ) THEN
                  IS1 = I1
                  IS2 = I2
                  IS3 = I
                  SALTM = SALT
                  TROUVE = .TRUE.
               ENDIF
 24         CONTINUE
C
 22      CONTINUE
C
 20   CONTINUE
C
      IF ( TROUVE ) THEN
C
         CALL RCVALE ( NOMMAT, 'FATIGUE', 1, 'SIGM', SALTM, 1,
     +                         'WOHLER', NADM, CODRET, 'F ' )
         U1KL = 1.D0 / NADM
C
         SNKL = SN(NBSIG2*(IS1-1)+(IS2-1))
         CALL RC36SA ( NOMMAT, MATSE, MATSE, SNKL, SP, SALTKL, SM )
         CALL RCVALE ( NOMMAT, 'FATIGUE', 1, 'SIGM', SALTKL, 1,
     +                         'WOHLER', NADM, CODRET, 'F ' )
         U2KL = DBLE( 2*NSCY-1 ) / NADM
C
         IF ( NIV .GE. 2 ) THEN
           WRITE(IFM,1040)'=> SALT MAXI = ', SALTM, IS1, IS2
           WRITE(IFM,1020)'        U1KL = ', U1KL
           WRITE(IFM,1020)'        SNKL = ', SNKL
           WRITE(IFM,1020)'          SP = ', SP
           WRITE(IFM,1020)'      SALTKL = ', SALTKL
           WRITE(IFM,1020)'        U2KL = ', U2KL
         ENDIF
C
         IND1 = 4*NBSIG2*(IS1-1)
         IND2 = 4*(IS2-1)
         SALTIJ(IND1+IND2+IS3) = 0.D0
C
         IF ( NIV .GE. 2 ) THEN
           WRITE(IFM,*) 'MATRICE SALT MODIFIEE (SEISME)'
         WRITE(IFM,1010) ( NOC2(2*(L-1)+1),NOC2(2*(L-1)+2),L=1,NBSIG2 )
           DO 110 I = 1 , NBSIG1
             I1 = 4*NBSIG2*(I-1)
             WRITE(IFM,1000) NOC1(2*(I-1)+1),
     +          (SALTIJ(I1+4*(L-1)+1),SALTIJ(I1+4*(L-1)+3), L=1,NBSIG2)
             WRITE(IFM,1000) NOC1(2*(I-1)+2),
     +          (SALTIJ(I1+4*(L-1)+2),SALTIJ(I1+4*(L-1)+4), L=1,NBSIG2)
 110       CONTINUE
         ENDIF
C
         UG = UG + U1KL + U2KL
         GOTO 10
C
      ENDIF
C
 9999 CONTINUE
C
 1000 FORMAT(1P,I9,'|',40(E9.2,1X,E9.2,'|'))
 1010 FORMAT(1P,9X,'|',40(I9,1X,I9'|'))
 1040 FORMAT(1P,A15,E12.5,' LIGNE ',I4,' COLONNE ',I4)
 1030 FORMAT(1P,A15,I12)
 1020 FORMAT(1P,A15,E12.5)
C
      END
