      SUBROUTINE RC3207 ( NBSIG1, NOC1, SIT1, NBSIG2, NOC2, SIT2,
     +                    SALIJS, SALTIJ, SALTSE, NS, NSCY, MATER )
      IMPLICIT   NONE
      INTEGER             NBSIG1, NOC1(*), SIT1(*), NBSIG2, NOC2(*), 
     +                    SIT2(*), NS, NSCY
      REAL*8              SALIJS(*), SALTIJ(*), SALTSE, UG
      CHARACTER*8         MATER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 23/02/2004   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     CALCUL DU FACTEUR D'USAGE 
C
C     ------------------------------------------------------------------
      INTEGER      IS1, IS2, IS3, I, I1, I2, IND1, IND2, IFM, NIV
      REAL*8       SALT, SALTM, NADM, U1KL, U2KL, UKL
      LOGICAL      ENDUR
      CHARACTER*2  CODRET
C     ------------------------------------------------------------------
C
      CALL INFNIV ( IFM, NIV )
C
      UG = 0.D0 
C
      DO 20 I1 = 1 , NBSIG1
C
         SALTM = 0.D0
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
               ENDIF
 24         CONTINUE
C
 22      CONTINUE
C
         write(IFM,1010) I1, SALTM

C
C ------ ON RECUPERE LA VALEUR ASSOCIEE AVEC PRISE EN COMPTE DU SEISME
C        (MATRICE SALIJS)
C
         IND1 = 4*NBSIG2*(IS1-1)
         IND2 = 4*(IS2-1)
         SALTM = SALIJS(IND1+IND2+IS3)
C
         CALL LIMEND( MATER,SALTM,ENDUR)
         IF (ENDUR) THEN
            U1KL=0.D0
         ELSE
            CALL RCVALE ( MATER, 'FATIGUE', 1, 'SIGM', SALTM, 1,
     +                         'WOHLER', NADM, CODRET, 'F ' )
         IF ( NADM .LT. 0 ) THEN
            CALL UTDEBM ('A','WOHLER','NOMBRE DE CYCLES ADMISSIBLES'//
     +                       ' NEGATIF, VERIFIER LA COURBE DE WOHLER')
            CALL UTIMPR ('L','   CONTRAINTE CALCULEE = ',1,SALTM)
            CALL UTIMPR ('L','   NADM = ',1,NADM)
            CALL UTFINM ()
         ENDIF
C
            U1KL = 1.D0 / NADM
         ENDIF
C
         CALL LIMEND( MATER,SALTSE,ENDUR)
         IF (ENDUR) THEN
            U2KL=0.D0
         ELSE
            CALL RCVALE ( MATER, 'FATIGUE', 1, 'SIGM', SALTSE, 1,
     +                         'WOHLER', NADM, CODRET, 'F ' )
         IF ( NADM .LT. 0 ) THEN
            CALL UTDEBM ('A','WOHLER','NOMBRE DE CYCLES ADMISSIBLES'//
     +                       ' NEGATIF, VERIFIER LA COURBE DE WOHLER')
            CALL UTIMPR ('L','   CONTRAINTE CALCULEE = ',1,SALTSE)
            CALL UTIMPR ('L','   NADM = ',1,NADM)
            CALL UTFINM ()
         ENDIF
C
            U2KL = DBLE( 2*NSCY-1 ) / NADM
         ENDIF

         UKL = U1KL + U2KL
         UG = UG + UKL
         write(IFM,1020) I1,  UKL
C
 20   CONTINUE
C
         write(IFM,1030) UG
C
 1010 FORMAT(1P,' SITUATION ',I2,' SALTM = ',E12.5)
 1020 FORMAT(1P,' SITUATION ',I2,' UKL = ',E12.5)
 1030 FORMAT(1P,'              UG = ',E12.5)
C
      END
