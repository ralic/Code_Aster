      SUBROUTINE RC32FS ( NBSIG1, NOC1, SIT1, NBSIG2, NOC2, SIT2,
     +                    SALIJS, SALTIJ, SALTSE, NS, NSCY, MATER, UG )
      IMPLICIT   NONE
      INTEGER             NBSIG1, NOC1(*), SIT1(*), NBSIG2, NOC2(*), 
     +                    SIT2(*), NS, NSCY
      REAL*8              SALIJS(*), SALTIJ(*), SALTSE, UG
      CHARACTER*8         MATER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/04/2007   AUTEUR VIVAN L.VIVAN 
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
C     CALCUL DU FACTEUR D'USAGE 
C
C     ------------------------------------------------------------------
      INTEGER      IS1, IS2, IS3, I, I1, I2, IND1, IND2, IFM, L,  
     +             NIV, NS2, ICOMP
      REAL*8       SALT, SALTM, NADM, U1KL, U2KL, VALE(2)
      LOGICAL      TROUVE,ENDUR
      CHARACTER*2  CODRET, K2C, K2L
C     ------------------------------------------------------------------
C
      CALL INFNIV ( IFM, NIV )
C
      IF ( NIV .GE. 2 ) THEN
        WRITE(IFM,*) 'MATRICE SALT INITIALE (SEISME)'
        WRITE(IFM,1012) ( SIT2(2*(L-1)+1),SIT2(2*(L-1)+2),L=1,NBSIG2 )
        WRITE(IFM,1010) ( NOC2(2*(L-1)+1),NOC2(2*(L-1)+2),L=1,NBSIG2 )
        DO 100 I = 1 , NBSIG1
          I1 = 4*NBSIG2*(I-1)
          WRITE(IFM,1000) SIT1(2*(I-1)+1), NOC1(2*(I-1)+1),
     +       (SALTIJ(I1+4*(L-1)+1),SALTIJ(I1+4*(L-1)+3), L=1,NBSIG2)
          WRITE(IFM,1002) SIT1(2*(I-1)+2), NOC1(2*(I-1)+2),
     +       (SALTIJ(I1+4*(L-1)+2),SALTIJ(I1+4*(L-1)+4), L=1,NBSIG2)
 100    CONTINUE
      ENDIF
C
      UG = 0.D0
      NS2 = NS / 2
      ICOMP = 0
C
 10   CONTINUE
      SALTM = 0.D0
      TROUVE = .FALSE.
      ICOMP = ICOMP + 1
      IF ( ICOMP .GT. NS2 ) GOTO 9999
C
C --- ON SELECTIONNE LES 'NS2' COMBINAISONS LES PLUS PENALISANTES
C     SANS PRENDRE EN COMPTE LE SEISME (MATRICE SALTIJ)
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
C ------ ON RECUPERE LA VALEUR ASSOCIEE AVEC PRISE EN COMPTE DU SEISME
C        (MATRICE SALIJS)
C
         IND1 = 4*NBSIG2*(IS1-1)
         IND2 = 4*(IS2-1)
         SALTM = SALIJS(IND1+IND2+IS3)
C
         CALL LIMEND ( MATER,SALTM,'WOHLER',ENDUR)
         IF ( ENDUR ) THEN
            U1KL=0.D0
         ELSE
            CALL RCVALE ( MATER, 'FATIGUE', 1, 'SIGM    ', SALTM, 1,
     +                         'WOHLER  ', NADM, CODRET, 'F ' )
            IF ( NADM .LT. 0 ) THEN
               VALE(1) = SALTM
               VALE(2) = NADM
               CALL U2MESG('A', 'POSTRCCM_32',0,' ',0,0,2,VALE)
            ENDIF
            U1KL = 1.D0 / NADM
         ENDIF
C
         CALL LIMEND ( MATER, SALTSE, 'WOHLER', ENDUR )
         IF ( ENDUR ) THEN
            U2KL = 0.D0
         ELSE
            CALL RCVALE ( MATER, 'FATIGUE', 1, 'SIGM    ', SALTSE, 1,
     +                         'WOHLER  ', NADM, CODRET, 'F ' )
            IF ( NADM .LT. 0 ) THEN
               VALE(1) = SALTSE
               VALE(2) = NADM
               CALL U2MESG('A', 'POSTRCCM_32',0,' ',0,0,2,VALE)
            ENDIF
             U2KL = DBLE( 2*NSCY-1 ) / NADM
         ENDIF
C
         IF ( NIV .GE. 2 ) THEN
           IF ( IS3.EQ.1 .OR. IS3.EQ.3 ) THEN
              K2L = '_A'
           ELSE
              K2L = '_B'
           ENDIF
           IF ( IS3.EQ.1 .OR. IS3.EQ.2 ) THEN
              K2C = '_A'
           ELSE
              K2C = '_B'
           ENDIF
           WRITE(IFM,1040)'=> SALT MAXI = ', SALTM, SIT1(2*(IS1-1)+1),
     +                     K2L, SIT2(2*(IS2-1)+1), K2C
           WRITE(IFM,1020)'        U1KL = ', U1KL
           WRITE(IFM,1020)'        U2KL = ', U2KL
         ENDIF
C
         IND1 = 4*NBSIG2*(IS1-1)
         IND2 = 4*(IS2-1)
         SALTIJ(IND1+IND2+IS3) = 0.D0
         IND1 = 4*NBSIG2*(IS2-1)
         IND2 = 4*(IS1-1)
         IF ( IS3 .EQ. 2 ) THEN
            SALTIJ(IND1+IND2+3) = 0.D0
         ELSEIF ( IS3 .EQ. 3 ) THEN
            SALTIJ(IND1+IND2+2) = 0.D0
         ELSE
            SALTIJ(IND1+IND2+IS3) = 0.D0
         ENDIF
C
         IF ( NIV .GE. 2 ) THEN
           WRITE(IFM,*) 'MATRICE SALT MODIFIEE (SEISME)'
         WRITE(IFM,1012) ( SIT2(2*(L-1)+1),SIT2(2*(L-1)+2),L=1,NBSIG2 )
         WRITE(IFM,1010) ( NOC2(2*(L-1)+1),NOC2(2*(L-1)+2),L=1,NBSIG2 )
           DO 110 I = 1 , NBSIG1
             I1 = 4*NBSIG2*(I-1)
             WRITE(IFM,1000) SIT1(2*(I-1)+1), NOC1(2*(I-1)+1),
     +          (SALTIJ(I1+4*(L-1)+1),SALTIJ(I1+4*(L-1)+3), L=1,NBSIG2)
             WRITE(IFM,1002) SIT1(2*(I-1)+2), NOC1(2*(I-1)+2),
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
 1000 FORMAT(1P,I7,'_A',I9,'|',40(E9.2,1X,E9.2,'|'))
 1002 FORMAT(1P,I7,'_B',I9,'|',40(E9.2,1X,E9.2,'|'))
 1010 FORMAT(1P,9X,'NB_OCCUR ','|',40(I9,1X,I9,'|'))
 1012 FORMAT(1P,9X,'SITUATION','|',40(I7,'_A',1X,I7,'_B|'))
 1040 FORMAT(1P,A15,E12.5,', LIGNE:',I4,A2,', COLONNE:',I4,A2)
 1030 FORMAT(1P,A15,I12)
 1020 FORMAT(1P,A15,E12.5)
C
      END
