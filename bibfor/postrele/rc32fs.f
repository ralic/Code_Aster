      SUBROUTINE RC32FS ( NBSIGR, NOCC, SITU, SALIJS, SALTIJ, 
     +                    SALTSE, NS, NSCY, MATER, UG )
      IMPLICIT   NONE
      INTEGER             NBSIGR, NOCC(*), SITU(*), NS, NSCY
      REAL*8              SALIJS(*), SALTIJ(*), SALTSE, UG
      CHARACTER*8         MATER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 21/10/2008   AUTEUR VIVAN L.VIVAN 
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
      INTEGER      IS1, IS2, I, I1, IFM, K, L, NIV, NS2, ICOMP
      REAL*8       SALT, SALTM, NADM, U1KL, U2KL, VALE(2)
      LOGICAL      TROUVE,ENDUR
      CHARACTER*2  CODRET
C     ------------------------------------------------------------------
C
      CALL INFNIV ( IFM, NIV )
C
      IF ( NIV .GE. 2 ) THEN
        WRITE(IFM,*) 'MATRICE SALT INITIALE (SEISME)'
        WRITE(IFM,1012) ( SITU(L),L=1,NBSIGR )
        WRITE(IFM,1010) ( NOCC(L),L=1,NBSIGR )
        DO 100 I = 1 , NBSIGR
          I1 = NBSIGR*(I-1)
          WRITE(IFM,1000) SITU(I), NOCC(I), (SALTIJ(I1+L),L=1,NBSIGR)
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
      DO 20 K = 1 , NBSIGR
C
         DO 22 L = 1 , NBSIGR
C
            SALT = SALTIJ(NBSIGR*(K-1)+L)
C
            IF ( SALT .GT. SALTM ) THEN
               IS1 = K
               IS2 = L
               SALTM = SALT
               TROUVE = .TRUE.
            ENDIF
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
         SALTM = SALIJS(NBSIGR*(IS1-1)+IS2)
C
         CALL LIMEND ( MATER, SALTM, 'WOHLER', ENDUR )
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
           WRITE(IFM,1040)'=> SALT MAXI = ', 
     +               SALTIJ(NBSIGR*(IS1-1)+IS2), SITU(IS1), SITU(IS2)
           WRITE(IFM,1020)'        U1KL = ', U1KL
           WRITE(IFM,1020)'        U2KL = ', U2KL
         ENDIF
C
         SALTIJ(NBSIGR*(IS1-1)+IS2) = 0.D0
C
         IF ( NIV .GE. 2 ) THEN
           WRITE(IFM,*) 'MATRICE SALT MODIFIEE (SEISME)'
         WRITE(IFM,1012) ( SITU(L),L=1,NBSIGR )
         WRITE(IFM,1010) ( NOCC(L),L=1,NBSIGR )
           DO 110 I = 1 , NBSIGR
             I1 = NBSIGR*(I-1)
             WRITE(IFM,1000) SITU(I), NOCC(I), (SALTIJ(I1+L),L=1,NBSIGR)
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
 1000 FORMAT(1P,I7,I9,'|',40(E9.2,'|'))
 1010 FORMAT(1P,7X,'NB_OCCUR ','|',40(I9,'|'))
 1012 FORMAT(1P,7X,'SITUATION','|',40(I9,'|'))
 1040 FORMAT(1P,A15,E12.5,', LIGNE:',I4,', COLONNE:',I4)
 1030 FORMAT(1P,A15,I12)
 1020 FORMAT(1P,A15,E12.5)
C
      END
