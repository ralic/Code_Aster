      SUBROUTINE RC36FU ( NBSIGR, NOCC, SITU, SALTIJ,
     &                    NOMMAT, UG, FACTUS )
      IMPLICIT   NONE
      INTEGER             NBSIGR, NOCC(*), SITU(*)
      REAL*8              SALTIJ(*), UG, FACTUS(*)
      CHARACTER*(*)       NOMMAT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
C
C     CALCUL DU FACTEUR D'USAGE
C
C     ------------------------------------------------------------------
      INTEGER      ISK, ISL, K, L, NK, NL, N0, I1, I1A4,
     &             IFM, NIV, ICOMPT
      REAL*8       SALTM, NADM, UKL, VALE(2)
      LOGICAL      TROUVE, ENDUR
      INTEGER ICODRE
      CHARACTER*2  K2C, K2L
C     ------------------------------------------------------------------
C
      CALL INFNIV ( IFM, NIV )
C
      IF ( NIV .GE. 2 ) THEN
        WRITE(IFM,*) 'MATRICE SALT INITIALE'
        WRITE(IFM,1012) ( SITU(2*(L-1)+1),SITU(2*(L-1)+2),L=1,NBSIGR )
        WRITE(IFM,1010) ( NOCC(2*(L-1)+1),NOCC(2*(L-1)+2),L=1,NBSIGR )
        DO 100 K = 1 , NBSIGR
          I1 = 4*NBSIGR*(K-1)
          WRITE(IFM,1000) SITU(2*(K-1)+1), NOCC(2*(K-1)+1),
     &       (SALTIJ(I1+4*(L-1)+1),SALTIJ(I1+4*(L-1)+3), L=1,NBSIGR)
          WRITE(IFM,1002) SITU(2*(K-1)+2), NOCC(2*(K-1)+2),
     &       (SALTIJ(I1+4*(L-1)+2),SALTIJ(I1+4*(L-1)+4), L=1,NBSIGR)
 100    CONTINUE
      ENDIF
C
      ICOMPT = 0
      UG = 0.D0
C
 10   CONTINUE
      SALTM = 0.D0
      TROUVE = .FALSE.
C
C --- RECHERCHE DU SALT MAXI
C
      CALL RC36F0 ( NBSIGR, NOCC, SALTIJ, SALTM, TROUVE,
     &                                    ISK, ISL, I1A4, NK, NL )
C
      IF ( .NOT. TROUVE ) GOTO 9999
C
      N0 = MIN ( NK , NL )
      CALL LIMEND ( NOMMAT, SALTM, 'WOHLER', ENDUR )
      IF ( ENDUR ) THEN
         UKL = 0.D0
      ELSE
         CALL RCVALE ( NOMMAT, 'FATIGUE', 1, 'SIGM    ', SALTM, 1,
     &                         'WOHLER  ', NADM, ICODRE, 2)
         IF ( NADM .LT. 0 ) THEN
            VALE(1) = SALTM
            VALE(2) = NADM
            CALL U2MESG('A', 'POSTRCCM_32',0,' ',0,0,2,VALE)
         ENDIF
         UKL = DBLE( N0 ) / NADM
      ENDIF
C
      IF ( ICOMPT .LE. 49 ) THEN
         ICOMPT = ICOMPT + 1
         FACTUS(4*(ICOMPT-1)+1) = I1A4
         FACTUS(4*(ICOMPT-1)+2) = SITU(2*(ISK-1)+1)
         FACTUS(4*(ICOMPT-1)+3) = SITU(2*(ISL-1)+1)
         FACTUS(4*(ICOMPT-1)+4) = UKL
      ENDIF
C
      IF ( NIV .GE. 2 ) THEN
         IF ( I1A4.EQ.1 .OR. I1A4.EQ.3 ) THEN
            K2L = '_A'
         ELSE
            K2L = '_B'
         ENDIF
         IF ( I1A4.EQ.1 .OR. I1A4.EQ.2 ) THEN
            K2C = '_A'
         ELSE
            K2C = '_B'
         ENDIF
         WRITE(IFM,1040)'=> SALT MAXI = ', SALTM, SITU(2*(ISK-1)+1),
     &                     K2L, SITU(2*(ISL-1)+1), K2C
         WRITE(IFM,1030)'          N0 = ', N0
         WRITE(IFM,1020)'        NADM = ', NADM
         WRITE(IFM,1020)'         UKL = ', UKL
      ENDIF
C
C --- MISE A ZERO DES LIGNES ET COLONNES DE LA MATRICE SALT SUIVANT
C     LE NOMBRE D'OCCURENCE EGAL A ZERO
C
      CALL RC36F2 ( NBSIGR, NOCC, SALTIJ, I1A4, ISK, ISL, NK, NL, N0 )
C
      IF ( NIV .GE. 2 ) THEN
         WRITE(IFM,*) 'MATRICE SALT MODIFIEE'
         WRITE(IFM,1012) ( SITU(2*(L-1)+1),SITU(2*(L-1)+2),L=1,NBSIGR )
         WRITE(IFM,1010) ( NOCC(2*(L-1)+1),NOCC(2*(L-1)+2),L=1,NBSIGR )
         DO 110 K = 1 , NBSIGR
            I1 = 4*NBSIGR*(K-1)
            WRITE(IFM,1000) SITU(2*(K-1)+1), NOCC(2*(K-1)+1),
     &          (SALTIJ(I1+4*(L-1)+1),SALTIJ(I1+4*(L-1)+3), L=1,NBSIGR)
            WRITE(IFM,1002) SITU(2*(K-1)+2), NOCC(2*(K-1)+2),
     &          (SALTIJ(I1+4*(L-1)+2),SALTIJ(I1+4*(L-1)+4), L=1,NBSIGR)
 110     CONTINUE
      ENDIF
C
      UG = UG + UKL
      GOTO 10
C
 9999 CONTINUE
C
 1000 FORMAT(1P,I7,'_A',I9,'|',40(E9.2,1X,E9.2,'|'))
 1002 FORMAT(1P,I7,'_B',I9,'|',40(E9.2,1X,E9.2,'|'))
 1010 FORMAT(1P,9X,'NB_OCCUR ','|',40(I9,1X,I9,'|'))
 1012 FORMAT(1P,9X,'SITUATION','|',40(I7,'_A',1X,I7,'_B|'))
 1040 FORMAT(1P,A15,E12.5,', LIGNE:',I4,A2,', COLONNE:',I4,A2)
C 1040 FORMAT(1P,A15,E12.5,' LIGNE ',I4,' COLONNE ',I4,' INDICE ',I4)
 1030 FORMAT(1P,A15,I12)
 1020 FORMAT(1P,A15,E12.5)
C
      END
