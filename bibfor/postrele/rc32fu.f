      SUBROUTINE RC32FU ( NBSIGR, NOCC, SITU, FUIJ, 
     +                    NOMMAT, UG, FACTUS )    
      IMPLICIT   NONE
      INTEGER             NBSIGR, NOCC(*), SITU(*)
      REAL*8              FUIJ(*), UG, FACTUS(*)
      CHARACTER*(*)       NOMMAT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/11/2008   AUTEUR MACOCCO K.MACOCCO 
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
C
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
C     ROUTI?E IDENTIQUE A RC36FU
C
C     CALCUL DU FACTEUR D'USAGE 
C
C     ------------------------------------------------------------------
      INTEGER      ISK, ISL, K, L, NK, NL, N0, I1, NSITUP,
     +             IFM, NIV, ICOMPT
      REAL*8       FUIJM, NADM, UKL, VALE(2)
      LOGICAL      TROUVE, ENDUR
      CHARACTER*2  CODRET
      CHARACTER*8  K8B
C     ------------------------------------------------------------------
C
      CALL INFNIV ( IFM, NIV )
C
      IF ( NIV .GE. 2 ) THEN
        WRITE(IFM,*) 'MATRICE FACTEUR D USAGE INITIALE'
        WRITE(IFM,1012) ( SITU(L),L=1,NBSIGR )
        WRITE(IFM,1010) ( NOCC(L),L=1,NBSIGR )
        DO 100 K = 1 , NBSIGR
          I1 = NBSIGR*(K-1)
          WRITE(IFM,1000) SITU(K), NOCC(K), (FUIJ(I1+L),L=1,NBSIGR)
 100    CONTINUE
      ENDIF
C
      ICOMPT = 0
      UG = 0.D0
C
 10   CONTINUE
      FUIJM = 0.D0
      TROUVE = .FALSE.
C
C --- RECHERCHE DU SALT MAXI
C
      CALL RC32F0 ( NBSIGR, NOCC, FUIJ, FUIJM, TROUVE,
     &                                    ISK, ISL, NK, NL )
C
      IF ( .NOT. TROUVE ) GOTO 9999
C
      N0 = MIN ( NK , NL )
      UKL = DBLE( N0 ) * FUIJM
C
      IF ( ICOMPT .LE. 49 ) THEN
         ICOMPT = ICOMPT + 1
         FACTUS(4*(ICOMPT-1)+1) = 1
         FACTUS(4*(ICOMPT-1)+2) = SITU(ISK)
         FACTUS(4*(ICOMPT-1)+3) = SITU(ISL)
         FACTUS(4*(ICOMPT-1)+4) = UKL
      ENDIF
C
      IF ( NIV .GE. 2 ) THEN
         WRITE(IFM,1040)'=> FU MAXI = ', FUIJM, SITU(ISK), SITU(ISL)
         WRITE(IFM,1030)'          N0 = ', N0
         WRITE(IFM,1020)'         UKL = ', UKL
      ENDIF
C
C --- MISE A ZERO DES LIGNES ET COLONNES DE LA MATRICE SALT SUIVANT
C     LE NOMBRE D'OCCURENCE EGAL A ZERO
C
      CALL RC32F2 ( NBSIGR, NOCC, FUIJ, ISK, ISL, NK, NL, N0 )
C
      IF ( NIV .GE. 2 ) THEN
         WRITE(IFM,*) 'MATRICE FACTEUR D USAGE MODIFIEE'
         WRITE(IFM,1012) ( SITU(L),L=1,NBSIGR )
         WRITE(IFM,1010) ( NOCC(L),L=1,NBSIGR )
         DO 110 K = 1 , NBSIGR
            I1 = NBSIGR*(K-1)
            WRITE(IFM,1000) SITU(K), NOCC(K), (FUIJ(I1+L),L=1,NBSIGR)
 110     CONTINUE
      ENDIF
C
      UG = UG + UKL 
      GOTO 10
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
