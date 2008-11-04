      SUBROUTINE RC32FP ( NBSIGR, NOCC, SITU, SIGR,
     +                    FUIJ, NOMMAT, UG, FACTUS )    
      IMPLICIT   NONE
      INTEGER             NBSIGR, NOCC(*), SITU(*), SIGR(*)
      REAL*8              FUIJ(*), UG, FACTUS(*)
      CHARACTER*(*)       NOMMAT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/11/2008   AUTEUR MACOCCO K.MACOCCO 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ROUTINE IDENTIQUE A RC32FU
C
C     CALCUL DU FACTEUR D'USAGE POUR LES SITUATIONS DE PASSAGE
C
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI        
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNOM,JEXNUM,JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C     ------------------------------------------------------------------
      INTEGER      ISK, ISL, K, L, NK, NL, N0, I1, NSITUP,
     +             IFM, NIV, ICOMPT, NPASS, JSPAS, NBSG1, NBSG2, NBSG3,
     +             NBP12, NBP23, NBP13
      REAL*8       FUMAX, NADM, UKL, VALE(2)
      LOGICAL      TROUVE, ENDUR, YAPASS
      CHARACTER*2  CODRET
      CHARACTER*3  TYPASS
      CHARACTER*8  K8B
C     ------------------------------------------------------------------
C
      CALL INFNIV ( IFM, NIV )
C
      CALL JEVEUO ('&&RC32SI.PASSAGE_SIT', 'L', JSPAS )
      CALL JELIRA ('&&RC32SI.PASSAGE_1_2','LONUTI', NBP12, K8B)
      CALL JELIRA ('&&RC32SI.PASSAGE_2_3','LONUTI', NBP23, K8B)
      CALL JELIRA ('&&RC32SI.PASSAGE_1_3','LONUTI', NBP13, K8B)
      NBSG1 = ZI(JSPAS  )
      NBSG2 = ZI(JSPAS+1)
      NBSG3 = ZI(JSPAS+2)
C
C --- MISE A ZERO DES LIGNES ET COLONNES DE LA MATRICE SALT 
C     S'IL N'EXISTE PAS DE SITUATIONS DE PASSAGE
C
      CALL RC32F5 ( NBP12, NBP23, NBP13, NBSIGR, NBSG1,
     +              NBSG2, NBSG3, FUIJ )
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
      FUMAX = 0.D0
      TROUVE = .FALSE.
C
C --- RECHERCHE DU SALT MAXI
C
      CALL RC32F0 ( NBSIGR, NOCC, FUIJ, FUMAX, TROUVE,
     &                                    ISK, ISL, NK, NL )
C
      IF ( .NOT. TROUVE ) GOTO 9999
C
C --- DETERMINATION DU N0
C     RECHERCHE DES CHEMINS DE PASSAGE
C
      CALL RC32F1 ( NBSIGR, NOCC, FUIJ, ISK, ISL, NK, NL, N0, 
     +              NBP12, NBP23, NBP13, SIGR, YAPASS, TYPASS, NSITUP )
C
      UKL = DBLE( N0 ) * FUMAX
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
         IF ( YAPASS ) THEN
            WRITE(IFM,1040)'=> FU MAXI = ',FUMAX,SITU(ISK),SITU(ISL),
     +                                       TYPASS,SITU(NSITUP)
         ELSE
            WRITE(IFM,1042)'=> FU MAXI = ',FUMAX,SITU(ISK),SITU(ISL)
         ENDIF
         WRITE(IFM,1030)'          N0 = ', N0
         WRITE(IFM,1020)'         UKL = ', UKL
      ENDIF
C
C --- MISE A ZERO DES LIGNES ET COLONNES DE LA MATRICE SALT SUIVANT
C     LE NOMBRE D'OCCURENCE EGAL A ZERO
C
      CALL RC32F2 ( NBSIGR, NOCC, FUIJ, ISK, ISL, NK, NL, N0 )
C
C --- IDEM POUR LE CHEMIN DE PASSAGE 
C     
      IF ( YAPASS ) THEN
         NOCC(NSITUP) = MAX(0,NOCC(NSITUP)-N0)
         IF ( NOCC(NSITUP).EQ.0 ) THEN
             IF ( TYPASS .EQ. '1_2' ) THEN
                NBP12 = NBP12 - 1         
             ELSEIF ( TYPASS .EQ. '1_3' ) THEN
                NBP13 = NBP13 - 1         
             ELSEIF ( TYPASS .EQ. '2_3' ) THEN
                NBP23 = NBP23 - 1         
            ENDIF         
         ENDIF         
         CALL RC32F3 ( NBSIGR, NOCC, FUIJ, NSITUP )
         CALL RC32F4 ( TYPASS, NBP12, NBP23, NBP13, NBSIGR, NBSG1,
     +                 NBSG2, NBSG3, FUIJ )
      ENDIF
C
C --- ON VERIFIE SI LA COMBINAISON A ANNULEE DES CHEMINS DE PASSAGE
      CALL RC32F6 ( NBP12, NBP23, NBP13, NBSIGR, NBSG1,
     +              NBSG2, NBSG3, SIGR, NOCC, FUIJ )
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
 1040 FORMAT(1P,A15,E12.5,', LIGNE:',I4,', COLONNE:',I4,
     +       ', PASSAGE: ',A3,', SITUATION DE PASSAGE: ',I4)
 1042 FORMAT(1P,A15,E12.5,', LIGNE:',I4,', COLONNE:',I4)
 1030 FORMAT(1P,A15,I12)
 1020 FORMAT(1P,A15,E12.5)
C
      END
