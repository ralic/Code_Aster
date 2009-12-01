      SUBROUTINE RC32F1 ( NBSIGR, NOCC, SALTIJ, ISK, ISL, NK, NL, N0, 
     +               NBP12, NBP23, NBP13, SIGR, YAPASS, TYPASS, NSITUP )
      IMPLICIT   NONE        
      INTEGER             NBSIGR, NOCC(*), ISK, ISL, NK, NL, N0, NSITUP,
     +                    NBP12, NBP23, NBP13, SIGR(*)
      REAL*8              SALTIJ(*)
      LOGICAL             YAPASS
      CHARACTER*3         TYPASS
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 16/02/2009   AUTEUR GALENNE E.GALENNE 
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
C     CALCUL DU FACTEUR D'USAGE POUR LES SITUATIONS DE PASSAGE
C     DETERMINATION DU CHEMIN DE PASSAGE
C OUT : N0     : NOMBRE D'OCCURRENCE
C OUT : YAPASS : UNE SITUATION DE PASSAGE EXISTE
C OUT : TYPASS : PASSAGE D'UNE SITUATION A UNE AUTRE
C                1_2 : PASSAGE GROUPE 1 A GROUPE 2
C                1_2 : PASSAGE GROUPE 2 A GROUPE 3
C                1_3 : PASSAGE GROUPE 1 A GROUPE 3
C OUT : NSITUP : NUMERO DU CHEMIN DE SITUATION DE PASSAGE
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C     ------------------------------------------------------------------
      INTEGER      JSIGR, IG1, IG2, NBSIPS, JNPASS, I, K, I1, NSITU,
     +             NUMG1, NUMG2, SIPASS, NPASS, IOC1, IOC2
      REAL*8       SALMIA, SALT1, SALTAM
      LOGICAL      CHEMIN
      CHARACTER*8  K8B
C     ------------------------------------------------------------------
C
      CALL JEVEUO('&&RC32SI.SITU_GROUP', 'L', JSIGR  )
C
      IOC1 = SIGR(ISK)
      IOC2 = SIGR(ISL)
      NUMG1 = ZI(JSIGR+2*IOC1-2)
      IG1   = ZI(JSIGR+2*IOC1-1)
      NUMG2 = ZI(JSIGR+2*IOC2-2)
      IG2   = ZI(JSIGR+2*IOC2-1)
C
      YAPASS = .FALSE.     
      TYPASS = '?_?'
      NSITUP = 0  
C     
      IF ( NUMG1 .EQ. NUMG2 ) THEN
C ------ MEME GROUPE
         N0 = MIN ( NK , NL )
         GOTO 9999
      ELSEIF ( NUMG1 .EQ. IG2 ) THEN
C ------ MEME GROUPE
         N0 = MIN ( NK , NL )
         GOTO 9999
      ELSEIF ( NUMG2 .EQ. IG1 ) THEN
C ------ MEME GROUPE
         N0 = MIN ( NK , NL )
         GOTO 9999
      ELSEIF ( IG1 .EQ. IG2 ) THEN
C ------ MEME GROUPE
         N0 = MIN ( NK , NL )
         GOTO 9999
      ENDIF
C
      IF ( ( NUMG1.EQ.1 .AND. NUMG2.EQ.2 ) .OR.
     +     ( NUMG1.EQ.2 .AND. NUMG2.EQ.1 ) ) THEN
         IF ( NBP12 .EQ. 0 ) THEN
            IF ( ( IG1.EQ.1 .AND. IG2.EQ.3 ) .OR.
     +           ( IG1.EQ.3 .AND. IG2.EQ.1 ) ) THEN
               TYPASS = '1_3'
               YAPASS = .TRUE.     
            ELSEIF ( ( IG1.EQ.2 .AND. IG2.EQ.3 ) .OR.
     +               ( IG1.EQ.3 .AND. IG2.EQ.2 ) ) THEN
               TYPASS = '2_3'
               YAPASS = .TRUE.     
            ENDIF
         ELSE
            TYPASS = '1_2'
            YAPASS = .TRUE.     
         ENDIF
      ELSEIF ( ( NUMG1.EQ.2 .AND. NUMG2.EQ.3 ) .OR.
     +         ( NUMG1.EQ.3 .AND. NUMG2.EQ.2 ) ) THEN
         IF ( NBP23 .EQ. 0 ) THEN
            IF ( ( IG1.EQ.1 .AND. IG2.EQ.2 ) .OR.
     +           ( IG1.EQ.2 .AND. IG2.EQ.1 ) ) THEN
               TYPASS = '1_2'
               YAPASS = .TRUE.     
            ELSEIF ( ( IG1.EQ.1 .AND. IG2.EQ.3 ) .OR.
     +               ( IG1.EQ.3 .AND. IG2.EQ.1 ) ) THEN
               TYPASS = '1_3'
               YAPASS = .TRUE.     
            ENDIF
         ELSE
            TYPASS = '2_3'
            YAPASS = .TRUE.     
         ENDIF
      ELSEIF ( ( NUMG1.EQ.1 .AND. NUMG2.EQ.3 ) .OR.
     +         ( NUMG1.EQ.3 .AND. NUMG2.EQ.1 ) ) THEN
         IF ( NBP13 .EQ. 0 ) THEN
            IF ( ( IG1.EQ.1 .AND. IG2.EQ.2 ) .OR.
     +           ( IG1.EQ.2 .AND. IG2.EQ.1 ) ) THEN
               TYPASS = '1_2'
               YAPASS = .TRUE.     
            ELSEIF ( ( IG1.EQ.2 .AND. IG2.EQ.3 ) .OR.
     +               ( IG1.EQ.3 .AND. IG2.EQ.2 ) ) THEN
               TYPASS = '2_3'
               YAPASS = .TRUE.     
            ENDIF
         ELSE
            TYPASS = '1_3'
            YAPASS = .TRUE.     
         ENDIF
      ENDIF
C
C --- RECHERCHE DU CHEMIN DE PASSAGE
C
      CALL JELIRA('&&RC32SI.PASSAGE_'//TYPASS,'LONUTI',NBSIPS,K8B)
      CALL JEVEUO('&&RC32SI.PASSAGE_'//TYPASS,'L',JNPASS)
      CHEMIN = .FALSE.
      SALMIA = 1.D+50
      DO 10 I = 1 , NBSIPS
         SIPASS = ZI(JNPASS+I-1)
         DO 12 K = 1 , NBSIGR
           IF (SIGR(K).EQ.SIPASS) THEN
              IOC1 = K
              GOTO 14
           ENDIF
 12      CONTINUE
         CALL U2MESS('F','POSTRCCM_36')
 14      CONTINUE
         NPASS = NOCC(IOC1)
         IF ( NPASS .EQ. 0 ) GOTO 10
         CHEMIN = .TRUE.     
C --------- ON RECHERCHE LE MIN DES SALT MAX
          SALTAM = 0.D0
          DO 16 K = 1 , NBSIGR
             SALT1 = SALTIJ(NBSIGR*(K-1)+IOC1)
             IF ( SALT1 .GT. SALTAM ) THEN
                SALTAM = SALT1
                NSITU = IOC1
             ENDIF
 16       CONTINUE
          IF ( SALTAM .LT. SALMIA ) THEN
             SALMIA = SALTAM
             NSITUP = NSITU
          ENDIF
 10   CONTINUE
      IF ( CHEMIN ) THEN
         NPASS = NOCC(NSITUP)
         N0 = MIN ( NK , NL, NPASS )
      ELSE
         YAPASS = .FALSE.     
         N0 = MIN ( NK , NL )
      ENDIF
C
 9999 CONTINUE
C
      END
