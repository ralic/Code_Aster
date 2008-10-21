      SUBROUTINE RC36F6 ( NBP12, NBP23, NBP13, NBSIGR, NBSG1,
     +                    NBSG2, NBSG3, SIGR, NOCC, SALTIJ )
      IMPLICIT   NONE        
      INTEGER             NBP12, NBP23, NBP13, NBSIGR, NOCC(*),
     +                    NBSG1, NBSG2, NBSG3, SIGR(*)
      REAL*8              SALTIJ(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 21/10/2008   AUTEUR VIVAN L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C        SI NOCC(CHEMINS DE PASSAGE) = 0 
C        ALORS IL N'EXISTE PLUS DE CHEMIN DE PASSAGE
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
      CHARACTER*32     JEXNOM, JEXNUM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C     ------------------------------------------------------------------
      INTEGER      NBSIPS, JNPASS, I, K, IOC1, NSITUP
      CHARACTER*3  TYPASS
      CHARACTER*8  K8B
C     ------------------------------------------------------------------
C
      TYPASS = '1_2'
      IF ( NBP12 .EQ. 0 ) GOTO 9999
      CALL JELIRA('&&RC32SI.PASSAGE_'//TYPASS,'LONUTI',NBSIPS,K8B)
      CALL JEVEUO('&&RC32SI.PASSAGE_'//TYPASS,'L',JNPASS)
      DO 10 I = 1 , NBSIPS
         NSITUP = ZI(JNPASS+I-1)
         DO 12 K = 1 , NBSIGR
           IF (SIGR(K).EQ.NSITUP) THEN
              IOC1 = K
              GOTO 14
           ENDIF
 12      CONTINUE
         CALL U2MESS('F','POSTRCCM_36')
 14      CONTINUE
         IF ( NOCC(2*(IOC1-1)+1).NE.0 .OR.
     +        NOCC(2*(IOC1-1)+2).NE.0 ) GOTO 9999
 10   CONTINUE
      NBP12 = 0
      CALL RC36F4 ( TYPASS, NBP12, NBP23, NBP13, NBSIGR, NBSG1,
     +                      NBSG2, NBSG3, NOCC, SALTIJ, NSITUP )
C
 9999 CONTINUE
      TYPASS = '2_3'
      IF ( NBP23 .EQ. 0 ) GOTO 9997
      CALL JELIRA('&&RC32SI.PASSAGE_'//TYPASS,'LONUTI',NBSIPS,K8B)
      CALL JEVEUO('&&RC32SI.PASSAGE_'//TYPASS,'L',JNPASS)
      DO 20 I = 1 , NBSIPS
         NSITUP = ZI(JNPASS+I-1)
         DO 22 K = 1 , NBSIGR
           IF (SIGR(K).EQ.NSITUP) THEN
              IOC1 = K
              GOTO 24
           ENDIF
 22      CONTINUE
         CALL U2MESS('F','POSTRCCM_36')
 24      CONTINUE
         IF ( NOCC(2*(IOC1-1)+1).NE.0 .OR.
     +        NOCC(2*(IOC1-1)+2).NE.0 ) GOTO 9997
 20   CONTINUE
      NBP23 = 0
      CALL RC36F4 ( TYPASS, NBP12, NBP23, NBP13, NBSIGR, NBSG1,
     +                      NBSG2, NBSG3, NOCC, SALTIJ, NSITUP )
C
 9997 CONTINUE
      TYPASS = '1_3'
      IF ( NBP13 .EQ. 0 ) GOTO 9995
      CALL JELIRA('&&RC32SI.PASSAGE_'//TYPASS,'LONUTI',NBSIPS,K8B)
      CALL JEVEUO('&&RC32SI.PASSAGE_'//TYPASS,'L',JNPASS)
      DO 30 I = 1 , NBSIPS
         NSITUP = ZI(JNPASS+I-1)
         DO 32 K = 1 , NBSIGR
           IF (SIGR(K).EQ.NSITUP) THEN
              IOC1 = K
              GOTO 34
           ENDIF
 32      CONTINUE
         CALL U2MESS('F','POSTRCCM_36')
 34      CONTINUE
         IF ( NOCC(2*(IOC1-1)+1).NE.0 .OR.
     +        NOCC(2*(IOC1-1)+2).NE.0 ) GOTO 9995
 30   CONTINUE
      NBP13 = 0
      CALL RC36F4 ( TYPASS, NBP12, NBP23, NBP13, NBSIGR, NBSG1,
     +                      NBSG2, NBSG3, NOCC, SALTIJ, NSITUP )
C
 9995 CONTINUE
C
      END
