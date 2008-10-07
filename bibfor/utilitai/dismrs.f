      SUBROUTINE DISMRS(CODMES,QUESTI,NOMOBZ,REPI,REPKZ,IERD)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 07/10/2008   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C     --     DISMOI(RESULTAT)
C     ARGUMENTS:
C     ----------
      INTEGER REPI,IERD
      CHARACTER*(*) QUESTI,CODMES
      CHARACTER*32 REPK
      CHARACTER*19 NOMOB
      CHARACTER*(*) NOMOBZ,REPKZ
C ----------------------------------------------------------------------
C     IN:
C       CODMES : CODE DES MESSAGES A EMETTRE : 'F', 'A', ...
C       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
C       NOMOBZ : NOM D'UN OBJET DE TYPE RESULTAT
C     OUT:
C       REPI   : REPONSE ( SI ENTIERE )
C       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
C       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
C
C ----------------------------------------------------------------------
C     VARIABLES LOCALES:
C     ------------------
C --------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR,JEXR8
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL,DIVERS
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24,OBJDES
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C --------------- FIN COMMUNS NORMALISES  JEVEUX  --------------------
      CHARACTER*4 DOCU
      CHARACTER*24 VALK(2)
      CHARACTER*8 K8BID
      CHARACTER*19 NOMCH



      CALL JEMARQ()
      NOMOB = NOMOBZ
      REPI  = 0
      REPK  = ' '


      IF (QUESTI.EQ.'TYPE_RESU') THEN
C         ---------------------
         CALL JEEXIN(NOMOB//'.DESC',IBID)
         IF (IBID.GT.0) THEN
           OBJDES=NOMOB//'.DESC'
         ELSE
           OBJDES=NOMOB//'.CELD'
         END IF
C
         CALL JELIRA ( OBJDES, 'GENR', IBID, K8BID )
         IF ( K8BID(1:1) .EQ. 'N' ) THEN
            CALL JELIRA ( OBJDES, 'DOCU', IBID, DOCU )
            CALL RSDOCU ( DOCU , REPK, IRET )
            IF ( IRET .NE. 0 ) THEN
                VALK(1) = DOCU
                VALK(2) = NOMOB
                CALL U2MESK(CODMES,'UTILITAI_68', 2 ,VALK)
               IERD=1
               GO TO 9999
            ENDIF
         ELSE
            REPK = 'CHAMP'
         ENDIF



      ELSE IF  ((QUESTI.EQ.'NOM_MODELE').OR.
     &         (QUESTI.EQ.'MODELE').OR.
     &         (QUESTI.EQ.'CHAM_MATER').OR.
     &         (QUESTI.EQ.'CARA_ELEM')) THEN
C     ------------------------------------------
        IF  ((QUESTI.EQ.'NOM_MODELE').OR.
     &       (QUESTI.EQ.'MODELE')) THEN
          CALL RSLIPA(NOMOB,'MODELE','&&DISMRS.LIPAR',JLIPAR,N1)
        ELSEIF  (QUESTI.EQ.'CARA_ELEM') THEN
          CALL RSLIPA(NOMOB,'CARAELEM','&&DISMRS.LIPAR',JLIPAR,N1)
        ELSEIF  (QUESTI.EQ.'CHAM_MATER')THEN
          CALL RSLIPA(NOMOB,'CHAMPMAT','&&DISMRS.LIPAR',JLIPAR,N1)
        ENDIF
        CALL ASSERT(N1.GE.1)
        REPK=' '
        ICO=0
        DO 10, K=1,N1
          IF (ZK8(JLIPAR-1+K).NE.' ') THEN
            IF (ZK8(JLIPAR-1+K).NE.REPK) THEN
              ICO=ICO+1
              REPK=ZK8(JLIPAR-1+K)
            ENDIF
          ENDIF
10      CONTINUE
        IF (ICO.EQ.0) REPK='#AUCUN'
        IF (ICO.GT.1) REPK='#PLUSIEURS'
        CALL JEDETR('&&DISMRS.LIPAR')



      ELSE IF  (QUESTI.EQ.'NOM_MAILLA') THEN
C     ------------------------------------------
         CALL JELIRA(JEXNUM(NOMOB//'.TACH',1),
     &                                         'LONMAX',NBCH,K8BID)
         CALL JEVEUO(JEXNUM(NOMOB//'.TACH',1),'L',IATACH)
         DO 1, I=1,NBCH
           NOMCH=ZK24(IATACH-1+I)(1:19)
           IF(NOMCH(1:1).NE.' ') THEN
             CALL DISMCP(CODMES,QUESTI,NOMCH,REPI,REPK,IERD)
             GO TO 9999
           END IF
 1       CONTINUE
C
C        -- SINON ON PARCOURT TOUS LES CHAMPS DU RESULTAT :
         CALL JELIRA(NOMOB//'.TACH','NMAXOC',NBSY,K8BID)
         DO 2, J=2,NBSY
           CALL JELIRA(JEXNUM(NOMOB//'.TACH',J),
     &                                           'LONMAX',NBCH,K8BID)
           CALL JEVEUO(JEXNUM(NOMOB//'.TACH',J),'L',IATACH)
           DO 3, I=1,NBCH
             NOMCH=ZK24(IATACH-1+I)(1:19)
             IF(NOMCH(1:1).NE.' ') THEN
               CALL DISMCP(CODMES,QUESTI,NOMCH,REPI,REPK,IERD)
               GO TO 9999
             END IF
 3         CONTINUE
 2       CONTINUE
         CALL U2MESS(CODMES,'UTILITAI_69')
         IERD=1



      ELSE IF ( (QUESTI.EQ.'NB_CHAMP_MAX')
     &     .OR. (QUESTI.EQ.'NB_CHAMP_UTI')) THEN
C     ------------------------------------------
         CALL JELIRA(NOMOB//'.DESC','GENR',IBID,K8BID)
         IF (K8BID(1:1).EQ.'N') THEN
            CALL DISMRC(CODMES,QUESTI,NOMOB,REPI,REPK,IERD)
         ELSE
            REPI = 1
         END IF

      ELSE
         REPK = QUESTI
         CALL U2MESK(CODMES,'UTILITAI_49',1,REPK)
         IERD=1
         GO TO 9999
      END IF
C
 9999 CONTINUE
      REPKZ = REPK
      CALL JEDEMA()
      END
