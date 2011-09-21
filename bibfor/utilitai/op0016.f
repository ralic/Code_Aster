      SUBROUTINE OP0016()
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     DIRECTIVE IMPR_JEVEUX
C
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*8  FICH
      CHARACTER*24 NOMOBJ,NOM
      CHARACTER*32 JEXNOM,JEXNUM,NOML32
      CHARACTER*80 TXT
      CHARACTER*16 NOMENT
      CHARACTER*1  NOMCLA,K1BID
      INTEGER      IARG
C
      CALL INFMAJ()
      IRES=IUNIFI('RESULTAT')
      IMES=IUNIFI('MESSAGE')
      FICH='MESSAGE'
      IUNI=IMES
C
      CALL GETVIS('IMPRESSION','UNITE',1,IARG,1,IUNI,NUNI)
      CALL ULDEFI( IUNI,' ',' ','A','N','O' )
      CALL GETVTX('IMPRESSION','NOM',1,IARG,1,FICH,NFIC)
      IF (NFIC.NE.0) THEN
C        CALL LXCAPS(FICH)
         IF(FICH(1:8).EQ.'RESULTAT') THEN
            IUNI=IRES
         ELSE IF(FICH(1:7).EQ.'MESSAGE') THEN
            IUNI=IMES
         ELSE
            WRITE(IMES,*) FICH,': FICHIER INCONNU'
            WRITE(IMES,*) 'LES ECRITURES SONT SUR LE FICHIER MESSAGE'
         END IF
         IF (NUNI.NE.0) THEN
            WRITE(IMES,*) 'CHOISIR ENTRE LES MOTS-CLE NOM ET UNITE'
            WRITE(IMES,*) 'LES ECRITURES SONT SUR LE FICHIER MESSAGE'
         END IF
      END IF
C
C
      CALL GETVTX(' ','ENTITE',0,IARG,1,NOMENT,N)
      IF ( N .EQ. 0 ) GOTO 9999
C
      CALL GETVTX(' ','COMMENTAIRE',0,IARG,1,TXT,N)
      IF (N.EQ.0) TXT=' '
C
      IF ( NOMENT(1:6) .EQ. 'DISQUE' ) THEN
C
         NOMCLA = ' '
         CALL GETVTX(' ','CLASSE',0,IARG,1,NOMCLA,N)
         WRITE(IUNI,*) ' '
         CALL JEIMPD(FICH,NOMCLA,TXT)
         WRITE(IUNI,*) ' '
C
      ELSE IF ( NOMENT(1:7) .EQ. 'MEMOIRE' ) THEN
C
         WRITE(IUNI,*) ' '
         CALL JEIMPM(FICH,TXT)
         WRITE(IUNI,*) ' '
C
      ELSE IF ( NOMENT .EQ. 'REPERTOIRE' ) THEN
C
         NOMCLA = ' '
         CALL GETVTX(' ','CLASSE',0,IARG,1,NOMCLA,N)
         WRITE(IUNI,*) ' '
         CALL JEIMPR(FICH,NOMCLA,TXT)
         WRITE(IUNI,*) ' '
C
      ELSE IF ( NOMENT(1:5) .EQ. 'OBJET' ) THEN
C
         CALL GETVTX(' ','NOMOBJ',0,IARG,1,NOMOBJ,N)
         NOML32 = NOMOBJ
         CALL JJVERN(NOML32,0,IRET)
         WRITE(IUNI,*) ' '
         WRITE(IUNI,*) ' '
         IF (IRET.EQ.0) THEN
           WRITE(IUNI,*) ' DIRECTIVE IMPR_JEVEUX '
           WRITE(IUNI,*) ' L''OBJET "',NOMOBJ,'" N''EXISTE PAS'
           GOTO 9999
         ELSE
            WRITE(IUNI,*) ' '
            WRITE(IUNI,*)' ECRITURE DE L''OBJET : "',NOMOBJ,'"'
         END IF
         CALL GETVTX(' ','COMMENTAIRE',0,IARG,1,TXT,N)
         IF (N.EQ.0) TXT=' '
         WRITE(IUNI,*) ' '
         CALL JEIMPA(IUNI,NOMOBJ,TXT)
         WRITE(IUNI,*) ' '
         WRITE(IUNI,*) ' '
         IF (IRET.EQ.2) THEN
            CALL GETVIS(' ','NUMOC',0,IARG,1,NUM,N1)
            CALL GETVTX(' ','NOMOC',0,IARG,1,NOM,N2)
            CALL GETVTX(' ','NOMATR',0,IARG,1,NOM,N3)
C           CALL LXCAPS(NOM)
            IF (N1.NE.0) THEN
               CALL JEEXIN(JEXNUM(NOMOBJ,NUM),IRET)
               IF (IRET.EQ.0) THEN
                  WRITE(IUNI,*) ' L''OBJET : "',NUM,
     &              '" DE LA COLLECTION : "',NOMOBJ,'" N''EXISTE PAS'
               ELSE
                  WRITE(IUNI,*) ' CONTENU DE L''OBJET : "',NUM,
     &              '" DE LA COLLECTION : "',NOMOBJ,'"'
                  WRITE(IUNI,*) ' '
                  CALL JEIMPA(IUNI,JEXNUM(NOMOBJ,NUM),TXT)
                  CALL JEIMPO(IUNI,JEXNUM(NOMOBJ,NUM),' ',TXT)
                  WRITE(IUNI,*) ' '
               END IF
            ELSE IF (N2.NE.0) THEN
               CALL JEEXIN(JEXNOM(NOMOBJ,NOM),IRET)
               IF (IRET.EQ.0) THEN
                  WRITE(IUNI,*) ' L''OBJET : "',NOM,
     &              '" DE LA COLLECTION : "',NOMOBJ,'" N''EXISTE PAS'
               ELSE
                  WRITE(IUNI,*) ' CONTENU DE L''OBJET : "',NOM,
     &              '" DE LA COLLECTION : "',NOMOBJ,'"'
                  WRITE(IUNI,*) ' '
                  CALL JEIMPA(IUNI,JEXNOM(NOMOBJ,NOM),TXT)
                  CALL JEIMPO(IUNI,JEXNOM(NOMOBJ,NOM),' ',TXT)
                  WRITE(IUNI,*) ' '
               END IF
            ELSE IF (N3.NE.0) THEN
               NOML32 = NOMOBJ
               CALL JEPRAT(IUNI,NOML32,NOM(1:8),' ',TXT)
               WRITE(IUNI,*) ' '
            ELSE
               CALL JELIRA(NOMOBJ,'NMAXOC',NOC,K1BID)
               DO 1 I=1,NOC
                  CALL JEEXIN(JEXNUM(NOMOBJ,I),IRET)
                  IF (IRET.NE.0) THEN
                     WRITE(IUNI,*) ' CONTENU DE L''OBJET : "',I,
     &              '" DE LA COLLECTION : "',NOMOBJ,'"'
                     WRITE(IUNI,*) ' '
                     CALL JEIMPA(IUNI,JEXNUM(NOMOBJ,I),TXT)
                     CALL JEIMPO(IUNI,JEXNUM(NOMOBJ,I),' ',TXT)
                     WRITE(IUNI,*) ' '
                  END IF
    1          CONTINUE
            END IF
         ELSE
            WRITE(IUNI,*) ' '
            WRITE(IUNI,*) ' CONTENU DE L''OBJET : "',NOMOBJ,'"'
            CALL JEIMPO(IUNI,NOMOBJ,' ',TXT)
            WRITE(IUNI,*) ' '
         END IF
         WRITE(IUNI,*) ' '
         WRITE(IUNI,*) ' FIN DE L''OBJET : "',NOMOBJ,'"'
         WRITE(IUNI,*) ' '
C
      ELSE IF ( NOMENT(1:7) .EQ. 'SYSTEME' ) THEN
C
         NOMCLA = ' '
         CALL GETVTX(' ','CLASSE',0,IARG,1,NOMCLA,N)
         CALL GETVTX(' ','NOMATR',0,IARG,1,NOM,N3)
         IF (N3.NE.0) THEN
            CALL JEPRAT(IUNI,'$'//NOMCLA(1:1),NOM(1:8),' ',TXT)
            WRITE(IUNI,*) ' '
         END IF
C
      ELSE IF ( NOMENT(1:8) .EQ. 'ATTRIBUT' ) THEN
C
         CALL GETVTX(' ','NOMOBJ',0,IARG,1,NOMOBJ,N)
         CALL JEEXIN(NOMOBJ,IRET)
         WRITE(IUNI,*) ' '
         IF (IRET.EQ.0) THEN
           WRITE(IUNI,*) ' DIRECTIVE IMPR_JEVEUX '
           WRITE(IUNI,*) ' L''OBJET "',NOMOBJ,'" N''EXISTE PAS'
           GOTO 9999
         ELSE
            WRITE(IUNI,*) ' '
            WRITE(IUNI,*)' ECRITURE DES ATTRIBUTS DE "',NOMOBJ,'"'
         END IF
         WRITE(IUNI,*) ' '
         CALL JEIMPA(IUNI,NOMOBJ,TXT)
         WRITE(IUNI,*) ' '
C
      ELSE IF ( NOMENT(1:14) .EQ. 'ENREGISTREMENT' ) THEN
C
         NOMCLA = ' '
         CALL GETVTX(' ','CLASSE',0,IARG,1,NOMCLA,N)
         CALL GETVIS(' ','NUMERO',0,IARG,1,NUMERG,NRG)
         CALL GETVIS(' ','INFO',0,IARG,1,INFO,NIF)
         WRITE(IUNI,*) ' '
         CALL JEPREG(FICH,NOMCLA,NUMERG,TXT,INFO)
         WRITE(IUNI,*) ' '
C
      ENDIF
 9999 CONTINUE
      END
