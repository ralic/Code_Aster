      SUBROUTINE DISMCM(CODMES,QUESTI,NOMOBZ,REPI,REPKZ,IERD)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 17/06/2003   AUTEUR VABHHTS J.PELLET 
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
C     --     DISMOI(CHAM_MATER)
C     ARGUMENTS:
C     ----------
      INTEGER REPI,IERD
      CHARACTER*(*) QUESTI,CODMES
      CHARACTER*(*) NOMOBZ, REPKZ
      CHARACTER*32 REPK
      CHARACTER*8  NOMOB
C ----------------------------------------------------------------------
C     IN:
C       CODMES : CODE DES MESSAGES A EMETTRE : 'F', 'A', ...
C       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
C       NOMOBZ : NOM D'UN OBJET DE TYPE NUM_DDL
C     OUT:
C       REPI   : REPONSE ( SI ENTIERE )
C       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
C       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
C
C ----------------------------------------------------------------------
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*8   KBID, MATER, CODRET, NOMF
      CHARACTER*10  NOMRC
      CHARACTER*24  QUEST2,NOMOBJ(100)
      LOGICAL       TROUVE
C
      CALL JEMARQ()
      NOMOB  = NOMOBZ
      IF (QUESTI.EQ.'EXI_AMOR_ALPHA'.OR.QUESTI.EQ.'EXI_AMOR_NOR'.OR.
     +    QUESTI.EQ.'EXI_AMOR_TAN'.OR.QUESTI.EQ.'EXI_AMOR_HYST') THEN
C     --------------------------------------
         CALL JEVEUO(NOMOB//'.CHAMP_MAT .VALE','L',IAVALE)
         CALL JELIRA(NOMOB//'.CHAMP_MAT .VALE','LONMAX',NMAT,KBID)
         TROUVE=.FALSE.
         QUEST2=QUESTI
         DO 1, I=1,NMAT
           MATER= ZK8(IAVALE-1+I)
           IF (MATER.NE.' ') THEN
             CALL JELSTC ( 'G' , MATER , 1 , 100 , NOMOBJ , N )
             IF (N.LT.0) CALL UTMESS('F','DISMCM','TROP D OBJETS')
             DO 3, II=1,N
               IF (NOMOBJ(II)(20:24).EQ.'.VALK') THEN
                 CALL JEVEUO(NOMOBJ(II),'L',IAOBJ)
                 CALL JELIRA(NOMOBJ(II),'LONMAX',LONOBJ,KBID)
                 DO 4 ,III=1,LONOBJ
                   IF (ZK8(IAOBJ-1+III).EQ.QUEST2(5:12)) THEN
                     TROUVE=.TRUE.
                     GO TO 2
                   END IF
 4               CONTINUE
               END IF
 3           CONTINUE
           END IF
 1       CONTINUE
 2       CONTINUE
         REPK='NON'
         IF (TROUVE) REPK='OUI'
      ELSE IF (QUESTI.EQ.'ELAS_F_TEMP') THEN
C     --------------------------------------
         REPK='NON'
         CALL JEVEUO(NOMOB//'.CHAMP_MAT .VALE','L',IAVALE)
         CALL JELIRA(NOMOB//'.CHAMP_MAT .VALE','LONMAX',NMAT,KBID)
C
         DO 10, I=1,NMAT
           MATER= ZK8(IAVALE-1+I)
           CALL JEVEUO(MATER//'.MATERIAU.NOMRC','L',IANORC)
           CALL JELIRA(MATER//'.MATERIAU.NOMRC','LONMAX',NBRC,KBID)
           DO 11, IRC=1,NBRC
             NOMRC=ZK16(IANORC-1+IRC)(1:10)
C
C            -- SI LE MATERIAU EST ISSU DE LA COMMANDE DEFI_COQU_MULT :
             IF ((IRC.EQ.3).AND.(NOMRC.EQ.' ')) GO TO 13
C
             IF (NOMRC(1:4).NE.'ELAS') GO TO 11
             CALL JEVEUO(MATER//'.'//NOMRC//'.VALK','L',IAVALK)
             CALL JELIRA(MATER//'.'//NOMRC//'.VALK','LONUTI',N1,KBID)
             CALL JELIRA(MATER//'.'//NOMRC//'.VALR','LONUTI',NR,KBID)
             CALL JELIRA(MATER//'.'//NOMRC//'.VALC','LONUTI',NC,KBID)
             NF=(N1-NR-NC)/2
             DO 12,IF=1,NF
               NOMF=ZK8(IAVALK-1+NR+NC+NF+IF)
               CALL JEVEUO(NOMF//'           .PROL','L',IAPROL)
               IF (ZK16(IAPROL-1+1).EQ.'NAPPE') THEN
C              -- CAS D'UNE FONCTION A 2 VARIABLES :
                 IF (ZK16(IAPROL-1+3).EQ.'TEMP') REPK='OUI'
                 IF (ZK16(IAPROL-1+6).EQ.'TEMP') REPK='OUI'
               ELSE
C              -- CAS D'UNE FONCTION A 1 VARIABLE :
                 IF (ZK16(IAPROL-1+3).EQ.'TEMP') REPK='OUI'
               END IF
 12          CONTINUE
 11        CONTINUE
 13        CONTINUE
 10      CONTINUE
      ELSE IF (QUESTI.EQ.'THER_F_INST') THEN
C     --------------------------------------
         REPK='NON'
         CALL JEVEUO(NOMOB//'.CHAMP_MAT .VALE','L',IAVALE)
         CALL JELIRA(NOMOB//'.CHAMP_MAT .VALE','LONMAX',NMAT,KBID)
C
         DO 20, I=1,NMAT
           MATER= ZK8(IAVALE-1+I)
           CALL JEVEUO(MATER//'.MATERIAU.NOMRC','L',IANORC)
           CALL JELIRA(MATER//'.MATERIAU.NOMRC','LONMAX',NBRC,KBID)
           DO 21, IRC=1,NBRC
             NOMRC=ZK16(IANORC-1+IRC)(1:10)
C
C            -- SI LE MATERIAU EST ISSU DE LA COMMANDE DEFI_COQU_MULT :
             IF ((IRC.EQ.3).AND.(NOMRC.EQ.' ')) GO TO 23
C
             IF (NOMRC(1:4).NE.'THER') GO TO 21
             CALL JEVEUO(MATER//'.'//NOMRC//'.VALK','L',IAVALK)
             CALL JELIRA(MATER//'.'//NOMRC//'.VALK','LONUTI',N1,KBID)
             CALL JELIRA(MATER//'.'//NOMRC//'.VALR','LONUTI',NR,KBID)
             CALL JELIRA(MATER//'.'//NOMRC//'.VALC','LONUTI',NC,KBID)
             NF=(N1-NR-NC)/2
             DO 22,IF=1,NF
               NOMF=ZK8(IAVALK-1+NR+NC+NF+IF)
               CALL JEVEUO(NOMF//'           .PROL','L',IAPROL)
               IF (ZK16(IAPROL-1+1).EQ.'NAPPE') THEN
C              -- CAS D'UNE FONCTION A 2 VARIABLES :
                 IF (ZK16(IAPROL-1+3).EQ.'INST') REPK='OUI'
                 IF (ZK16(IAPROL-1+6).EQ.'INST') REPK='OUI'
               ELSE
C              -- CAS D'UNE FONCTION A 1 VARIABLE :
                 IF (ZK16(IAPROL-1+3).EQ.'INST') REPK='OUI'
               END IF
 22          CONTINUE
 21        CONTINUE
 23        CONTINUE
 20      CONTINUE
      ELSE IF (QUESTI.EQ.'ELAS_F_HYDR') THEN
C     --------------------------------------
         REPK='NON'
         CALL JEVEUO(NOMOB//'.CHAMP_MAT .VALE','L',IAVALE)
         CALL JELIRA(NOMOB//'.CHAMP_MAT .VALE','LONMAX',NMAT,KBID)
C
         DO 30, I=1,NMAT
           MATER= ZK8(IAVALE-1+I)
           CALL JEVEUO(MATER//'.MATERIAU.NOMRC','L',IANORC)
           CALL JELIRA(MATER//'.MATERIAU.NOMRC','LONMAX',NBRC,KBID)
           DO 31, IRC=1,NBRC
             NOMRC=ZK16(IANORC-1+IRC)(1:10)
C
C            -- SI LE MATERIAU EST ISSU DE LA COMMANDE DEFI_COQU_MULT :
             IF ((IRC.EQ.3).AND.(NOMRC.EQ.' ')) GO TO 33
C
             IF (NOMRC(1:4).NE.'ELAS') GO TO 31
             CALL JEVEUO(MATER//'.'//NOMRC//'.VALK','L',IAVALK)
             CALL JELIRA(MATER//'.'//NOMRC//'.VALK','LONUTI',N1,KBID)
             CALL JELIRA(MATER//'.'//NOMRC//'.VALR','LONUTI',NR,KBID)
             CALL JELIRA(MATER//'.'//NOMRC//'.VALC','LONUTI',NC,KBID)
             NF=(N1-NR-NC)/2
             DO 32,IF=1,NF
               NOMF=ZK8(IAVALK-1+NR+NC+NF+IF)
               CALL JEVEUO(NOMF//'           .PROL','L',IAPROL)
               IF (ZK16(IAPROL-1+1).EQ.'NAPPE') THEN
C              -- CAS D'UNE FONCTION A 2 VARIABLES :
                 IF (ZK16(IAPROL-1+3).EQ.'HYDR') REPK='OUI'
                 IF (ZK16(IAPROL-1+6).EQ.'HYDR') REPK='OUI'
               ELSE
C              -- CAS D'UNE FONCTION A 1 VARIABLE :
                 IF (ZK16(IAPROL-1+3).EQ.'HYDR') REPK='OUI'
               END IF
 32          CONTINUE
 31        CONTINUE
 33        CONTINUE
 30      CONTINUE
      ELSE IF (QUESTI.EQ.'ELAS_F_SECH') THEN
C     --------------------------------------
         REPK='NON'
         CALL JEVEUO(NOMOB//'.CHAMP_MAT .VALE','L',IAVALE)
         CALL JELIRA(NOMOB//'.CHAMP_MAT .VALE','LONMAX',NMAT,KBID)
C
         DO 40, I=1,NMAT
           MATER= ZK8(IAVALE-1+I)
           CALL JEVEUO(MATER//'.MATERIAU.NOMRC','L',IANORC)
           CALL JELIRA(MATER//'.MATERIAU.NOMRC','LONMAX',NBRC,KBID)
           DO 41, IRC=1,NBRC
             NOMRC=ZK16(IANORC-1+IRC)(1:10)
C
C            -- SI LE MATERIAU EST ISSU DE LA COMMANDE DEFI_COQU_MULT :
             IF ((IRC.EQ.3).AND.(NOMRC.EQ.' ')) GO TO 43
C
             IF (NOMRC(1:4).NE.'ELAS') GO TO 41
             CALL JEVEUO(MATER//'.'//NOMRC//'.VALK','L',IAVALK)
             CALL JELIRA(MATER//'.'//NOMRC//'.VALK','LONUTI',N1,KBID)
             CALL JELIRA(MATER//'.'//NOMRC//'.VALR','LONUTI',NR,KBID)
             CALL JELIRA(MATER//'.'//NOMRC//'.VALC','LONUTI',NC,KBID)
             NF=(N1-NR-NC)/2
             DO 42,IF=1,NF
               NOMF=ZK8(IAVALK-1+NR+NC+NF+IF)
               CALL JEVEUO(NOMF//'           .PROL','L',IAPROL)
               IF (ZK16(IAPROL-1+1).EQ.'NAPPE') THEN
C              -- CAS D'UNE FONCTION A 2 VARIABLES :
                 IF (ZK16(IAPROL-1+3).EQ.'SECH') REPK='OUI'
                 IF (ZK16(IAPROL-1+6).EQ.'SECH') REPK='OUI'
               ELSE
C              -- CAS D'UNE FONCTION A 1 VARIABLE :
                 IF (ZK16(IAPROL-1+3).EQ.'SECH') REPK='OUI'
               END IF
 42          CONTINUE
 41        CONTINUE
 43        CONTINUE
 40      CONTINUE
      ELSE
C     --------------------------------------
         REPK = QUESTI
         CALL UTMESS(CODMES,'DISMCM:',
     +               'LA QUESTION : "'//REPK//'" EST INCONNUE')
         IERD=1
         GO TO 9999
      END IF
C
 9999 CONTINUE
      REPKZ = REPK
      CALL JEDEMA()
      END
