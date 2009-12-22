      SUBROUTINE DISMCM(CODMES,QUESTI,NOMOBZ,REPI,REPKZ,IERD)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 22/12/2009   AUTEUR ABBAS M.ABBAS 
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
      CHARACTER*8   KBID, MATER, NOMF
      CHARACTER*10  NOMRC
      CHARACTER*24  QUEST2,NOMOBJ(100)
      LOGICAL       TROUVE
      INTEGER       NBMAX,IZONE,I
      PARAMETER (NBMAX=30)
C
      CALL JEMARQ()
      NOMOB  = NOMOBZ


      IF (QUESTI.EQ.'EXI_AMOR_ALPHA'.OR.QUESTI.EQ.'EXI_AMOR_NOR'.OR.
     &    QUESTI.EQ.'EXI_AMOR_TAN'.OR.QUESTI.EQ.'EXI_AMOR_HYST'.OR.
     &    QUESTI.EQ.'EXI_AMOR_BETA') THEN
C     ---------------------------------------------------------------
         CALL JEVEUO(NOMOB//'.CHAMP_MAT .VALE','L',IAVALE)
         CALL JELIRA(NOMOB//'.CHAMP_MAT .VALE','LONMAX',NMAT,KBID)
         CALL JEVEUO(NOMOB//'.CHAMP_MAT .DESC','L',JDESC)
         TROUVE=.FALSE.
         QUEST2=QUESTI
         NBZONE=ZI(JDESC+2)
C
         DO 1 IZONE=1,NBZONE
           DO 5 IMAX=1,NBMAX
             I=(IZONE-1)*NBMAX+IMAX
             MATER= ZK8(IAVALE-1+I)
             IF (MATER.EQ.' ') GOTO 1
             IF (MATER.EQ.'TREF=>') GOTO 1

             CALL JELSTC ( 'G' , MATER , 1 , 100 , NOMOBJ , N )
             IF (N.LT.0) CALL U2MESS('F','UTILITAI_54')
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
 5         CONTINUE
 1       CONTINUE
 2       CONTINUE
         REPK='NON'
         IF (TROUVE) REPK='OUI'


      ELSEIF (QUESTI.EQ.'EXI_ANISO') THEN
C     -----------------------------------
         REPK='NON'
         CALL JEVEUO(NOMOB//'.CHAMP_MAT .VALE','L',IAVALE)
         CALL JELIRA(NOMOB//'.CHAMP_MAT .VALE','LONMAX',NMAT,KBID)
         CALL JEVEUO(NOMOB//'.CHAMP_MAT .DESC','L',JDESC)
         NBZONE=ZI(JDESC+2)
C
         DO 10 IZONE=1,NBZONE
           DO 15 IMAX=1,NBMAX
             I=(IZONE-1)*NBMAX+IMAX
             MATER= ZK8(IAVALE-1+I)
             IF (MATER.EQ.' ') GOTO 10
             IF (MATER.EQ.'TREF=>') GOTO 10

             CALL JEVEUO(MATER//'.MATERIAU.NOMRC','L',IANORC)
             CALL JELIRA(MATER//'.MATERIAU.NOMRC','LONMAX',NBRC,KBID)
             DO 12, IRC=1,NBRC
               NOMRC=ZK16(IANORC-1+IRC)(1:10)
               IF (NOMRC.EQ.'ELAS_COQUE') THEN
                  REPK='OUI'
                  GO TO 14
               ELSEIF (NOMRC.EQ.'THER_COQUE') THEN
                  REPK='OUI'
                  GO TO 14
               ELSEIF (NOMRC.EQ.'ELAS_ORTH') THEN
                  REPK='OUI'
                  GO TO 14
               ELSEIF (NOMRC.EQ.'THER_ORTH') THEN
                  REPK='OUI'
                  GO TO 14
               ELSEIF (NOMRC.EQ.'ELAS_COQMU') THEN
                  REPK='OUI'
                  GO TO 14
               ELSEIF (NOMRC.EQ.'THER_COQMU') THEN
                  REPK='OUI'
                  GO TO 14
               END IF
 12          CONTINUE
 15        CONTINUE
 10      CONTINUE
 14      CONTINUE


      ELSE IF (QUESTI.EQ.'ELAS_F_TEMP') THEN
C     --------------------------------------
         REPK='NON'
         CALL JEVEUO(NOMOB//'.CHAMP_MAT .VALE','L',IAVALE)
         CALL JELIRA(NOMOB//'.CHAMP_MAT .VALE','LONMAX',NMAT,KBID)
         CALL JEVEUO(NOMOB//'.CHAMP_MAT .DESC','L',JDESC)
         NBZONE=ZI(JDESC+2)
C
         DO 20 IZONE=1,NBZONE
           DO 25 IMAX=1,NBMAX
             I=(IZONE-1)*NBMAX+IMAX
             MATER= ZK8(IAVALE-1+I)
             IF (MATER.EQ.' ') GOTO 20
             IF (MATER.EQ.'TREF=>') GOTO 20

             CALL JEVEUO(MATER//'.MATERIAU.NOMRC','L',IANORC)
             CALL JELIRA(MATER//'.MATERIAU.NOMRC','LONMAX',NBRC,KBID)
             DO 21, IRC=1,NBRC
               NOMRC=ZK16(IANORC-1+IRC)(1:10)
C
C            -- SI LE MATERIAU EST ISSU DE LA COMMANDE DEFI_COQU_MULT :
               IF (NOMRC(5:10).EQ.'_COQMU') GO TO 23
C
               IF (NOMRC(1:4).NE.'ELAS') GO TO 21
               CALL JEVEUO(MATER//'.'//NOMRC//'.VALK','L',IAVALK)
               CALL JELIRA(MATER//'.'//NOMRC//'.VALK','LONUTI',N1,KBID)
               CALL JELIRA(MATER//'.'//NOMRC//'.VALR','LONUTI',NR,KBID)
               CALL JELIRA(MATER//'.'//NOMRC//'.VALC','LONUTI',NC,KBID)
               NF=(N1-NR-NC)/2
               DO 22,IF=1,NF
                 NOMF=ZK8(IAVALK-1+NR+NC+NF+IF)
                 CALL JEVEUO(NOMF//'           .PROL','L',IAPROL)
                 IF (ZK24(IAPROL-1+1).EQ.'NAPPE') THEN
C              -- CAS D'UNE FONCTION A 2 VARIABLES :
                   IF (ZK24(IAPROL-1+3).EQ.'TEMP') REPK='OUI'
                   IF (ZK24(IAPROL-1+7).EQ.'TEMP') REPK='OUI'
                 ELSE
C              -- CAS D'UNE FONCTION A 1 VARIABLE :
                   IF (ZK24(IAPROL-1+3).EQ.'TEMP') REPK='OUI'
                 END IF
 22            CONTINUE
 21          CONTINUE
 23          CONTINUE
 25        CONTINUE
 20      CONTINUE


      ELSE IF (QUESTI.EQ.'THER_F_INST') THEN
C     --------------------------------------
         REPK='NON'
         CALL JEVEUO(NOMOB//'.CHAMP_MAT .VALE','L',IAVALE)
         CALL JELIRA(NOMOB//'.CHAMP_MAT .VALE','LONMAX',NMAT,KBID)
         CALL JEVEUO(NOMOB//'.CHAMP_MAT .DESC','L',JDESC)
         NBZONE=ZI(JDESC+2)
C
         DO 30 IZONE=1,NBZONE
           DO 35 IMAX=1,NBMAX
             I=(IZONE-1)*NBMAX+IMAX
             MATER= ZK8(IAVALE-1+I)
             IF (MATER.EQ.' ') GOTO 30
             IF (MATER.EQ.'TREF=>') GOTO 30

             CALL JEVEUO(MATER//'.MATERIAU.NOMRC','L',IANORC)
             CALL JELIRA(MATER//'.MATERIAU.NOMRC','LONMAX',NBRC,KBID)
             DO 31, IRC=1,NBRC
               NOMRC=ZK16(IANORC-1+IRC)(1:10)
C
C            -- SI LE MATERIAU EST ISSU DE LA COMMANDE DEFI_COQU_MULT :
               IF (NOMRC(5:10).EQ.'_COQMU') GO TO 33
C
               IF (NOMRC(1:4).NE.'THER') GO TO 31
               CALL JEVEUO(MATER//'.'//NOMRC//'.VALK','L',IAVALK)
               CALL JELIRA(MATER//'.'//NOMRC//'.VALK','LONUTI',N1,KBID)
               CALL JELIRA(MATER//'.'//NOMRC//'.VALR','LONUTI',NR,KBID)
               CALL JELIRA(MATER//'.'//NOMRC//'.VALC','LONUTI',NC,KBID)
               NF=(N1-NR-NC)/2
               DO 32,IF=1,NF
                 NOMF=ZK8(IAVALK-1+NR+NC+NF+IF)
                 CALL JEVEUO(NOMF//'           .PROL','L',IAPROL)
                 IF (ZK24(IAPROL-1+1).EQ.'NAPPE') THEN
C              -- CAS D'UNE FONCTION A 2 VARIABLES :
                   IF (ZK24(IAPROL-1+3).EQ.'INST') REPK='OUI'
                   IF (ZK24(IAPROL-1+7).EQ.'INST') REPK='OUI'
                 ELSE
C              -- CAS D'UNE FONCTION A 1 VARIABLE :
                   IF (ZK24(IAPROL-1+3).EQ.'INST') REPK='OUI'
                 END IF
 32            CONTINUE
 31          CONTINUE
 33          CONTINUE
 35        CONTINUE
 30      CONTINUE


      ELSE IF (QUESTI.EQ.'ELAS_F_HYDR') THEN
C     --------------------------------------
         REPK='NON'
         CALL JEVEUO(NOMOB//'.CHAMP_MAT .VALE','L',IAVALE)
         CALL JELIRA(NOMOB//'.CHAMP_MAT .VALE','LONMAX',NMAT,KBID)
         CALL JEVEUO(NOMOB//'.CHAMP_MAT .DESC','L',JDESC)
         NBZONE=ZI(JDESC+2)
C
         DO 40 IZONE=1,NBZONE
           DO 45 IMAX=1,NBMAX
             I=(IZONE-1)*NBMAX+IMAX
             MATER= ZK8(IAVALE-1+I)
             IF (MATER.EQ.' ') GOTO 40
             IF (MATER.EQ.'TREF=>') GOTO 40

             CALL JEVEUO(MATER//'.MATERIAU.NOMRC','L',IANORC)
             CALL JELIRA(MATER//'.MATERIAU.NOMRC','LONMAX',NBRC,KBID)
             DO 41, IRC=1,NBRC
               NOMRC=ZK16(IANORC-1+IRC)(1:10)
C
C            -- SI LE MATERIAU EST ISSU DE LA COMMANDE DEFI_COQU_MULT :
               IF (NOMRC(5:10).EQ.'_COQMU') GO TO 43
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
                 IF (ZK24(IAPROL-1+1).EQ.'NAPPE') THEN
C              -- CAS D'UNE FONCTION A 2 VARIABLES :
                   IF (ZK24(IAPROL-1+3).EQ.'HYDR') REPK='OUI'
                   IF (ZK24(IAPROL-1+7).EQ.'HYDR') REPK='OUI'
                 ELSE
C              -- CAS D'UNE FONCTION A 1 VARIABLE :
                   IF (ZK24(IAPROL-1+3).EQ.'HYDR') REPK='OUI'
                 END IF
 42            CONTINUE
 41          CONTINUE
 43          CONTINUE
 45      CONTINUE
 40      CONTINUE


      ELSE IF (QUESTI.EQ.'ELAS_F_SECH') THEN
C     --------------------------------------
         REPK='NON'
         CALL JEVEUO(NOMOB//'.CHAMP_MAT .VALE','L',IAVALE)
         CALL JELIRA(NOMOB//'.CHAMP_MAT .VALE','LONMAX',NMAT,KBID)
         CALL JEVEUO(NOMOB//'.CHAMP_MAT .DESC','L',JDESC)
         NBZONE=ZI(JDESC+2)
C
         DO 50 IZONE=1,NBZONE
           DO 55 IMAX=1,NBMAX
             I=(IZONE-1)*NBMAX+IMAX
             MATER= ZK8(IAVALE-1+I)
             IF (MATER.EQ.' ') GOTO 50
             IF (MATER.EQ.'TREF=>') GOTO 50

             CALL JEVEUO(MATER//'.MATERIAU.NOMRC','L',IANORC)
             CALL JELIRA(MATER//'.MATERIAU.NOMRC','LONMAX',NBRC,KBID)
             DO 51, IRC=1,NBRC
               NOMRC=ZK16(IANORC-1+IRC)(1:10)
C
C            -- SI LE MATERIAU EST ISSU DE LA COMMANDE DEFI_COQU_MULT :
               IF (NOMRC(5:10).EQ.'_COQMU') GO TO 53
C
               IF (NOMRC(1:4).NE.'ELAS') GO TO 51
               CALL JEVEUO(MATER//'.'//NOMRC//'.VALK','L',IAVALK)
               CALL JELIRA(MATER//'.'//NOMRC//'.VALK','LONUTI',N1,KBID)
               CALL JELIRA(MATER//'.'//NOMRC//'.VALR','LONUTI',NR,KBID)
               CALL JELIRA(MATER//'.'//NOMRC//'.VALC','LONUTI',NC,KBID)
               NF=(N1-NR-NC)/2
               DO 52,IF=1,NF
                 NOMF=ZK8(IAVALK-1+NR+NC+NF+IF)
                 CALL JEVEUO(NOMF//'           .PROL','L',IAPROL)
                 IF (ZK24(IAPROL-1+1).EQ.'NAPPE') THEN
C              -- CAS D'UNE FONCTION A 2 VARIABLES :
                   IF (ZK24(IAPROL-1+3).EQ.'SECH') REPK='OUI'
                   IF (ZK24(IAPROL-1+7).EQ.'SECH') REPK='OUI'
                 ELSE
C              -- CAS D'UNE FONCTION A 1 VARIABLE :
                   IF (ZK24(IAPROL-1+3).EQ.'SECH') REPK='OUI'
                 END IF
 52            CONTINUE
 51          CONTINUE
 53          CONTINUE
 55      CONTINUE
 50      CONTINUE
      ELSE IF (QUESTI.EQ.'EXI_VARC') THEN
C     --------------------------------------
         REPK='NON'
         CALL JEEXIN(NOMOB// '.CVRCVARC',IRET)
         IF ( IRET.NE.0) THEN
           REPK='OUI'
         ENDIF
         
         
      ELSE
C     --------------------------------------
         REPK = QUESTI
         CALL U2MESK(CODMES,'UTILITAI_49',1,REPK)
         IERD=1
         GO TO 9999
      END IF
C
 9999 CONTINUE
      REPKZ = REPK
      REPI  = 0
      CALL JEDEMA()
      END
