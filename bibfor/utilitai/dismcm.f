      SUBROUTINE DISMCM(QUESTI,NOMOBZ,REPI,REPKZ,IERD)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 04/10/2010   AUTEUR PELLET J.PELLET 
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
      IMPLICIT REAL*8(A-H,O-Z)
C     --     DISMOI(CHAM_MATER)
C     ARGUMENTS:
C     ----------
      INTEGER REPI,IERD
      CHARACTER*(*) QUESTI
      CHARACTER*(*) NOMOBZ,REPKZ
      CHARACTER*32 REPK
      CHARACTER*8 NOMOB
C ----------------------------------------------------------------------
C     IN:
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*8 KBID,MATER,NOMF
      CHARACTER*10 NOMRC
      CHARACTER*24 QUEST2,NOMOBJ(100)
      LOGICAL TROUVE
      INTEGER NBMAX,IZONE,I
      PARAMETER(NBMAX=30)
C
      CALL JEMARQ()
      NOMOB=NOMOBZ
      REPK=' '


      IF (QUESTI.EQ.'EXI_AMOR_ALPHA' .OR. QUESTI.EQ.'EXI_AMOR_NOR' .OR.
     &    QUESTI.EQ.'EXI_AMOR_TAN' .OR. QUESTI.EQ.'EXI_AMOR_HYST' .OR.
     &    QUESTI.EQ.'EXI_AMOR_BETA') THEN
C     ---------------------------------------------------------------
        CALL JEVEUO(NOMOB//'.CHAMP_MAT .VALE','L',IAVALE)
        CALL JELIRA(NOMOB//'.CHAMP_MAT .VALE','LONMAX',NMAT,KBID)
        CALL JEVEUO(NOMOB//'.CHAMP_MAT .DESC','L',JDESC)
        TROUVE=.FALSE.
        QUEST2=QUESTI
        NBZONE=ZI(JDESC+2)
C
        DO 40 IZONE=1,NBZONE
          DO 30 IMAX=1,NBMAX
            I=(IZONE-1)*NBMAX+IMAX
            MATER=ZK8(IAVALE-1+I)
            IF (MATER.EQ.' ')GOTO 40
            IF (MATER.EQ.'TREF=>')GOTO 40

            CALL JELSTC('G',MATER,1,100,NOMOBJ,N)
            IF (N.LT.0) CALL U2MESS('F','UTILITAI_54')
            DO 20,II=1,N
              IF (NOMOBJ(II)(20:24).EQ.'.VALK') THEN
                CALL JEVEUO(NOMOBJ(II),'L',IAOBJ)
                CALL JELIRA(NOMOBJ(II),'LONMAX',LONOBJ,KBID)
                DO 10,III=1,LONOBJ
                  IF (ZK8(IAOBJ-1+III).EQ.QUEST2(5:12)) THEN
                    TROUVE=.TRUE.
                    GOTO 50

                  ENDIF
   10           CONTINUE
              ENDIF
   20       CONTINUE
   30     CONTINUE
   40   CONTINUE
   50   CONTINUE
        REPK='NON'
        IF (TROUVE)REPK='OUI'


      ELSEIF (QUESTI.EQ.'EXI_ANISO') THEN
C     -----------------------------------
        REPK='NON'
        CALL JEVEUO(NOMOB//'.CHAMP_MAT .VALE','L',IAVALE)
        CALL JELIRA(NOMOB//'.CHAMP_MAT .VALE','LONMAX',NMAT,KBID)
        CALL JEVEUO(NOMOB//'.CHAMP_MAT .DESC','L',JDESC)
        NBZONE=ZI(JDESC+2)
C
        DO 80 IZONE=1,NBZONE
          DO 70 IMAX=1,NBMAX
            I=(IZONE-1)*NBMAX+IMAX
            MATER=ZK8(IAVALE-1+I)
            IF (MATER.EQ.' ')GOTO 80
            IF (MATER.EQ.'TREF=>')GOTO 80

            CALL JEVEUO(MATER//'.MATERIAU.NOMRC','L',IANORC)
            CALL JELIRA(MATER//'.MATERIAU.NOMRC','LONMAX',NBRC,KBID)
            DO 60,IRC=1,NBRC
              NOMRC=ZK16(IANORC-1+IRC)(1:10)
              IF (NOMRC.EQ.'ELAS_COQUE') THEN
                REPK='OUI'
                GOTO 90

              ELSEIF (NOMRC.EQ.'THER_COQUE') THEN
                REPK='OUI'
                GOTO 90

              ELSEIF (NOMRC.EQ.'ELAS_ORTH') THEN
                REPK='OUI'
                GOTO 90

              ELSEIF (NOMRC.EQ.'THER_ORTH') THEN
                REPK='OUI'
                GOTO 90

              ELSEIF (NOMRC.EQ.'ELAS_COQMU') THEN
                REPK='OUI'
                GOTO 90

              ELSEIF (NOMRC.EQ.'THER_COQMU') THEN
                REPK='OUI'
                GOTO 90

              ENDIF
   60       CONTINUE
   70     CONTINUE
   80   CONTINUE
   90   CONTINUE


      ELSEIF (QUESTI.EQ.'THER_F_INST') THEN
C     --------------------------------------
        REPK='NON'
        CALL JEVEUO(NOMOB//'.CHAMP_MAT .VALE','L',IAVALE)
        CALL JELIRA(NOMOB//'.CHAMP_MAT .VALE','LONMAX',NMAT,KBID)
        CALL JEVEUO(NOMOB//'.CHAMP_MAT .DESC','L',JDESC)
        NBZONE=ZI(JDESC+2)
C
        DO 140 IZONE=1,NBZONE
          DO 130 IMAX=1,NBMAX
            I=(IZONE-1)*NBMAX+IMAX
            MATER=ZK8(IAVALE-1+I)
            IF (MATER.EQ.' ')GOTO 140
            IF (MATER.EQ.'TREF=>')GOTO 140

            CALL JEVEUO(MATER//'.MATERIAU.NOMRC','L',IANORC)
            CALL JELIRA(MATER//'.MATERIAU.NOMRC','LONMAX',NBRC,KBID)
            DO 110,IRC=1,NBRC
              NOMRC=ZK16(IANORC-1+IRC)(1:10)
C
C            -- SI LE MATERIAU EST ISSU DE LA COMMANDE DEFI_COQU_MULT :
              IF (NOMRC(5:10).EQ.'_COQMU')GOTO 120
C
              IF (NOMRC(1:4).NE.'THER')GOTO 110
              CALL JEVEUO(MATER//'.'//NOMRC//'.VALK','L',IAVALK)
              CALL JELIRA(MATER//'.'//NOMRC//'.VALK','LONUTI',N1,KBID)
              CALL JELIRA(MATER//'.'//NOMRC//'.VALR','LONUTI',NR,KBID)
              CALL JELIRA(MATER//'.'//NOMRC//'.VALC','LONUTI',NC,KBID)
              NF=(N1-NR-NC)/2
              DO 100,IF=1,NF
                NOMF=ZK8(IAVALK-1+NR+NC+NF+IF)
                CALL JEVEUO(NOMF//'           .PROL','L',IAPROL)
                IF (ZK24(IAPROL-1+1).EQ.'NAPPE') THEN
C              -- CAS D'UNE FONCTION A 2 VARIABLES :
                  IF (ZK24(IAPROL-1+3).EQ.'INST')REPK='OUI'
                  IF (ZK24(IAPROL-1+7).EQ.'INST')REPK='OUI'
                ELSE
C              -- CAS D'UNE FONCTION A 1 VARIABLE :
                  IF (ZK24(IAPROL-1+3).EQ.'INST')REPK='OUI'
                ENDIF
  100         CONTINUE
  110       CONTINUE
  120       CONTINUE
  130     CONTINUE
  140   CONTINUE


C     -- CETTE QUESTION N'EXISTE PLUS. IL NE FAUT PAS L'UTILISER.
C        JE LA CONSERVE JUSTE LE TEMPS DE FAIRE MA RESTIT LA MEME
C        SEMAINE QUE SEBASTIEN MEUNIER QUI MODIFIE VECTME.F
      ELSEIF (QUESTI.EQ.'ELAS_F_TEMP') THEN
C     --------------------------------------
        REPK='???'
        REPI=-99999


      ELSEIF (QUESTI.EQ.'ELAS_FO') THEN
C     --------------------------------------
        REPK='NON'
        CALL JEVEUO(NOMOB//'.CHAMP_MAT .VALE','L',IAVALE)
        CALL JELIRA(NOMOB//'.CHAMP_MAT .VALE','LONMAX',NMAT,KBID)
        CALL JEVEUO(NOMOB//'.CHAMP_MAT .DESC','L',JDESC)
        NBZONE=ZI(JDESC+2)
C
        DO 190 IZONE=1,NBZONE
          DO 180 IMAX=1,NBMAX
            I=(IZONE-1)*NBMAX+IMAX
            MATER=ZK8(IAVALE-1+I)
            IF (MATER.EQ.' ')GOTO 190
            IF (MATER.EQ.'TREF=>')GOTO 190

            CALL JEVEUO(MATER//'.MATERIAU.NOMRC','L',IANORC)
            CALL JELIRA(MATER//'.MATERIAU.NOMRC','LONMAX',NBRC,KBID)
            DO 160,IRC=1,NBRC
              NOMRC=ZK16(IANORC-1+IRC)(1:10)
C
C            -- SI LE MATERIAU EST ISSU DE LA COMMANDE DEFI_COQU_MULT :
              IF (NOMRC(5:10).EQ.'_COQMU')GOTO 170
C
              IF (NOMRC(1:4).NE.'ELAS')GOTO 160
              CALL JEVEUO(MATER//'.'//NOMRC//'.VALK','L',IAVALK)
              CALL JELIRA(MATER//'.'//NOMRC//'.VALK','LONUTI',N1,KBID)
              CALL JELIRA(MATER//'.'//NOMRC//'.VALR','LONUTI',NR,KBID)
              CALL JELIRA(MATER//'.'//NOMRC//'.VALC','LONUTI',NC,KBID)
              NF=(N1-NR-NC)/2
              DO 150,IF=1,NF
                NOMF=ZK8(IAVALK-1+NR+NC+NF+IF)
                CALL JEVEUO(NOMF//'           .PROL','L',IAPROL)
                IF (ZK24(IAPROL-1+1).EQ.'CONSTANT') THEN
C                  -- CAS D'UNE FONCTION CONSTANTE :
                ELSE
C                  -- CAS D'UNE FONCTION VARIABLE :
                  REPK='OUI'
                ENDIF
  150         CONTINUE
  160       CONTINUE
  170       CONTINUE
  180     CONTINUE
  190   CONTINUE


      ELSEIF (QUESTI.EQ.'EXI_VARC') THEN
C     --------------------------------------
        REPK='NON'
        CALL JEEXIN(NOMOB//'.CVRCVARC',IRET)
        IF (IRET.NE.0) THEN
          REPK='OUI'
        ENDIF


      ELSE
C     --------------------------------------
        IERD=1
      ENDIF
C
  200 CONTINUE
      REPKZ=REPK
      CALL JEDEMA()
      END
