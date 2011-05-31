      SUBROUTINE PECHLI(RESU,MODELE,MATE)
      IMPLICIT   NONE
      CHARACTER*8 MODELE
      CHARACTER*24 MATE
      CHARACTER*(*) RESU
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 30/05/2011   AUTEUR DESROCHE X.DESROCHES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================

C     OPERATEUR   POST_ELEM

C     TRAITEMENT DU MOT CLE-FACTEUR "CHAR_LIMITE"

C ----------------------------------------------------------------------
C --------- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

      LOGICAL CHRCST,EXIGEO
      INTEGER I,IBID,IRET,JINST,JPILO
      INTEGER NBORD,JORD,NUMORD
      REAL*8 CHLIM(3),CHMAX(3),INST,ETA,PREC,VALER(3),F0U,M,R8MIEM
      COMPLEX*16 C16B

      CHARACTER*8 CRIT,RESULT,LPAIN(4),LPAOUT(1)
      CHARACTER*8 K8B,TYPARR(4),CHLI(3)
      CHARACTER*16 OPTION,NOPARR(4)
      CHARACTER*24 LIGRMO,CHGEOM,DEPLA,CHTIME
      CHARACTER*24 LCHIN(4),LCHOUT(1),LISORD
      CHARACTER*72 REP
C
C ----------------------------------------------------------------------
C

      CALL JEMARQ()
      LISORD = '&&PECHLI.VECTORDR'
      CHTIME = '&&PECHLI.CH_INST_R'
      F0U = 0


C -- VERIFICATIONS INITIALES

      CALL GETVID(' ','RESULTAT',0,1,1,RESULT,IRET)
      CALL RSEXPA(RESULT,2,'ETA_PILOTAGE',IRET)

      IF (IRET.EQ.0) CALL U2MESK('F','POSTELEM_3',1,RESULT)


C -- EXISTENCE D'UN CHARGEMENT CONSTANT
      CALL GETVTX('CHAR_LIMITE','CHAR_CSTE',1,1,1,REP,IRET)
      CHRCST = REP .EQ. 'OUI'




C -- ECRITURE DE LA TABLE RESULTAT

      TYPARR(1) = 'I'
      TYPARR(2) = 'R'
      TYPARR(3) = 'R'
      TYPARR(4) = 'R'

      NOPARR(1) = 'NUME_ORDRE'
      NOPARR(2) = 'INST'
      NOPARR(3) = 'CHAR_LIMI_SUP'

      IF (CHRCST) THEN
        NOPARR(4) = 'PUIS_CHAR_CSTE'
      ELSE
        NOPARR(4) = 'CHAR_LIMI_ESTIM'
      END IF

      CALL TBCRSD(RESU,'G')
      CALL TBAJPA(RESU,4,NOPARR,TYPARR)



      CALL MEGEOM(MODELE,' ',EXIGEO,CHGEOM)
      LIGRMO = MODELE//'.MODELE'


C -- EXTRACTION DES NUMEROS D'ORDRE DU CALCUL

      CALL GETVR8(' ','PRECISION',1,1,1,PREC,IRET)
      CALL GETVTX(' ','CRITERE',1,1,1,CRIT,IRET)
      CALL RSUTNU(RESULT,' ',0,LISORD,NBORD,PREC,CRIT,IRET)
      IF (IRET.NE.0) CALL U2MESK('F','POSTELEM_1',1,RESULT)
      CALL JEVEUO(LISORD,'L',JORD)


C -- CALCUL DES CHARGES LIMITES AUX DIFFERENTS INSTANTS

      DO 10 I = 1,NBORD
        CALL JEMARQ()
        CALL JERECU('V')

C      EXTRACTION DU CHAMP DE DEPLACEMENT
        NUMORD = ZI(JORD-1+I)
        CALL RSEXCH(RESULT,'DEPL',NUMORD,DEPLA,IRET)
        IF (IRET.NE.0) CALL U2MESI('F','POSTELEM_2',1,NUMORD)


C      CREACTION DE LA CARTE DE L INSTANT DE CALCUL
        CALL RSADPA(RESULT,'L',1,'INST',NUMORD,0,JINST,K8B)
        INST = ZR(JINST)
        CALL MECACT('V',CHTIME,'MODELE',LIGRMO,'INST_R',1,'INST',IBID,
     &              INST,C16B,K8B)


C      CALCUL DES TERMES ELEMENTAIRES
        LPAOUT(1) = 'PECHLI'
        LCHOUT(1) = '&&PECHLI'
        LPAIN(1) = 'PGEOMER'
        LCHIN(1) = CHGEOM
        LPAIN(2) = 'PDEPLAR'
        LCHIN(2) = DEPLA
        LPAIN(3) = 'PMATERC'
        LCHIN(3) = MATE
        LPAIN(4) = 'PTEMPSR'
        LCHIN(4) = CHTIME
        OPTION = 'CHAR_LIMITE'
        CALL CALCUL('S',OPTION,LIGRMO,4,LCHIN,LPAIN,1,LCHOUT,LPAOUT,'V',
     &                 'OUI')


C      SOMMATION DE TOUS LES TERMES ELEMENTAIRES
C       CHLIM(1) : CHAR_LIMI_SUP
C       CHLIM(2) : CHAR_LIMI_ESTIM
C       CHLIM(3) : MAX UTILE AU CALCUL DE CHAR_LIMI_ESTIM

        CALL MESOMM(LCHOUT(1),3,IBID,CHLIM,C16B,0,IBID)

        CHLI(1) = 'CHLI1'
        CHLI(2) = 'CHLI2'
        CHLI(3) = 'CHLI3'
        CALL MEMAX('MAX',LCHOUT(1),'CHLI3',3,CHLI,CHMAX,0,IBID)

C      CALCUL DU CHARGEMENT PERMANENT SI NECESSAIRE
        IF (CHRCST) THEN
          CALL RSADPA(RESULT,'L',1,'ETA_PILOTAGE',NUMORD,0,JPILO,K8B)
          ETA = ZR(JPILO)
          M = 1 + 10** (1-INST)
          F0U = M*CHLIM(2) - ETA
          CHLIM(1) = CHLIM(1) - F0U
        ELSE
          IF(CHMAX(3).LE.R8MIEM()) THEN
            CHLIM(2) = 0.0D0
          ELSE
            CHLIM(2) = CHLIM(2)/CHMAX(3)
          END IF
        END IF


C      ECRITURE DANS LA TABLE RESU DE LA CHARGE LIMITE
        VALER(1) = INST
        VALER(2) = CHLIM(1)
        IF (CHRCST) THEN
          VALER(3) = F0U
        ELSE
          VALER(3) = CHLIM(2)
        END IF
        CALL TBAJLI(RESU,4,NOPARR,NUMORD,VALER,C16B,K8B,0)

        CALL JEDEMA()
   10 CONTINUE

      CALL JEDETC('V','&&PECHLI',1)
      CALL JEDEMA()
      END
