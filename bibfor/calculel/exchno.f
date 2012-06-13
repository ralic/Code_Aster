      SUBROUTINE EXCHNO(IMODAT,IPARG)
      IMPLICIT NONE

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE                            VABHHTS J.PELLET
C     ARGUMENTS:
C     ----------
      INCLUDE 'jeveux.h'
      INTEGER IMODAT,IPARG
C ----------------------------------------------------------------------
C     ENTREES:
C        IMODAT  : INDICE DANS LA COLLECTION MODELOC
C        IGR    : NUMERO DU GREL A TRAITER.
C     SORTIES:
C       ECRITURE DANS LE CHAMP LOCAL
C ----------------------------------------------------------------------
      INTEGER IGD,NEC,NCMPMX,IACHIN,IACHLO,IICHIN,IANUEQ,LPRNO
      INTEGER ILCHLO,ITYPGD
      COMMON /CAII01/IGD,NEC,NCMPMX,IACHIN,IACHLO,IICHIN,IANUEQ,LPRNO,
     &       ILCHLO,ITYPGD
      CHARACTER*8 TYPEGD
      COMMON /CAKK02/TYPEGD
      INTEGER IAOPTT,LGCO,IAOPMO,ILOPMO,IAOPNO,ILOPNO,IAOPDS,IAOPPA,
     &        NPARIO,NPARIN,IAMLOC,ILMLOC,IADSGD
      COMMON /CAII02/IAOPTT,LGCO,IAOPMO,ILOPMO,IAOPNO,ILOPNO,IAOPDS,
     &       IAOPPA,NPARIO,NPARIN,IAMLOC,ILMLOC,IADSGD
      INTEGER IAMACO,ILMACO,IAMSCO,ILMSCO,IALIEL,ILLIEL
      COMMON /CAII03/IAMACO,ILMACO,IAMSCO,ILMSCO,IALIEL,ILLIEL
      INTEGER IACHII,IACHIK,IACHIX
      COMMON /CAII04/IACHII,IACHIK,IACHIX
      INTEGER        NBGR,IGR,NBELGR,JCTEAT,LCTEAT,IAWLOC,IAWLO2,IAWTYP
      COMMON /CAII06/NBGR,IGR,NBELGR,JCTEAT,LCTEAT,IAWLOC,IAWLO2,IAWTYP

C     FONCTIONS EXTERNES:
C     -------------------
      INTEGER NUMAIL,NUMGLM,NUMGLS

C     VARIABLES LOCALES:
C     ------------------
      INTEGER IMA,INO,NNO,LONG,NUGL,NUM,JPARAL,IRET,IEL
      INTEGER DESC,PRNO1,PRNO2,MODLOC,ITYPLO
      INTEGER DEB1,DEB2,IDG1,IDG2,NBPT,NBPT2,LGCATA,NCMP
      INTEGER IAUX1,K,IEC,DEBUGR
      LOGICAL LPARAL

      LOGICAL DIFF,MOYENN

C     -- FONCTIONS FORMULES :
C     NUMAIL(IGR,IEL)=NUMERO DE LA MAILLE ASSOCIEE A L'ELEMENT IEL
      NUMAIL(IGR,IEL)=ZI(IALIEL-1+ZI(ILLIEL+IGR-1)+IEL-1)
C     NUMGLM(IMA,INO)=NUMERO GLOBAL DU NOEUD INO DE LA MAILLE IMA
C                     IMA ETANT UNE MAILLE DU MAILLAGE.
      NUMGLM(IMA,INO)=ZI(IAMACO-1+ZI(ILMACO+IMA-1)+INO-1)
C     NUMGLS(IMA,INO)=NUMERO GLOBAL DU NOEUD INO DE LA MAILLE IMA
C                     IMA ETANT UNE MAILLE SUPPLEMENTAIRE DU LIGREL
      NUMGLS(IMA,INO)=ZI(IAMSCO-1+ZI(ILMSCO+IMA-1)+INO-1)
C DEB-------------------------------------------------------------------

C     PARALLELE OR NOT ?
C     -------------------------
      CALL JEEXIN('&CALCUL.PARALLELE',IRET)
      IF (IRET.NE.0) THEN
        LPARAL=.TRUE.
        CALL JEVEUO('&CALCUL.PARALLELE','L',JPARAL)
      ELSE
        LPARAL=.FALSE.
      ENDIF

      DESC=ZI(IACHII-1+11*(IICHIN-1)+4)
      NUM=ZI(DESC-1+2)
      MODLOC=IAMLOC-1+ZI(ILMLOC-1+IMODAT)
      ITYPLO=ZI(MODLOC-1+1)
      DEBUGR=ZI(IAWLO2-1+5*(NBGR*(IPARG-1)+IGR-1)+5)
      LGCATA=ZI(IAWLO2-1+5*(NBGR*(IPARG-1)+IGR-1)+2)

      CALL ASSERT(ITYPLO.LT.4)

C     1-  CAS: CHNO -> ELGA :
C     -----------------------
C     LE CAS ITYPLO=3 N EST PAS PREVU : DEVELOPPEMENT A FAIRE ...
      CALL ASSERT(ITYPLO.NE.3)

C     2-  CAS: CHNO -> ELNO :
C         CAS: CHNO -> ELEM (MOYENNE)
C     --------------------------------
      IF ((ITYPLO.EQ.2) .OR. (ITYPLO.EQ.1)) THEN
        IF (ITYPLO.EQ.2) THEN
          MOYENN=.FALSE.
        ELSE
          MOYENN=.TRUE.
        ENDIF


C       2.1 ON CHERCHE NNO SUR LE 1ER ELEMENT :
C       ---------------------------------------
        IMA=NUMAIL(IGR,1)
        CALL ASSERT(IMA.NE.0)
        IF (IMA.GT.0) THEN
          NNO=ZI(ILMACO-1+IMA+1)-ZI(ILMACO-1+IMA)
        ELSE
          NNO=ZI(ILMSCO-1-IMA+1)-ZI(ILMSCO-1-IMA)-1
        ENDIF


C       2.2 ON RECUPERE LE DEBUT DU DESCRIPTEUR GRANDEUR :
C       --------------------------------------------------
        NBPT=ZI(MODLOC-1+4)
        NBPT2=MOD(NBPT,10000)
        IF (NBPT.NE.NBPT2) THEN
          DIFF=.TRUE.
        ELSE
          DIFF=.FALSE.
          IDG2=5
        ENDIF

C       MOYENN => (NBPT2=1)
        CALL ASSERT((.NOT.MOYENN) .OR. (NBPT2.EQ.1))

C       .NOT.MOYENN => (NBPT2=NNO)
        CALL ASSERT(MOYENN .OR. (NBPT2.EQ.NNO))


C       2.3 SI MOYENN, IL FAUT METTRE A ZERO LE CHAMP LOCAL
C           (POUR POUVOIR CUMULER)
C       --------------------------------------------------
        IF (MOYENN) THEN
          NCMP=LGCATA
          IF (TYPEGD.EQ.'R') THEN
            IF (LPARAL) THEN
              DO 20 IEL=1,NBELGR
                IF (ZL(JPARAL-1+IEL)) THEN
                  IAUX1=IACHLO+DEBUGR-1+(IEL-1)*NCMP
                  DO 10 K=1,NCMP
                    ZR(IAUX1-1+K)=0.D0
   10             CONTINUE
                ENDIF
   20         CONTINUE
            ELSE
              DO 30,K=1,NBELGR*NCMP
                ZR(IACHLO+DEBUGR-1-1+K)=0.D0
   30         CONTINUE
            ENDIF
          ELSEIF (TYPEGD.EQ.'C') THEN
            IF (LPARAL) THEN
              DO 50 IEL=1,NBELGR
                IF (ZL(JPARAL-1+IEL)) THEN
                  IAUX1=IACHLO+DEBUGR-1+(IEL-1)*NCMP
                  DO 40 K=1,NCMP
                    ZC(IAUX1-1+K)=(0.D0,0.D0)
   40             CONTINUE
                ENDIF
   50         CONTINUE
            ELSE
              DO 60,K=1,NBELGR*NCMP
                ZC(IACHLO+DEBUGR-1-1+K)=(0.D0,0.D0)
   60         CONTINUE
            ENDIF
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF
        ENDIF



C        ---SI C'EST 1 CHAMP A REPRESENTATION CONSTANTE (NUM<0):
C        -------------------------------------------------------
        IF (NUM.LT.0) THEN
          LONG=-NUM
          DEB2=DEBUGR
          DO 90,IEL=1,NBELGR
            IF (LPARAL) THEN
              IF (.NOT.ZL(JPARAL-1+IEL)) THEN
                DEB2=DEB2+LGCATA
                GOTO 90
              ENDIF
            ENDIF
            IMA=NUMAIL(IGR,IEL)
            CALL ASSERT(IMA.NE.0)
            DO 80 INO=1,NNO
              IF (DIFF)IDG2=5+NEC*(INO-1)
              IF (IMA.GT.0) THEN
                NUGL=NUMGLM(IMA,INO)
              ELSE
                NUGL=NUMGLS(-IMA,INO)
              ENDIF
              DEB1=(NUGL-1)*LONG+1

              IF (NUGL.GT.0) THEN
                CALL TRIGD(ZI(DESC-1+3),DEB1,ZI(MODLOC-1+IDG2),DEB2,
     &                     MOYENN,INO,NNO)
              ELSE
C                 ON VERIFIE QUE LE MODLOC AFFIRME NCMP=0:
                DO 70,IEC=1,NEC
                  IF (ZI(MODLOC-1+IDG2-1+IEC).NE.0) THEN
                    CALL U2MESS('F','CALCULEL2_52')
                  ENDIF
   70           CONTINUE
              ENDIF
   80       CONTINUE
   90     CONTINUE
        ELSE

C        --- C'EST 1 CHAMP AVEC PROFIL_NOEUD:
C        ------------------------------------
          PRNO1=ZI(IACHII-1+11*(IICHIN-1)+8)
          PRNO2=ZI(IACHII-1+11*(IICHIN-1)+9)
          DEB2=DEBUGR
          DO 110,IEL=1,NBELGR
            IF (LPARAL) THEN
              IF (.NOT.ZL(JPARAL-1+IEL)) THEN
                DEB2=DEB2+LGCATA
                GOTO 110
              ENDIF
            ENDIF
            IMA=NUMAIL(IGR,IEL)
            CALL ASSERT(IMA.NE.0)
            DO 100 INO=1,NNO
              IF (DIFF)IDG2=5+NEC*(INO-1)
              IF (IMA.GT.0) THEN
                NUGL=NUMGLM(IMA,INO)
              ELSE
                NUGL=NUMGLS(-IMA,INO)
              ENDIF
              DEB1=(ABS(NUGL)-1)*(NEC+2)+1
              IDG1=(ABS(NUGL)-1)*(NEC+2)+3

              IF (NUGL.GT.0) THEN
                CALL TRIGD(ZI(PRNO1-1+IDG1),ZI(PRNO1-1+DEB1),
     &                     ZI(MODLOC-1+IDG2),DEB2,MOYENN,INO,NNO)
              ELSE
                CALL TRIGD(ZI(PRNO2-1+IDG1),ZI(PRNO2-1+DEB1),
     &                     ZI(MODLOC-1+IDG2),DEB2,MOYENN,INO,NNO)
              ENDIF
  100       CONTINUE

  110     CONTINUE
        ENDIF


        IF (MOYENN) THEN
          NCMP=LGCATA
          IF (TYPEGD.EQ.'R') THEN
            IF (LPARAL) THEN
              DO 130 IEL=1,NBELGR
                IF (ZL(JPARAL-1+IEL)) THEN
                  IAUX1=IACHLO+DEBUGR-1+(IEL-1)*NCMP
                  DO 120 K=1,NCMP
                    ZR(IAUX1-1+K)=ZR(IAUX1-1+K)/DBLE(NNO)
  120             CONTINUE
                ENDIF
  130         CONTINUE
            ELSE
              DO 140,K=1,NBELGR*NCMP
                ZR(IACHLO-1+K)=ZR(IACHLO+DEBUGR-1-1+K)/DBLE(NNO)
  140         CONTINUE
            ENDIF
          ELSEIF (TYPEGD.EQ.'C') THEN
            IF (LPARAL) THEN
              DO 160 IEL=1,NBELGR
                IF (ZL(JPARAL-1+IEL)) THEN
                  IAUX1=IACHLO+DEBUGR-1+(IEL-1)*NCMP
                  DO 150 K=1,NCMP
                    ZC(IAUX1-1+K)=ZC(IAUX1-1+K)/DBLE(NNO)
  150             CONTINUE
                ENDIF
  160         CONTINUE
            ELSE
              DO 170,K=1,NBELGR*NCMP
                ZC(IACHLO-1+K)=ZC(IACHLO+DEBUGR-1-1+K)/DBLE(NNO)
  170         CONTINUE
            ENDIF
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF
        ENDIF

      ENDIF


      END
