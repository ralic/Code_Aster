      SUBROUTINE EXCHNO(CHIN,LIGREL,IMODAT,IPARG)
      IMPLICIT REAL*8 (A-H,O-Z)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 09/10/2002   AUTEUR VABHHTS J.PELLET 
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
C RESPONSABLE                            VABHHTS J.PELLET
C     ARGUMENTS:
C     ----------
      CHARACTER*19 CHIN,LIGREL
      INTEGER IMODAT,IPARG
C ----------------------------------------------------------------------
C     ENTREES:
C        IMODAT  : INDICE DANS LA COLLECTION MODELOC
C        CHIN   : NOM DU CHAMP GLOBAL
C        LIGREL : NOM DU LIGREL
C        IGR    : NUMERO DU GREL A TRAITER.
C     SORTIES:
C       ECRITURE DANS LE CHAMP LOCAL
C ----------------------------------------------------------------------
      COMMON /CAII01/IGD,NEC,NCMPMX,IACHIN,IACHLO,IICHIN,IANUEQ,LPRNO,
     &               ILCHLO
      CHARACTER*8 TYPEGD
      COMMON /CAKK02/TYPEGD
      COMMON /CAII02/IAOPTT,LGCO,IAOPMO,ILOPMO,IAOPNO,ILOPNO,IAOPDS,
     &       IAOPPA,NPARIO,NPARIN,IAMLOC,ILMLOC,IADSGD
      COMMON /CAII03/IAMACO,ILMACO,IAMSCO,ILMSCO,IALIEL,ILLIEL
      COMMON /CAII04/IACHII,IACHIK,IACHIX
      COMMON /CAII06/IAWLOC,IAWTYP,NBELGR,IGR
      COMMON /CAII08/IEL

C     FONCTIONS EXTERNES:
C     -------------------
      INTEGER NUMAIL,NUMGLM,NUMGLS
      CHARACTER*32 JEXNUM,JEXNOM

C     VARIABLES LOCALES:
C     ------------------
      INTEGER NEC,IMA,INO,NNO,LONG,NUGL,NUM
      INTEGER DESC,PRNO1,PRNO2,MODLOC,ITYPLO
      INTEGER DEB1,DEB2,IDG1,IDG2
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL,DIFF,MOYENN
      CHARACTER*8 ZK8,NOMA,NOMNO
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80

C     -- FONCTIONS FORMULES :
C     NUMAIL(IGR,IEL)=NUMERO DE LA MAILLE ASSOCIEE A L'ELEMENT IEL
      NUMAIL(IGR,IEL) = ZI(IALIEL-1+ZI(ILLIEL+IGR-1)+IEL-1)
C     NUMGLM(IMA,INO)=NUMERO GLOBAL DU NOEUD INO DE LA MAILLE IMA
C                     IMA ETANT UNE MAILLE DU MAILLAGE.
      NUMGLM(IMA,INO) = ZI(IAMACO-1+ZI(ILMACO+IMA-1)+INO-1)
C     NUMGLS(IMA,INO)=NUMERO GLOBAL DU NOEUD INO DE LA MAILLE IMA
C                     IMA ETANT UNE MAILLE SUPPLEMENTAIRE DU LIGREL
      NUMGLS(IMA,INO) = ZI(IAMSCO-1+ZI(ILMSCO+IMA-1)+INO-1)
C DEB-------------------------------------------------------------------

      DESC = ZI(IACHII-1+11* (IICHIN-1)+4)
      NUM = ZI(DESC-1+2)
      MODLOC = IAMLOC - 1 + ZI(ILMLOC-1+IMODAT)
      ITYPLO = ZI(MODLOC-1+1)


C     1-  CAS: CHNO -> ELGA :
C     -----------------------
      IF (ITYPLO.EQ.3) THEN
        CALL UTMESS('F',' EXCHNO','A FAIRE ...')


C     2-  CAS: CHNO -> ASSE :
C     -----------------------
      ELSE IF (ITYPLO.GE.4) THEN
        CALL UTMESS('F',' EXCHNO','IMPOSSIBLE')


C     3-  CAS: CHNO -> ELNO :
C         CAS: CHNO -> ELEM (MOYENNE)
C     --------------------------------
      ELSE IF ((ITYPLO.EQ.2).OR.(ITYPLO.EQ.1)) THEN
        IF (ITYPLO.EQ.2) THEN
          MOYENN=.FALSE.
        ELSE
          MOYENN=.TRUE.
        END IF


C       4.1 ON CHERCHE NNO SUR LE 1ER ELEMENT :
C       ---------------------------------------
        IMA = NUMAIL(IGR,1)
        IF (IMA.EQ.0) CALL UTMESS('F',' EXCHNO','1')
        IF (IMA.GT.0) THEN
          NNO = ZI(ILMACO-1+IMA+1) - ZI(ILMACO-1+IMA)
        ELSE
          NNO = ZI(ILMSCO-1-IMA+1) - ZI(ILMSCO-1-IMA) - 1
        END IF


C       4.2 ON RECUPERE LE DEBUT DU DESCRIPTEUR GRANDEUR :
C       --------------------------------------------------
        NBPT = ZI(MODLOC-1+4)
        NBPT2=MOD(NBPT,10000)
        IF (NBPT.NE.NBPT2) THEN
          DIFF = .TRUE.
        ELSE
          DIFF = .FALSE.
          IDG2 = 5
        END IF

C       MOYENN => (NBPT2=1)
        CALL ASSERT((.NOT.MOYENN).OR.(NBPT2.EQ.1))

C       .NOT.MOYENN => (NBPT2=NNO)
        CALL ASSERT(MOYENN.OR.(NBPT2.EQ.NNO))


C       4.3 SI MOYENN, IL FAUT METTRE A ZERO LE CHAMP LOCAL
C           (POUR POUVOIR CUMULER)
C       --------------------------------------------------
        IF (MOYENN) THEN
          LGCATA = ZI(IAWLOC-1+7* (IPARG-1)+4)
          NCMP=LGCATA
          IF (TYPEGD.EQ.'R') THEN
            DO 71,K=1,NBELGR *NCMP
              ZR(IACHLO-1+K) = 0.D0
71          CONTINUE
          ELSE IF (TYPEGD.EQ.'C') THEN
            DO 72,K=1,NBELGR *NCMP
              ZC(IACHLO-1+K) = (0.D0,0.D0)
72          CONTINUE
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
        END IF



C        ---SI C'EST 1 CHAMP A REPRESENTATION CONSTANTE (NUM<0):
C        -------------------------------------------------------
        IF (NUM.LT.0) THEN
          LONG = -NUM
          DEB2 = 1
          DO 30,IEL = 1,NBELGR
            IMA = NUMAIL(IGR,IEL)
            IF (IMA.EQ.0) CALL UTMESS('F',' EXCHNO','2')
            DO 20 INO = 1,NNO
              IF (DIFF) IDG2 = 5 + NEC* (INO-1)
              IF (IMA.GT.0) THEN
                NUGL = NUMGLM(IMA,INO)
              ELSE
                NUGL = NUMGLS(-IMA,INO)
              END IF
              DEB1 = (NUGL-1)*LONG + 1

              IF (NUGL.GT.0) THEN
                CALL TRIGD(ZI(DESC-1+3),DEB1,ZI(MODLOC-1+IDG2),
     &          DEB2,MOYENN,INO,NNO)
              ELSE
C                 ON VERIFIE QUE LE MODLOC AFFIRME NCMP=0:
                DO 10,IEC = 1,NEC
                  IF (ZI(MODLOC-1+IDG2-1+IEC).NE.0) THEN
                    CALL UTMESS('F','EXCHNO','PROBLEME NOEUD TARDIF '//
     &                        'POUR UN CHAMP A REPRESENTATION CONSTANTE'
     &                          )
                  END IF
   10           CONTINUE
              END IF
   20       CONTINUE
   30     CONTINUE
        ELSE

C        --- C'EST 1 CHAMP AVEC PROFIL_NOEUD:
C        ------------------------------------
          PRNO1 = ZI(IACHII-1+11* (IICHIN-1)+8)
          PRNO2 = ZI(IACHII-1+11* (IICHIN-1)+9)
          DEB2 = 1
          DO 50,IEL = 1,NBELGR
            IMA = NUMAIL(IGR,IEL)
            IF (IMA.EQ.0) CALL UTMESS('F',' EXCHNO','3')
            DO 40 INO = 1,NNO
              IF (DIFF) IDG2 = 5 + NEC* (INO-1)
              IF (IMA.GT.0) THEN
                NUGL = NUMGLM(IMA,INO)
              ELSE
                NUGL = NUMGLS(-IMA,INO)
              END IF
              DEB1 = (ABS(NUGL)-1)* (NEC+2) + 1
              IDG1 = (ABS(NUGL)-1)* (NEC+2) + 3

              IF (NUGL.GT.0) THEN
                CALL TRIGD(ZI(PRNO1-1+IDG1),ZI(PRNO1-1+DEB1),
     &                     ZI(MODLOC-1+IDG2),DEB2,MOYENN,INO,NNO)
              ELSE
                CALL TRIGD(ZI(PRNO2-1+IDG1),ZI(PRNO2-1+DEB1),
     &                     ZI(MODLOC-1+IDG2),DEB2,MOYENN,INO,NNO)
              END IF
   40       CONTINUE

   50     CONTINUE
        END IF


        IF (MOYENN) THEN
          LGCATA = ZI(IAWLOC-1+7* (IPARG-1)+4)
          NCMP=LGCATA
          IF (TYPEGD.EQ.'R') THEN
            DO 171,K=1,NBELGR *NCMP
              ZR(IACHLO-1+K) = ZR(IACHLO-1+K)/DBLE(NNO)
171         CONTINUE
          ELSE IF (TYPEGD.EQ.'C') THEN
            DO 172,K=1,NBELGR *NCMP
              ZC(IACHLO-1+K) = ZC(IACHLO-1+K)/DBLE(NNO)
172         CONTINUE
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
        END IF

      END IF


   70 CONTINUE
      END
