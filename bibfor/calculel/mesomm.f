      SUBROUTINE MESOMM(CHAMP,LONG,VI,VR,VC,NBMAIL,NUMAIL)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) CHAMP
      INTEGER LONG,VI(*),NBMAIL,NUMAIL(*)
      REAL*8 VR(*)
      COMPLEX*16 VC(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C ----------------------------------------------------------------------
C     BUT :  FAIRE LA "SOMME" D'UN CHAM_ELEM (OU D'UN RESUELEM)
C                  OU D'UNE PARTIE D'UN CHAM_ELEM
C            (NOTION D'INTEGRALE DU CHAMP SUR LE MODELE)
C            LA SEULE CONTRAINTE EST QUE TOUS LES TYPE_ELEMENT DU LIGREL
C            CONNAISSENT LA GRANDEUR AVEC LA MEME LONGUEUR CUMULEE :

C            L'EXEMPLE SUIVANT SERA TRAITE PAR LA ROUTINE, ALORS QUE
C            SA SIGNIFICATION EST PROBABLEMENT DOUTEUSE ...
C              TRI3 :  E 2 IDEN 3 ....
C              SEG2 :  E 3 IDEN 2 ....
C              POI1 :  E 6 IDEN 1 ....

C IN  : CHAMP  :  NOM DU CHAMP A SOMMER
C IN  : LONG   :  LONGUEUR DES VECTEURS VI VR OU VC
C IN  : NBMAIL :  = 0   , CALCUL SUR TOUT LE CHAM_ELEM
C                 SINON , CALCUL SUR UNE PARTIE DU CHAM_ELEM
C IN  : NUMAIL :  NUMERO DES MAILLES
C OUT : VI     :  VECTEUR CONTENANT LA "SOMME" DU CHAMP SI LA GRANDEUR
C                 EST ENTIERE.
C OUT : VR     :  VECTEUR CONTENANT LA "SOMME" DU CHAMP SI LA GRANDEUR
C                 EST REELLE.
C OUT : VC     :  VECTEUR CONTENANT LA "SOMME" DU CHAMP SI LA GRANDEUR
C                 EST COMPLEXE.
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C     ------------------------------------------------------------------
      INTEGER NBGREL,NBELEM,DIGDEL
      CHARACTER*8 SCALAI
      INTEGER LONGT,NCMPEL,MODE,J,IGD
      REAL*8 RZERO
      CHARACTER*4 CVAL,TYPCH
      CHARACTER*8 SCAL
      CHARACTER*19 CHAMP2,LIGREL
      LOGICAL FIRST
      CALL JEMARQ()

      CHAMP2 = CHAMP
      RZERO = 0.0D0


C     1- ON CALCULE : TYPCH,LIGREL,IGD ET SCAL :
C     -----------------------------------------

      CALL JEEXIN(CHAMP2//'.CELD',IER1)
      CALL JEEXIN(CHAMP2//'.RESL',IER2)
      IF (IER1+IER2.EQ.0) CALL U2MESK('F','CALCULEL3_73',1,CHAMP2)


      IF (IER1.GT.0) THEN
        TYPCH='CHML'
C       -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
        CALL CELVER(CHAMP2,'NBVARI_CST','STOP',IBID)
        CALL CELVER(CHAMP2,'NBSPT_1','STOP',IBID)
        CALL JEVEUO(CHAMP2//'.CELD','L',JCELD)
      ELSE
        TYPCH='RESL'
        CALL JEVEUO(CHAMP2//'.DESC','L',JCELD)
      END IF

      CALL JEVEUO(CHAMP2//'.CELK','L',IACELK)
      LIGREL = ZK24(IACELK-1+1) (1:19)

      IGD = ZI(JCELD-1+1)
      SCAL = SCALAI(IGD)


C     2- ON VERIFIE LES LONGUEURS:
C     ----------------------------
      FIRST = .TRUE.
      NBGR = NBGREL(LIGREL)
      DO 10,J = 1,NBGR
        MODE = ZI(JCELD-1+ZI(JCELD-1+4+J)+2)
        IF (MODE.EQ.0) GO TO 10
        NCMPEL = DIGDEL(MODE)
        ICOEF = MAX(1,ZI(JCELD-1+4))
        NCMPEL = NCMPEL*ICOEF
        IF (FIRST) THEN
          LONGT = NCMPEL
        ELSE
          IF (LONGT.NE.NCMPEL) THEN
            CALL U2MESS('F','CALCULEL3_54')
          END IF
        END IF
        FIRST = .FALSE.
   10 CONTINUE

C     -- ON MET A ZERO LE VECTEUR "VSCAL":
C     ------------------------------------
      IF (LONGT.GT.LONG) THEN
        CALL U2MESS('F','CALCULEL3_55')
      END IF
      DO 20,I = 1,LONGT
        IF (SCAL(1:1).EQ.'I') THEN
          VI(I) = 0
        ELSE IF (SCAL(1:1).EQ.'R') THEN
          VR(I) = RZERO
        ELSE IF (SCAL(1:1).EQ.'C') THEN
          VC(I) = DCMPLX(RZERO,RZERO)
        ELSE
          CALL U2MESK('F','CALCULEL3_74',1,SCAL)
        END IF
   20 CONTINUE

C        -- ON CUMULE :
C        --------------
      IF (TYPCH.EQ.'CHML') THEN
C        -- (CAS DES CHAM_ELEM):
        CALL JEVEUO(CHAMP2//'.CELV','L',IAVALE)
        IF (NBMAIL.LE.0) THEN
          DO 50,J = 1,NBGR
            MODE = ZI(JCELD-1+ZI(JCELD-1+4+J)+2)
            IF (MODE.EQ.0) GO TO 50
            NEL = NBELEM(LIGREL,J)
            IDECGR = ZI(JCELD-1+ZI(JCELD-1+4+J)+8)
            DO 40,K = 1,NEL
              DO 30,I = 1,LONGT
                IF (SCAL(1:1).EQ.'I') THEN
                  VI(I) = VI(I) + ZI(IAVALE-1+IDECGR+ (K-1)*LONGT+I-1)
                ELSE IF (SCAL(1:1).EQ.'R') THEN
                  VR(I) = VR(I) + ZR(IAVALE-1+IDECGR+ (K-1)*LONGT+I-1)
                ELSE IF (SCAL(1:1).EQ.'C') THEN
                  VC(I) = VC(I) + ZC(IAVALE-1+IDECGR+ (K-1)*LONGT+I-1)
                END IF
   30         CONTINUE
   40       CONTINUE
   50     CONTINUE
        ELSE
          CALL JEVEUO(LIGREL//'.LIEL','L',JLIGR)
          DO 90 IM = 1,NBMAIL
            INUM = 0
            DO 80 J = 1,NBGR
              MODE = ZI(JCELD-1+ZI(JCELD-1+4+J)+2)
              IF (MODE.EQ.0) GO TO 80
              NEL = NBELEM(LIGREL,J)
              IDECGR = ZI(JCELD-1+ZI(JCELD-1+4+J)+8)
              DO 70 K = 1,NEL
                IEL = ZI(JLIGR+INUM+K-1)
                IF (IEL.NE.NUMAIL(IM)) GO TO 70
                DO 60 I = 1,LONGT
                  IF (SCAL(1:1).EQ.'I') THEN
                    VI(I) = VI(I) + ZI(IAVALE-1+IDECGR+ (K-1)*LONGT+I-1)
                  ELSE IF (SCAL(1:1).EQ.'R') THEN
                    VR(I) = VR(I) + ZR(IAVALE-1+IDECGR+ (K-1)*LONGT+I-1)
                  ELSE IF (SCAL(1:1).EQ.'C') THEN
                    VC(I) = VC(I) + ZC(IAVALE-1+IDECGR+ (K-1)*LONGT+I-1)
                  END IF
   60           CONTINUE
                GO TO 90
   70         CONTINUE
              INUM = INUM + NEL + 1
   80       CONTINUE
   90     CONTINUE
        END IF

      ELSE IF (TYPCH.EQ.'RESL') THEN
C        -- (CAS DES RESUELEM):
        IF (NBMAIL.LE.0) THEN
          NUMEL1 = 0
          DO 120,J = 1,NBGR
            MODE = ZI(JCELD-1+ZI(JCELD-1+4+J)+2)
            IF (MODE.EQ.0) GO TO 120
            CALL JEVEUO(JEXNUM(CHAMP2//'.RESL',J),'L',IAVALE)
            NCMPEL = DIGDEL(MODE)
            NEL = NBELEM(LIGREL,J)
            NUMEL1 = NUMEL1 + NEL
            DO 110,K = 1,NEL
              DO 100,I = 1,LONGT
                IF (SCAL(1:1).EQ.'I') THEN
                  VI(I) = VI(I) + ZI(IAVALE+ (K-1)*NCMPEL-1+I)
                ELSE IF (SCAL(1:1).EQ.'R') THEN
                  VR(I) = VR(I) + ZR(IAVALE+ (K-1)*NCMPEL-1+I)
                ELSE IF (SCAL(1:1).EQ.'C') THEN
                  VC(I) = VC(I) + ZC(IAVALE+ (K-1)*NCMPEL-1+I)
                END IF
  100         CONTINUE
  110       CONTINUE
            CALL JELIBE(JEXNUM(CHAMP2//'.RESL',J))
  120     CONTINUE
        ELSE
          CALL JEVEUO(LIGREL//'.LIEL','L',JLIGR)
          DO 160 IM = 1,NBMAIL
            INUM = 0
            NUMEL1 = 0
            DO 150 J = 1,NBGR
              MODE = ZI(JCELD-1+ZI(JCELD-1+4+J)+2)
              IF (MODE.EQ.0) GO TO 150
              NCMPEL = DIGDEL(MODE)
              NEL = NBELEM(LIGREL,J)
              NUMEL1 = NUMEL1 + NEL
              DO 140 K = 1,NEL
                IEL = ZI(JLIGR+INUM+K-1)
                IF (IEL.NE.NUMAIL(IM)) GO TO 140
                CALL JEVEUO(JEXNUM(CHAMP2//'.RESL',J),'L',IAVALE)
                DO 130 I = 1,LONGT
                  IF (SCAL(1:1).EQ.'I') THEN
                    VI(I) = VI(I) + ZI(IAVALE+ (K-1)*NCMPEL-1+I)
                  ELSE IF (SCAL(1:1).EQ.'R') THEN
                    VR(I) = VR(I) + ZR(IAVALE+ (K-1)*NCMPEL-1+I)
                  ELSE IF (SCAL(1:1).EQ.'C') THEN
                    VC(I) = VC(I) + ZC(IAVALE+ (K-1)*NCMPEL-1+I)
                  END IF
  130           CONTINUE
                CALL JELIBE(JEXNUM(CHAMP2//'.RESL',J))
                GO TO 160
  140         CONTINUE
              INUM = INUM + NEL + 1
  150       CONTINUE
  160     CONTINUE
        END IF

      END IF

  170 CONTINUE

      CALL JEDEMA()
      END
