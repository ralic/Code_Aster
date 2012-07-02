      SUBROUTINE MEMOY ( CHAMPA, NCPA, CHAMPB, NCPB,VR,NBMAIL,NUMAIL)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*)     CHAMPA, CHAMPB
      INTEGER           NCPA, NCPB, NBMAIL, NUMAIL(*)
      REAL*8            VR(2)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C ----------------------------------------------------------------------
C     BUT :  FAIRE LA "MOYENNE" DE LA COMPOSANTE NCPA D'UN CHAM_ELEM
C            EN PONDERANT PAR LA COMPOSANTE NCPB D'UN AUTRE CHAM_ELEM
C            LA SEULE CONTRAINTE EST QUE TOUS LES TYPE_ELEMENT DU LIGREL
C            CONNAISSENT LA GRANDEUR AVEC LA MEME LONGUEUR CUMULEE :
C
C IN  : CHAMPA :  NOM DU CHAMP A MOYENNER
C IN  : CHAMPB :  NOM DU CHAMP DE PONDERATION
C IN  : NCPA   :  NUMERO DE COMPOSANTE DU CHAMP A
C IN  : NCPB   :  NUMERO DE COMPOSANTE DU CHAMP B
C IN  : NBMAIL :  = 0   , CALCUL SUR TOUT LE CHAM_ELEM
C                 SINON , CALCUL SUR UNE PARTIE DU CHAM_ELEM
C IN  : NUMAIL :  NUMERO DES MAILLES
C OUT : VR     :  VECTEUR RESULTAT
C
C       LE RESULTAT EST DONNE SOUS LA FORME DE DEUX COMPOSANTES
C          (1) VALEUR DE LA MOYENNE
C          (2) SOMME DES VALEURS DU CHAMP DE PONDERATION
C
C ----------------------------------------------------------------------
C     ------------------------------------------------------------------
      INTEGER      NBGREL, NBELEM, DIGDEL
      CHARACTER*8  SCALAI
      INTEGER      LONGT1, LONGT2, NCMPEL, MODE, J, IGD1, IGD2
      REAL*8       RZERO
      CHARACTER*8  SCAL1, SCAL2
      CHARACTER*19 CHAMP1, CHAMP2, LIGREL, LIGRE1, LIGRE2
      LOGICAL      FIRST
C
C-----------------------------------------------------------------------
      INTEGER I ,IACELK ,IAVAL1 ,IAVAL2 ,IBID ,ICOEF ,IDECG1 
      INTEGER IDECG2 ,IEL ,IM ,INUM ,JCELD1 ,JCELD2 ,JLIGR 
      INTEGER K ,MODE1 ,MODE2 ,NBGR ,NEL 
C-----------------------------------------------------------------------
      CALL JEMARQ()
      CHAMP1 = CHAMPA
      CHAMP2 = CHAMPB
      RZERO = 0.0D0
      IF ((NCPA.LE.0).OR.(NCPB.LE.0)) THEN
         CALL U2MESS('F','CALCULEL3_57')
      ENDIF
C
C     -- ON RETROUVE LE NOM DU LIGREL:
C     --------------------------------
      CALL JEVEUO (CHAMP1//'.CELK','L',IACELK)
      LIGRE1 = ZK24(IACELK-1+1)(1:19)
C
      CALL JEVEUO (CHAMP2//'.CELK','L',IACELK)
      LIGRE2 = ZK24(IACELK-1+1)(1:19)
C
      IF (LIGRE1.NE.LIGRE2) THEN
         CALL U2MESS('F','CALCULEL3_58')
      ENDIF
      LIGREL = LIGRE1
C
      CALL JEEXIN (CHAMP1//'.CELD',IBID)
      IF (IBID.EQ.0) CALL U2MESK('F','CALCULEL3_59',1,CHAMP1)

C     -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
      CALL CELVER ( CHAMP1, 'NBVARI_CST', 'STOP', IBID )
      CALL CELVER ( CHAMP1, 'NBSPT_1',    'STOP', IBID )

      CALL JEVEUO (CHAMP1//'.CELD','L',JCELD1)
      IGD1  = ZI(JCELD1-1+1)
      SCAL1 = SCALAI(IGD1)
      IF (SCAL1(1:1).NE.'R') THEN
         CALL U2MESS('F','CALCULEL3_53')
      ENDIF
C
      CALL JEEXIN (CHAMP2//'.CELD',IBID)
      IF (IBID.EQ.0) CALL U2MESK('F','CALCULEL3_59',1,CHAMP2)

C     -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
      CALL CELVER ( CHAMP2, 'NBVARI_CST', 'STOP', IBID )
      CALL CELVER  (CHAMP2, 'NBSPT_1',    'STOP', IBID )

      CALL JEVEUO (CHAMP2//'.CELD','L',JCELD2)
      IGD2  = ZI(JCELD2-1+1)
      SCAL2 = SCALAI(IGD2)
      IF (SCAL2(1:1).NE.'R') THEN
         CALL U2MESS('F','CALCULEL3_53')
      ENDIF
C
C     -- ON VERIFIE LES LONGUEURS DE CHAQUE CHAMP:
C     --------------------------------------------
      FIRST = .TRUE.
      NBGR  = NBGREL(LIGREL)
      DO 1 ,J = 1,NBGR
         MODE=ZI(JCELD1-1+ZI(JCELD1-1+4+J) +2)
         IF (MODE.EQ.0) GOTO 1
         NCMPEL = DIGDEL(MODE)
         ICOEF=MAX(1,ZI(JCELD1-1+4))
         NCMPEL = NCMPEL * ICOEF
         IF (FIRST) THEN
            LONGT1 = NCMPEL
         ELSE
            IF (LONGT1.NE.NCMPEL) THEN
               CALL U2MESS('F','CALCULEL3_60')
            ENDIF
         ENDIF
         FIRST = .FALSE.
   1  CONTINUE
C
      FIRST = .TRUE.
      NBGR  = NBGREL(LIGREL)
      DO 100 ,J = 1,NBGR
         MODE=ZI(JCELD2-1+ZI(JCELD2-1+4+J) +2)
         IF (MODE.EQ.0) GOTO 100
         NCMPEL = DIGDEL(MODE)
         ICOEF=MAX(1,ZI(JCELD2-1+4))
         NCMPEL = NCMPEL * ICOEF
         IF (FIRST) THEN
            LONGT2 = NCMPEL
         ELSE
            IF (LONGT2.NE.NCMPEL) THEN
               CALL U2MESS('F','CALCULEL3_61')
            ENDIF
         ENDIF
         FIRST = .FALSE.
 100  CONTINUE
C
      IF ((NCPA.GT.LONGT1).OR.(NCPB.GT.LONGT2)) THEN
         CALL U2MESS('F','CALCULEL3_62')
      ENDIF
C
C     -- ON MET A ZERO LE VECTEUR "VSCAL":
C     ------------------------------------
      DO 10, I = 1,2
         VR(I) = RZERO
 10   CONTINUE
C
C --- ON MOYENNE :
C     ------------
C
      CALL JEVEUO (CHAMP1//'.CELV','L',IAVAL1)
      CALL JEVEUO (CHAMP2//'.CELV','L',IAVAL2)
      IF (NBMAIL.LE.0) THEN
         DO 2 ,J = 1,NBGR
            MODE1 = ZI(JCELD1-1+ZI(JCELD1-1+4+J) +2)
            MODE2 = ZI(JCELD2-1+ZI(JCELD2-1+4+J) +2)
            IF ((MODE1.EQ.0 ).OR.(MODE2.EQ.0)) GOTO 2
            NEL = NBELEM(LIGREL,J)
            IDECG1 = ZI(JCELD1-1+ZI(JCELD1-1+4+J)+8)
            IDECG2 = ZI(JCELD2-1+ZI(JCELD2-1+4+J)+8)
            DO 3 , K = 1,NEL
               VR(1) = VR(1) + ZR(IAVAL1-1+IDECG1+(K-1)*LONGT1+NCPA-1)
     &                        *ZR(IAVAL2-1+IDECG2+(K-1)*LONGT2+NCPB-1)
               VR(2) = VR(2) + ZR(IAVAL2-1+IDECG2+(K-1)*LONGT2+NCPB-1)
 3          CONTINUE
 2       CONTINUE
         VR(1) = VR(1)/VR(2)
      ELSE
         CALL JEVEUO (LIGREL//'.LIEL','L',JLIGR)
         DO 30 IM = 1,NBMAIL
            INUM = 0
            DO 20 J = 1,NBGR
               MODE1 = ZI(JCELD1-1+ZI(JCELD1-1+4+J) +2)
               MODE2 = ZI(JCELD2-1+ZI(JCELD2-1+4+J) +2)
               NEL = NBELEM(LIGREL,J)
               IF ((MODE1.EQ.0 ).OR.(MODE2.EQ.0)) THEN
                  INUM = INUM + NEL + 1
                  GOTO 20
               ENDIF
               IDECG1 = ZI(JCELD1-1+ZI(JCELD1-1+4+J)+8)
               IDECG2 = ZI(JCELD2-1+ZI(JCELD2-1+4+J)+8)
               DO 22 K = 1,NEL
                  IEL = ZI(JLIGR+INUM+K-1)
                  IF (IEL.NE.NUMAIL(IM)) GOTO 22
                  VR(1) =VR(1) + ZR(IAVAL1-1+IDECG1+(K-1)*LONGT1+NCPA-1)
     &                          *ZR(IAVAL2-1+IDECG2+(K-1)*LONGT2+NCPB-1)
                  VR(2)= VR(2) + ZR(IAVAL2-1+IDECG2+(K-1)*LONGT2+NCPB-1)
                  GOTO 30
 22            CONTINUE
               INUM = INUM + NEL + 1
 20         CONTINUE
 30      CONTINUE
         VR(1) = VR(1)/VR(2)
      ENDIF
C
      CALL JEDEMA()
      END
