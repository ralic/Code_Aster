      SUBROUTINE PEENCA(CHAMP,LONG,VR,NBMAIL,NUMMAI)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*)     CHAMP
      INTEGER                 LONG,   NBMAIL,NUMMAI(*)
      REAL*8                       VR(LONG)
C     ------------------------------------------------------------------
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
C     FAIRE DES OPERATIONS SUR UN CHAM_ELEM DE TYPE ENERGIE
C            (NOTION D'INTEGRALE DU CHAMP SUR LE MODELE)
C     ------------------------------------------------------------------
C IN  : CHAMP  : NOM DU CHAM_ELEM
C IN  : LONG   : LONGUEUR DU VECTEUR VR
C OUT : VR     : VECTEUR CONTENANT LES RESULATTS GLOBAUX
C IN  : NBMAIL : = 0 , CALCUL SUR TOUT LE CHAM_ELEM
C                SINON CALCUL SUR UN NOMBRE DE MAILLES
C IN  : NUMMAI : NUMEROS DES MAILLES
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
      INTEGER      NBGREL, NBELEM, DIGDEL
      INTEGER      LONGT, MODE
      REAL*8       RZERO, ZTOT
      CHARACTER*4  DOCU
      CHARACTER*8  SCAL, SCALAI
      CHARACTER*19 CHAMP2, LIGREL
C     ------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER I ,IBID ,ICOEF ,IDECGR ,IEL ,IM ,INUM 
      INTEGER J ,JCELD ,JLIGR ,K ,LCELK ,LVALE ,NBGR 
      INTEGER NEL 
      REAL*8 R8PREM 
C-----------------------------------------------------------------------
      CALL JEMARQ()
      CHAMP2 = CHAMP
      RZERO  = 0.0D0

C     -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
      CALL CELVER(CHAMP2,'NBVARI_CST','STOP',IBID)
      CALL CELVER(CHAMP2,'NBSPT_1','STOP',IBID)

      CALL JELIRA (CHAMP2//'.CELD','DOCU',IBID,DOCU)
      IF( DOCU.NE.'CHML')THEN
         CALL U2MESS('F','CALCULEL3_52')
      ENDIF
      CALL JEVEUO (CHAMP2//'.CELK','L',LCELK)
      LIGREL = ZK24(LCELK)(1:19)
C
      CALL JEVEUO (CHAMP2//'.CELD','L',JCELD)
C
C     --- TYPE DE LA GRANDEUR ---
      SCAL= SCALAI(ZI(JCELD))

      NBGR  =  NBGREL(LIGREL)

C     -- ON MET A ZERO LE VECTEUR "VSCAL":
      IF (SCAL(1:1).EQ.'R') THEN
         DO 12  I = 1,LONG
            VR(I) = RZERO
 12      CONTINUE
      ELSE
         CALL U2MESK('F','CALCULEL3_74',1,SCAL)
      ENDIF
C
      CALL JEVEUO (CHAMP2//'.CELV','L',LVALE)
      IF ( NBMAIL.LE.0 ) THEN
         DO 30 J = 1,NBGR
            MODE=ZI(JCELD-1+ZI(JCELD-1+4+J) +2)
            IF (MODE .EQ.0 ) GOTO 30
            LONGT = DIGDEL(MODE)
            ICOEF=MAX(1,ZI(JCELD-1+4))
            LONGT = LONGT * ICOEF
            NEL   = NBELEM(LIGREL,J)
            IDECGR=ZI(JCELD-1+ZI(JCELD-1+4+J)+8)
            DO 32 K = 1,NEL
C
C              --- TOTALE ---
               I = 1
               ZTOT  = ZR(LVALE-1+IDECGR+(K-1)*LONGT+I-1)
               VR(1) = VR(1)+ ZTOT
 32         CONTINUE
 30      CONTINUE
         VR(2) = 100.0D0
      ELSE
         ZTOT = RZERO
         DO 34 J = 1,NBGR
            MODE=ZI(JCELD-1+ZI(JCELD-1+4+J) +2)
            IF (MODE .EQ.0 ) GOTO 34
            LONGT = DIGDEL(MODE)
            ICOEF=MAX(1,ZI(JCELD-1+4))
            LONGT = LONGT * ICOEF
            NEL   = NBELEM(LIGREL,J)
            IDECGR=ZI(JCELD-1+ZI(JCELD-1+4+J)+8)
            DO 36 K = 1,NEL
               ZTOT = ZTOT + ZR(LVALE-1+IDECGR+(K-1)*LONGT)
 36         CONTINUE
 34      CONTINUE
         CALL JEVEUO(LIGREL//'.LIEL','L',JLIGR)
         DO 40 IM = 1,NBMAIL
            INUM = 0
            DO 42 J = 1,NBGR
               MODE=ZI(JCELD-1+ZI(JCELD-1+4+J) +2)
               NEL = NBELEM(LIGREL,J)

               IF (MODE .EQ.0 ) THEN 
                    INUM = INUM + NEL + 1
                    GOTO 42
               ENDIF
               LONGT = DIGDEL(MODE)
               ICOEF=MAX(1,ZI(JCELD-1+4))
               LONGT = LONGT * ICOEF

               IDECGR=ZI(JCELD-1+ZI(JCELD-1+4+J)+8)
               DO 44 K = 1,NEL
                  IEL = ZI(JLIGR+INUM+K-1)
                  IF (IEL.NE.NUMMAI(IM)) GOTO 44
C
C                 --- TOTALE ---
                  I = 1
                  VR(1) = VR(1)+ ZR(LVALE-1+IDECGR+(K-1)*LONGT+I-1)
                  GOTO 40
 44            CONTINUE
               INUM = INUM + NEL + 1
 42         CONTINUE
 40      CONTINUE
         IF (( VR(1).LT.R8PREM() ).AND.( ZTOT.LT.R8PREM() )) THEN
            VR(2) = 0.0D0
         ELSE
            VR(2) = 100.0D0 * VR(1) / ZTOT
         ENDIF
      ENDIF
C
      CALL JEDEMA()
      END
