      SUBROUTINE COPICH ( BASE, CH1, CH2 )

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 11/09/2002   AUTEUR VABHHTS J.PELLET 
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
C RESPONSABLE VABHHTS J.PELLET

      IMPLICIT NONE
      CHARACTER*1   BASE
      CHARACTER*19  CH1, CH2
C ----------------------------------------------------------------------
C
C   BUT:
C   DUPLIQUER UN CHAMP_GD SOUS UN AUTRE NOM.
C    L'EXISTENCE DE CH1 EST OBLIGATOIRE
C   (SI CH2 EXISTE DEJA, ON L'ECRASE)
C
C     IN       BASE        'G' , 'V' , ... : BASE DE CREATION DE CH2
C     IN       CH1    K19  NOM DU CHAMP_GD A DUPLIQUER
C     IN/JXOUT CH2    K19  NOM DU CHAMP_GD A CREER
C
C-----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------


      CHARACTER*4  DOCU
      CHARACTER*24 O1, O2
      INTEGER      IBID,IRET,IRET1,IRET2


      CALL JEMARQ()

      CALL JEEXIN(CH1 // '.DESC',IRET1)
      CALL JEEXIN(CH1 // '.CELD',IRET2)
      IRET= MAX (IRET1,IRET2)
      IF (IRET.EQ.0) CALL UTMESS('F','COPICH','CHAMP IN INEXISTANT')

      IF (IRET1.GT.0) THEN
        CALL JELIRA(CH1//'.DESC','DOCU',IBID,DOCU)
      ELSE
        CALL JELIRA(CH1//'.CELD','DOCU',IBID,DOCU)
      END IF


C     -- CAS DES CHAM_NO :
C     ----------------------
      IF  (DOCU.EQ.'CHNO') THEN

C       -- .DESC :
        O1=CH1//'.DESC'
        O2=CH2//'.DESC'
        CALL JEEXIN(O1,IRET)
        IF (IRET.GT.0) CALL JEDUPO(O1,BASE,O2,.FALSE.)

C       -- .REFE :
        O1=CH1//'.REFE'
        O2=CH2//'.REFE'
        CALL JEEXIN(O1,IRET)
        IF (IRET.GT.0) CALL JEDUPO(O1,BASE,O2,.FALSE.)

C       -- .VALE :
        O1=CH1//'.VALE'
        O2=CH2//'.VALE'
        CALL JEEXIN(O1,IRET)
        IF (IRET.GT.0) CALL JEDUPO(O1,BASE,O2,.FALSE.)


C     -- CAS DES CARTES :
C     ----------------------

      ELSE IF  (DOCU.EQ.'CART') THEN
C
C       -- .DESC :
        O1=CH1//'.DESC'
        O2=CH2//'.DESC'
        CALL JEEXIN(O1,IRET)
        IF (IRET.GT.0) CALL JEDUPO(O1,BASE,O2,.FALSE.)
C
C       -- .LIMA :
        O1=CH1//'.LIMA'
        O2=CH2//'.LIMA'
        CALL JEEXIN(O1,IRET)
        IF (IRET.GT.0) CALL JEDUPO(O1,BASE,O2,.FALSE.)
C
C       -- .NOLI :
        O1=CH1//'.NOLI'
        O2=CH2//'.NOLI'
        CALL JEEXIN(O1,IRET)
        IF (IRET.GT.0) CALL JEDUPO(O1,BASE,O2,.FALSE.)
C
C       -- .NOMA :
        O1=CH1//'.NOMA'
        O2=CH2//'.NOMA'
        CALL JEEXIN(O1,IRET)
        IF (IRET.GT.0) CALL JEDUPO(O1,BASE,O2,.FALSE.)
C
C       -- .VALE :
        O1=CH1//'.VALE'
        O2=CH2//'.VALE'
        CALL JEEXIN(O1,IRET)
        IF (IRET.GT.0) CALL JEDUPO(O1,BASE,O2,.FALSE.)


C     -- CAS DES CHAM_ELEM :
C     ----------------------
      ELSE IF  (DOCU.EQ.'CHML') THEN

C       -- .CELD :
        O1=CH1//'.CELD'
        O2=CH2//'.CELD'
        CALL JEEXIN(O1,IRET)
        IF (IRET.GT.0) CALL JEDUPO(O1,BASE,O2,.FALSE.)

C       -- .CELK :
        O1=CH1//'.CELK'
        O2=CH2//'.CELK'
        CALL JEEXIN(O1,IRET)
        IF (IRET.GT.0) CALL JEDUPO(O1,BASE,O2,.FALSE.)

C       -- .CELV :
        O1=CH1//'.CELV'
        O2=CH2//'.CELV'
        CALL JEEXIN(O1,IRET)
        IF (IRET.GT.0) CALL JEDUPO(O1,BASE,O2,.FALSE.)


C     -- CAS DES RESUELEM :
C     ----------------------
      ELSE IF  (DOCU.EQ.'RESL') THEN

C       -- .DESC :
        O1=CH1//'.DESC'
        O2=CH2//'.DESC'
        CALL JEEXIN(O1,IRET)
        IF (IRET.GT.0) CALL JEDUPO(O1,BASE,O2,.FALSE.)

C       -- .NOLI :
        O1=CH1//'.NOLI'
        O2=CH2//'.NOLI'
        CALL JEEXIN(O1,IRET)
        IF (IRET.GT.0) CALL JEDUPO(O1,BASE,O2,.FALSE.)

C       -- .RESL :
        O1=CH1//'.RESL'
        O2=CH2//'.RESL'
        CALL JEEXIN(O1,IRET)
        IF (IRET.GT.0) CALL JEDUPO(O1,BASE,O2,.FALSE.)


      ELSE
         CALL UTMESS('F',' COPICH ','TYPE DE CHAMP INCONNU')
      END IF

      CALL JEDEMA()
      END
