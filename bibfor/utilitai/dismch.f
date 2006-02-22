      SUBROUTINE DISMCH(CODMES,QUESTI,NOMOBZ,REPI,REPKZ,IERD)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 21/02/2006   AUTEUR REZETTE C.REZETTE 
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
C     --     DISMOI(CHARGE)
C     ARGUMENTS:
C     ----------
      INTEGER REPI,IERD
      CHARACTER*(*) QUESTI,CODMES
      CHARACTER*(*) NOMOBZ,REPKZ
      CHARACTER*32 REPK
      CHARACTER*8 NOMOB
C ----------------------------------------------------------------------
C     IN:
C       CODMES : CODE DES MESSAGES A EMETTRE : 'F', 'A', ...
C       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
C       NOMOB  : NOM D'UN OBJET DE TYPE  CHARGE
C     OUT:
C       REPI   : REPONSE ( SI ENTIERE )
C       REPK   : REPONSE ( SI CHAINE DE CARACTERES )
C       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
C
C ----------------------------------------------------------------------
C     VARIABLES LOCALES:
C     ------------------
      CHARACTER*4  SUF
      CHARACTER*8  K8BID,TEMPE,HYDRAT,SECHAG
C --------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR,JEXR8
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16,TYPECO
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C --------------- FIN COMMUNS NORMALISES  JEVEUX  --------------------
C
C
C
      CALL JEMARQ()
      NOMOB = NOMOBZ
      REPK  = REPKZ
      CALL JEVEUO(NOMOB//'.TYPE           ','L',ITYPE)
      IF (ZK8(ITYPE) (1:5).EQ.'MECA_') THEN
         IPHEN = 1
         SUF = 'CHME'
      ELSE IF (ZK8(ITYPE)(1:5).EQ.'THER_') THEN
         IPHEN = 2
         SUF = 'CHTH'
      ELSE IF (ZK8(ITYPE)(1:5).EQ.'ACOU_') THEN
         IPHEN = 3
         SUF = 'CHAC'
      ELSE IF (ZK8(ITYPE)(1:5).EQ.'CIME_') THEN
         IPHEN = 1
         SUF = 'CIME'
      ELSE IF (ZK8(ITYPE)(1:5).EQ.'CITH_') THEN
         IPHEN = 2
         SUF = 'CITH'
      ELSE IF (ZK8(ITYPE)(1:5).EQ.'CIAC_') THEN
         IPHEN = 3
         SUF = 'CIAC'
      ELSE
         CALL UTMESS ('F','DISMCH','TYPE DE CHARGE INCONNU')
      END IF
C
C
      IF (QUESTI.EQ.'PHENOMENE') THEN
         IF (IPHEN.EQ.1) THEN
            REPK = 'MECANIQUE'
         ELSE IF (IPHEN.EQ.2) THEN
            REPK = 'THERMIQUE'
         ELSE IF (IPHEN.EQ.3) THEN
            REPK = 'ACOUSTIQUE'
         ELSE
            REPK = ' '
         END IF
C
      ELSE IF (QUESTI.EQ.'EXI_TEMPER') THEN
         IF((SUF(3:4).EQ.'TH').OR.(SUF(3:4).EQ.'AC').OR.
     &      (SUF.EQ.'CIME')) THEN
            REPK = 'NON'
            GO TO 9999
         ELSE IF (SUF.EQ.'CHME') THEN
            CALL JEEXIN(NOMOB//'.CHME.TEMPE.TEMP',IRET)
            IF (IRET.EQ.0) THEN
               REPK = 'NON'
               GO TO 9999
            END IF
            CALL JEVEUO(NOMOB//'.CHME.TEMPE.TEMP','L',IATEMP)
            TEMPE = ZK8(IATEMP)
            CALL GETTCO(TEMPE,TYPECO)
            IF (TYPECO.EQ.'EVOL_THER') THEN
               REPK = 'EVOL'
               GO TO 9999
            ELSE
               REPK = 'CHGD'
               GO TO 9999
            END IF
         ELSE
            CALL UTMESS('F','DISMCH','SUFFIXE INCONU: '//SUF)
         END IF
C
      ELSE IF (QUESTI.EQ.'EXI_HYDRAT') THEN
         IF((SUF(3:4).EQ.'TH').OR.(SUF(3:4).EQ.'AC').OR.
     &      (SUF.EQ.'CIME')) THEN
            REPK = 'NON'
            GO TO 9999
         ELSE IF (SUF.EQ.'CHME') THEN
            CALL JEEXIN(NOMOB//'.CHME.EVOL.HYDR',IRET)
            IF (IRET.EQ.0) THEN
               REPK = 'NON'
               GO TO 9999
            END IF
            CALL JEVEUO(NOMOB//'.CHME.EVOL.HYDR','L',IATEMP)
            HYDRAT = ZK8(IATEMP)
            CALL GETTCO(HYDRAT,TYPECO)
            IF (TYPECO.EQ.'EVOL_THER') THEN
               REPK = 'EVOL'
               GO TO 9999
            ELSE 
               REPK = 'CHGD'
               GO TO 9999
            END IF
         ELSE
            CALL UTMESS('F','DISMCH','SUFFIXE INCONU: '//SUF)
         END IF
C
      ELSE IF (QUESTI.EQ.'EXI_SECHAG') THEN
         IF((SUF(3:4).EQ.'TH').OR.(SUF(3:4).EQ.'AC').OR.
     &      (SUF.EQ.'CIME')) THEN
            REPK = 'NON'
            GO TO 9999
         ELSE IF (SUF.EQ.'CHME') THEN
            CALL JEEXIN(NOMOB//'.CHME.EVOL.SECH',IRET)
            IF (IRET.EQ.0) THEN
               REPK = 'NON'
               GO TO 9999
            END IF
            CALL JEVEUO(NOMOB//'.CHME.EVOL.SECH','L',IATEMP)
            SECHAG = ZK8(IATEMP)
            CALL GETTCO(SECHAG,TYPECO)
            IF (TYPECO.EQ.'EVOL_THER') THEN
               REPK = 'EVOL'
               GO TO 9999
            ELSE 
               REPK = 'CHGD'
               GO TO 9999
            END IF
         ELSE
            CALL UTMESS('F','DISMCH','SUFFIXE INCONU: '//SUF)
         END IF
C
      ELSE IF (QUESTI.EQ.'NOM_MODELE') THEN
         CALL JEVEUO(NOMOB//'.'//SUF//'.MODEL.NOMO','L',IANOMO)
         REPK = ZK8(IANOMO)
C
      ELSE IF (QUESTI.EQ.'TYPE_CHARGE') THEN
         CALL JEVEUO(NOMOB//'.TYPE','L',IATYPE)
         REPK = ZK8(IATYPE)
C
      ELSE IF (QUESTI.EQ.'NOM_MAILLA') THEN
         CALL JEVEUO(NOMOB//'.'//SUF//'.MODEL.NOMO','L',IANOMO)
         CALL DISMMO(CODMES,QUESTI,ZK8(IANOMO),REPI,REPK,IERD)
C
      ELSE IF (QUESTI.EQ.'NOM_LIGREL') THEN
         REPK = NOMOB//'.'//SUF//'.LIGRE'
C
      ELSE
         REPK = QUESTI
         CALL UTMESS('F','DISMCH:',
     +               'LA QUESTION : "'//REPK//'" EST INCONNUE')
      END IF
C
 9999 CONTINUE
      REPKZ = REPK
      CALL JEDEMA()
      END
