      SUBROUTINE ACEVPO(NBOCC,NLM,NLG,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           NBOCC,NLM,NLG,IER
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 05/10/2004   AUTEUR CIBHHLV L.VIVAN 
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
C     AFFE_CARA_ELEM
C     VERIFICATION DES MOTS CLES POUR L'ELEMENT POUTRE
C ----------------------------------------------------------------------
C IN  : NBOCC  : NOMBRE D'OCCURENCE
C OUT : NLM    : NOMBRE TOTAL DE MAILLE
C OUT : NLG    : NOMBRE TOTAL DE GROUPE DE MAILLE
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      REAL*8        R8B
      LOGICAL       BON
      CHARACTER*8   K8B, NOMU, CARA(100), KIOC
      CHARACTER*16  K16B, SEC, VSEC, TOU, CONCEP, CMD
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL GETRES(NOMU,CONCEP,CMD)
C
      NLM = 0
      NLG = 0
      DO 100 IOC = 1 , NBOCC
         CALL CODENT(IOC,'G',KIOC)
         CALL GETVID ( 'POUTRE', 'GROUP_MA' , IOC,1,0,  K8B, NG )
         CALL GETVID ( 'POUTRE', 'MAILLE'   , IOC,1,0,  K8B, NM )
         CALL GETVTX ( 'POUTRE', 'SECTION'  , IOC,1,1,  SEC, NS )
         CALL GETVTX ( 'POUTRE', 'VARI_SECT', IOC,1,1, VSEC, NVS)
         CALL GETVTX ( 'POUTRE', 'CARA'     , IOC,1,0,  K8B, NC )
         NCAR = -NC
         CALL GETVTX ( 'POUTRE', 'CARA'     , IOC,1,NCAR,CARA,NC)
         CALL GETVR8 ( 'POUTRE', 'VALE'     , IOC,1,0,  R8B, NV )
         NVAL = -NV
C
         IF ( NVAL .NE. NCAR ) THEN
            CALL UTDEBM('E',CMD,'POUTRE : OCCURENCE '//KIOC//' : ')
            CALL UTIMPI('L','"CARA" NOMBRE DE VALEURS ENTREES: ',1,KIC)
            CALL UTIMPI('L','"VALE" NOMBRE DE VALEURS ENTREES: ',1,KIV)
            CALL UTIMPI('L','VERIFIER VOS DONNEES',0,KIV)
            CALL UTFINM()
            IER = IER + 1
         ENDIF
C
         IF ( SEC .EQ. 'RECTANGLE' ) THEN
            IF ( VSEC .EQ. 'AFFINE' ) THEN

            ENDIF
         ELSEIF ( SEC .EQ. 'CERCLE' ) THEN
            IF ( VSEC .EQ. 'CONSTANT' ) THEN
               BON = .FALSE.
               DO 20 I = 1 , NCAR
                  IF ( CARA(I) .EQ. 'R' ) BON = .TRUE.
 20            CONTINUE
               IF ( .NOT. BON ) THEN
                  CALL UTMESS('E',CMD,'POUTRE : OCCURENCE '//KIOC//
     +                    ' : SECTION "CERCLE", VARI_SECT "CONSTANT"'//
     +                      ' LA CARACTERISTIQUE "R" EST OBLIGATOIRE')
                  IER = IER + 1
               ENDIF
            ENDIF
         ENDIF
C
C ---    GROUP_MA + GROUP_NO + NOEUD + MAILLE
         NSOM = NG + NM
         IF (NSOM.EQ.NG .OR. NSOM.EQ.NM) THEN
            NLM = MAX(NLM,-NM)
            NLG = MAX(NLG,-NG)
         ENDIF
C
 100  CONTINUE
C
      CALL JEDEMA()
      END
