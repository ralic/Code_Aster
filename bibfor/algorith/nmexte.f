      SUBROUTINE NMEXTE(PHASE ,EXTRCH,NVALCP,VAL   ,VALRES)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/01/2011   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT      NONE
      CHARACTER*4   PHASE
      CHARACTER*8   EXTRCH
      REAL*8        VAL(*),VALRES(*)
      INTEGER       NVALCP
C
C ----------------------------------------------------------------------
C
C ROUTINE *_NON_LINE (EXTRACTION - UTILITAIRE)
C
C GERER LES VALEURS DES COMPOSANTES - CAS DU CHAMP AUX NOEUDS
C
C ----------------------------------------------------------------------
C
C
C IN  PHASE  : PHASE
C    'INIT' - INITIALISATION <VAL> SUIVANT OPTION EXTRACTION
C    'SAVE' - RECOPIE <VALRES> DANS <VAL> SUIVANT <EXTRCH>
C IN  EXTRCH : TYPE D'EXTRACTION SUR LE CHAMP
C IN  NVALCP : NOMBRE DE COMPOSANTES
C I/O VAL    : LISTE DES COMPOSANTES - SD RESULTANTE
C I/O VALRES : LISTE DES COMPOSANTES - DONNNES UTILISATEUR
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      INTEGER      ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8       ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16   ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL      ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8  ZK8
      CHARACTER*16    ZK16
      CHARACTER*24        ZK24
      CHARACTER*32            ZK32
      CHARACTER*80                ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER      IVALCP
      REAL*8       VALR,VAL2R,R8MAEM
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS      
C
      IF (PHASE.EQ.'INIT') THEN
        DO 10 IVALCP  = 1,NVALCP
          IF (EXTRCH.EQ.'VALE') THEN
            VAL(IVALCP) = 0.D0
          ELSEIF (EXTRCH.EQ.'MAX') THEN
            VAL(IVALCP) = -R8MAEM()
          ELSEIF (EXTRCH.EQ.'MIN') THEN
            VAL(IVALCP) = +R8MAEM()
          ELSEIF (EXTRCH.EQ.'MOY') THEN
            VAL(IVALCP) = 0.D0
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF
  10    CONTINUE
      ELSEIF (PHASE.EQ.'SAVE') THEN
        DO 20 IVALCP = 1,NVALCP
          VALR    = VAL(IVALCP)
          VAL2R   = VALRES(IVALCP)
          IF (EXTRCH.EQ.'VALE') THEN
            VAL(IVALCP) = VAL2R
          ELSEIF (EXTRCH.EQ.'MAX') THEN
            VAL(IVALCP) = MAX(VALR,VAL2R)
          ELSEIF (EXTRCH.EQ.'MIN') THEN
            VAL(IVALCP) = MIN(VALR,VAL2R)
          ELSEIF (EXTRCH.EQ.'MOY') THEN
            VAL(IVALCP) = VALR+VAL2R
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF
  20    CONTINUE
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      CALL JEDEMA()
C
      END
