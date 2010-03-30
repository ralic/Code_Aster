      SUBROUTINE  CRSMOS(NOMSTO,TYPROZ,NEQ)
      IMPLICIT    NONE
      CHARACTER*(*)  NOMSTO,TYPROZ
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 29/03/2010   AUTEUR BOITEAU O.BOITEAU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_4
C-----------------------------------------------------------------------
C    BUT: CREER UN STOCKAGE_MORSE POUR UNE MATRICE PLEINE OU DIAGONALE
C
C    DETERMINER LA NUMEROTATION GENERALISEE A PARTIR D'UN MODE_MECA
C    OU D'UN MODE_GENE
C    LA NUMEROTATION SERA PAR DEFAUT PLEINE
C
C IN  JXOUT K19 NOMSTO  : NOM DU STOCKAGE A CREER
C IN        K*  TYPROZ : 'PLEIN' /'DIAG'
C IN        I   NEQ     : DIMENSION DE LA MATRICE
C-----------------------------------------------------------------------
C-------- DEBUT COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER*4 ZI4
      COMMON  /I4VAJE/ZI4(1)
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C
C----------  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER     I,J,NTERM,ICO,JSMDE,JSMDI,JSMHC,NEQ
      CHARACTER*19  STO19
      CHARACTER*5 TYPROF
C     ------------------------------------------------------------------


      CALL JEMARQ()
      STO19=NOMSTO
      TYPROF=TYPROZ

      CALL ASSERT(TYPROF.EQ.'PLEIN' .OR. TYPROF.EQ.'DIAG')
      IF (TYPROF.EQ.'DIAG') THEN
         NTERM=NEQ
      ELSE
         NTERM=NEQ*(NEQ+1)/2
      ENDIF

      CALL WKVECT ( STO19//'.SMHC', 'G V S', NTERM, JSMHC )
      CALL WKVECT ( STO19//'.SMDI', 'G V I', NEQ , JSMDI )
      CALL WKVECT ( STO19//'.SMDE', 'G V I', 6, JSMDE )


      ZI(JSMDE-1+1  ) = NEQ
      ZI(JSMDE-1+2) = NTERM
      ZI(JSMDE-1+3) = 1


      IF (TYPROF.EQ.'DIAG') THEN
         DO 201 I = 1 , NEQ
            ZI(JSMDI+I-1) = I
            ZI4(JSMHC+I-1) = I
201      CONTINUE
      ELSEIF (TYPROF.EQ.'PLEIN') THEN
         ICO=0
         DO 202 I = 1 , NEQ
            ZI(JSMDI+I-1) = I*(I+1)/2
            DO 203 J=1,I
               ICO=ICO+1
               ZI4(JSMHC-1+ICO) = J
203         CONTINUE
202      CONTINUE
      ENDIF

      CALL JEDEMA()
      END
