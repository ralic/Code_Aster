      SUBROUTINE INGRMA ( SDMAIL, NOMMA,
     >                    LGRMA,  NBGRMA, CODRET )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 14/09/2004   AUTEUR MCOURTOI M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C     RETOURNE LA LISTE DES GROUPES DE MAILLES CONTENANT UNE MAILLE
C     PARTICULIERE DONT ON DONNE LE NOM OU LE NUMERO
C-----------------------------------------------------------------------
C     ENTREES:
C        SDMAIL : NOM DE LA SD MAILLAGE
C        NOMMA  : NOM DE LA MAILLE
C     SORTIES:
C        LGRMA  : ADR DU TABLEAU DES GROUP_MA CONTENANT LA MAILLE
C        NBGRMA : NBRE DE GROUPES DANS CETTE LISTE
C        CODRET : 0 SI OK, <>0 SI ERREUR
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
C
C 0.1. ==> ARGUMENTS
C
      CHARACTER*8    SDMAIL, NOMMA
      INTEGER        LGRMA(*), NBGRMA, CODRET
C
C 0.2. ==> JEVEUX
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*32 JEXNUM,JEXNOM
C
C 0.3. ==> VARIABLES LOCALES
C
      CHARACTER*24   NOMMAI, GRPMAI
      CHARACTER*8    NOMG
      CHARACTER*6    NOMPRO
      CHARACTER*1    K1B
      PARAMETER    ( NOMPRO = 'INGRMA' )
      INTEGER        I, J, IER, NUM, NBG, NBMAG, JGRMA
C
C====
C 1. PREALABLES
C====
C
C 1.1. ==> INITIALISATIONS
C
      CODRET = 0
      NBGRMA = 0
      NOMMAI = SDMAIL//'.NOMMAI         '
      GRPMAI = SDMAIL//'.GROUPEMA       '
C
C 1.2. ==> VERIFICATIONS
C
      CALL JEEXIN(NOMMAI,IER)
      IF ( IER.EQ.0 ) THEN
         CALL UTMESS('A', NOMPRO,
     &               '.NOMMAI DU MAILLAGE INEXISTANT : '//SDMAIL)
         CODRET=4
         GOTO 9999
      ENDIF
C
      CALL JEEXIN(GRPMAI,IER)
      IF ( IER.EQ.0 ) THEN
         CALL UTMESS('A', NOMPRO,
     &               '.GROUPEMA DU MAILLAGE INEXISTANT : '//SDMAIL)
         CODRET=4
         GOTO 9999
      ENDIF
C
      NUM = 0
      CALL JEEXIN( JEXNOM(NOMMAI,NOMMA), IER)
      IF (IER.NE.0) THEN
         CALL JENONU( JEXNOM(NOMMAI,NOMMA), NUM)
      ENDIF
      IF (NUM.EQ.0) THEN
         WRITE(6,1001) NOMPRO,'MAILLE '//NOMG//' NON TROUVEE'
         CODRET = 1
         GOTO 9999
      ENDIF
C
C====
C 2. BOUCLE SUR LES GROUP_MA
C====
C
      CALL JELIRA( GRPMAI, 'NOMUTI', NBG, K1B )
      DO 200 I = 1, NBG
         CALL JENUNO( JEXNUM(GRPMAI, I), NOMG )
         CALL JEVEUO( JEXNUM(GRPMAI, I), 'L', JGRMA)
         CALL JELIRA( JEXNUM(GRPMAI, I), 'LONMAX', NBMAG, K1B)
C     --- BCLE SUR LES MAILLES DU GROUP_MA
         DO 210 J = 1, NBMAG
            IF (ZI(JGRMA-1+J).EQ.NUM) THEN
               NBGRMA = NBGRMA + 1
               LGRMA(NBGRMA) = I
            ENDIF
 210     CONTINUE
 200  CONTINUE
C
C====
C 99. SORTIE
C====
C
 1001 FORMAT(/,'<',A6,'> ',A,/)
C
 9999 CONTINUE
      END
