      SUBROUTINE CAZOCV(CHAR,MOTFAC,IREAD,IWRITE)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 23/01/2007   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
      IMPLICIT     NONE
      CHARACTER*8  CHAR
      CHARACTER*16 MOTFAC
      INTEGER      IREAD
      INTEGER      IWRITE  
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : CAZOCO
C ----------------------------------------------------------------------
C
C LECTURE DES PRINCIPALES CARACTERISTIQUES DU CONTACT (SURFACE IREAD)
C REMPLISSAGE DE LA SD 'DEFICO' (SURFACE IWRITE) POUR 
C LA METHODE SANS CONTACT
C
C IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
C IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'CONTACT')
C IN  NOMA   : NOM DU MAILLAGE
C IN  NOMO   : NOM DU MODELE
C IN  NDIM   : NOMBRE DE DIMENSIONS DU PROBLEME
C IN  IREAD  : INDICE POUR LIRE LES DONNEES DANS AFFE_CHAR_MECA
C IN  IWRITE : INDICE POUR ECRIRE LES DONNEES DANS LA SD DEFICONT
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
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
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER      CFMMVD,ZMETH,ZTOLE,ZCONV
      INTEGER      NOC
      CHARACTER*16 ISTO
      CHARACTER*24 METHCO,CONVCO,TOLECO
      INTEGER      JMETH,JCONV,JTOLE
      REAL*8       JEU
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      JEU= 0.D0

C ======================================================================
C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C ======================================================================

      METHCO = CHAR(1:8)//'.CONTACT.METHCO'
      TOLECO = CHAR(1:8)//'.CONTACT.TOLECO'
      CONVCO = CHAR(1:8)//'.CONTACT.CONVCO'

      CALL JEVEUO(METHCO,'E',JMETH)
      CALL JEVEUO(TOLECO,'E',JTOLE)
      CALL JEVEUO(CONVCO,'E',JCONV)

      ZMETH = CFMMVD('ZMETH')
      ZCONV = CFMMVD('ZCONV')
      ZTOLE = CFMMVD('ZTOLE')
      

      CALL GETVTX(MOTFAC,'STOP_INTERP',IREAD,1,1,ISTO,NOC)
      IF (ISTO.EQ.'OUI') THEN
         ZI(JCONV+ZCONV*(IWRITE-1)+3) = 1
      ELSEIF (ISTO.EQ.'NON') THEN
         ZI(JCONV+ZCONV*(IWRITE-1)+3) = 0
      ENDIF

      CALL GETVR8(MOTFAC,'TOLE_INTERP',IREAD,1,1,JEU,NOC)
      ZR(JTOLE+ZTOLE*(IWRITE-1)+3)  = JEU
C
C --- IL N'Y A JAMAIS DE REACTUALISATION
C
      ZI(JMETH+ZMETH*(IWRITE-1)+7) = 0
C
      CALL JEDEMA()

      END
