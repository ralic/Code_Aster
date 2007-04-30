      SUBROUTINE SYMECO (CHAR,MOTFAC,NZOCO,
     &                   NSYME)
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 30/04/2007   AUTEUR ABBAS M.ABBAS 
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
C
      IMPLICIT     NONE
      CHARACTER*8  CHAR
      CHARACTER*16 MOTFAC
      INTEGER      NZOCO
      INTEGER      NSYME

      
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : CALICO
C ----------------------------------------------------------------------
C
C DETERMINATION DU NOMBRE DE ZONES DE CONTACT SYMETRIQUES
C REMPLISSAGE DE LA SD ASSOCIEE
C
C IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
C IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'CONTACT')
C IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
C OUT NSYME  : NOMBRE DE ZONES SYMETRIQUES DE CONTACT
C
C ======================================================================
C ----------------------------------------------------------------------
C SD CHAR(1:8)//'.CONTACT'// 
C ----------------------------------------------------------------------
CCCCC DEFINITION DE LA SD
C   CHAQUE ZONE DE CONTACT DECRITE COMME UNE APPARIEMENT SYMETRIQUE VA 
C   CREER
C   UNE ZONE DE CONTACT SUPPLEMENTAIRE A LA FIN DES ZONES.
C   ON APPELLE <ZONE DE REFERENCE> LA ZONE DE CONTACT DEFINIE COMME 
C   ETANT 
C   SYMETRIQUE PAR L'UTILISATEUR DANS AFFE_CHAR_MECA. 
CCCCC STRUCTURE DE LA SD
C   SYMECO(1)  : NOMBRE DE ZONES DE CONTACT SYMETRIQUES
C   SYMECO(1+N): NUMERO DE LA ZONE DE REFERENCE POUR LA SYMETRIE
CCCCC EXEMPLE DE SD
C   ZONES 1,2,3 et 5: NON SYMETRIQUE
C   ZONE 4: SYMETRIQUE
C   CLASSEMENT:
C     ZONE 1 - ZONE 2 - ZONE 3 - ZONE 4REF - ZONE 5 - ZONE 4SYM
C   ZONE 4REF: PREMIERE ZONE DE CONTACT CREEE PAR LES MAITRES/ESCLAVES 
C              DEFINIS
C              DANS LE CONTACT PAR AFFE_CHAR_MECA (ZONE DE REFERENCE)
C   ZONE 4SYM: SECONDE ZONE DE CONTACT CREEE PAR ECHANGE DES 
C              MAITRES/ESCLAVES
C              DEFINIS DANS LE CONTACT SYMETRIQUE PAR AFFE_CHAR_MECA
C   SYMECO(1) = 1 (UNE ZONE EST DE TYPE SYMETRIQUE)
C   SYMECO(2) = 4 (CETTE ZONE SYMETRIQUE A POUR ZONE DE REFERENCE 4)
C ----------------------------------------------------------------------
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
      CHARACTER*24  SYMEC
      INTEGER       JSYME
      CHARACTER*16  APPAR
      INTEGER       IOC,NOC
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C --- NOM JEVEUX
      SYMEC  = CHAR(1:8)//'.CONTACT.SYMECO'

C --- CREATION DE LA SD SYMECO
      CALL WKVECT (SYMEC,'G V I',NZOCO+1,JSYME)     

      NSYME     = 0

      DO 1 IOC = 1,NZOCO   
         CALL GETVTX(MOTFAC,'APPARIEMENT',IOC,1,1,APPAR,NOC)
         IF ((APPAR.EQ.'NODAL_SYME').OR.
     &        (APPAR.EQ.'MAIT_ESCL_SYME')) THEN
            NSYME = NSYME + 1
C             NUMERO DE LA ZONE DE REFERENCE
            ZI(JSYME+NSYME) = IOC   
         ENDIF
   1  CONTINUE

C      NOMBRE DE ZONES DE CONTACT SYMETRIQUES
      ZI(JSYME) = NSYME
C
C ----------------------------------------------------------------------
C
      CALL JEDEMA()
      END
