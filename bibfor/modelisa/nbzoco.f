      SUBROUTINE NBZOCO(CHAR,MOTFAC,NOMA,IREAD,IWRITE,JZONE,
     &                  NSUCO,NGTOT)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 07/10/2004   AUTEUR MABBAS M.ABBAS 
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
      CHARACTER*8  NOMA
      INTEGER      IREAD
      INTEGER      IWRITE  
      INTEGER      JZONE    
      INTEGER      NSUCO
      INTEGER      NGTOT
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : POINCO
C ----------------------------------------------------------------------
C
C DETERMINATION DU NOMBRE DE SURFACES DE CONTACT ET DU NOMBRE TOTAL DE
C MAILLES ET DE GROUPES DE MAILLES POUR LA ZONE IREAD
C REMPLISSAGE DU POINTEUR ASSOCIE JZONE
C
C IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
C IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'CONTACT')
C IN  NOMA   : NOM DU MAILLAGE
C IN  IREAD  : INDICE POUR LIRE LES DONNEES DANS AFFE_CHAR_MECA
C IN  IWRITE : INDICE POUR ECRIRE LES DONNEES DANS LA SD DEFICONT
C IN  JZONE  : POINTEUR DES ZONES DE CONTACT
C OUT NSUCO  : REPERE DU NOMBRE DE SURFACES DE CONTACT POUR CETTE 
C              ZONE DE CONTACT
C OUT NGTOT  : NOMBRE TOTAL DE GROUPES DE MAILLES ET DE MAILLES 
C              IMPLIQUES POUR CETTE ZONE DE CONTACT
C ----------------------------------------------------------------------
C LE REPERAGE NSUCO EST GLOBAL POUR TOUTES LES ZONES DE CONTACT
C      EXEMPLE:
C       ZONE 1: GROUP_MA_MAIT + MAILLE_ESCL
C       ZONE 2: GROUP_MA_MAIT + MAILLE_MAIT + MAILLE_ESCL
C       ZI(JZONE+0) = 0 (INITIALISE DANS POINCO)
C       ZI(JZONE+1) = 2
C       ZI(JZONE+2) = 5
C /!\   NSUCO EST INITIALISE DANS POINCO
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
      CHARACTER*8  K8BID
      INTEGER      NGMAIT,NGESCL,NMMAIT,NMESCL
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NGTOT  = 0
C
C ======================================================================
C              DETERMINATION DU NOMBRE TOTAL DE SURFACES
C                  ET REMPLISSAGE DU POINTEUR PZONE
C ======================================================================
C
C      RECUPERATION DU NOMBRE DE:
C      1/ GROUP_MA_MAIT -> NGMAIT                
      CALL GETVEM(NOMA,'GROUP_MA',MOTFAC,'GROUP_MA_MAIT',
     +            IREAD,1,0,K8BID,NGMAIT)
C      2/ GROUP_MA_ESCL -> NGESCL      
      CALL GETVEM(NOMA,'GROUP_MA',MOTFAC,'GROUP_MA_ESCL',
     +            IREAD,1,0,K8BID,NGESCL)
C      3/ MAILLE_MAIT   -> NMMAIT      
      CALL GETVEM(NOMA,'MAILLE',MOTFAC,'MAILLE_MAIT',
     +            IREAD,1,0,K8BID,NMMAIT)
C      4/ MAILLE_ESCL   -> NMESCL      
      CALL GETVEM(NOMA,'MAILLE',MOTFAC,'MAILLE_ESCL',
     +            IREAD,1,0,K8BID,NMESCL)
C      NSUCO: REFERENCE POUR LE NOMBRE DE SURFACES DE CONTACT 
C            (QUATRE SURFACES MAXI PAR ZONE)
      IF (NGMAIT.NE.0) NSUCO = NSUCO + 1
      IF (NGESCL.NE.0) NSUCO = NSUCO + 1
      IF (NMMAIT.NE.0) NSUCO = NSUCO + 1
      IF (NMESCL.NE.0) NSUCO = NSUCO + 1
C      NGTOT:  NOMBRE TOTAL DE GROUPES DE MAILLES ET DE MAILLES
C              IMPLIQUES POUR CETTE ZONE DE CONTACT
      NGTOT = ABS(NGMAIT) + ABS(NGESCL) + ABS(NMMAIT) + ABS(NMESCL)
C      MISE A JOUR DU POINTEUR SUR LES SURFACES DE CONTACT DE LA ZONE
      ZI(JZONE+IWRITE) = NSUCO
C
C ----------------------------------------------------------------------
C
      CALL JEDEMA()
      END
