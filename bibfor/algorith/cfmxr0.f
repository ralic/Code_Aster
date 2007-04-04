      SUBROUTINE CFMXR0(RESOCO,MODELE,CNSINR)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 04/04/2007   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT     NONE
      CHARACTER*19 CNSINR
      CHARACTER*24 MODELE,RESOCO
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (TOUTES METHODES - POST-TRAITEMENT)
C
C CREER LE CHAM_NO_S POUR L ARCHIVAGE DU CONTACT PAR NMARCH
C
C ----------------------------------------------------------------------
C
C
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  MODELE : SD MODELE
C I/O CNSINR : CHAM_NO_S POUR L'ARCHIVAGE DU CONTACT
C
C ------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C --------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------
C 
      INTEGER      CFMMVD,ZRESU
      INTEGER      IFM,NIV
      INTEGER      ILIAI,IBID
      INTEGER      NBLIAI,NESCL
      INTEGER      ICMP,ICONTA
      CHARACTER*8  LICMPR(20),NOMCMP(20), NOMA
      CHARACTER*24 APPARI
      INTEGER      JAPPAR
      INTEGER      IRET
      INTEGER      JCNSVR,JCNSLR
C ----------------------------------------------------------------------
      DATA NOMCMP
     &   / 'CONT','JEU' ,'RN'  ,
     &     'RNX' ,'RNY' ,'RNZ' ,
     &     'GLIX','GLIY','GLI' ,
     &     'RTAX','RTAY','RTAZ',
     &     'RTGX','RTGY','RTGZ',
     &     'RX'  ,'RY'  ,'RZ'  ,
     &     'R'   ,'HN'/
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)
C ======================================================================
C --- SORTIE SI PAS DE CONTACT 
C ======================================================================
      APPARI = RESOCO(1:14)//'.APPARI'
      CALL JEEXIN(APPARI,ICONTA)
      CNSINR = '&&RESUCO.CNSINR'
       
      IF (ICONTA.EQ.0) THEN
        GOTO 999
      ENDIF

C ======================================================================
C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C ======================================================================

      CALL JEVEUO(APPARI,'L',JAPPAR)

      ZRESU  = CFMMVD('ZRESU')
      
C --- CHANGEZ LA TAILLE DE LICMPR,NOMCMP   
      IF (ZRESU.GT.20) CALL ASSERT(.FALSE.)
      
C -- MAILLAGE SOUS-TENDU PAR LE MODELE
      CALL DISMOI('F','NOM_MAILLA',MODELE,'MODELE',IBID,NOMA,IRET)

C ======================================================================
C
C --- CREATION DU CHAM_NO_S VIDE POUR LE CONTACT INITIAL
C
C ======================================================================
      NESCL  = ZI(JAPPAR)
      NBLIAI = NESCL
      

      DO 1 ICMP = 1,ZRESU
        LICMPR(ICMP) = NOMCMP(ICMP)
 1    CONTINUE

      CALL CNSCRE(NOMA,'INFC_R',ZRESU,LICMPR,'V',CNSINR)
      CALL JEVEUO(CNSINR//'.CNSV','E',JCNSVR)
      CALL JEVEUO(CNSINR//'.CNSL','E',JCNSLR)

      DO 10 ILIAI = 1,NBLIAI
         DO 11 ICMP = 1,ZRESU
            ZR(JCNSVR-1+(ILIAI-1)*ZRESU+ICMP) = 0.0D0
            ZL(JCNSLR-1+(ILIAI-1)*ZRESU+ICMP) = .TRUE.
 11      CONTINUE
         ZR(JCNSVR-1+(ILIAI-1)*ZRESU+2 ) = 0.0D0
 10   CONTINUE

999   CONTINUE
      CALL JEDEMA()
      END
