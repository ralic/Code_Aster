      SUBROUTINE LISTCO (MOTFAZ,NOMAZ,IOC,NTRAV,NBMA,NBNO,NBNOQU,
     +                   LISTMA,LISTNO,LISTQU,CHARZ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 11/07/2001   AUTEUR CIBHHBC R.FERNANDES 
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
C
      IMPLICIT NONE
C
      INTEGER       IOC,NTRAV,NBMA,NBNO,LISTMA(NBMA),LISTNO(NBNO)
      INTEGER       NBNOQU, LISTQU(3*NBNOQU)
      CHARACTER*(*) MOTFAZ,NOMAZ,CHARZ
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : CALICO
C ----------------------------------------------------------------------
C
C STOCKAGE DES MAILLES ET NOEUDS DE CONTACT DES DIFFERENTES SURFACES.
C
C IN  MOTFAZ : MOT-CLE FACTEUR (VALANT 'CONTACT')
C IN  NOMAZ  : NOM DU MAILLAGE
C IN  IOC    : NUMERO D'OCCURENCE DU MOT-CLE FACTEUR 'CONTACT' (ZONE)
C IN  NMBA   : NOMBRE DE MAILLES DANS LA ZONE IOC
C IN  NBNO   : NOMBRE DE NOEUDS DANS LA ZONE IOC
C IN  NTRAV  : DIMENSION DU TABLEAU DE TRAVAIL
C VAR LISTMA : LISTE DES NUMEROS DES MAILLES DE CONTACT
C VAR LISTNO : LISTE DES NUMEROS DES NOEUDS DE CONTACT
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
      CHARACTER*32       JEXNUM , JEXNOM
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER      NG,NGR,NMAI,N1,N2,NBMAIL,NB,NUMAIL
      INTEGER      II1,II2,IPMA,IPNO,INO,JCOOR,NOUNDF
      INTEGER      JGRO,JBID,JDES,IBID,IATYMA,ITYP,NUTYP
      INTEGER      N1Q, N2Q, IPQU, NOEUSO, NOEUMI, NBNOMI
      CHARACTER*1  K1BID
      CHARACTER*8  K8BID,NOMA,NOMTM,CHAR,MOTCLE
      CHARACTER*16 MOTFAC
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      MOTFAC = MOTFAZ
      NOMA   = NOMAZ
      CHAR   = CHARZ
C
      CALL WKVECT ('&&LISTCO.TRAV','V V K8',NTRAV,JBID)
C
      NBMAIL = NBMA
      IPMA   = 0
      IPNO   = 0
      IPQU   = 0
C
C ======================================================================
C      REMPLISSAGE DE LA LISTE DE MAILLES ET DE LA LISTE DE NOEUDS
C ======================================================================
C
      CALL JEVEUO(NOMA//'.TYPMAIL','L',IATYMA)
C --- MOT-CLE GROUP_MA_1
C
         CALL GETVEM(NOMA,'GROUP_MA',MOTFAC,'GROUP_MA_1',
     +            IOC,1,0,K8BID,NG)
         IF (NG.NE.0) THEN
             MOTCLE = 'GROUP_MA'
             NG = - NG
             CALL GETVEM(NOMA,'GROUP_MA',MOTFAC,'GROUP_MA_1',
     +                IOC,1,NG,ZK8(JBID),NGR)
             CALL EXNOEL(CHAR,NOMA,MOTCLE,NGR,ZK8(JBID),
     +                   NBMA,NBNO,NBNOQU,LISTMA,LISTNO,
     +                   LISTQU,IPMA, IPNO, IPQU)
         END IF
C
C --- MOT-CLE MAILLE_1
C
         CALL GETVEM(NOMA,'MAILLE',MOTFAC,'MAILLE_1',
     +          IOC,1,0,K8BID,NBMA)
         IF (NBMA.NE.0) THEN
             MOTCLE = 'MAILLE'
             NBMA = -NBMA
             CALL GETVEM(NOMA,'MAILLE',MOTFAC,'MAILLE_1',
     +              IOC,1,NBMA,ZK8(JBID),NMAI)
             CALL EXNOEL(CHAR,NOMA,MOTCLE,0,ZK8(JBID),NBMA,
     +                   NBNO,NBNOQU,LISTMA,LISTNO,LISTQU,
     +                   IPMA, IPNO, IPQU)
         ENDIF
C
C --- MOT-CLE GROUP_MA_2
C
         CALL GETVEM(NOMA,'GROUP_MA',MOTFAC,'GROUP_MA_2',
     +            IOC,1,0,K8BID,NG)
         IF (NG.NE.0) THEN
             MOTCLE = 'GROUP_MA'
             NG = -NG
             CALL GETVEM(NOMA,'GROUP_MA',MOTFAC,'GROUP_MA_2',
     +                IOC,1,NG,ZK8(JBID),NGR)
             CALL EXNOEL(CHAR,NOMA,MOTCLE,NGR,ZK8(JBID),
     +                   NBMA,NBNO,NBNOQU,LISTMA,LISTNO,
     +                   LISTQU,IPMA, IPNO, IPQU)
         END IF
C
C --- MOT-CLE MAILLE_2
C
         CALL GETVEM(NOMA,'MAILLE',MOTFAC,'MAILLE_2',
     +          IOC,1,0,K8BID,NB)
         IF (NB.NE.0) THEN
             MOTCLE = 'MAILLE'
             NB = -NB
             CALL GETVEM(NOMA,'MAILLE',MOTFAC,'MAILLE_2',
     +              IOC,1,NB,ZK8(JBID),NMAI)
             CALL EXNOEL(CHAR,NOMA,MOTCLE,NGR,ZK8(JBID),
     +                   NBMA,NBNO,NBNOQU,LISTMA,LISTNO,
     +                   LISTQU,IPMA, IPNO, IPQU)
         ENDIF
C
C --- VERIFICATIONS ET ECRITURES
C
      NBMA = NBMAIL
      IF (IPMA.NE.NBMA)   CALL UTMESS('F','LISTCO_01','ERREUR SUR IPMA')
      IF (IPNO.NE.NBNO)   CALL UTMESS('F','LISTCO_02','ERREUR SUR IPNO')
      IF (IPQU.NE.NBNOQU) CALL UTMESS('F','LISTCO_03','ERREUR SUR IPQU')
C
      CALL JEDETR ('&&LISTCO.TRAV')
C
C ----------------------------------------------------------------------
C
      CALL JEDEMA()
      END
