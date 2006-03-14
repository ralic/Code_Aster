      SUBROUTINE VTCREB(CHAMPZ,NUMEDZ,BASEZ,TYPCZ,NEQ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 14/03/2006   AUTEUR MABBAS M.ABBAS 
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
C-----------------------------------------------------------------------
C     CREATION D'UNE STRUCTURE CHAM_NO "CHAMP"
C
C     IN  CHAMPZ : K19 : NOM DU CHAM_NO A CREER
C     IN  NUMEDZ : K24 : PROF_CHNO DU CHAM_NO
C     IN  BASEZ  : CH1 : NOM DE LA BASE SUR LAQUELLE LE CHAM_NO DOIT
C                        ETRE CREE
C     IN  TYPCZ  :     : TYPE DES VALEURS DU CHAM_NO A CREER
C                  'R'    ==> COEFFICIENTS REELS
C                  'C'    ==> COEFFICIENTS COMPLEXES
C     OUT   NEQ   : I   : INTEGER
C     REMARQUE:  AUCUN CONTROLE SUR LE "TYPC" QUE L'ON PASSE TEL QUEL
C                A JEVEUX_MON_NEVEU
C     PRECAUTIONS D'EMPLOI :
C       1) LE CHAM_NO "CHAMP" NE DOIT PAS EXISTER
C       2) LES COEFFICIENTS DU CHAM_NO "CHAMP" NE SONT PAS AFFECTES
C----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS      
      CHARACTER*(*) CHAMPZ,NUMEDZ,BASEZ,TYPCZ
      INTEGER NEQ
      
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
      CHARACTER*32 JEXNUM
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------

C DECLARATION VARIABLES LOCALES
      INTEGER      IDIME,NBSD,IFETC,IDD,ILIMPI,IFETN,NEQL
      CHARACTER*1  CLASSE,TYPC
      CHARACTER*8  K8BID
      CHARACTER*11 K11B      
      CHARACTER*24 CHAMP,NUMEDD,METHOD,SDFETI,K24BID,K24B
      
C------------------------------------------------------------------
C INIT.
      CALL JEMARQ()
      CHAMP  = CHAMPZ
      NUMEDD = NUMEDZ
      CLASSE = BASEZ(1:1)
      TYPC   = TYPCZ(1:1)

C --------------------------------------------------------------
C CREATION ET REMPLISSAGE DE LA SD CHAM_NO "MAITRE"
C --------------------------------------------------------------      
      
      CALL VTCRE1(CHAMP,NUMEDD,CLASSE,TYPC,METHOD,SDFETI,0,NEQ)
C --------------------------------------------------------------
C CREATION ET REMPLISSAGE DE LA SD CHAM_NO.REFE "ESCLAVE" LIEE A
C CHAQUE SOUS-DOMAINE
C --------------------------------------------------------------
      IF (METHOD(1:4).EQ.'FETI') THEN
      
        CALL JEVEUO(SDFETI(1:19)//'.FDIM','L',IDIME)
        NBSD=ZI(IDIME)

C CONSTITUTION DE L'OBJET JEVEUX .FETC
        CALL WKVECT(CHAMP(1:19)//'.FETC',CLASSE//' V K24',NBSD,
     &    IFETC)
C STOCKE &&//NOMPRO(1:6)//'.2.' POUR COHERENCE AVEC L'EXISTANT     
        K11B=CHAMP(1:10)//'.'

        K24B=' '
        CALL JEVEUO('&FETI.LISTE.SD.MPI','L',ILIMPI)
        DO 10 IDD=1,NBSD
          IF (ZI(ILIMPI+IDD).EQ.1) THEN
          
            CALL JEMARQ()        
C REMPLISSAGE OBJET .FETC
C NOUVELLE CONVENTION POUR LES CHAM_NOS FILS, GESTTION DE NOMS
C ALEATOIRES
            CALL GCNCON('.',K8BID)
            K8BID(1:1)='F'          
            K24B(1:19)=K11B//K8BID          
            ZK24(IFETC+IDD-1)=K24B
            CALL JEVEUO(NUMEDD(1:14)//'.FETN','L',IFETN)          
            CALL VTCRE1(K24B,ZK24(IFETN+IDD-1),CLASSE,TYPC,K24BID,
     &        K24BID,IDD,NEQL)
            CALL JEDEMA()
            
          ENDIF
   10   CONTINUE

      ENDIF
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

                        
C FIN ------------------------------------------------------------------
      CALL JEDEMA()
      END
