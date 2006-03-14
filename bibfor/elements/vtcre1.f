      SUBROUTINE VTCRE1(CHAMP,NUMEDD,CLASSE,TYPE,METHOD,SDFETI,
     &                  NUMSD,NEQ)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 14/03/2006   AUTEUR MABBAS M.ABBAS 
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
C    - FONCTION REALISEE:  CREATION DES OBJETS JEVEUX DE LA STRUCTURE
C      DE DONNEES CHAMNO. UTILITAIRE DE BAS NIVEAU LANCE PAR VTCREB.
C
C     IN  CHAMP  : K24 : NOM DU CHAM_NO A CREER
C     IN  NUMEDD : K24 : PROF_CHNO DU CHAM_NO
C     IN  CLASSE : K1  : NOM DE LA BASE SUR LAQUELLE LE CHAM_NO DOIT
C                        ETRE CREE
C     IN  TYPCE  : K1  : TYPE DES VALEURS DU CHAM_NO A CREER
C                  'R'    ==> COEFFICIENTS REELS
C                  'C'    ==> COEFFICIENTS COMPLEXES
C     IN NUMSD   : I   : NUMERO DE SOUS-DOMAINE
C     OUT METHOD,SDFETI: K24 : PARAMETRES DU SOLVEUR.
C     OUT   NEQ   : I   : INTEGER
C   -------------------------------------------------------------------
C     SUBROUTINES APPELLEES:
C       DIVERS: DISMOI,SDCHGD,UTIMSD.
C       JEVEUX:JEMARQ,JEDEMA,WKVECT,JEVEUO,JEECRA.
C
C     FONCTIONS INTRINSEQUES:
C       NONE.
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       25/11/03 (OB): CREATION.
C----------------------------------------------------------------------
C RESPONSABLE BOITEAU O.BOITEAU
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      CHARACTER*1  CLASSE,TYPE
      CHARACTER*24 CHAMP,NUMEDD,METHOD,SDFETI
      INTEGER      NUMSD,NEQ

C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX --------------------

C DECLARATION VARIABLES LOCALES
      INTEGER      JCHAMP,JREFN,IBID,IERD,JNEQ,LCHP,NBREFN
      CHARACTER*8  K8BID
      CHARACTER*24 VALE,REFE,DESC

      DATA VALE/'                   .VALE'/
      DATA REFE/'                   .REFE'/
      DATA DESC/'                   .DESC'/
            
C------------------------------------------------------------------
      CALL JEMARQ()

C ------------------------------- REFE --------------------------------
C --- AFFECTATION DES INFORMATIONS DE REFERENCE A CHAMP

C ON ETOFFE L'OBJET .REFE DU CHAMNO AVEC LE TYPE DE RESOLUTION ET LE
C NOM DE LA SD_FETI PRIS DANS LE .REFN DU NUME_DDL SOUS-JACENT

      CALL JEVEUO(NUMEDD(1:14)//'.NUME.REFN','L',JREFN)
      REFE(1:19) = CHAMP 
           
C NUME_DDL ET DONC CHAM_NO ETENDU, OUI OU NON ?      
      CALL JELIRA(NUMEDD(1:14)//'.NUME.REFN','LONMAX',NBREFN,K8BID)
      IF (NBREFN.NE.4) THEN      
        METHOD='XXXX'
        SDFETI='XXXX'
        CALL WKVECT(REFE,CLASSE//' V K24',2,JCHAMP)                     
      ELSE
        CALL WKVECT(REFE,CLASSE//' V K24',4,JCHAMP)
        METHOD = ZK24(JREFN+2)
        SDFETI = ZK24(JREFN+3)
        ZK24(JCHAMP+2) = METHOD
        ZK24(JCHAMP+3) = SDFETI
      ENDIF
      ZK24(JCHAMP) = ZK24(JREFN)
      ZK24(JCHAMP+1) = NUMEDD(1:14)//'.NUME'
      
C ------------------------------- DESC --------------------------------
C --- AFFECTATION DES INFORMATIONS DE REFERENCE A CHAMP

      DESC(1:19) = CHAMP
      CALL WKVECT(DESC,CLASSE//' V I',2,JCHAMP)
      CALL JEECRA(DESC,'DOCU',IBID,'CHNO')
      CALL DISMOI('F','NUM_GD_SI',NUMEDD,'NUME_DDL',ZI(JCHAMP),
     &            K8BID,IERD)
      ZI(JCHAMP+1) = 1

C ------------------------------- VALE --------------------------------
C --- CREATION DE L'OBJET SIMPLE DES VALEURS
C --- TYPE DES VALEURS, LONGUEUR D'UN VECTEUR

      CALL JEVEUO(NUMEDD(1:14)//'.NUME.NEQU','L',JNEQ)
      NEQ = ZI(JNEQ)
      VALE(1:19) = CHAMP
      CALL JECREO(VALE,CLASSE//' V '//TYPE)
      CALL JEECRA(VALE,'LONMAX',NEQ,K8BID)
      CALL JEVEUO(VALE,'E',LCHP)

C --- CHANGER LA GRANDEUR
      CALL SDCHGD(CHAMP,TYPE)

C FIN ------------------------------------------------------
      CALL JEDEMA()
      END
