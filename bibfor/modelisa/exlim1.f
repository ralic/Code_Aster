      SUBROUTINE EXLIM1 (LISMAZ, LONLIS, MODELZ, BASEZ, LIGREZ)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)      LISMAZ, MODELZ, BASEZ, LIGREZ
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 05/05/2004   AUTEUR BOITEAU O.BOITEAU 
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
C IN  : LISMAZ : NOM DE LA LISTE DES MAILLES CONSTITUANT LE
C                LIGREL A CREER
C IN  : LONLIS : LONGUEUR DE LA LISTE DES MAILLES
C IN  : MODELZ : NOM DU MODELE REFERENCANT LES MAILLES DE LISMAI
C                DES GRELS
C IN  : BASEZ  : BASE SUR-LAQUELLE ON CREE LE LIGREL
C OUT : LIGREZ : LIGREL A CREER
C     ------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       02/11/03 (OB): MODIF. POUR FETI: RAJOUT 'DOCU' POUR 'NOMA'.
C----------------------------------------------------------------------

C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*32       JEXNOM,        JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

      CHARACTER*1     BASE,K1BID
      CHARACTER*8     MODELE,NOMA
      CHARACTER*16    PHENO
      CHARACTER*19    LIGREL,LIGRMO
      CHARACTER*24    NOMMAI,CPTLIE,LISMAI
      INTEGER     IBID
C     ------------------------------------------------------------------

      CALL JEMARQ()
      
      BASE   = BASEZ
      LISMAI = LISMAZ
      MODELE = MODELZ
      LIGREL = LIGREZ

      NBGREL = LONLIS

C --- RECUPERATION DE LA LISTE DE MAILLES
C     -----------------------------------
      CALL JEVEUO(LISMAI,'L',ILISMA)

C --- MAILLAGE ASSOCIE AU MODELE
C     --------------------------
      CALL DISMOI('F','NOM_MAILLA',MODELE,'MODELE',IB,NOMA  ,IE)
      NOMMAI = NOMA//'.NOMMAI'

C --- LIGREL DU MODELE
C     ----------------
      CALL DISMOI('F','NOM_LIGREL',MODELE,'MODELE',IB,LIGRMO,IE)
      CALL JEVEUO(LIGRMO//'.REPE','L',JREPE)

C     --- OBJET NBNO
C         ----------
      CALL WKVECT(LIGREL//'.NBNO',BASE//' V I',1,JDNB)
      ZI(JDNB) = 0

C     --- OBJET NOMA
C         ----------
      CALL WKVECT(LIGREL//'.NOMA',BASE//' V K8',1,JDNM)
      ZK8(JDNM) = NOMA
C                               9012345678901234    
      CALL JELIRA(MODELE(1:8)//'.MODELE    .NOMA','DOCU',IBID,PHENO)
      CALL JEECRA(LIGREL//'.NOMA','DOCU',IBID,PHENO)      

C     --- OBJET LIEL
C         ----------
      CPTLIE = LIGREL//'.LIEL'
      LONT = 2 * NBGREL
      CALL JECREC(CPTLIE,BASE//' V I','NU','CONTIG','VARIABLE',NBGREL)
      CALL JEECRA(CPTLIE,'LONT',LONT,' ')
      CALL JEVEUO(CPTLIE,'E',JDLI)

C     --- STOCKAGE DES GROUPES ELEMENTS DANS LIEL
C         ---------------------------------------
      NUMVEC = 0
      NMGREL = 2
      DO 10 I = 1 , NBGREL
         CALL JENONU(JEXNOM(NOMMAI,ZK8(ILISMA+I-1)),NUMAIL)
         CALL JECROC(JEXNUM(CPTLIE,I))
         CALL JEECRA(JEXNUM(CPTLIE,I),'LONMAX',NMGREL,' ')
         NUMVEC = NUMVEC + 1
         ZI(JDLI+NUMVEC-1) = NUMAIL
         IGREL = ZI(JREPE+2*(NUMAIL-1))
         IF (IGREL.EQ.0) THEN
           CALL UTMESS('F','EXLIM1','LA MAILLE : '//ZK8(ILISMA+I-1)
     &     //' N''EST PAS AFFECTEE PAR UN ELEMENT FINI.')
         END IF
         CALL JEVEUO(JEXNUM(LIGRMO//'.LIEL',IGREL),'L',IALIEL)
         CALL JELIRA(JEXNUM(LIGRMO//'.LIEL',IGREL),'LONMAX',NEL,K1BID)
         NUMVEC = NUMVEC + 1
         ZI(JDLI+NUMVEC-1) = ZI(IALIEL+NEL-1)
 10   CONTINUE            
 

C     ---  ADAPTATION DE LA TAILLE DES GRELS
C          ---------------------------------
      CALL ADALIG(LIGREL)

C     --- CREATION DE LA CORRESPONDANCE MAILLE --> (IGREL,IM)
C         ---------------------------------------------------
      CALL CORMGI(BASE, LIGREL)

      CALL JEDEMA()
      END
