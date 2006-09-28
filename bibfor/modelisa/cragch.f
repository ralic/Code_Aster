      SUBROUTINE CRAGCH (LONG, TYPCOE, TYPVAL, LIGRCH)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
      CHARACTER*4  TYPCOE, TYPVAL
      CHARACTER*19 LIGRCH
C ---------------------------------------------------------------------
C     CREATION OU EXTENSION DES CARTES .CMULT ET .CIMPO
C     DU LIGREL DE CHARGE LIGRCH D'UN NOMBRE DE TERMES
C     EGAL A LONG
C
C     LONG DOIT ETRE > 0
C
C----------------------------------------------------------------------
C  LONG          - IN   - I    - : NOMBRE DE GRELS A RAJOUTER A LIGRCH-
C----------------------------------------------------------------------
C  TYPCOE        - IN   - K4   - : TYPE DES COEFFICIENTS A ECRIRE DANS-
C                -      -      - :  LA CARTE CMULT := 'REEL' OU 'COMP'-
C----------------------------------------------------------------------
C  TYPVAL        - IN   - K4   - : TYPE DES VALEURS A ECRIRE DANS L
C                -      -      - :  CARTE CIMPO :='REEL' OU 'COMP'
C                -      -      - :                 OU 'FONC'
C----------------------------------------------------------------------
C  LIGRCH        - IN   - K24  - : NOM DU LIGREL DE CHARGE
C                - JXVAR-      -
C----------------------------------------------------------------------
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
      COMMON  / KVARJE /ZK8(1),ZK16(1),ZK24(1),ZK32(1), ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
C --------- VARIABLES LOCALES ------------------------------------------
      CHARACTER*8   NOMA, MOD,BASE
      CHARACTER*19  CA1, CA2
      INTEGER IRET, LONG, LONGUT,IBID,IER,JNOMA,JDESC,NGDMX,NEDIT,NDISP
C --------- FIN  DECLARATIONS  VARIABLES LOCALES ----------------------
C
      CALL JEMARQ()
      IF (LONG.LE.0) THEN
               CALL U2MESS('F','MODELISA4_37')
      ENDIF
C
C --- CARTES DE LA CHARGE ---
C
      IF (LIGRCH(12:13).EQ.'TH')  THEN
        CA1= LIGRCH(1:8)//'.CHTH.CMULT'
        CA2= LIGRCH(1:8)//'.CHTH.CIMPO'
C     OLIVIER
      ELSE IF (LIGRCH(12:13).EQ.'ME') THEN
        CA1= LIGRCH(1:8)//'.CHME.CMULT'
        CA2= LIGRCH(1:8)//'.CHME.CIMPO'
      ELSE
        CA1= LIGRCH(1:8)//'.CHAC.CMULT'
        CA2= LIGRCH(1:8)//'.CHAC.CIMPO'
      ENDIF
C
C --- ON CREE LES CARTES .CMULT ET .CIMPO SI ELLES N'EXISTENT PAS ---
C
      CALL JEEXIN(CA1//'.DESC',IRET)
      IF (IRET.EQ.0) THEN
C
C --- MODELE ASSOCIE AU LIGREL DE CHARGE ---
C
         CALL DISMOI('F','NOM_MODELE',LIGRCH(1:8),'CHARGE',IBID,
     &                MOD,IER)
C
C --- MAILLAGE ASSOCIE AU MODELE ---
C
         CALL JEVEUO(MOD(1:8)//'.MODELE    '//'.NOMA','L',JNOMA)
         NOMA = ZK8(JNOMA)
C
         IF (TYPCOE.EQ.'REEL'.OR. TYPCOE.EQ.'FONC') THEN
             CALL ALCART('G',CA1, NOMA, 'DDLM_R')
         ELSE IF (TYPCOE.EQ.'COMP') THEN
             CALL ALCART('G',CA1, NOMA, 'DDLM_C')
         ELSE
            CALL U2MESK('F','MODELISA2_37',1,TYPCOE)
         ENDIF
C
         IF (TYPVAL.EQ.'REEL') THEN
            CALL ALCART('G',CA2, NOMA, 'DDLI_R')
         ELSE IF (TYPVAL.EQ.'FONC') THEN
            CALL ALCART('G',CA2, NOMA, 'DDLI_F')
         ELSE IF (TYPVAL.EQ.'COMP') THEN
            CALL ALCART('G',CA2, NOMA, 'DDLI_C')
         ELSE
            CALL U2MESK('F','MODELISA2_37',1,TYPVAL)
         END IF
      END IF


C
C --- VERIFICATION DE L'ADEQUATION DE LA TAILLE DES CARTES ---
C --- .CMULT ET .CIMPO DE LA CHARGE                        ---
C
      CALL JEVEUO(CA1(1:19)//'.DESC','L',JDESC)
      NGDMX = ZI(JDESC-1+2)
      NEDIT = ZI(JDESC-1+3)
      NDISP = NGDMX - NEDIT
      CALL ASSERT(NDISP.GE.0)
      IF (LONG.GT.NDISP) THEN
C ---       LA TAILLE DES CARTES .CMULT ET .CIMPO EST    ---
C ---       INSUFFISANTE ON LES REDIMENSIONNE DE MANIERE ---
C ---       ADEQUATE                                     ---
          LONGUT = NEDIT + LONG
          CALL AGCART(LONGUT, CA1)
          CALL AGCART(LONGUT, CA2)

C ---     AGRANDISSEMENT DE CA1.LIMA :
          CALL JEDUPO(CA1//'.LIMA','V',CA1//'.TRAV',.FALSE.)
          CALL JELIRA(CA1//'.LIMA','CLAS',IBID,BASE)
          CALL JEDETR(CA1//'.LIMA')
          CALL COCOPG(CA1//'.TRAV', CA1//'.LIMA', LONGUT,2*LONGUT,BASE)
          CALL JEDETR(CA1//'.TRAV')

C ---     AGRANDISSEMENT DE CA2.LIMA :
          CALL JEDUPO(CA2//'.LIMA','V',CA2//'.TRAV',.FALSE.)
          CALL JELIRA(CA2//'.LIMA','CLAS',IBID,BASE)
          CALL JEDETR(CA2//'.LIMA')
          CALL COCOPG(CA2//'.TRAV', CA2//'.LIMA', LONGUT,2*LONGUT,BASE)
          CALL JEDETR(CA2//'.TRAV')
      ENDIF
C
      CALL JEDEMA()
      END
