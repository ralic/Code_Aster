      SUBROUTINE  MASYNS(MATAS)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 11/04/97   AUTEUR VABHHTS J.PELLET 
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
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C      MASYNS :   LA MATRICE ASSEMBLEE MATAS, SYMETRIQUE ET
C                 STOCKEE EN LIGNE DE CIEL EST TRANSFORMEE
C                 EN MATRICE NON-SYMETRIQUE
C
C   ARGUMENT        E/S  TYPE         ROLE
C    MATAS          VAR    K*       MATRICE ASSEMBLEE, EN LIGNE DE CIEL
C                                   SYMETRIQUE     EN ENTREE
C                                   NON-SYMETRIQUE EN SORTIE
C                                   (I.E. LE NOMBRE DE BLOCS EST
C                                    DOUBLE ET LES VALEURS DES TERMES
C                                    RESTENT LES MEMES)
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
           CHARACTER*(*)  MATAS
C -----  VARIABLES LOCALES
           CHARACTER*1    BASE
           CHARACTER*8    K8BID
           CHARACTER*19   MAWORK, MATA19
C -----  COMMUNS NORMALISES  JEVEUX
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32, JEXNUM
      CHARACTER*80 ZK80
C -----  FIN COMMUNS NORMALISES  JEVEUX
C
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ()
C
      MAWORK = '&&MASYNS.VALE'
      MATA19 = MATAS(1:19)
C
C ----  RECUPERATION DE LA BASE SUR-LAQUELLE SE TROUVE LA MATRICE :
C       ---------------------------------------------------------
      CALL JELIRA(MATAS(1:19)//'.VALE','CLAS',IBID,BASE)
C
C ----  CONSTRUCTION DU DESCRIPTEUR DE LA MATRICE :
C       -----------------------------------------
      CALL MTDSCR(MATAS)
C
C ----  RECUPERATION DU DESCRIPTEUR DE LA MATRICE :
C       -----------------------------------------
      CALL JEVEUO(MATAS(1:19)//'.&INT','L',LMAT)
C
C ----  ON VERIFIE QUE LES VALEURS DE LA MATRICE SONT REELLES :
C       -----------------------------------------------------
      IF (ZI(LMAT+3).NE.1) THEN
          CALL UTMESS('F','MASYNS','LES VALEURS DE LA MATRICE '//MATA19
     +              //' DOIVENT ETRE REELLES, ON NE TRAITE PAS '//
     +                'ENCORE LES MATRICES NON-SYMETRIQUES COMPLEXES.')
      ENDIF
C
C ----  ON VERIFIE QUE LA MATRICE EST SYMETRIQUE :
C       ----------------------------------------
      IF (ZI(LMAT+4).NE.1) THEN
          CALL UTMESS('F','MASYNS','LA MATRICE '//MATA19//
     +              ' A TRANSFORMER EN MATRICE NON-SYMETRIQUE '//
     +                'DOIT ETRE SYMETRIQUE.')
      ENDIF
C
C ----  RECUPERATION DU NOMBRE DE BLOCS DE LA MATRICE SYMETRIQUE :
C       --------------------------------------------------------
      NBLOC = ZI(LMAT+13)
      LONG  = 2*NBLOC
C
C ----  RECUPERATION DE LA LONGUEUR D'UN BLOC :
C       -------------------------------------
      LGBLOC = ZI(LMAT+14)
C
C ----  COPIE DES VALEURS DE LA MATRICE SUR LA COLLECTION DE
C ----  TRAVAIL MAWORK  :
C       --------------
      CALL JEDUPO(MATAS(1:19)//'.VALE','V',MAWORK,.FALSE.)
C
      CALL JEDETR(MATAS(1:19)//'.VALE')
C
C ----  RECREATION DE LA COLLECTION DES BLOCS DE LA MATRICE EN
C ----  MULTIPLIANT CE NOMBRE DE BLOCS PAR 2  :
C       ------------------------------------
      CALL COCOPG(MAWORK,MATAS(1:19)//'.VALE',LONG,LONG,BASE)
C
C ----  DESTRUCTION DE LA MATRICE DE TRAVAIL :
C       ------------------------------------
       CALL JEDETR('&&MASYNS.VALE')
C
C ----  AFFECTATION DE L'ATTRIBUT NON-SYMETRIQUE A LA MATRICE :
C       -----------------------------------------------------
      CALL JEECRA(MATAS(1:19)//'.VALE','DOCU',IBID,'MR')
C
C ----  AFFECTATION DES VALEURS DE LA TRIANGULAIRE INFERIEURE
C ----  DE LA MATRICE A LA TRIANGULAIRE SUPERIEURE :
C       ------------------------------------------
      DO 10 IBLOC = 1, NBLOC
C
C ----     RECUPERATION DU BLOC SUPERIEUR COURANT :
C          --------------------------------------
           CALL JEVEUO(JEXNUM(MATAS(1:19)//'.VALE',IBLOC),'L',IDBLOS)
C
C ----     RECUPERATION DU BLOC INFERIEUR CORRESPONDANT :
C          --------------------------------------------
           CALL JEVEUO(JEXNUM(MATAS(1:19)//'.VALE',IBLOC+NBLOC),'E',
     +                 IDBLOI)
C
C ----     AFFECTATION DES VALEURS DU BLOC SUP AU BLOC INF :
C          -----------------------------------------------
           DO 20 I = 1, LGBLOC
              ZR(IDBLOI+I-1) = ZR(IDBLOS+I-1)
 20        CONTINUE
C
           CALL JELIBE(JEXNUM(MATAS(1:19)//'.VALE',IBLOC))
           CALL JELIBE(JEXNUM(MATAS(1:19)//'.VALE',IBLOC+NBLOC))
C
 10   CONTINUE
C
C ----  DESTRUCTION DU DESCRIPTEUR DE LA MATRICE :
C       ----------------------------------------
      CALL JEDETR(MATAS(1:19)//'.&INT')
      CALL JEDETR(MATAS(1:19)//'.&IN2')
C
      CALL JEDEMA()
C.============================ FIN DE LA ROUTINE ======================
      END
