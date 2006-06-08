      SUBROUTINE IMPMAT ( IFM, NOMSDZ, TYPSDZ, GRAINZ, OPTIOZ, NBNO,
     +                    LISNOZ, NBMA, LISMAZ, NBCMP, LISCMZ, NBCHIF,
     +                    EPS )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 16/06/2004   AUTEUR DURAND C.DURAND 
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
C      IMPMAT -- IMPRESSION DE LA S.D DE NOM NOMSD ET DE TYPE TYPSD
C                DANS LE FICHIER DE NOM FICHIER.
C                
C
C   ARGUMENT        E/S  TYPE         ROLE
C    IFM             IN    IS     UNITE LOGIQUE D'IMPRESSION DE LA S.D,
C    NOMSDZ          IN    K*     NOM DE LA S.D.
C    TYPSDZ          IN    K*     TYPE DE S.D.
C                                 = 'MATR_ASSE'
C                              OU = 'MATR_ELEM'
C                              OU = 'VECT_ELEM'
C                              OU = 'RESU_ELEM'
C    GRAINZ          IN    K*     'GRAIN' DE L'IMPRESSION
C                                     = 'VALEUR'
C                                  OU = 'NOEUD'
C                                  SI = 'VALEUR' ON IMPRIME UNE VALEUR
C                                                PAR LIGNE
C                                  SI = 'NOEUD' LE GRAIN D'IMPRESSION 
C                                               EST CONSTITUE PAR UNE
C                                               SOUS-MATRICE DONT LES 
C                                               TERMES COUPLENT 2 NOEUDS
C                                  SI = 'MAILLE' LE GRAIN D'IMPRESSION 
C                                                EST CONSTITUE PAR LA
C                                                MATRICE ELEMENTAIRE
C                                  (VALABLE POUR LES MATR_ELEM)
C    OPTIOZ          IN    K*     OPTION D'IMPRESSION
C                                     = 'SOUS_MATRICE'
C                                  OU = 'LIGNE'
C                                  OU = 'COLONNE'
C                                  SI = 'SOUS_MATRICE' ON IMPRIME 
C                                       UNIQUEMENT LES COMPOSANTES 
C                                       LICITES DES NOEUDS DE LA LISTE
C                                       LISNOZ ET SI CELLE-CI EST VIDE
C                                       (NBNO = 0)
C                                       ON IMPRIME CELLES DE TOUS LES
C                                       NOEUDS (PHYSIQUES) DU MAILLAGE
C                                  SI = 'LIGNE' ON IMPRIME TOUTES LES
C                                       LES LIGNES RELATIVES AUX 
C                                       COMPOSANTES LICITES DES NOEUDS
C                                       DE LA LISTE LISNOZ ET DE TOUS
C                                       LES NOEUDS SI CETTE LISTE EST 
C                                       VIDE (NBNO =0 )
C                                  SI = 'COLONNE' ON IMPRIME TOUTES LES
C                                       LES COLONNES RELATIVES AUX 
C                                       COMPOSANTES LICITES DES NOEUDS
C                                       DE LA LISTE LISNOZ ET DE TOUS
C                                       LES NOEUDS SI CETTE LISTE EST 
C                                       VIDE (NBNO =0)
C                                  CET ARGUMENT NE SERT QUE POUR LES
C                                  MATR_ASSE      
C    NBNO            IN    I     NOMBRE DE NOEUDS DE LA LISTE LISNOZ
C                                SI = 0 LA LISTE LISNOZ EST VIDE ET
C                                L'ON PREND EN COMPTE TOUS LES NOEUDS
C                                (PHYSIQUES) DU MAILLAGE
C    LISNOZ          IN    K*    LISTE DES NOEUDS POUR-LESQUELS ON
C                                DESIRE L'IMPRESSION DES VALEURS
C                                DE LA MATR_ASSE
C    NBMA            IN    I     NOMBRE DE MAILLES DE LA LISTE LISMAZ
C                                SI = 0 LA LISTE LISMAZ EST VIDE ET
C                                L'ON PREND EN COMPTE TOUTES LES
C                                MAILLES DU MATR_ELEM
C    LISMAZ          IN    K*    LISTE DES MAILLES POUR-LESQUELS ON
C                                DESIRE L'IMPRESSION DES VALEURS
C                                DES MATRICES DU MATR_ELEM
C    NBCMP           IN    I     NOMBRE DE COMPOSANTES DE LA LISTE
C                                LISCMZ DES COMPOSANTES
C    LISCMZ          IN    K*    LISTE DES COMPOSANTES POUR-LESQUELLES
C                                ON DESIRE L'IMPRESSION DES VALEURS
C                                DE LA MATRICE
C    NBCHIF          IN    I     NOMBRE DE CHIFFRES SIGNIFICATIFS
C    EPS             IN    R     PRECISION SERVANT A TESTER LA NULLITE
C                                D'UN TERME DE LA MATRICE
C.========================= DEBUT DES DECLARATIONS ====================
C ----- COMMUNS NORMALISES  JEVEUX 
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM
C -----  ARGUMENTS
      CHARACTER*(*) NOMSDZ,TYPSDZ,GRAINZ,OPTIOZ,LISNOZ,LISCMZ
      CHARACTER*(*) LISMAZ
C -----  VARIABLES LOCALES
      CHARACTER*2   KLONLG , KLONLS
      CHARACTER*6   GRAIN 
      CHARACTER*7   TYPSYM 
      CHARACTER*8   NOMSD8 
      CHARACTER*9   TYPSD 
      CHARACTER*24  RESU, NOMSD
      CHARACTER*72  FORM1, FORM2, FORM3, FORM4
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ ( )
C
      TYPSD  = TYPSDZ
      NOMSD  = NOMSDZ
      RESU   = NOMSDZ
      GRAIN  = GRAINZ
C
C --- LONGUEUR UTILE DE LA CHAINE DE CARACTERES NOMSD :
C     -----------------------------------------------
      LGNOMS = LXLGUT(NOMSD)
      LGNOM2 = LGNOMS + 4
C
      CALL CODENT(LGNOMS,'G',KLONLS)
      CALL CODENT(LGNOM2,'G',KLONLG)
C
      FORM1 = '('//'//,2X,'//KLONLG//'("*")'//')'
      FORM3 = '(2X,'//KLONLG//'("*")'//')'
      FORM2 = '(2X,'//'"*",X,A'//KLONLS//',X,"*"'//')'
      FORM4 = '(//)'
C
C --- IMPRESSION DU NOM DE LA S.D. :
C     ----------------------------
      WRITE(IFM,FORM1)
      WRITE(IFM,FORM2) NOMSD(1:LGNOMS)
      WRITE(IFM,FORM3)
      IF (TYPSD.EQ.'MATR_ASSE'.AND.GRAIN.EQ.'VALEUR') THEN
          WRITE(IFM,FORM4)
      ENDIF
C
      IF (TYPSD.EQ.'MATR_ASSE') THEN
        CALL IMPMTR ( IFM, NOMSDZ, GRAINZ, OPTIOZ, NBNO,
     +                LISNOZ, NBCMP, LISCMZ, NBCHIF, EPS )
      ELSEIF (TYPSD.EQ.'MATR_ELEM') THEN
        CALL IMPMEL ( IFM, NOMSDZ, GRAINZ, NBMA, LISMAZ, 
     +                NBCMP, LISCMZ,NBCHIF )
      ELSEIF (TYPSD.EQ.'VECT_ELEM') THEN
        CALL IMPVEL ( IFM, NOMSDZ, GRAINZ, NBMA, LISMAZ, 
     +                NBCMP, LISCMZ,NBCHIF )
      ELSEIF (TYPSD.EQ.'RESU_ELEM') THEN
C
C --- CONSTRUCTION D'UN RESUELEM.LISTE_RESU EN VUE DE
C --- REUTILISER LES ROUTINES POUR LES MATR_ELEM ET LES VECT_ELEM :
C     -----------------------------------------------------------
        CALL JEEXIN('&&IMPMAT'//'.LISTE_RESU',IRET)
        IF (IRET.NE.0) GOTO 9999
        CALL WKVECT('&&IMPMAT'//'.LISTE_RESU','V V K24',1,IDLRES)
        ZK24(IDLRES+1-1) = RESU
        CALL DISMOI('F','TYPE_MATRICE',RESU,'RESUELEM',IBID,TYPSYM,IER)
C
C ---    CAS OU IL S'AGIT D'UN RESUELEM D'UN MATR_ELEM :
C        ---------------------------------------------
        IF (TYPSYM.EQ.'SYMETRI'.OR.TYPSYM.EQ.'NON_SYM') THEN
            CALL IMPMEL ( IFM, '&&IMPMAT'//'.LISTE_RESU', GRAINZ, 
     +                   NBMA, LISMAZ,  NBCMP, LISCMZ, NBCHIF )
C
C ---    CAS OU IL S'AGIT D'UN RESUELEM D'UN VECT_ELEM :
C        ---------------------------------------------
        ELSE
            CALL IMPVEL ( IFM, '&&IMPMAT'//'.LISTE_RESU', GRAINZ, 
     +                   NBMA, LISMAZ,  NBCMP, LISCMZ, NBCHIF )
        ENDIF
C
        CALL JEDETR('&&IMPMAT'//'.LISTE_RESU')
C
      ELSE
        CALL UTMESS ('F','IMPMAT','LES SEULS TYPES DE S.D.'
     +             //' AUTORISES SONT "MATR_ASSE" , "MATR_ELEM"'
     +             //' "VECT_ELEM" ET "RESU_ELEM", LE TYPE DONNE '
     +             //TYPSD// ' N''EST PAS RECONNU .') 
      ENDIF
C
 9999 CONTINUE
      CALL JEDEMA()
C
      END
