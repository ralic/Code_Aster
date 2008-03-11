      SUBROUTINE CETULE ( MODEL0, TBGRCA, CODRET )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 11/03/2008   AUTEUR MEUNIER S.MEUNIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C GRANDEURS CARACTERISTIQUES DE L'ETUDE - LECTURE
C           *                     ***     **
C     ------------------------------------------------------------------
C      RECUPERATION DES GRANDEURS CARACTERISTIQUES CONTENUES
C      DANS LE MODELE
C      QUAND ELLES SONT FOURNIES, CES GRANDEURS SONT > 0
C      PAR DEFAUT, ON MET DONC DES VALEURS NEGATIVES POUR TESTER ENSUITE
C      REMARQUE : LES GRANDEURS SONT STOCKEES PAR LE SP CETUCR
C
C IN  : MODELE  : NOM DE LA SD MODELE
C OUT : TBGRCA  : TABLEAU DES GRANDEURS CARACTERISTIQUES AVEC LA
C                 CONVENTION SUIVANTE :
C                 1 : LONGUEUR
C                 2 : PRESSION
C                 3 : TEMPERATURE
C OUT : CODRET  : 0 : OK
C                 1 : LA TABLE N'EXISTE PAS
C                 2 : UN DES PARAMETRES EST DEFINI 0 OU PLUSIEURS FOIS
C     ------------------------------------------------------------------
C
      IMPLICIT   NONE
C
C DECLARATION PARAMETRES
C
      INTEGER      NBMCLE
      PARAMETER  ( NBMCLE = 3 )
C
C DECLARATION PARAMETRES D'APPELS
C -------------------------------
      CHARACTER*(*)  MODEL0
      REAL*8        TBGRCA(NBMCLE)
      INTEGER       CODRET
C     ------------------------------------------------------------------
C
C DECLARATION VARIABLES LOCALES
C
      INTEGER      IBID, IAUX, VALI, IRET
C
      REAL*8       VALEUR, RBID
C
      COMPLEX*16   CBID, VALC
C
      CHARACTER*1  CTYPE
      CHARACTER*8  NOMGRD(NBMCLE)
      CHARACTER*8  MODELE
      CHARACTER*8  KBID, VALK
      CHARACTER*19 TABLE
C
C     ------------------------------------------------------------------
      DATA NOMGRD / 'LONGUEUR', 'PRESSION', 'TEMP' /
C     ------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
      MODELE = MODEL0(1:8)
C
C====
C 1. VALEURS PAR DEFAUT (R8VIDE ? VOIR TE0500)
C====
C
      DO 10 , IAUX = 1 , NBMCLE
        TBGRCA(IAUX) = -1.D0
 10   CONTINUE
C
C====
C 2. REPERAGE DE LA TABLE
C====
C
      CALL JEEXIN ( MODELE//'           .LTNT', IRET )
      IF ( IRET .NE. 0 ) THEN
        CALL LTNOTB ( MODELE , 'CARA_ETUDE' , TABLE )
        CODRET = 0
      ELSE
        CODRET = 1
      ENDIF
C
C====
C 3. DECODAGE DE LA TABLE
C====
C
      IF ( CODRET.EQ.0 ) THEN
C
        DO 30 , IAUX = 1 , NBMCLE
C
          CALL TBLIVA (TABLE,
     &                 1,'GRANDEUR',IBID,RBID,CBID,NOMGRD(IAUX),KBID,
     &                 RBID,'VALE',CTYPE,VALI,VALEUR,VALC,VALK,IRET)
C
          IF ( IRET.EQ.0 ) THEN
C
            TBGRCA(IAUX) = VALEUR
C
          ELSEIF ( IRET.GE.2 ) THEN
C
            CODRET = 2
C
          ENDIF
C
 30     CONTINUE
C
      ENDIF
C
      CALL JEDEMA ( )
C
      END
