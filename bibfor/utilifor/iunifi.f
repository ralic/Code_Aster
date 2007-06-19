      INTEGER FUNCTION IUNIFI( NAME )
      IMPLICIT NONE
      CHARACTER*(*)            NAME
C
C     ------------------------------------------------------------------
C     RETOURNE L'UNITE LOGIQUE ATTACHEE AU NOM LOCAL NAME (DDNAME)
C     ------------------------------------------------------------------
C
C IN  NAME   : CH*16 : NOM "LOCALE" DONT ON RECHERCHE LE NUMERO D'UNITE 
C                      LOGIQUE ASSOCIEE
C OUT IUNIFI : IS    : NUMERO D'UNITE LOGIQUE ASSOCIE A "NAME"
C                      RENVOI 0 SI LE NOM N'EST PAS DANS LES TABLES
C
C     ------------------------------------------------------------------
C     REMARQUE : SUPPOSE QUE LA DEFINITION DU COUPLE (UNITE LOGIQUE,NOM)
C                EST DEJA FAITE (CF ULDEFI)
C     REMARQUE : SI L'INITIALISATION N'A PAS ETE FAITE LA ROUTINE S'EN
C                CHARGERA (APPEL A ULINIT)
C     ------------------------------------------------------------------
C MODIF UTILIFOR  DATE 03/11/2004   AUTEUR MCOURTOI M.COURTOIS 
C RESPONSABLE D6BHHJP J.P.LEFEBVRE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
      INTEGER       MXIMPR
      PARAMETER   ( MXIMPR = 5)
      INTEGER          MXF
      PARAMETER       (MXF=100)
      CHARACTER*1      TYPEFI(MXF),ACCEFI(MXF),ETATFI(MXF),MODIFI(MXF)
      CHARACTER*16     DDNAME(MXF)
      CHARACTER*255    NAMEFI(MXF)
      INTEGER          FIRST, UNITFI(MXF) , NBFILE
      COMMON/ ASGFI1 / FIRST, UNITFI      , NBFILE
      COMMON/ ASGFI2 / NAMEFI,DDNAME,TYPEFI,ACCEFI,ETATFI,MODIFI
C
      CHARACTER*16     NAME16
      INTEGER          I
C     ------------------------------------------------------------------
C     CONSERVER LA COHERENCE AVEC IBIMPR
      CHARACTER*16  NOMPR (MXIMPR)
      CHARACTER*1   TYPPR (MXIMPR) , AUTPR(MXIMPR)
      INTEGER       UNITPR (MXIMPR)   , PRESPR(MXIMPR)
      DATA          NOMPR  /'VIGILE'  , 'MESSAGE'   , 'RESULTAT',
     +                      'ERREUR'  ,  'MED'      /
      DATA          UNITPR /    0     ,      6      ,     8     ,
     +                          9     ,    80       /
C     ------------------------------------------------------------------
C
      IF ( FIRST .NE. 17111990 ) CALL ULINIT
C
      NAME16 = NAME
      IUNIFI = 0
C
C     VALEUR PAR DEFAUT POUR LES NOMS INTERNES
      DO 10 I = 1, MXIMPR
        IF( NAME16 .EQ. NOMPR(I) ) THEN
          IUNIFI = UNITPR(I)
          GOTO 99
        ENDIF
  10  CONTINUE
C
      DO 20 I = 1, NBFILE
        IF( NAME16 .EQ. DDNAME(I) ) THEN
          IUNIFI = UNITFI(I)
          GOTO 99
        ENDIF
  20  CONTINUE
C
  99  CONTINUE
      END
