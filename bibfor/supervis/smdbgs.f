      SUBROUTINE SMDBGS( IFV, IETAT, ICLASS, ACTION, IVAL, RVAL, KVAL )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            IFV, IETAT, ICLASS, ACTION, IVAL
      REAL*8                                               RVAL(*)
      CHARACTER*(*)                                              KVAL
C
C     ------------------------------------------------------------------
C              DEBUG DE L'ANALYSEUR SEMANTIQUE DES COMMANDES
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 23/06/97   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         -
C     ROUTINE(S) FORTRAN     :
C         MIN    MAX    WRITE
C     ------------------------------------------------------------------
C FIN SMDBGS
C     ------------------------------------------------------------------
C
      CHARACTER*12 PGM
      CHARACTER*80 CVAL
      CVAL = KVAL
      PGM = ' <SMDBGS>:  '
C
      IF ( IFV .LT.1 ) GOTO 9999
      WRITE(IFV,'(1X,72(''-''))' )
      WRITE(IFV,*) PGM,' ETAT= ',IETAT,' * CLASSE= ',ICLASS,
     +                        ' * ACTION= ',ACTION
      I = MAX(1 , MIN (IVAL,80) )
C
      IPILE = ACTION / 100
      IACT  = ACTION - 100*IPILE
      IF (IPILE.EQ.1) WRITE(IFV,*) PGM,'  ON EMPILE LES INFORMATIONS'
C
      IF ( IACT .EQ. 0 ) THEN
         WRITE(IFV,*) PGM,' AUCUNE ACTION ENTREPRISE'
      ELSEIF ( IACT .EQ. 1 ) THEN
         WRITE(IFV,*) PGM,'SMSCAN ON EMPILE LES INFORMATIONS'
      ELSEIF ( IACT .EQ. 2 ) THEN
          WRITE(IFV,*) PGM,' %COMMANDE : "'//CVAL(1:I)//'"'
      ELSEIF ( IACT .EQ. 3 ) THEN
          WRITE(IFV,*) PGM,' %RESULTAT : "'//CVAL(1:I)//'"'
      ELSEIF ( IACT .EQ. 4 ) THEN
          WRITE(IFV,*) PGM,' %OPERATEUR : "'//CVAL(1:I)//'"'
      ELSEIF ( IACT .EQ. 5 ) THEN
          WRITE(IFV,*) PGM,'  IDENTIFICATEUR : "'//CVAL(1:I)//'"'
      ELSEIF ( IACT .EQ. 6 ) THEN
          WRITE(IFV,*) PGM,' %MOT_CLE : "'//CVAL(1:I)//'"'
      ELSEIF ( IACT .EQ. 7 ) THEN
          WRITE(IFV,*) PGM,'  %MOT_CLE_FACTEUR : "'//CVAL(1:I)//'"'
      ELSEIF ( IACT .EQ. 8 ) THEN
          WRITE(IFV,*) PGM,' %MOTCLE D"UN MOTCLEFAC:"',CVAL(1:I)//'"'
      ELSEIF ( IACT .EQ. 9 ) THEN
          WRITE(IFV,*) PGM,' %VALEUR_ENTIERE ',IVAL
      ELSEIF ( IACT .EQ. 10 ) THEN
          WRITE(IFV,*) PGM,' %VALEUR_REELLE ' , RVAL(1)
      ELSEIF ( IACT .EQ. 11) THEN
          WRITE(IFV,*) PGM,' %VALEUR_CONCEPT "'//CVAL(:I)//'"'
      ELSEIF ( IACT .EQ. 12) THEN
          WRITE(IFV,*) PGM,' %VALEUR_TEXTE "'//CVAL(:I)//'"'
      ELSEIF ( IACT .EQ. 13) THEN
          WRITE(IFV,*) PGM,' %VALEUR_COMPLEXE ',RVAL(1),RVAL(2)
      ELSEIF ( IACT .EQ. 14) THEN
          WRITE(IFV,*) PGM,' %VALEUR_LOGIQUE  ', IVAL
      ELSEIF ( IACT .EQ. 15) THEN
          WRITE(IFV,*) PGM,' %TYPE LISTE : ERREUR <<<<'
      ELSEIF ( IACT .EQ. 16) THEN
          WRITE(IFV,*) PGM,' %FIN DE L''OPERATEUR '
      ELSEIF ( IACT .EQ. 17) THEN
         WRITE(IFV,*) PGM,'  IL FAUT DEPILER '
      ELSEIF ( IACT .EQ. 18) THEN
         WRITE(IFV,*) PGM,' %FIN DE PHRASE '
      ELSEIF ( IACT .EQ. 19) THEN
         WRITE(IFV,*) PGM,' %FIN DE MOT CLE FACTEUR'
      ELSEIF ( IACT .EQ. 20) THEN
         WRITE(IFV,*) PGM,' %DEBUT DE LISTE DE VALEUR'
      ELSEIF ( IACT .EQ. 21) THEN
         WRITE(IFV,*) PGM,' %FIN DE LISTE DE VALEUR'
      ELSEIF ( IACT .EQ. 22) THEN
         WRITE(IFV,*) PGM,' %DEBUT LISTE DE VALEUR ET IL FAUT DEPILER '
      ELSEIF ( IACT .EQ. 23) THEN
         WRITE(IFV,*) PGM,' %SUITE MOT CLE FACTEUR'
      ELSE
         WRITE(IFV,*) PGM,'>>>  ERREUR ACTION NON PREVU : ',IACT
      ENDIF
 9999 CONTINUE
      END
