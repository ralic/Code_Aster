      SUBROUTINE UTLCAL(TYPQUE,ALGO,VALR)
      IMPLICIT      NONE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/02/2011   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE GENIAUT S.GENIAUT
C
      CHARACTER*8   TYPQUE
      CHARACTER*16  ALGO
      REAL*8        VALR
C
C ----------------------------------------------------------------------
C
C
C ROUTINE UTILITAIRE POUR LES LOIS DE COMPORTEMENTS
C   FAIT LE LIEN ENTRE LE NOM DE L'ALGORIHTME D'INTEGRATION LOCALE
C   ET SON N° (VALEUR REELLE)
C
C ----------------------------------------------------------------------
C
C IN  TYPQUE    : SI 'NOM_VALE' : TRADUCTION NOM -> VALEUR REELE
C                 SI 'VALE_NOM' : TRADUCTION VALEUR REELE -> NOM
C IN/OUT VALR   : VALEUR REELE
C IN/OUT VALR   : NOM DE L'ALGORITHME
C
C
      CALL ASSERT(TYPQUE.EQ.'NOM_VALE'.OR.
     &            TYPQUE.EQ.'VALE_NOM')

      IF (TYPQUE.EQ.'NOM_VALE') THEN


        IF     (ALGO.EQ.'ANALYTIQUE') THEN
           VALR = 0.D0
        ELSEIF (ALGO.EQ.'SECANTE') THEN
           VALR = 1.D0
        ELSEIF (ALGO.EQ.'DEKKER')   THEN
           VALR = 2.D0
        ELSEIF (ALGO.EQ.'NEWTON_1D')  THEN
           VALR = 3.D0
        ELSEIF (ALGO.EQ.'NEWTON')    THEN
           VALR = 4.D0
        ELSEIF (ALGO.EQ.'NEWTON_RELI')  THEN
           VALR = 5.D0
        ELSEIF (ALGO.EQ.'RUNGE_KUTTA')   THEN
           VALR = 6.D0
        ELSEIF (ALGO.EQ.'SPECIFIQUE')   THEN
           VALR = 7.D0
        ELSEIF (ALGO.EQ.'SANS_OBJET') THEN
           VALR = 8.D0
        ELSEIF (ALGO.EQ.'BRENT') THEN
           VALR = 9.D0
        ELSEIF (ALGO.EQ.'NEWTON_PERT') THEN
           VALR =10.D0
        ELSE
           CALL ASSERT(.FALSE.)
        ENDIF


      ELSEIF (TYPQUE.EQ.'VALE_NOM') THEN


        IF     (VALR.EQ.0.D0) THEN
          ALGO = 'ANALYTIQUE'
        ELSEIF (VALR.EQ.1.D0) THEN
          ALGO = 'SECANTE'
        ELSEIF (VALR.EQ.2.D0) THEN
          ALGO = 'DEKKER'
        ELSEIF (VALR.EQ.3.D0) THEN
          ALGO = 'NEWTON_1D'
        ELSEIF (VALR.EQ.4.D0) THEN
          ALGO = 'NEWTON'
        ELSEIF (VALR.EQ.5.D0) THEN
          ALGO = 'NEWTON_RELI'
        ELSEIF (VALR.EQ.6.D0) THEN
          ALGO = 'RUNGE_KUTTA'
        ELSEIF (VALR.EQ.7.D0) THEN
          ALGO = 'SPECIFIQUE'
        ELSEIF (VALR.EQ.8.D0) THEN
          ALGO = 'SANS_OBJET'
        ELSEIF (VALR.EQ.9.D0) THEN
          ALGO = 'BRENT'
        ELSEIF (VALR.EQ.10.D0) THEN
          ALGO = 'NEWTON_PERT'
        ELSE
           CALL ASSERT(.FALSE.)
        ENDIF

      ENDIF
     
      END
