      SUBROUTINE NMIMPT(NUMINS,SDDISC,SDIMPR)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/04/2013   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      INCLUDE      'jeveux.h'
      INTEGER      NUMINS
      CHARACTER*24 SDIMPR
      CHARACTER*19 SDDISC
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (AFFICHAGE)
C
C AFFICHAGE ENTETE
C
C ----------------------------------------------------------------------
C
C
C IN  SDIMPR : SD AFFICHAGE
C IN  SDDISC : SD DISCRETISATION TEMPORELLE
C IN  NUMINS : NUMERO INSTANT COURANT
C
C ----------------------------------------------------------------------
C
      REAL*8       R8BID
      REAL*8       INSTAN
      INTEGER      DININS,LENIVO
      CHARACTER*16 METLIS
      INTEGER      IBID
      LOGICAL      LPRINT
C
C ----------------------------------------------------------------------
C
C
C --- METHODE DE GESTION DE LA LISTE D'INSTANTS
C
      CALL UTDIDT('L'   ,SDDISC,'LIST',IBID  ,'METHODE' ,
     &            R8BID ,IBID  ,METLIS)
      IF (METLIS.EQ.'MANUEL') THEN
        LENIVO = DININS(SDDISC,NUMINS)
      ELSEIF (METLIS.EQ.'AUTO') THEN
        LENIVO = 0
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
C --- AFFICHAGE ACTIF ?
C
      CALL OBGETB(SDIMPR,'PRINT',LPRINT)
C
C --- AFFICHAGE ENTETE
C
      IF (LPRINT) THEN
        IF (LENIVO.EQ.0) THEN
          CALL U2MESR('I','MECANONLINE6_6',1,INSTAN)
        ELSE
          CALL U2MESG('I','MECANONLINE6_1',0,' ',1,LENIVO,1,INSTAN)
        ENDIF
      ENDIF
C
C --- AFFICHAGE TITRE DES COLONNES
C
      CALL NMIMEN(SDIMPR)

      END
