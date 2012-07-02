      FUNCTION JXHCOD ( CHAIN , LREP )
C TOLE CRP_4
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      INTEGER  JXHCOD ,         LREP
      INTEGER  I1
      CHARACTER*(*)     CHAIN
C ----------------------------------------------------------------------
      INTEGER*4          I(8)
C
C   ATTENTION, IL FAUT IMPERATIVEMENT UTILISER UN TABLEAU I AYANT
C   POUR LONGUEUR TOTALE 32 OCTETS (ICI 4*8) POUR S'ALIGNER SUR LA 
C   CHAINE PASSEE EN ARGUMENT (32 CARACTERES MAXIMUM)
C
C-----------------------------------------------------------------------
      INTEGER K 
C-----------------------------------------------------------------------
      CALL STRMOV ( CHAIN , 1 , 32 , I(1) , 1 )
      DO 1 K = 2,8
         I(1) = IEOR( I(1) , I(K) )
 1    CONTINUE
      I1=I(1)
      JXHCOD = 1 + MOD(I1,LREP)
      END
