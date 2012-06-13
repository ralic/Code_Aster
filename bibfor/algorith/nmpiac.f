      SUBROUTINE NMPIAC(SDPILO,ETA   )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C      
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*19 SDPILO
      REAL*8       ETA
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME - PILOTAGE)
C
C REACTUALISATION DES BORNES DE PILOTAGE SI DEMANDE
C
C ----------------------------------------------------------------------
C
C
C IN  SDPILO : SD PILOTAGE      
C IN  ETA    : PARAMETRE DE PILOTAGE
C
C
C
C
      CHARACTER*24 EVOLPA,TYPSEL,TYPPIL
      INTEGER      JPLTK, JPLIR
C
C ----------------------------------------------------------------------
C      
      CALL JEVEUO(SDPILO(1:19)// '.PLTK','L',JPLTK)
      TYPPIL = ZK24(JPLTK)
      TYPSEL = ZK24(JPLTK+5)
      EVOLPA = ZK24(JPLTK+6)
      CALL JEVEUO(SDPILO(1:19)// '.PLIR','E',JPLIR)
      IF(TYPSEL.EQ.'ANGL_INCR_DEPL'.AND.(TYPPIL.EQ.'LONG_ARC'
     &   .OR.TYPPIL.EQ.'SAUT_LONG_ARC')) THEN
         ZR(JPLIR-1+6)=ZR(JPLIR)
      ENDIF

            
      IF (EVOLPA .EQ. 'SANS') GOTO 9999        
      IF (EVOLPA .EQ. 'CROISSANT') THEN
        ZR(JPLIR+4) = ETA
      ELSE
        ZR(JPLIR+3) = ETA
      END IF
      
 9999 CONTINUE
      END
