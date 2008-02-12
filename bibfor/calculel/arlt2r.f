      SUBROUTINE ARLT2R(TYPEMA,ELREFE)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 12/02/2008   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*8   TYPEMA,ELREFE 
C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN - UTILITAIRE
C
C CONVERSION TYPE MAILLE GEOMETRIQUE -> NOM MAILLE ELREFE
C
C ----------------------------------------------------------------------
C
C IN  TYPEMA : TYPE GEOMETRIQUE DE LA MAILLE
C OUT ELREFE : NOM DE L'ELREFE 
C
C ----------------------------------------------------------------------
C       
      ELREFE = '        '
C      
      IF (TYPEMA(1:3).EQ.'SEG') THEN
        ELREFE = 'SE2     '
      ELSEIF ( TYPEMA(1:5) .EQ. 'TRIA3' ) THEN
        ELREFE = 'TR3     '
      ELSEIF ( TYPEMA(1:5) .EQ. 'TRIA6' ) THEN
        ELREFE = 'TR6     '
      ELSEIF ( TYPEMA(1:5) .EQ. 'QUAD4' ) THEN
        ELREFE = 'QU4     '
      ELSEIF ( TYPEMA(1:5) .EQ. 'QUAD8' ) THEN
        ELREFE = 'QU8     '
      ELSEIF ( TYPEMA(1:3) .EQ.  'TET') THEN        
        ELREFE = 'TE4     '   
      ELSEIF ( TYPEMA(1:3) .EQ. 'PEN' ) THEN
        ELREFE = 'PE6     '
      ELSEIF ( TYPEMA(1:3) .EQ. 'HEX' ) THEN 
        ELREFE = 'HE8     '
      ELSEIF ( TYPEMA(1:3) .EQ. 'PYR' ) THEN 
        ELREFE = 'PY5     '
      ELSE
        CALL U2MESG('F', 'ARLEQUIN_41',1,TYPEMA,0,0,0,0.D0)
      ENDIF
C      
      END
