      SUBROUTINE PGAUSS ( TYPEMA, FG, PG, G, NG, NDIM )
      IMPLICIT NONE
      INTEGER       FG, NG, NDIM
      INTEGER VALI
      REAL*8        PG(*), G(*)
      CHARACTER*8   TYPEMA
      CHARACTER*24 VALK

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C ----------------------------------------------------------------------
C     POIDS ET COORDONNEES DES POINTS DE GAUSS
C ----------------------------------------------------------------------
C     VARIABLES D'ENTREE
C IN  : TYPEMA     : TYPE DE LA MAILLE
C       FG         : FAMILLE D'INTEGRATION
C
C OUT : PG(NG)     : POIDS DE GAUSS (P1,P2,...)
C       G(DIME,NG) : POINTS DE GAUSS (X1,[Y1],[Z1],X2,...)
C       NG         : NOMBRE DE POINTS DE GAUSS
C       DIME       : DIMENSION DE LA MAILLE
C
C ----------------------------------------------------------------------
C
      IF (TYPEMA(1:3).EQ.'SEG') THEN
         IF (FG.EQ.1) THEN
            CALL ELRAGA ( 'SE2', 'FPG1', NDIM, NG, G, PG )
         ELSEIF (FG.EQ.2) THEN
            CALL ELRAGA ( 'SE2', 'FPG2', NDIM, NG, G, PG )
         ELSEIF (FG.EQ.3) THEN
            CALL ELRAGA ( 'SE2', 'FPG3', NDIM, NG, G, PG )
         ELSEIF (FG.EQ.4) THEN
            CALL ELRAGA ( 'SE2', 'FPG4', NDIM, NG, G, PG )
         ELSE
         VALK = TYPEMA
         VALI = FG
            CALL U2MESG('F', 'CALCULEL6_34',1,VALK,1,VALI,0,0.D0)
         ENDIF

      ELSEIF ( TYPEMA(1:3) .EQ. 'TRI' ) THEN
         IF (FG.EQ.1) THEN
            CALL ELRAGA ( 'TR3', 'FPG1', NDIM, NG, G, PG )
         ELSEIF (FG.EQ.2) THEN
            CALL ELRAGA ( 'TR3', 'NOEU', NDIM, NG, G, PG )
         ELSEIF (FG.EQ.3) THEN
            CALL ELRAGA ( 'TR3', 'COT3', NDIM, NG, G, PG )
         ELSEIF (FG.EQ.4) THEN
            CALL ELRAGA ( 'TR3', 'FPG3', NDIM, NG, G, PG )
         ELSEIF (FG.EQ.5) THEN
            CALL ELRAGA ( 'TR3', 'FPG4', NDIM, NG, G, PG )
         ELSEIF (FG.EQ.6) THEN
            CALL ELRAGA ( 'TR3', 'FPG6', NDIM, NG, G, PG )
         ELSEIF (FG.EQ.7) THEN
            CALL ELRAGA ( 'TR3', 'FPG7', NDIM, NG, G, PG )
         ELSEIF (FG.EQ.8) THEN
            CALL ELRAGA ( 'TR3', 'FPG12', NDIM, NG, G, PG )
         ELSE
         VALK = TYPEMA
         VALI = FG
            CALL U2MESG('F', 'CALCULEL6_34',1,VALK,1,VALI,0,0.D0)
         ENDIF

      ELSEIF ( TYPEMA(1:3) .EQ. 'QUA' ) THEN
         IF (FG.EQ.1) THEN
            CALL ELRAGA ( 'QU4', 'FPG1', NDIM, NG, G, PG )
         ELSEIF (FG.EQ.2) THEN
            CALL ELRAGA ( 'QU4', 'NOEU', NDIM, NG, G, PG )
         ELSEIF (FG.EQ.3) THEN
            CALL ELRAGA ( 'QU4', 'FPG4', NDIM, NG, G, PG )
         ELSEIF (FG.EQ.4) THEN
            CALL ELRAGA ( 'QU8', 'NOEU', NDIM, NG, G, PG )
            PG(1)  =  -1.D0/3.D0
            PG(2)  =  PG(1)           
            PG(3)  =  PG(1)           
            PG(4)  =  PG(1) 
            PG(5)  =  4.D0/3.D0
            PG(6)  =  PG(5)           
            PG(7)  =  PG(5)           
            PG(8)  =  PG(5)
         ELSEIF (FG.EQ.5) THEN
            CALL ELRAGA ( 'QU9', 'NOEU', NDIM, NG, G, PG )
            PG(1)  =  1.D0/9.D0
            PG(2)  =  PG(1)           
            PG(3)  =  PG(1)           
            PG(4)  =  PG(1) 
            PG(5)  =  4.D0/9.D0
            PG(6)  =  PG(5)           
            PG(7)  =  PG(5)           
            PG(8)  =  PG(5)
            PG(9)  =  16.D0/9.D0
         ELSEIF (FG.EQ.6) THEN
            CALL ELRAGA ( 'QU4', 'FPG9', NDIM, NG, G, PG )
         ELSEIF (FG.EQ.7) THEN
            CALL ELRAGA ( 'QU4', 'FPG16', NDIM, NG, G, PG )
         ELSE
         VALK = TYPEMA
         VALI = FG
            CALL U2MESG('F', 'CALCULEL6_34',1,VALK,1,VALI,0,0.D0)
         ENDIF

      ELSEIF ( TYPEMA(1:3) .EQ.  'TET') THEN        
         IF (FG.EQ.1) THEN
            CALL ELRAGA ( 'TE4', 'FPG4', NDIM, NG, G, PG )
         ELSEIF (FG.EQ.2) THEN
            CALL ELRAGA ( 'TE4', 'FPG5', NDIM, NG, G, PG )
         ELSEIF (FG.EQ.3) THEN
            CALL ELRAGA ( 'TE4', 'FPG15', NDIM, NG, G, PG )
         ELSE 
         VALK = TYPEMA
         VALI = FG
            CALL U2MESG('F', 'CALCULEL6_34',1,VALK,1,VALI,0,0.D0)
         ENDIF 

      ELSEIF ( TYPEMA(1:3) .EQ. 'PEN' ) THEN
         IF (FG.EQ.1) THEN
            CALL ELRAGA ( 'PE6', 'FPG6' , NDIM, NG, G, PG )
         ELSEIF (FG.EQ.2) THEN
            CALL ELRAGA ( 'PE6', 'FPG6B', NDIM, NG, G, PG )
         ELSEIF (FG.EQ.3) THEN
            CALL ELRAGA ( 'PE6', 'FPG8' , NDIM, NG, G, PG )
         ELSEIF (FG.EQ.4) THEN
            CALL ELRAGA ( 'PE6', 'FPG21', NDIM, NG, G, PG )
         ELSE
         VALK = TYPEMA
         VALI = FG
            CALL U2MESG('F', 'CALCULEL6_34',1,VALK,1,VALI,0,0.D0)
         ENDIF

      ELSEIF ( TYPEMA(1:3) .EQ. 'HEX' ) THEN 
         IF (FG.EQ.1) THEN
            CALL ELRAGA ( 'HE8', 'FPG8', NDIM, NG, G, PG )
         ELSEIF (FG.EQ.2) THEN
            CALL ELRAGA ( 'HE8', 'FPG27', NDIM, NG, G, PG )
         ELSEIF (FG.EQ.3) THEN
            CALL ELRAGA ( 'HE8', 'FPG64', NDIM, NG, G, PG )
         ELSE
         VALK = TYPEMA
         VALI = FG
            CALL U2MESG('F', 'CALCULEL6_34',1,VALK,1,VALI,0,0.D0)
         ENDIF

      ELSEIF ( TYPEMA(1:3) .EQ. 'PYR' ) THEN 
         IF (FG.EQ.1) THEN
            CALL ELRAGA ( 'PY5', 'FPG5' , NDIM, NG, G, PG )
         ELSEIF (FG.EQ.2) THEN
            CALL ELRAGA ( 'PY5', 'FPG6' , NDIM, NG, G, PG )
         ELSEIF (FG.EQ.3) THEN
            CALL ELRAGA ( 'PY5', 'FPG27', NDIM, NG, G, PG )
         ELSE
         VALK = TYPEMA
         VALI = FG
            CALL U2MESG('F', 'CALCULEL6_34',1,VALK,1,VALI,0,0.D0)
         ENDIF

      ELSE
         VALK = TYPEMA
         CALL U2MESG('F', 'CALCULEL6_41',1,VALK,0,0,0,0.D0)
      ENDIF

      END
