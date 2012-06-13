      SUBROUTINE ASCREP ( MAILLA, LTP1 )
      IMPLICIT   NONE 
      INCLUDE 'jeveux.h'
      CHARACTER*8         MAILLA
      REAL*8              LTP1
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C     OPERATEUR: "MODI_MAILLAGE" , MOTCLE FACTEUR "PLAQ_TUBE"
C
C     CHANGEMENT DE REPERE : PLAQUE --> UTILISATEUR
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER       NBNO, ICOOR, IDIME, INO, NDIM
      REAL*8        XP, YP, ZP
      CHARACTER*24  COORD, DIME 
C     ------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
      COORD  = MAILLA//'.COORDO    .VALE'
      DIME   = MAILLA//'.DIME           '
C      
      CALL JEVEUO (COORD,'E',ICOOR)      
      CALL JEVEUO (DIME ,'E',IDIME)
      NBNO  = ZI(IDIME)
      NDIM  = ZI(IDIME+5)
C  
      DO 100 INO=1, NBNO
         XP = ZR(ICOOR+NDIM*(INO-1))
         YP = ZR(ICOOR+NDIM*(INO-1)+1)
         ZP = ZR(ICOOR+NDIM*(INO-1)+2)
         ZR(ICOOR+NDIM*(INO-1))   = - YP
         ZR(ICOOR+NDIM*(INO-1)+1) = - XP
         ZR(ICOOR+NDIM*(INO-1)+2) = - (ZP + LTP1)
 100  CONTINUE
C
      CALL JEDEMA ( )
C      
      END
