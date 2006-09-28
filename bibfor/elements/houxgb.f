      FUNCTION HOUXGB (XX,N)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C--------------------------------------------------------
C ELEMENT SHB8-PS A.COMBESCURE, S.BAGUET INSA LYON 2003 /
C-------------------------------------------------------
      IMPLICIT NONE
      REAL *8  XX(24)
      INTEGER  N
      REAL *8  HOUXGB


      IF(N.EQ.1) THEN
               HOUXGB =   XX(1)  +  XX(4)  -  XX(7)  -   XX(10)
     &                  - XX(13) -  XX(16) +  XX(19) +   XX(22)
       ELSEIF(N.EQ.2) THEN
               HOUXGB =   XX(1)  -  XX(4)  -  XX(7)  +   XX(10)
     &                  - XX(13) +  XX(16) +  XX(19) -   XX(22)
       ELSEIF(N.EQ.3) THEN
               HOUXGB =   XX(1)  -  XX(4)  +  XX(7)  -   XX(10)
     &                 +  XX(13) -  XX(16) +  XX(19) -   XX(22)
       ELSEIF(N.EQ.4) THEN
               HOUXGB = - XX(1)  +  XX(4)  -  XX(7)  +   XX(10)
     &                 +  XX(13) -  XX(16) +  XX(19) -   XX(22)
       ELSE
               CALL U2MESS('F','ELEMENTS2_26')
      ENDIF

      END
