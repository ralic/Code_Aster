      INTEGER FUNCTION INDK80(LK80,K80Z,RANG,NBK80)
      IMPLICIT NONE

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 23/06/2005   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C INSPI INDIK24
C     ARGUMENTS:
C     ----------
      INTEGER NBK80,RANG
      CHARACTER*(*) K80Z,LK80(*)
      CHARACTER*80 K80,LK80Z
C ----------------------------------------------------------------------
C     ENTREES:
C     LK80 : LISTE DE K80 OU ON DOIT CHERCHER LE MOT K80
C     K80 : MOT A CHERCHER
C     NBK80: NOMBRE DE MOTS DE LK80
C     RANG : ON CHERCHE LE RANG-IEME MOT K80 DANS LA LISTE.

C     SORTIES:
C     INDK80 : POSITION DU MOT CHERCHE DANS LA LISTE.
C           SI LE MOT EST ABSENT: INDK80=0

C ----------------------------------------------------------------------

C     VARIABLES LOCALES:
C     ------------------
      INTEGER I,J
C DEB-------------------------------------------------------------------

      K80 = K80Z
      J = 0
      DO 10 I = 1,NBK80
        LK80Z = LK80(I)
        IF (LK80Z.EQ.K80) THEN
          J = J + 1
          IF (J.EQ.RANG) GO TO 20
        END IF
   10 CONTINUE
      INDK80 = 0
      GO TO 30
   20 CONTINUE
      INDK80 = I
   30 CONTINUE
      END
