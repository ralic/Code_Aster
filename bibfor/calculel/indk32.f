      INTEGER FUNCTION INDK32(LK32,K32Z,RANG,NBK32)
      IMPLICIT NONE

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 08/09/2003   AUTEUR VABHHTS J.PELLET 
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
C INSPI INDIK24
C     ARGUMENTS:
C     ----------
      INTEGER NBK32,RANG
      CHARACTER*(*) K32Z,LK32(*)
      CHARACTER*32 K32,LK32Z
C ----------------------------------------------------------------------
C     ENTREES:
C     LK32 : LISTE DE K32 OU ON DOIT CHERCHER LE MOT K32
C     K32 : MOT A CHERCHER
C     NBK32: NOMBRE DE MOTS DE LK32
C     RANG : ON CHERCHE LE RANG-IEME MOT K32 DANS LA LISTE.

C     SORTIES:
C     INDK32 : POSITION DU MOT CHERCHE DANS LA LISTE.
C           SI LE MOT EST ABSENT: INDK32=0

C ----------------------------------------------------------------------

C     VARIABLES LOCALES:
C     ------------------
      INTEGER I,J
C DEB-------------------------------------------------------------------

      K32 = K32Z
      J = 0
      DO 10 I = 1,NBK32
        LK32Z = LK32(I)
        IF (LK32Z.EQ.K32) THEN
          J = J + 1
          IF (J.EQ.RANG) GO TO 20
        END IF
   10 CONTINUE
      INDK32 = 0
      GO TO 30
   20 CONTINUE
      INDK32 = I
   30 CONTINUE
      END
