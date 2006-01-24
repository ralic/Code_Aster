      SUBROUTINE IRVCMP ( NCMPMX, NOMCGD, NOMCMP, NBCMPT )
      IMPLICIT   NONE
      INTEGER             NCMPMX,                 NBCMPT
      CHARACTER*(*)               NOMCGD(*),NOMCMP
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 24/01/2006   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C     BUT :   TROUVER SI UNE COMPOSANTE EST PRESENTE DANS LA GRANDEUR
C     ENTREES:
C        NCMPMX : NOMBRE DE COMPOSANTES DE LA GRANDEUR
C        NOMCGD : NOMS DES COMPOSANTES DE LA GRANDEUR 
C        NOMCMP : NOM D'UNE COMPOSANTE
C     SORTIES:
C        NBCMPT : COMPOSANTE PRESENTE DANS LA GRANDEUR 
C ----------------------------------------------------------------------
      INTEGER       ICMP
C
      DO 10 ICMP = 1 , NCMPMX
         IF ( NOMCMP .EQ. NOMCGD(ICMP) ) THEN
            NBCMPT=NBCMPT+1
            GO TO 12
         ENDIF
  10  CONTINUE
  12  CONTINUE
C
      END
