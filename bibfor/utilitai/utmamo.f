      SUBROUTINE  UTMAMO(MODELE,NBTROU,LITROU)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      INTEGER NBTROU
      CHARACTER*8 MODELE
      CHARACTER*(*) LITROU
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 20/11/2012   AUTEUR DELMAS J.DELMAS 
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
C
C RESPONSABLE DELMAS J.DELMAS
C
C     BUT:
C       FILTRER LES MAILLES AFFECTEES PAR LE MODELE
C                   **                       **
C       IDEM QUE UTMAM2 MAIS AVEC UN VECTEUR JEVEUX
C
C     ARGUMENTS:
C     ----------
C
C      ENTREE :
C-------------
C IN   MODELE    : NOM DU MODELE
C
C      SORTIE :
C-------------
C OUT  NBTROU    : NOMBRE DE MAILLE TROUVEES
C OUT  LITROU    : LISTE DES MAILLES TROUVEES (OBJET JEVEUX)
C                  SI NBTROU = 0, L'OBJET JEVEUX N'EST PAS CREE
C
C.......................................................................
C
      INTEGER NBMAIL
      INTEGER ITRMA

      CHARACTER*8 K8BID
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL JELIRA(MODELE//'.MAILLE','LONMAX',NBMAIL,K8BID)
C      
      CALL WKVECT(LITROU,'V V I',NBMAIL,ITRMA)
C
      CALL UTMAM2(MODELE,NBMAIL,NBTROU,ZI(ITRMA))
C
      IF (NBTROU.GT.0) THEN
        CALL JUVECA(LITROU,NBTROU)
      ELSE
        CALL JEDETR(LITROU)
      ENDIF
C
      CALL JEDEMA()
C
      END
