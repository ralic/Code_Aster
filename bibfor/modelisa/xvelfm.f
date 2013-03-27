      SUBROUTINE XVELFM(NFISS,FISS,MODX)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 26/03/2013   AUTEUR CUVILLIE M.CUVILLIEZ 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER      NFISS
      CHARACTER*8  FISS(NFISS),MODX
C
C ----------------------------------------------------------------------
C
C ROUTINE XFEM (VERIFICATION DES SD)
C
C VERIFICATION QUE LES FISSURES OBTENUES EN SCRUTANT UN MOT-CLE ET 
C STOCKEES DANS LA LISTE FISS(1:NFISS), APPARTIENNENT BIEN AU MODELE
C MODX
C
C   -> ON S'ASSURE AU PREALABLE QUE MODX EST UN MODELE X-FEM
C
C ----------------------------------------------------------------------
C
C IN  NFISS  : NOMBRE DE FISSURES
C IN  FISS   : LISTE DES NOMS DES FISSURES
C IN  MODX   : NOM DU MODELE EN ENTREE
C
C ----------------------------------------------------------------------
C
      INTEGER      IFISS,IRET
      LOGICAL      LTROUV,XVFIMO
      CHARACTER*8  VALK(2)
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C     SI LE MODELE N'EST PAS UN MODELE X-FEM -> ERREUR FATALE
      CALL EXIXFE(MODX,IRET)
      IF (IRET.EQ.0) THEN
        VALK(1)=MODX
        CALL U2MESK('F','XFEM_72',1,VALK)
      ENDIF
C
C     BOUCLE SUR LES FISSURES IN
      DO 100 IFISS = 1,NFISS
C
        LTROUV=XVFIMO(MODX,FISS(IFISS))
C
C       SI FISS(IFISS) EST ABSENTE DU MODELE -> ERREUR FATALE
        IF (.NOT.LTROUV) THEN
          VALK(1)=FISS(IFISS)
          VALK(2)=MODX
          CALL U2MESK('F','XFEM_73',2,VALK)
        ENDIF 
C
 100  CONTINUE
C
      CALL JEDEMA()
      END
