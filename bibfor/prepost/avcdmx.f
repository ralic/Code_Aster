      SUBROUTINE AVCDMX( NBVEC, DOMTOT, CUDOMX, VNORMX,NBPLAN)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 02/04/2013   AUTEUR TRAN V-X.TRAN 
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
C RESPONSABLE F1BHHAJ J.ANGLES
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      INTEGER    NBVEC, VNORMX(2)
      REAL*8     DOMTOT(NBVEC), CUDOMX
C ----------------------------------------------------------------------
C BUT: CALCULER LE MAX DES CUMULS DE DOMMAGE ET DETERMINER LE VECTEUR
C      NORMAL ASSOCIE.
C ----------------------------------------------------------------------
C ARGUMENTS :
C  NBVEC    IN   I  : NOMBRE DE VECTEURS NORMAUX.
C  DOMTOT   IN   R  : VECTEUR CONTENANT LES DOMMAGES TOTAUX (CUMUL)
C                     DE CHAQUE VECTEUR NORMAL.
C  VNORMX   OUT  I  : NUMERO DU VECTEUR NORMAL ASSOCIE AU MAX DES CUMULS
C                     DE DOMMAGE.
C  CUDOMX   OUT  R  : VALEUR DU MAX DES CUMULS DE DOMMAGE.
C ----------------------------------------------------------------------
C     ------------------------------------------------------------------
      INTEGER    IVECT, NBPLAN
      REAL*8     PREC, R8PREM
C     ------------------------------------------------------------------
C234567                                                              012
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      PREC=100.D0*R8PREM()
C
      CUDOMX = 0.0D0
      VNORMX(1) = 1

      NBPLAN = 1
      
C
      DO 10 IVECT=1, NBVEC
         IF ( DOMTOT(IVECT) .GT. CUDOMX ) THEN
            CUDOMX = DOMTOT(IVECT)
            VNORMX(1) = IVECT
         ENDIF
 10   CONTINUE
      
C ON CHERCHE SI EXISTE DIFFERENT PLAN
      VNORMX(2) = VNORMX(1)
      
      DO 431 IVECT=1, NBVEC
         IF ( (ABS(DOMTOT(IVECT)-CUDOMX) .LT. PREC )
     &           .AND. (IVECT .NE. VNORMX(1)) ) THEN
            NBPLAN = NBPLAN + 1 
            VNORMX(2) = IVECT
         ENDIF

 431  CONTINUE           

      CALL JEDEMA()
C
      END
