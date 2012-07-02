      SUBROUTINE ENLIRD(DATEUR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C ......................................................................
      IMPLICIT NONE
C   - FONCTION REALISEE:
C       ECRITURE DE LA D.A.T.E D'AUJOURD'HUI SUR LA VARIABLE DATEUR
C   - OUT :
C       DATEUR : CHARACTER*24
C   - AUTEUR : PRIS A SIVA POUR ASTER
C ......................................................................
      INTEGER I,DATE9(9)
      CHARACTER*2 JOUR2(0:6),DATE2(2:7)
      CHARACTER*4 MOIS4(12),ANNEE
      CHARACTER*(*) DATEUR
      CHARACTER*24  DATEUZ
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DATA JOUR2/'LU','MA','ME','JE','VE','SA','DI'/
      DATA MOIS4/'JANV','FEVR','MARS','AVRI','MAI ','JUIN',
     &           'JUIL','AOUT','SEPT','OCTO','NOVE','DECE'/
C     APPEL A LA GENERALE
      CALL KLOKLO(DATE9)
      DATE2(2) = '00'
      IF (DATE9(2).LT.10) THEN
         WRITE(DATE2(2)(2:2),'(I1)') DATE9(2)
      ELSE
         WRITE(DATE2(2)(1:2),'(I2)') DATE9(2)
      END IF
      WRITE(ANNEE,'(I4)') DATE9(4)
      DO 1 I=5,7
         DATE2(I) = '00'
         IF (DATE9(I).LT.10) THEN
            WRITE(DATE2(I)(2:2),'(I1)') DATE9(I)
         ELSE
            WRITE(DATE2(I)(1:2),'(I2)') DATE9(I)
         END IF
    1 CONTINUE
      WRITE (DATEUZ,101) JOUR2(DATE9(1)),DATE2(2),
     &      MOIS4(DATE9(3)),ANNEE,(DATE2(I),I=5,7)
      DATEUR = DATEUZ
  101 FORMAT(A2,'-',A2,'-',A4,'-',A4,1X,A2,':',A2,':',A2)
      END 
