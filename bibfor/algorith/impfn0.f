      SUBROUTINE IMPFN0(ISOR,IFN,IBL,FNMOYT,FNMOYC,FNRMST,FNRMSC,
     &                  FMAX )
C***********************************************************************
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/10/95   AUTEUR GIBHHAY A.Y.PORTABILITE 
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
C
C     IMPRESSION DES FORCES NORMALES AMV
C
C
C
      IMPLICIT REAL *8 (A-H,O-Z)
      INTEGER ISOR,IFN
      REAL*8 FNMOYT,FNMOYC,FNRMST,FNRMSC,FMAX
C
C
      IF (IBL.EQ.1) THEN
      WRITE(ISOR,*)
      WRITE(ISOR,*) ' ***** STATISTIQUES FORCE NORMALE *****'
      WRITE(ISOR,*)  '+--+-------------+-------------+-------------+',
     &                '-------------+-------------+'
      WRITE(ISOR,*)  '!IB! FN MOY TTOT ! FN MOY TCHOC! FN RMS TTOT !',
     &                   ' FN RMS TCHOC! FN MAX      !'
      WRITE(ISOR,*)  '+--+-------------+-------------+-------------+',
     &                '-------------+-------------+'
      ELSEIF (IBL.EQ.0) THEN
      WRITE(ISOR,*)
      WRITE(ISOR,*) ' ***** STATISTIQUES GLOBALES FNORM *****'
      WRITE(ISOR,*)  '+--+-------------+-------------+-------------+',
     &                '-------------+-------------+'
      WRITE(ISOR,*)  '!IB! FN MOY TTOT ! FN MOY TCHOC! FN RMS TTOT !',
     &                   ' FN RMS TCHOC! FN MAX      !'
      WRITE(ISOR,*)  '+--+-------------+-------------+-------------+',
     &                '-------------+-------------+'
      ENDIF
      WRITE(ISOR,10) IBL,FNMOYT,FNMOYC,FNRMST,FNRMSC,FMAX
C
 10   FORMAT(' !',I2,'!',1PE12.5,' !',1PE12.5,' !',1PE12.5,' !',
     &        1PE12.5,' !',1PE12.5 ,' !')
 9999 CONTINUE
      END
