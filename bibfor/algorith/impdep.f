      SUBROUTINE IMPDEP(ISOR,IDEP,IBL,DMOY,DETYP,DRMS,DMAX,DMIN)
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
C     IMPRESSION DES DEPLACEMENTS
C
      IMPLICIT REAL *8 (A-H,O-Z)
      INTEGER ISOR,IDEP
      REAL*8 DMOY,DETYP,DRMS,DMAX,DMIN
C
C
      IF (IDEP.EQ.1) THEN
        IF (IBL.EQ.1) THEN
         WRITE(ISOR,*)
         WRITE(ISOR,*) ' ***** STATISTIQUES DEPLACEMENTS X LOCAL *****'
         WRITE(ISOR,*)
         WRITE(ISOR,*) '---------------------------------------------',
     &                '----------------------------'
         WRITE(ISOR,*)  '!IB! DX MOYEN    ! DX E.TYPE   !',
     &                ' DX RMS      ! DX MAX      ! DX MIN      !'
         WRITE(ISOR,*) '---------------------------------------------',
     &                '----------------------------'
        ELSEIF (IBL.EQ.0) THEN
         WRITE(ISOR,*)
         WRITE(ISOR,*) ' ***** STATISTIQUES GLOBALES  DEPX *****'
         WRITE(ISOR,*)
         WRITE(ISOR,*) '---------------------------------------------',
     &                '----------------------------'
         WRITE(ISOR,*)'!IB! DX MOYEN    ! DX E.TYPE   ! DX RMS      !',
     &                ' DX MAX      ! DX MIN      !'
         WRITE(ISOR,*)'---------------------------------------------',
     &                '----------------------------'
        ENDIF
        WRITE(ISOR,10) IBL,DMOY,DETYP,DRMS,DMAX,DMIN
      ELSEIF (IDEP.EQ.2) THEN
        IF (IBL.EQ.1) THEN
         WRITE(ISOR,*)
         WRITE(ISOR,*)' ***** STATISTIQUES DEPLACEMENTS Y LOCAL *****'
         WRITE(ISOR,*)
         WRITE(ISOR,*)'---------------------------------------------',
     &                '----------------------------'
         WRITE(ISOR,*)'!IB! DY MOYEN    ! DY E.TYPE   ! DY RMS      !',
     &                ' DY MAX      ! DY MIN      !'
         WRITE(ISOR,*)'---------------------------------------------',
     &                '----------------------------'
        ELSEIF (IBL.EQ.0) THEN
         WRITE(ISOR,*)
         WRITE(ISOR,*) ' ***** STATISTIQUES GLOBALES  DEPY *****'
         WRITE(ISOR,*)
         WRITE(ISOR,*)'---------------------------------------------',
     &                '----------------------------'
         WRITE(ISOR,*)'!IB! DY MOYEN    ! DY E.TYPE   ! DY RMS      !',
     &                ' DY MAX      ! DY MIN      !'
         WRITE(ISOR,*)'---------------------------------------------',
     &                '----------------------------'
        ENDIF
        WRITE(ISOR,10) IBL,DMOY,DETYP,DRMS,DMAX,DMIN
      ELSEIF (IDEP.EQ.3) THEN
        IF (IBL.EQ.1) THEN
         WRITE(ISOR,*)
         WRITE(ISOR,*)' ***** STATISTIQUES DEPLACEMENTS Z LOCAL *****'
         WRITE(ISOR,*)
         WRITE(ISOR,*)'----------------------------------------------',
     &                '----------------------------'
         WRITE(ISOR,*)'!IB! DZ MOYEN    ! DZ E.TYPE   ! DZ RMS      !',
     &                ' DZ MAX      ! DZ MIN      !'
         WRITE(ISOR,*)'----------------------------------------------',
     &                '----------------------------'
        ELSEIF (IBL.EQ.0) THEN
         WRITE(ISOR,*)
         WRITE(ISOR,*) ' ***** STATISTIQUES GLOBALES  DEPZ *****'
         WRITE(ISOR,*)
         WRITE(ISOR,*)'----------------------------------------------',
     &                '----------------------------'
         WRITE(ISOR,*)'!IB! DZ MOYEN    ! DZ E.TYPE   ! DZ RMS      !',
     &                ' DZ MAX      ! DZ MIN      !'
         WRITE(ISOR,*)'----------------------------------------------',
     &                '----------------------------'
        ENDIF
        WRITE(ISOR,10) IBL,DMOY,DETYP,DRMS,DMAX,DMIN
      ELSEIF (IDEP.EQ.4) THEN
        IF (IBL.EQ.1) THEN
         WRITE(ISOR,*)
         WRITE(ISOR,*)' *****  STATISTIQUES DEPLACEMENT  RADIAL *****'
         WRITE(ISOR,*)
         WRITE(ISOR,*)'----------------------------------------------',
     &                '----------------------------'
         WRITE(ISOR,*)'!IB! R  MOYEN    ! R  E.TYPE   ! R  RMS      !',
     &                ' R  MAX      ! R  MIN      !'
         WRITE(ISOR,*)'----------------------------------------------',
     &                '----------------------------'
        ELSEIF (IBL.EQ.0) THEN
         WRITE(ISOR,*)
         WRITE(ISOR,*) ' ***** STATISTIQUES GLOBALES DEPL RADIAL ****'
         WRITE(ISOR,*)
         WRITE(ISOR,*)'----------------------------------------------',
     &                '----------------------------'
         WRITE(ISOR,*)'!IB! R  MOYEN    ! R  E.TYPE   ! R  RMS      !',
     &                ' R  MAX      ! R  MIN      !'
         WRITE(ISOR,*)'----------------------------------------------',
     &                '----------------------------'
        ENDIF
        WRITE(ISOR,10) IBL,DMOY,DETYP,DRMS,DMAX,DMIN
      ELSEIF (IDEP.EQ.5) THEN
        IF (IBL.EQ.1) THEN
         WRITE(ISOR,*)
         WRITE(ISOR,*)' ***** STATISTIQUES DEPLACEMENT ANGULAIRE ****'
         WRITE(ISOR,*)
         WRITE(ISOR,*)'----------------------------------------------',
     &                '----------------------------'
         WRITE(ISOR,*)'!IB! THETA MOYEN ! THETA E.TYP ! THETA RMS   !',
     &                ' THETA MAX   ! THETA MIN   !'
         WRITE(ISOR,*)'----------------------------------------------',
     &                '----------------------------'
        ELSEIF (IBL.EQ.0) THEN
         WRITE(ISOR,*)
         WRITE(ISOR,*) ' ***** STATISTIQUES GLOBALES DEPL ANGLE  ****'
         WRITE(ISOR,*)
         WRITE(ISOR,*)'----------------------------------------------',
     &                '----------------------------'
         WRITE(ISOR,*)'!IB! THETA MOYEN ! THETA E.TYP ! THETA RMS   !',
     &                ' THETA MAX   ! THETA MIN   !'
         WRITE(ISOR,*)'----------------------------------------------',
     &                '----------------------------'
        ENDIF
        WRITE(ISOR,10) IBL,DMOY,DETYP,DRMS,DMAX,DMIN
      ENDIF
C
C
 10   FORMAT(' !',I2,'!',1PE12.5,' !',1PE12.5,' !',1PE12.5,' !',
     &         1PE12.5,' !',1PE12.5,' !')
 9999 CONTINUE
      END
