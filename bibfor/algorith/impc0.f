      SUBROUTINE IMPC0(ISOR,IBL,NBC,TCM,TCMAX,TCMIN,NREBO,TREBM,
     &                 TCT,T,NBPT)
C***********************************************************************
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C     IMPRESSION DES CHOCS
C
C
      IMPLICIT NONE
      INTEGER ISOR,NBC,NREBO,NBPT
      REAL*8 TCM,TCMAX,TCMIN,TREBM,TCT
      REAL*8 T(*),DT,TACQUI
C
C-----------------------------------------------------------------------
      INTEGER IBL ,NREPC 
C-----------------------------------------------------------------------
      DT=T(2)-T(1)
      TACQUI = T(NBPT) - T(1)
      IF (NBC.NE.0) THEN
        NREPC=NREBO/NBC
      ELSE
        NREPC=0
      ENDIF
      IF (IBL.EQ.1) THEN
      WRITE(ISOR,*)' '
      WRITE(ISOR,*)' ***** STATISTIQUES DES CHOCS    ***** '
C
      WRITE(ISOR,*) '------------------------------'
      WRITE(ISOR,*) '! PAS ACQUIS  ! DUREE ACQUIS !'
      WRITE(ISOR,9) DT,TACQUI
      WRITE(ISOR,*) '------------------------------'
      WRITE(ISOR,*) '-----------------------------------------'//
     & '--------------------------------------------'
      WRITE(ISOR,*) '!IB! CHOC/S ! REB/CH ! TCHOC MOYEN !'//
     &         ' TCHOC MAX   ! TCHOC MIN   ! T REBOND MOY!%T. CHOC!'
      WRITE(ISOR,*) '-----------------------------------------'//
     & '--------------------------------------------'
      ELSEIF (IBL.EQ.0) THEN
      WRITE(ISOR,*)' '
      WRITE(ISOR,*)' ***** STATISTIQUES GLOBALES DES CHOCS    ***** '
C
      WRITE(ISOR,*) '------------------------------'
      WRITE(ISOR,*) '! PAS ACQUIS  ! DUREE ACQUIS !'
      WRITE(ISOR,9) DT,TACQUI
      WRITE(ISOR,*) '------------------------------'
      WRITE(ISOR,*) '-------------------------------'//
     & '------------------------------------------------------'
      WRITE(ISOR,*) '!IB! CHOC/S ! REB/CH ! TCHOC MOYEN !'//
     &         ' TCHOC MAX   ! TCHOC MIN   ! T REBOND MOY!%TEMPS CHOC!'
      WRITE(ISOR,*) '-----------------------------------------'//
     & '--------------------------------------------'
      ENDIF
      WRITE(ISOR,8) IBL,INT(NBC/TACQUI),NREPC,TCM,TCMAX,TCMIN,TREBM,
     &                  (100.D0*TCT/TACQUI)
C
 8    FORMAT(' !',I2,'!',I5,'   !',I5,'   !',1PD12.5,' !',
     &          1PD12.5,' !',1PD12.5,' !',1PD12.5,' !',1PD12.5,' %!')
 9    FORMAT(' !',1PD12.5,' !',1PD12.5,' !')
C
      END
