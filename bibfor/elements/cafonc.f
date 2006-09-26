      SUBROUTINE CAFONC(IFONC,XX,YY)

      
      IMPLICIT NONE
      
      INTEGER    IFONC, IMATE
      REAL*8    XX
      REAL*8    YY
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 25/09/2006   AUTEUR MARKOVIC D.MARKOVIC 
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

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C     CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL,VECTEU,MATRIC,TEMPNO
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------


      CHARACTER*2    CODRES(7)
      CHARACTER*8    NOMRES(7)      
      REAL*8         VALRES(7)
      
C-------------------------------------
      CHARACTER*19    FEPX(12)
      INTEGER         ZIMAT,IZIMAT,IPHEN
      CHARACTER*16    PHENOM

C-------------------------------------

      CALL JEMARQ()
      
      PHENOM = 'GLRC_DAMAGE'

      CALL JEVECH('PMATERC','L',IMATE)
      ZIMAT = ZI(IMATE)
      
      IF ( IFONC .EQ. 1) THEN
        NOMRES(1) = 'FMEX1'
        CALL RCVALA(ZIMAT,' ',PHENOM,1,'X ',XX,1,NOMRES,
     &              VALRES,CODRES,'FM')
        YY   = VALRES(1)

      ELSEIF ( IFONC .EQ. 2) THEN
        NOMRES(1) = 'FMEX2'
        CALL RCVALA(ZIMAT,' ',PHENOM,1,'X ',XX,1,NOMRES,
     &              VALRES,CODRES,'FM')
        YY   = VALRES(1)

      ELSEIF ( IFONC .EQ. 3) THEN
        NOMRES(1) = 'FMEY1'
        CALL RCVALA(ZIMAT,' ',PHENOM,1,'X ',XX,1,NOMRES,
     &              VALRES,CODRES,'FM')
        YY   = VALRES(1)

      ELSEIF ( IFONC .EQ. 4) THEN
        NOMRES(1) = 'FMEY2'
        CALL RCVALA(ZIMAT,' ',PHENOM,1,'X ',XX,1,NOMRES,
     &              VALRES,CODRES,'FM')
        YY   = VALRES(1)

      ELSEIF ( IFONC .EQ. 5) THEN
        NOMRES(1) = 'DFMEX1'
        CALL RCVALA(ZIMAT,' ',PHENOM,1,'X ',XX,1,NOMRES,
     &              VALRES,CODRES,'FM')
        YY   = VALRES(1)
      ELSEIF ( IFONC .EQ. 6) THEN
        NOMRES(1) = 'DFMEX2'
        CALL RCVALA(ZIMAT,' ',PHENOM,1,'X ',XX,1,NOMRES,
     &              VALRES,CODRES,'FM')
        YY   = VALRES(1)
      ELSEIF ( IFONC .EQ. 7) THEN
        NOMRES(1) = 'DFMEY1'
        CALL RCVALA(ZIMAT,' ',PHENOM,1,'X ',XX,1,NOMRES,
     &              VALRES,CODRES,'FM')
        YY   = VALRES(1)
      ELSEIF ( IFONC .EQ. 8) THEN
        NOMRES(1) = 'DFMEY2'
        CALL RCVALA(ZIMAT,' ',PHENOM,1,'X ',XX,1,NOMRES,
     &              VALRES,CODRES,'FM')
        YY   = VALRES(1)
      ELSEIF ( IFONC .EQ. 9) THEN
        NOMRES(1) = 'DDFMEX1'
        CALL RCVALA(ZIMAT,' ',PHENOM,1,'X ',XX,1,NOMRES,
     &              VALRES,CODRES,'FM')
        YY   = VALRES(1)
      ELSEIF ( IFONC .EQ. 10) THEN
        NOMRES(1) = 'DDFMEX2'
        CALL RCVALA(ZIMAT,' ',PHENOM,1,'X ',XX,1,NOMRES,
     &              VALRES,CODRES,'FM')
        YY   = VALRES(1)
      ELSEIF ( IFONC .EQ. 11) THEN
        NOMRES(1) = 'DDFMEY1'
        CALL RCVALA(ZIMAT,' ',PHENOM,1,'X ',XX,1,NOMRES,
     &              VALRES,CODRES,'FM')
        YY   = VALRES(1)
      ELSEIF ( IFONC .EQ. 12) THEN
        NOMRES(1) = 'DDFMEY2'
        CALL RCVALA(ZIMAT,' ',PHENOM,1,'X ',XX,1,NOMRES,
     &              VALRES,CODRES,'FM')
        YY   = VALRES(1)
      ELSE
        YY = 0.0D0
      ENDIF
      
      CALL JEDEMA()

      END 
