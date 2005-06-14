      SUBROUTINE DXREPE ( NNO , PGL , R )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 30/08/95   AUTEUR B8BHHHH J.R.LEVESQUE 
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
      IMPLICIT REAL*8 (A-H,O-Z)
C     ---------------------------------------------------
      INTEGER       NNO
      REAL*8        PGL(3,3) , R(*)
C     ------------------------------------------------------------------
C         CALCUL DES MATRICE T1VE ET T2VE DE PASSAGE D'UNE MATRICE
C         (3,3) ET (2,2) DU REPERE DE LA VARIETE AU REPERE ELEMENT
C         ET T2VE INVERSE DE T2EV
C
C         POUR TOUTES LES OPTIONS DE POST TRAITEMENT COQUE
C     ------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      REAL*8        DX , DY , DZ , S , C , NORM
      REAL*8        PS , PJDX , PJDY , PJDZ
      REAL*8        ALPHA , BETA , R8DGRD , R8PREM
C
      CALL JEVECH ('PCACOQU', 'L', JCOQU)
      ALPHA = ZR(JCOQU+1) * R8DGRD()
      BETA  = ZR(JCOQU+2) * R8DGRD()
C
      DX = COS(BETA)*COS(ALPHA)
      DY = COS(BETA)*SIN(ALPHA)
      DZ = SIN(BETA)
      NORM = SQRT (DX*DX + DY*DY + DZ*DZ)
      DX = DX/NORM
      DY = DY/NORM
      DZ = DZ/NORM
      PS = DX*PGL(3,1) + DY*PGL(3,2) + DZ*PGL(3,3)
      PJDX = DX - PS*PGL(3,1)
      PJDY = DY - PS*PGL(3,2)
      PJDZ = DZ - PS*PGL(3,3)
      NORM = SQRT (PJDX*PJDX + PJDY*PJDY + PJDZ*PJDZ)
      IF ( NORM .LE. R8PREM() ) THEN
          CALL UTMESS('F','DXREPE','L''AXE DE REFERENCE EST NORMAL A'
     &                //' UN ELEMENT DE PLAQUE - CALCUL OPTION'
     &                //' IMPOSSIBLE - ORIENTER CES MAILLES' )
      ENDIF
C
      IF ( NNO .EQ. 3 ) THEN
C        8 + 3 * NPG + 2 * NNO + 5 * NC ( NNO = NPG = NC = 3 )
         LT1VE = 38
      ELSE IF ( NNO .EQ. 4 ) THEN
C        8 + 3 * NPG + 2 * NNO + 9 * NC + 4 ( NNO = NPG = NC = 4 )
         LT1VE = 68
      ENDIF
      LT2VE = LT1VE + 9
      LT2EV = LT2VE + 4
C
      PJDX = PJDX/NORM
      PJDY = PJDY/NORM
      PJDZ = PJDZ/NORM
      C = PJDX*PGL(1,1) + PJDY*PGL(1,2) + PJDZ*PGL(1,3)
      S = PJDX*PGL(2,1) + PJDY*PGL(2,2) + PJDZ*PGL(2,3)
      R(LT2EV  ) =   C
      R(LT2EV+1) =   S
      R(LT2EV+2) = - S
      R(LT2EV+3) =   C
      R(LT2VE  ) =   C
      R(LT2VE+1) = - S
      R(LT2VE+2) =   S
      R(LT2VE+3) =   C
      R(LT1VE  ) =   C * C
      R(LT1VE+3) =   S * S
      R(LT1VE+6) =   C * S
      R(LT1VE+1) =   R(LT1VE+3)
      R(LT1VE+4) =   R(LT1VE  )
      R(LT1VE+7) = - R(LT1VE+6)
      R(LT1VE+2) = - R(LT1VE+6) - R(LT1VE+6)
      R(LT1VE+5) =   R(LT1VE+6) + R(LT1VE+6)
      R(LT1VE+8) =   R(LT1VE  ) - R(LT1VE+3)
      END
