      SUBROUTINE TE0290 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/04/2002   AUTEUR VABHHTS J.PELLET 
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
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
C                          OPTION : 'CALC_NOEU_BORD  '
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      CHARACTER*24       CARAC
      CHARACTER*8        ELREFE
      REAL*8             COOR(8),DX(4),DY(4),NX(9),NY(9),LON(4),SENS
C
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
      CHARACTER*6        PGC
      COMMON  / NOMAJE / PGC
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      CALL ELREF1(ELREFE)
      CALL JEMARQ()


      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,' ',ICARAC)
      NNO=ZI(ICARAC)
      IF(NOMTE(5:6).EQ.'QU') THEN
         NSOM = 4
      ELSE IF(NOMTE(5:6).EQ.'TR') THEN
         NSOM = 3
      ELSE
      CALL UTMESS('F','TE0290','ELEMENT NON 2D: PAS DE CALC_NOEU_BORD')
      ENDIF
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PVECTUR','E',IVECTU)
      DO 1 I=1,NSOM
        COOR(2*I-1) = ZR(IGEOM+2*(I-1))
        COOR(2*I)   = ZR(IGEOM+2*I-1)
1     CONTINUE
      DO 2 I=1,NSOM-1
        DX(I) = COOR(2*I+1)-COOR(2*I-1)
        DY(I) = COOR(2*I+2)-COOR(2*I)
2     CONTINUE
      DX(NSOM) = COOR(1)-COOR(2*NSOM-1)
      DY(NSOM) = COOR(2)-COOR(2*NSOM)
C
C   INITIALISATION A 0.
C
      DO 3 I=1,NNO
        ZR(IVECTU+2*I-2) = 0.D0
        ZR(IVECTU+2*I-1) = 0.D0
        NX(I) = 0.D0
        NY(I) = 0.D0
3     CONTINUE
      NX(1)  =  (DY(NSOM)+DY(1))
      NY(1)  = -(DX(NSOM)+DX(1))
      DO 4 I=2,NSOM
        NX(I)  =  (DY(I-1)+DY(I))
        NY(I)  = -(DX(I-1)+DX(I))
4     CONTINUE
      IF(NNO.NE.NSOM) THEN
        DO 6 I=NSOM+1,2*NSOM
          NX(I) =  DY(I-NSOM)
          NY(I) = -DX(I-NSOM)
6       CONTINUE
      ENDIF
C
C   VERIFICATION DU SENS DE L'ELEMENT
C
      SENS = DY(1)*DX(NSOM)-DX(1)*DY(NSOM)
      IF(SENS.EQ.0.D0) THEN
        CALL UTMESS('F','TE0290','ELEMENT DEGENERE:REVOIR LE MAILLAGE')
      ELSE IF(SENS.LT.0.D0) THEN
        DO 7 I=1,NNO
          NX(I) = -NX(I)
          NY(I) = -NY(I)
7       CONTINUE
      ENDIF
C
      DO 5 I=1,NNO
        ZR(IVECTU+2*I-2) = NX(I)
        ZR(IVECTU+2*I-1) = NY(I)
5     CONTINUE
      CALL JEDEMA()
      END
