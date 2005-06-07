      SUBROUTINE TE0079 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 30/03/2004   AUTEUR CIBHHLV L.VIVAN 
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
C                          OPTION : 'CHAR_TH_SOURCR  '
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      REAL*8             DFDX(9),DFDY(9),POIDS,R
      REAL*8             COORSE(18),VECTT(9)
      INTEGER            NDIM,NNO,NNOS,KP,NPG,I,K,IVECTT,ISOUR
      INTEGER            IPOIDS,IVF,IDFDE,IGEOM,JGANO
      INTEGER            NNOP2,C(6,9),ISE,NSE
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C
      CALL ELREF4(' ','MASS',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)


C      CALL TECAEL(IADZI,IAZK24)
C      write(6,*) '--->>> OPTION ', OPTION
C      write(6,1000) NOMTE, ZK24(IAZK24-1+3)(1:8), NPG, NNO
C 1000 FORMAT(1P,'-->> maille ',A16,' ',A8, ' NPG ', I2, ' NNO = ',I2 )

C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PSOURCR','L',ISOUR)
      CALL JEVECH('PVECTTR','E',IVECTT)
C
      CALL CONNEC ( NOMTE, NSE, NNOP2, C )

      DO 10 I=1,NNOP2
        VECTT(I)=0.D0
10    CONTINUE

C BOUCLE SUR LES SOUS-ELEMENTS

      DO 100 ISE=1,NSE

        DO 105 I=1,NNO
          DO 105 J=1,2
            COORSE(2*(I-1)+J) = ZR(IGEOM-1+2*(C(ISE,I)-1)+J)
105     CONTINUE

        DO 101 KP=1,NPG
          K=(KP-1)*NNO
          CALL DFDM2D ( NNO,KP,IPOIDS,IDFDE,COORSE,DFDX,DFDY,POIDS )
          IF ( NOMTE(3:4) .EQ. 'AX' ) THEN
             R = 0.D0
             DO 102 I=1,NNO
               R = R + COORSE(2*(I-1)+1)*ZR(IVF+K+I-1)
102          CONTINUE
             POIDS = POIDS*R
          ENDIF
CDIR$ IVDEP
CCC      write(6,*)  '--->>> ZR(ISOUR+',KP,'-1) = ', ZR(ISOUR+KP-1)

          DO 103 I=1,NNO
             K=(KP-1)*NNO
             VECTT(C(ISE,I)) = VECTT(C(ISE,I)) + POIDS
     &                    * ZR(IVF+K+I-1) * ZR(ISOUR+KP-1)
103       CONTINUE
101     CONTINUE
100   CONTINUE

      DO 200 I=1,NNOP2
        ZR(IVECTT-1+I)=VECTT(I)
200   CONTINUE

      END
