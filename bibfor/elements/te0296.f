      SUBROUTINE TE0296 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
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
C
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES EN 2D
C                      OPTION : 'CHAR_MECA_EPLG_R  ','CHAR_MECA_EPLG_F '
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
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
      INTEGER       NNO,KP,K,NPG,I,IDEFI,ITEMPS,IVECTU,IE1,IE2,IE3
      INTEGER       IPOIDS,IVF,IDFDE,IGEOM,IMATE,NNOS,NDIM,JGANO
      INTEGER       ITEMPE,IDEPL,IFORC,IALPH,ITHET,MATER,NBRES,IE4
C
      PARAMETER        ( NBRES = 2 )
C
      REAL*8             VALRES(NBRES),VALPAR(3),ZERO,UN,DEUX
      REAL*8             DFDX(9),DFDY(9),POIDS,R,Y,EXX,EYY,EXY,EZZ
      REAL*8             A11,A12,A21,A22,AAX,VF,TGD(2),VFI
      REAL*8             CP1,CP2,CP3,DP1,DP2,DP3
      REAL*8             ALPHA,C1,C2,C3,DTDM(7)
      REAL*8             BIDON(7),BIDON1(7),THETX,THETY
C
      CHARACTER*8        NOMRES(NBRES), NOMPAR(3)
      CHARACTER*2        CODRET(NBRES)
C ......................................................................
      DATA  ZERO , UN , DEUX / 0.0D0, 1.0D0, 2.0D0 /
C ......................................................................
C
      CALL JEMARQ()

      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      IF(OPTION(16:16).EQ.'R') THEN
         CALL JEVECH('PEPSINR','L',IDEFI)
         EXX=ZR(IDEFI  )
         EYY=ZR(IDEFI+1)
         EXY=ZR(IDEFI+3)
         EZZ=ZR(IDEFI+2)
      ELSE
         CALL JEVECH('PEPSINF','L',IDEFI)
         CALL JEVECH('PTEMPSR','L',ITEMPS)
         NOMPAR(1)='X'
         NOMPAR(2)='Y'
         NOMPAR(3)='INST'
         VALPAR(3)=ZR(ITEMPS)
      ENDIF
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PVECTUR','E',IVECTU)
      CALL JEVECH('PTHETAR','L',ITHET)
      CALL JEVECH('PALPHAR','L',IALPH)
      IDEPL=ITHET
      IFORC=ITHET
      ITEMPE=ITHET
      MATER=ZI(IMATE)
      NOMRES(1)='E'
      NOMRES(2)='NU'
      ALPHA    = ZR(IALPH)
        DO 101 KP=1,NPG
        K=(KP-1)*NNO
        CALL DFDM2D ( NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS )
        R = ZERO
        Y = ZERO
        THETX = ZERO
        THETY = ZERO
        DO 102 I=1,NNO
          VF = ZR(IVF+K+I-1)
          R    = R    +  ZR(IGEOM+2*I-2)*VF
          Y    = Y    +  ZR(IGEOM+2*I-1)*VF
          THETX = THETX + VF*ZR(ITHET + 2*(I-1))
          THETY = THETY + VF*ZR(ITHET + 2*(I-1) + 1)
102     CONTINUE
        IF(OPTION(16:16).EQ.'F') THEN
          VALPAR(1) = R + ALPHA*THETX
          VALPAR(2) = Y + ALPHA*THETY
          CALL FOINTE('FM',ZK8(IDEFI  ),3,NOMPAR,VALPAR,EXX,IE1)
          CALL FOINTE('FM',ZK8(IDEFI+1),3,NOMPAR,VALPAR,EYY,IE2)
          CALL FOINTE('FM',ZK8(IDEFI+3),3,NOMPAR,VALPAR,EXY,IE3)
          CALL FOINTE('FM',ZK8(IDEFI+2),3,NOMPAR,VALPAR,EZZ,IE4)
        ENDIF
C
        CALL RCVALA ( MATER,'ELAS',0,'TEMP',ZERO,2,NOMRES,
     +                VALRES, CODRET, 'FM' )
C
        IF ( NOMTE(3:4).EQ. 'AX' )  THEN
           C1=VALRES(1)/(UN+VALRES(2))
           C2=(UN-VALRES(2))/(UN-DEUX*VALRES(2))
           C3=VALRES(2)/(UN-DEUX*VALRES(2))
           CALL GDFONC ( DFDX,DFDY,KP,ZR(IVF),ZR(IDEPL),ZR(ITHET),
     S                   ZR(IFORC),ZR(ITEMPE),NNO,BIDON,DTDM,BIDON1,TGD)
           POIDS= POIDS*R
           A11=(UN+ALPHA*(DTDM(4)/R))*(UN+ALPHA*DTDM(2))
           A12=-ALPHA*DTDM(3)*(UN+ALPHA*(DTDM(4)/R))
           A21=-ALPHA*DTDM(5)*(UN+ALPHA*(DTDM(4)/R))
           A22=(UN+ALPHA*DTDM(1))*(UN+ALPHA*(DTDM(4)/R))
           AAX=( (UN+ALPHA*DTDM(1))*(UN+ALPHA*DTDM(2)) )
     +          -ALPHA*ALPHA*DTDM(3)*DTDM(5)
           DO 105 I=1,NNO
            VFI=ZR(IVF+K+I-1)
             ZR(IVECTU+2*I-2)=ZR(IVECTU+2*I-2)+POIDS*C1*
     +                   (  (A11*(C3*EYY+C2*EXX)+A12*EXY)
     +                   *DFDX(I)+(A21*(C3*EYY+C2*EXX)
     +                   +A22*EXY)*DFDY(I) +(AAX*C3*(EXX+EYY))
     +                   *VFI/R   )
             ZR(IVECTU+2*I-1)=ZR(IVECTU+2*I-1)+POIDS*C1*
     +                   ( ( A12*(C3*EXX+C2*EYY)+A11*EXY )
     +                   *DFDX(I)+( A22*(C3*EXX+C2*EYY)
     +                   +A21*EXY)*DFDY(I) )
105       CONTINUE
        ELSE
           CP1=VALRES(1)/(UN-VALRES(2)*VALRES(2))
           CP2=VALRES(2)
           CP3=(UN-VALRES(2))/DEUX
           DP1=( VALRES(1)*(UN-VALRES(2)) )/( (UN+VALRES(2))
     +          *(UN-DEUX*VALRES(2)))
           DP2=VALRES(2)/(UN-VALRES(2))
           DP3=(UN-DEUX*VALRES(2))/(DEUX*(UN-VALRES(2)))
           CALL GDFONC ( DFDX,DFDY,KP,ZR(IVF),ZR(IDEPL),ZR(ITHET),
     S                   ZR(IFORC),ZR(ITEMPE),NNO,BIDON,DTDM,BIDON1,TGD)
           A22=UN+ALPHA*DTDM(1)
           A11=UN+ALPHA*DTDM(2)
           A12=-ALPHA*DTDM(3)
           A21=-ALPHA*DTDM(5)
C
          IF ( NOMTE(3:4) .EQ. 'CP' ) THEN
           C1=CP1
           C2=CP2
           C3=CP3
           EZZ = ZERO
          ELSE
           C1=DP1
           C2=DP2
           C3=DP3
          ENDIF
C
          DO 104 I=1,NNO
           ZR(IVECTU+2*I-2)=ZR(IVECTU+2*I-2) + POIDS * C1* (
     +              ( A11*(EXX+(C2*EYY)+(C2*EZZ))+A12*2*C3*EXY )
     +               *DFDX(I)+( A21*(EXX+(C2*EYY)+(C2*EZZ))+2*C3*
     +               A22*EXY )*DFDY(I) )
C
           ZR(IVECTU+2*I-1)=ZR(IVECTU+2*I-1) + POIDS *C1* (
     +             ( A12*((C2*EXX)+EYY+(C2*EZZ))+(2*C3*A11*EXY) )*
     +              DFDX(I)+( A22*((C2*EXX)+(C2*EZZ)+EYY)+(A21*
     +              2*C3*EXY) )*DFDY(I) )

104       CONTINUE
C
        ENDIF
101   CONTINUE
C
      CALL JEDEMA()
      END
