      SUBROUTINE PTFOCP(ITYPE,OPTION,NOMTE,A,A2,XL,RAD,ANGS2,IST,
     +                  NNO,NC,PGL,PGL1,PGL2, FER, FEI )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           ITYPE
      CHARACTER*(*)           OPTION,NOMTE
      REAL*8            FER(12),FEI(12),PGL(3,3),PGL1(3,3),PGL2(3,3)
      REAL*8                               A,A2,XL,RAD,ANGS2
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 09/03/99   AUTEUR VABHHTS J.PELLET 
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
C     ------------------------------------------------------------------
C
C --------- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------
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
C --------- FIN  DECLARATIONS NORMALISEES JEVEUX -----------------------
C
      REAL*8       RHO,  COEF1,  COEF2, S, S2
      REAL*8       ZERO, UN,  XXX
      REAL*8       XX1, XX2
      REAL*8       U(3), V(3), W(6), W2(3)
      REAL*8       QR(12), QQR(12), QI(12), QQI(12)
      CHARACTER*2  CODRET
      CHARACTER*16 CH16
      LOGICAL      GLOBAL, NORMAL
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C     --- INITIALISATION  ---
      ZERO = 0.0D0
      UN   = 1.0D0
      DO 10 I =1,12
         QR(I)  = ZERO
         QQR(I) = ZERO
         FER(I) = ZERO
         QI(I)  = ZERO
         QQI(I) = ZERO
         FEI(I) = ZERO
 10   CONTINUE
      NNOC = 1
      NCC  = 6
      GLOBAL = .FALSE.
C
      IF ( OPTION.EQ.'CHAR_MECA_FC1D1D' ) THEN
         CALL JEVECH('PGEOMER', 'L',LX)
         LX = LX-1
         DO 20 I = 1,3
            W(I)   = ZR(LX+I)
            W(I+3) = ZR(LX+I+3)
            W2(I)  = W(I+3) - W(I)
 20      CONTINUE
C
C         --- FORCE POUTRE A VALEURS COMPLEXES ---
         CALL JEVECH ('PFC1D1D','L',LFORC)
         XXX = ABS( DBLE( ZC(LFORC+6) ) )
         GLOBAL = XXX .LT. 1.D-3
         NORMAL = XXX .GT. 1.001D0
         DO 30 I = 1, 3
            QR(I)   =   DBLE( ZC(LFORC-1+I) )
            QR(I+6) =  QR(I)
            QI(I)   =  DIMAG( ZC(LFORC-1+I) )
            QI(I+6) =  QI(I)
            XXX    =  ABS( DBLE( ZC(LFORC-1+3+I) ) )
            IF (XXX .GT. 1.D-20) THEN
               CALL UTMESS('F','ELEMENTS DE POUTRE (PTFOCP)',
     +                         'ON NE TRAITE PAS LES MOMENTS')
            ENDIF
 30      CONTINUE
C
         IF ( NORMAL ) THEN
           CALL PSCAL(3,W2,W2,S)
           S2=1.D0/S
           CALL PROVEC(W2,QR(1),U)
           CALL PSCAL(3,U,U,S)
           S3 = SQRT(S)
           CALL PSCAL(3,QR(1),QR(1),S)
           S4 = SQRT(S)
           S5 = S3*SQRT(S2)/S4
           CALL PROVEC(U,W2,V)
           CALL PSCVEC(3,S2,V,U)
           CALL PSCVEC(3,S5,U,QR(1))
           CALL PROVEC(W2,QR(7),U)
           CALL PSCAL(3,U,U,S)
           S3 = SQRT(S)
           CALL PSCAL(3,QR(7),QR(7),S)
           S4 = SQRT(S)
           S5 = S3*SQRT(S2)/S4
           CALL PROVEC(U,W2,V)
           CALL PSCVEC(3,S2,V,U)
           CALL PSCVEC(3,S5,U,QR(7))
C
           CALL PSCAL(3,W2,W2,S)
           S2=1.D0/S
           CALL PROVEC(W2,QI(1),U)
           CALL PSCAL(3,U,U,S)
           S3 = SQRT(S)
           CALL PSCAL(3,QI(1),QI(1),S)
           S4 = SQRT(S)
           S5 = S3*SQRT(S2)/S4
           CALL PROVEC(U,W2,V)
           CALL PSCVEC(3,S2,V,U)
           CALL PSCVEC(3,S5,U,QI(1))
           CALL PROVEC(W2,QI(7),U)
           CALL PSCAL(3,U,U,S)
           S3 = SQRT(S)
           CALL PSCAL(3,QI(7),QI(7),S)
           S4 = SQRT(S)
           S5 = S3*SQRT(S2)/S4
           CALL PROVEC(U,W2,V)
           CALL PSCVEC(3,S2,V,U)
           CALL PSCVEC(3,S5,U,QI(7))
         ENDIF
C        --- PASSAGE REPERE LOCAL DU VECTEUR FORCE (SI NECESSAIRE) ---
         IF ( GLOBAL .OR. NORMAL ) THEN
            IF ( NOMTE(1:12) .EQ. 'MECA_POU_C_T' ) THEN
               CALL UTPVGL ( NNOC, NCC, PGL1, QR(1), QQR(1))
               CALL UTPVGL ( NNOC, NCC, PGL2, QR(7), QQR(7))
               CALL UTPVGL ( NNOC, NCC, PGL1, QI(1), QQI(1))
               CALL UTPVGL ( NNOC, NCC, PGL2, QI(7), QQI(7))
            ELSE
               CALL UTPVGL ( NNO, NC, PGL, QR(1), QQR(1))
               CALL UTPVGL ( NNO, NC, PGL, QI(1), QQI(1))
            ENDIF
         ELSE
            DO 40 I = 1, 12
               QQR(I) = QR(I)
               QQI(I) = QI(I)
 40         CONTINUE
         ENDIF
C
C        ---A CAUSE DES CHARGEMENTS VARIABLES ---
         COEF1 = UN
         COEF2 = UN
C
      ELSE
         CH16 = OPTION
         CALL UTMESS('F','ELEMENTS DE POUTRE (PTFORP)',
     +                           'L''OPTION "'//CH16//'" EST INCONNUE')
      ENDIF
C *********************************************************************
C
C     --- RECUPERATION DU COEF_MULT ---
C
      CALL TECACH(.FALSE.,.FALSE.,'PCOEFFR',1,ICOER)
      CALL TECACH(.FALSE.,.FALSE.,'PCOEFFC',1,ICOEC)
C
      IF ( ICOER .NE. 0 ) THEN
         DO 400 I = 1 , 12
            QQR(I) = QQR(I) * ZR(ICOER)
            QQI(I) = QQI(I) * ZR(ICOER)
 400     CONTINUE
C
      ELSEIF ( ICOEC .NE. 0 ) THEN
         DO 410 I = 1 , 12
            QQR(I) = QQR(I) *  DBLE( ZC(ICOEC) )
            QQI(I) = QQI(I) * DIMAG( ZC(ICOEC) )
 410     CONTINUE
C
C
      ENDIF
C
      CALL PTFOP1 ( ITYPE, COEF1, COEF2, XL, RAD, ANGS2, GLOBAL,
     +                     QQR, FER )
      CALL PTFOP1 ( ITYPE, COEF1, COEF2, XL, RAD, ANGS2, GLOBAL,
     +                     QQI, FEI )
C
      END
