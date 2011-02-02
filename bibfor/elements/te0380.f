      SUBROUTINE TE0380 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 02/02/2011   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C IN OPTION    : K16 :  OPTION DE CALCUL
C        'EFCA_ELNO'
C IN NOMTE     : K16 : NOM DU TYPE ELEMENT
C        'MECABL2', 'MEPOULI' OU 'MECA_POU_D_T_GD'
C
C     ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C     ------------------------------------------------------------------
      REAL*8  ZERO, DEUX, XD(3), XUG(9), ROTABS(3,3)
      REAL*8  QI(3),GN(3),GM(3),PN(3),PM(3),ROT0(3,3),ROT(3,3)
C DEB ------------------------------------------------------------------
C
      CALL JEVECH ( 'PSIEFNOR', 'L', ISIGNO )
      CALL JEVECH ( 'PDEPLAR' , 'L', IDEPL  )
      CALL JEVECH ( 'PEFFORR' , 'E', IEFFCA )
C
      IF (NOMTE.NE.'MECA_POU_D_T_GD') THEN
         CALL JEVECH ( 'PGEOMER', 'L', IGEOM )
         IF(NOMTE.EQ.'MEPOULI') THEN
            NE2    = 9
         ELSEIF(NOMTE.EQ.'MECABL2') THEN
            NE2    = 6
         ENDIF
         DO 10 I = 1,NE2
            XUG(I) = ZR(IDEPL-1+I) + ZR(IGEOM-1+I)
 10      CONTINUE
C
         IF (NOMTE.NE.'MEPOULI') THEN
            CALL VDIFF(3,XUG(4),XUG(1),XD)
            S=DDOT(3,XD,1,XD,1)
            S = SQRT(S)
            S = ZR(ISIGNO)/S
            CALL PSCVEC(3,S,XD,ZR(IEFFCA))
            CALL PSCVEC(3,S,XD,ZR(IEFFCA+3))
         ELSE
            CALL VDIFF ( 3, XUG(7), XUG(1), XD )
            S=DDOT(3,XD,1,XD,1)
            S = ZR(ISIGNO) / SQRT(S)
            CALL PSCVEC ( 3, S, XD, ZR(IEFFCA) )
            CALL VDIFF ( 3, XUG(7), XUG(4), XD )
            S=DDOT(3,XD,1,XD,1)
            S = ZR(ISIGNO) / SQRT(S)
            CALL PSCVEC ( 3, S, XD, ZR(IEFFCA+3) )
            DO 12 I =1,3
               ZR(IEFFCA+I+5) = -(ZR(IEFFCA+I+2)+ZR(IEFFCA+I-1))
 12         CONTINUE
         ENDIF
C
      ELSE
C
         CALL JEVECH ( 'PCAORIE', 'L', LORIEN )
         CALL MATROT ( ZR(LORIEN) , ROT )
         CALL TRANSP (ROT,3,3,3,ROT0,3)
         DO 20 NE = 1, 2
            DO 22 IC = 1,3
               QI(IC) = ZR(IDEPL +2+6*(NE-1)+IC)
               GN(IC) = ZR(ISIGNO-1+6*(NE-1)+IC)
               GM(IC) = ZR(ISIGNO+2+6*(NE-1)+IC)
 22         CONTINUE
            CALL MAROTA ( QI, ROT )
            CALL PROMAT ( ROT   ,3,3,3,ROT0  ,3,3,3, ROTABS)
            CALL PROMAT ( ROTABS,3,3,3,GN    ,3,3,1, PN    )
            CALL PROMAT ( ROTABS,3,3,3,GM    ,3,3,1, PM    )
            DO 24 IC = 1,3
               ZR(IEFFCA-1+6*(NE-1)+IC) = PN(IC)
               ZR(IEFFCA+2+6*(NE-1)+IC) = PM(IC)
 24         CONTINUE
 20      CONTINUE
C
      ENDIF
C
      END
