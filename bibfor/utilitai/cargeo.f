      SUBROUTINE CARGEO ( MAILLA )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*)       MAILLA
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C  CALCULER DES CARACTERISTIQUES DU MAILLAGES:
C  X_MIN    : ABSCISSE MINIMALE DES NOEUDS DU MAILLAGE
C  X_MAX    : ABSCISSE MAXIMALE DES NOEUDS DU MAILLAGE
C  Y_MIN    : ORDONNEE MINIMALE DES NOEUDS DU MAILLAGE
C  Y_MAX    : ORDONNEE MAXIMALE DES NOEUDS DU MAILLAGE
C  Z_MIN    : COTE MINIMALE DES NOEUDS DU MAILLAGE
C  Z_MAX    : COTE MAXIMALE DES NOEUDS DU MAILLAGE
C  APPLAT_Z : = ( Z_MAX - Z_MIN ) / D
C             AVEC D = MAX((X_MAX-X_MIN),(Y_MAX-Y_MIN),1.D-100)
C  AR_MIN   : LONGUEUR DE LA PLUS PETITE ARRETE DU MAILLAGE (NON NULLE)
C  AR_MAX   : LONGUEUR DE LA PLUS GRANDE ARRETE DU MAILLAGE
C
C IN  : MAILLA  : NOM DE LA SD MAILLAGE
C     ------------------------------------------------------------------
C
      INTEGER       NBNOEU, JVALE, JDIME, NBMAIL, I, NBPARA, NBPART,
     +              IBID, IM, JTYPM, JCONE, N, IRET
      PARAMETER    ( NBPARA = 9 )
      REAL*8        APPLAT, XMAX, YMAX, ZMAX, XMIN, YMIN, ZMIN,
     +              VALE(NBPARA), ARMIN, ARMAX, X(8), Y(8), Z(8),
     +              D1, D2, D3, D4, R8GAEM,RMINSP
      COMPLEX*16    C16B
      CHARACTER*8   K8B, MA, NOPARA(NBPARA), TYPARA(NBPARA), TYPM
      CHARACTER*1   BAS1
      CHARACTER*19  NOMT19
      CHARACTER*24  NODIME, CONNEX, COORDO, TYPMAI
C
      DATA NOPARA / 'X_MIN' , 'X_MAX' , 'Y_MIN' , 'Y_MAX' , 'Z_MIN' ,
     +              'Z_MAX' , 'APPLAT_Z' , 'AR_MIN' , 'AR_MAX' /
      DATA TYPARA / 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R' /
C DEB------------------------------------------------------------------
C
      CALL JEMARQ ( )

      MA = MAILLA
      NODIME = MA//'.DIME           '
      CONNEX = MA//'.CONNEX         '
      COORDO = MA//'.COORDO    .VALE'
      TYPMAI = MA//'.TYPMAIL        '

      CALL JELIRA(NODIME,'CLAS',IBID,BAS1)
      CALL ASSERT(BAS1.EQ.'G' .OR. BAS1.EQ.'V')

      CALL JEVEUO ( NODIME , 'L' , JDIME )
      NBNOEU = ZI(JDIME)
      NBMAIL = ZI(JDIME+3-1)
C
      CALL JEVEUO ( COORDO , 'L' , JVALE )
      XMAX = ZR(JVALE  )
      YMAX = ZR(JVALE+1)
      ZMAX = ZR(JVALE+2)
      XMIN = XMAX
      YMIN = YMAX
      ZMIN = ZMAX
      DO 10 I = 2 , NBNOEU
         XMAX = MAX ( XMAX , ZR(JVALE+3*(I-1)  ) )
         XMIN = MIN ( XMIN , ZR(JVALE+3*(I-1)  ) )
         YMAX = MAX ( YMAX , ZR(JVALE+3*(I-1)+1) )
         YMIN = MIN ( YMIN , ZR(JVALE+3*(I-1)+1) )
         ZMAX = MAX ( ZMAX , ZR(JVALE+3*(I-1)+2) )
         ZMIN = MIN ( ZMIN , ZR(JVALE+3*(I-1)+2) )
 10   CONTINUE
      D1 = MAX( (XMAX-XMIN) , (YMAX-YMIN) , 1.D-100 )
      APPLAT = ( ZMAX - ZMIN ) / D1
      VALE(1) = XMIN
      VALE(2) = XMAX
      VALE(3) = YMIN
      VALE(4) = YMAX
      VALE(5) = ZMIN
      VALE(6) = ZMAX
      VALE(7) = APPLAT
C
      CALL JEEXIN ( CONNEX , IRET )
      IF ( IRET .EQ. 0 ) THEN
         NBPART = 7
         GOTO 100
      ELSE
         NBPART = NBPARA
      ENDIF
C
      ARMIN = R8GAEM()
      ARMAX = 0.D0
      CALL JEVEUO ( TYPMAI, 'L', JTYPM )
      DO 20 IM = 1 , NBMAIL
         CALL JENUNO ( JEXNUM('&CATA.TM.NOMTM', ZI(JTYPM+IM-1)), TYPM )
         CALL JEVEUO ( JEXNUM(CONNEX,IM), 'L', JCONE )
         IF     ( TYPM(1:3) .EQ. 'POI'   ) THEN
         ELSEIF ( TYPM(1:3) .EQ. 'SEG'   ) THEN
            DO 27 I = 1 , 2
               N = ZI(JCONE+I-1)
               X(I) = ZR(JVALE+3*(N-1)  )
               Y(I) = ZR(JVALE+3*(N-1)+1)
               Z(I) = ZR(JVALE+3*(N-1)+2)
 27         CONTINUE
            D1 = (X(2)-X(1))**2 + (Y(2)-Y(1))**2 + (Z(2)-Z(1))**2
            ARMIN = RMINSP( ARMIN , D1 , 0.D0, 0.D0, 0.D0)
            ARMAX = MAX ( ARMAX , D1 )
         ELSEIF ( TYPM(1:4) .EQ. 'TRIA' ) THEN
            DO 21 I = 1 , 3
               N = ZI(JCONE+I-1)
               X(I) = ZR(JVALE+3*(N-1)  )
               Y(I) = ZR(JVALE+3*(N-1)+1)
               Z(I) = ZR(JVALE+3*(N-1)+2)
 21         CONTINUE
            D1 = (X(2)-X(1))**2 + (Y(2)-Y(1))**2 + (Z(2)-Z(1))**2
            D2 = (X(3)-X(2))**2 + (Y(3)-Y(2))**2 + (Z(3)-Z(2))**2
            D3 = (X(1)-X(3))**2 + (Y(1)-Y(3))**2 + (Z(1)-Z(3))**2
            ARMIN = RMINSP( ARMIN , D1 , D2 , D3 ,0.D0)
            ARMAX = MAX ( ARMAX , D1 , D2 , D3 )
         ELSEIF ( TYPM(1:4) .EQ. 'QUAD' ) THEN
            DO 22 I = 1 , 4
               N = ZI(JCONE+I-1)
               X(I) = ZR(JVALE+3*(N-1)  )
               Y(I) = ZR(JVALE+3*(N-1)+1)
               Z(I) = ZR(JVALE+3*(N-1)+2)
 22         CONTINUE
            D1 = (X(2)-X(1))**2 + (Y(2)-Y(1))**2 + (Z(2)-Z(1))**2
            D2 = (X(3)-X(2))**2 + (Y(3)-Y(2))**2 + (Z(3)-Z(2))**2
            D3 = (X(4)-X(3))**2 + (Y(4)-Y(3))**2 + (Z(4)-Z(3))**2
            D4 = (X(1)-X(4))**2 + (Y(1)-Y(4))**2 + (Z(1)-Z(4))**2
            ARMIN = RMINSP( ARMIN , D1 , D2 , D3 , D4 )
            ARMAX = MAX ( ARMAX , D1 , D2 , D3 , D4 )
         ELSEIF ( TYPM(1:4) .EQ. 'HEXA'  ) THEN
            DO 23 I = 1 , 8
               N = ZI(JCONE+I-1)
               X(I) = ZR(JVALE+3*(N-1)  )
               Y(I) = ZR(JVALE+3*(N-1)+1)
               Z(I) = ZR(JVALE+3*(N-1)+2)
 23         CONTINUE
            D1 = (X(2)-X(1))**2 + (Y(2)-Y(1))**2 + (Z(2)-Z(1))**2
            D2 = (X(3)-X(2))**2 + (Y(3)-Y(2))**2 + (Z(3)-Z(2))**2
            D3 = (X(4)-X(3))**2 + (Y(4)-Y(3))**2 + (Z(4)-Z(3))**2
            D4 = (X(1)-X(4))**2 + (Y(1)-Y(4))**2 + (Z(1)-Z(4))**2
            ARMIN = RMINSP( ARMIN , D1 , D2 , D3 , D4 )
            ARMAX = MAX ( ARMAX , D1 , D2 , D3 , D4 )
            D1 = (X(6)-X(5))**2 + (Y(6)-Y(5))**2 + (Z(6)-Z(5))**2
            D2 = (X(7)-X(6))**2 + (Y(7)-Y(6))**2 + (Z(7)-Z(6))**2
            D3 = (X(8)-X(7))**2 + (Y(8)-Y(7))**2 + (Z(8)-Z(7))**2
            D4 = (X(5)-X(8))**2 + (Y(5)-Y(8))**2 + (Z(5)-Z(8))**2
            ARMIN = RMINSP( ARMIN , D1 , D2 , D3 , D4 )
            ARMAX = MAX ( ARMAX , D1 , D2 , D3 , D4 )
            D1 = (X(1)-X(5))**2 + (Y(1)-Y(5))**2 + (Z(1)-Z(5))**2
            D2 = (X(2)-X(6))**2 + (Y(2)-Y(6))**2 + (Z(2)-Z(6))**2
            D3 = (X(3)-X(7))**2 + (Y(3)-Y(7))**2 + (Z(3)-Z(7))**2
            D4 = (X(4)-X(8))**2 + (Y(4)-Y(8))**2 + (Z(4)-Z(8))**2
            ARMIN = RMINSP( ARMIN , D1 , D2 , D3 , D4 )
            ARMAX = MAX ( ARMAX , D1 , D2 , D3 , D4 )
         ELSEIF ( TYPM(1:5) .EQ. 'PENTA' ) THEN
            DO 24 I = 1 , 6
               N = ZI(JCONE+I-1)
               X(I) = ZR(JVALE+3*(N-1)  )
               Y(I) = ZR(JVALE+3*(N-1)+1)
               Z(I) = ZR(JVALE+3*(N-1)+2)
 24         CONTINUE
            D1 = (X(2)-X(1))**2 + (Y(2)-Y(1))**2 + (Z(2)-Z(1))**2
            D2 = (X(3)-X(2))**2 + (Y(3)-Y(2))**2 + (Z(3)-Z(2))**2
            D3 = (X(1)-X(3))**2 + (Y(1)-Y(3))**2 + (Z(1)-Z(3))**2
            ARMIN = RMINSP( ARMIN , D1 , D2 , D3 ,0.D0)
            ARMAX = MAX ( ARMAX , D1 , D2 , D3 )
            D1 = (X(5)-X(4))**2 + (Y(5)-Y(4))**2 + (Z(5)-Z(4))**2
            D2 = (X(6)-X(5))**2 + (Y(6)-Y(5))**2 + (Z(6)-Z(5))**2
            D3 = (X(4)-X(6))**2 + (Y(4)-Y(6))**2 + (Z(4)-Z(6))**2
            ARMIN = RMINSP( ARMIN , D1 , D2 , D3 ,0.D0)
            ARMAX = MAX ( ARMAX , D1 , D2 , D3 )
            D1 = (X(1)-X(4))**2 + (Y(1)-Y(4))**2 + (Z(1)-Z(4))**2
            D2 = (X(2)-X(5))**2 + (Y(2)-Y(5))**2 + (Z(2)-Z(5))**2
            D3 = (X(3)-X(6))**2 + (Y(3)-Y(6))**2 + (Z(3)-Z(6))**2
            ARMIN = RMINSP( ARMIN , D1 , D2 , D3 ,0.D0)
            ARMAX = MAX ( ARMAX , D1 , D2 , D3 )
         ELSEIF ( TYPM(1:5) .EQ. 'TETRA' ) THEN
            DO 25 I = 1 , 4
               N = ZI(JCONE+I-1)
               X(I) = ZR(JVALE+3*(N-1)  )
               Y(I) = ZR(JVALE+3*(N-1)+1)
               Z(I) = ZR(JVALE+3*(N-1)+2)
 25         CONTINUE
            D1 = (X(2)-X(1))**2 + (Y(2)-Y(1))**2 + (Z(2)-Z(1))**2
            D2 = (X(3)-X(2))**2 + (Y(3)-Y(2))**2 + (Z(3)-Z(2))**2
            D3 = (X(1)-X(3))**2 + (Y(1)-Y(3))**2 + (Z(1)-Z(3))**2
            ARMIN = RMINSP( ARMIN , D1 , D2 , D3 ,0.D0)
            ARMAX = MAX ( ARMAX , D1 , D2 , D3 )
            D1 = (X(4)-X(1))**2 + (Y(4)-Y(1))**2 + (Z(4)-Z(1))**2
            D2 = (X(4)-X(2))**2 + (Y(4)-Y(2))**2 + (Z(4)-Z(2))**2
            D3 = (X(4)-X(3))**2 + (Y(4)-Y(3))**2 + (Z(4)-Z(3))**2
            ARMIN = RMINSP( ARMIN , D1 , D2 , D3 ,0.D0)
            ARMAX = MAX ( ARMAX , D1 , D2 , D3 )
         ELSEIF ( TYPM(1:4) .EQ. 'PYRA'  ) THEN
            DO 26 I = 1 , 5
               N = ZI(JCONE+I-1)
               X(I) = ZR(JVALE+3*(N-1)  )
               Y(I) = ZR(JVALE+3*(N-1)+1)
               Z(I) = ZR(JVALE+3*(N-1)+2)
 26         CONTINUE
            D1 = (X(2)-X(1))**2 + (Y(2)-Y(1))**2 + (Z(2)-Z(1))**2
            D2 = (X(3)-X(2))**2 + (Y(3)-Y(2))**2 + (Z(3)-Z(2))**2
            D3 = (X(4)-X(3))**2 + (Y(4)-Y(3))**2 + (Z(4)-Z(3))**2
            D4 = (X(1)-X(4))**2 + (Y(1)-Y(4))**2 + (Z(1)-Z(4))**2
            ARMIN = RMINSP( ARMIN , D1 , D2 , D3 , D4 )
            ARMAX = MAX ( ARMAX , D1 , D2 , D3 , D4 )
            D1 = (X(5)-X(1))**2 + (Y(5)-Y(1))**2 + (Z(5)-Z(1))**2
            D2 = (X(5)-X(2))**2 + (Y(5)-Y(2))**2 + (Z(5)-Z(2))**2
            D3 = (X(5)-X(3))**2 + (Y(5)-Y(3))**2 + (Z(5)-Z(3))**2
            D4 = (X(5)-X(4))**2 + (Y(5)-Y(4))**2 + (Z(5)-Z(4))**2
            ARMIN = RMINSP( ARMIN , D1 , D2 , D3 , D4 )
            ARMAX = MAX ( ARMAX , D1 , D2 , D3 , D4 )
         ENDIF
 20   CONTINUE
      IF ( ARMIN .EQ. R8GAEM() ) ARMIN = 0.D0
      VALE(8) = SQRT( ARMIN )
      VALE(9) = SQRT( ARMAX )
C
 100  CONTINUE
C
      NOMT19 = ' '
      CALL JEEXIN ( MA//'           .LTNT', IRET )
      IF ( IRET .NE. 0 ) THEN
         CALL LTNOTB ( MAILLA , 'CARA_GEOM' , NOMT19 )
         CALL DETRSD ( 'TABLE' , NOMT19 )
      ELSE
         CALL LTCRSD ( MAILLA , BAS1 )
      ENDIF
      CALL LTNOTB ( MAILLA , 'CARA_GEOM' , NOMT19 )
C
      CALL JEEXIN ( NOMT19//'.TBBA', IRET )
      IF ( IRET .NE. 0 )  CALL DETRSD ( 'TABLE' , NOMT19 )
C
      CALL TBCRSD ( NOMT19, BAS1 )
      CALL TBAJPA ( NOMT19, NBPART, NOPARA, TYPARA )
      CALL TBAJLI ( NOMT19, NBPART, NOPARA,
     +                              IBID, VALE, C16B, K8B, 0 )
C
      CALL JEDEMA ( )
      END
