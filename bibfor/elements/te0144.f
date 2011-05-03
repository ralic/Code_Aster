      SUBROUTINE TE0144 ( OPTION , NOMTE )
      IMPLICIT  NONE
      CHARACTER*(*)       OPTION , NOMTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 02/05/2011   AUTEUR DELMAS J.DELMAS 
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
C
C     CALCUL
C       - DU VECTEUR ELEMENTAIRE EFFORT GENERALISE,
C       - DU VECTEUR ELEMENTAIRE CONTRAINTE
C     POUR LES ELEMENTS DE POUTRE D'EULER ET DE TIMOSHENKO.
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C        'SIEF_ELGA'
C IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
C        'MECA_POU_D_E' : POUTRE DROITE D'EULER       (SECTION VARIABLE)
C        'MECA_POU_D_T' : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
C        'MECA_POU_C_T' : POUTRE COURBE DE TIMOSHENKO(SECTION CONSTANTE)
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
      REAL*8      ZERO,DEUX
      PARAMETER  (ZERO = 0.D0,DEUX = 2.D0)

      INTEGER      NBRES,IPLOUF,NPG,NNO,NC,NNOC,NCC,JEFFO,LMATER,IRET
      INTEGER      IRET1,LSECT,ITYPE,LX,LRCOU,LORIEN,JDEPL,I,J
      PARAMETER    (NBRES=2)
      REAL*8       VALRES(NBRES)
      INTEGER CODRES(NBRES)
      CHARACTER*8  NOMRES(NBRES)

      REAL*8  UL(12), UG(12), PGL(3,3), KLC(12,12), KLV(78)
      REAL*8  FL(12), PGL1(3,3), PGL2(3,3), EPSITH
      REAL*8  TRIGOM, X, TEMP
      REAL*8  E,XNU,XL,RAD,ANGARC,ANGS2,ALONG

C
C     ------------------------------------------------------------------
      DATA NOMRES / 'E', 'NU'/
C     ------------------------------------------------------------------
      NNO  = 2
      NC   = 6
      NNOC = 1
      NCC  = 6
C     ------------------------------------------------------------------
C
      IF ( OPTION .EQ. 'SIEF_ELGA' ) THEN
         CALL JEVECH('PCONTRR','E',JEFFO)
      ELSE
C OPTION NON PROGRAMMEE
         CALL ASSERT(.FALSE.)
      ENDIF
C
C --- POINT DE GAUSS DE L'ELEMENT
      CALL ELREF4(' ','RIGI',IPLOUF,IPLOUF,IPLOUF,
     &            NPG,IPLOUF,IPLOUF,IPLOUF,IPLOUF)
      CALL ASSERT( (NPG.EQ.2).OR.(NPG.EQ.3) )
C
C --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
      CALL JEVECH ( 'PMATERC', 'L', LMATER )
C
C --- RECUPERATION DE LA TEMPERATURE :
      CALL VERIFM('RIGI',NPG,1,'+',ZI(LMATER),'ELAS',1,EPSITH,IRET)
      CALL MOYTEM('RIGI',NPG,1,'+',TEMP,IRET1)

      CALL RCVALB('RIGI',1,1,'+',ZI(LMATER),' ','ELAS',
     &             1,'TEMP',TEMP,2,NOMRES,VALRES,CODRES,1)
      E   = VALRES(1)
      XNU = VALRES(2)
C
C     --- CALCUL DE LA MATRICE DE RIGIDITE LOCALE ---
C
      CALL PORIGI ( NOMTE, E, XNU, KLV )
C
C     ---- MATRICE RIGIDITE LIGNE > MATRICE RIGIDITE CARRE
C
      CALL VECMA ( KLV, 78, KLC, 12 )
C
C     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
C
      CALL JEVECH ('PCAGNPO', 'L',LSECT)
      LSECT = LSECT-1
      ITYPE =  NINT(ZR(LSECT+23))
C
C     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
C
      CALL JEVECH ('PGEOMER', 'L',LX)
      LX = LX - 1
      XL = SQRT( (ZR(LX+4)-ZR(LX+1))**2
     &         + (ZR(LX+5)-ZR(LX+2))**2 + (ZR(LX+6)-ZR(LX+3))**2 )
      IF  ( ITYPE .EQ. 10 ) THEN
         CALL JEVECH ('PCAARPO', 'L',LRCOU)
         RAD    = ZR(LRCOU)
         ANGARC = ZR(LRCOU+1)
         X = XL / ( DEUX * RAD )
         ANGS2  = TRIGOM('ASIN', X )
         XL     = RAD * ANGS2 * DEUX
      ENDIF
C
C     --- MATRICE DE ROTATION PGL
C
      CALL JEVECH ('PCAORIE', 'L',LORIEN)
      IF  ( ITYPE .EQ. 10 ) THEN
         CALL MATRO2 ( ZR(LORIEN) , ANGARC , ANGS2 , PGL1 , PGL2 )
      ELSE
         CALL MATROT ( ZR(LORIEN) , PGL )
      ENDIF
C
      CALL JEVECH('PDEPLAR','L',JDEPL)
      DO 500 I = 1 , 12
         UG(I) = ZR(JDEPL+I-1)
 500  CONTINUE
C
C      --- VECTEUR DEPLACEMENT LOCAL  UL = PGL * UG
      IF  ( ITYPE .EQ. 10 ) THEN
         CALL UTPVGL ( NNOC, NCC, PGL1, UG , UL )
         CALL UTPVGL ( NNOC, NCC, PGL2, UG(7) , UL(7) )
      ELSE
         CALL UTPVGL ( NNO, NC, PGL, UG , UL )
      ENDIF
C
C     --- VECTEUR EFFORT       LOCAL  FL = KLC * UL
      CALL PMAVEC('ZERO',12,KLC,UL,FL)
C
C     --- TENIR COMPTE DES EFFORTS DUS A LA DILATATION ---
      IF ( EPSITH .NE. ZERO ) THEN
         DO 40 I = 1 , 12
            UG(I) = ZERO
 40      CONTINUE
C
         IF ( ITYPE .NE. 10 ) THEN
            UG(1) = -EPSITH * XL
            UG(7) = -UG(1)
         ELSE
            ALONG = 2.D0 * RAD * EPSITH * SIN(ANGS2)
            UG(1) = -ALONG * COS(ANGS2)
            UG(2) =  ALONG * SIN(ANGS2)
            UG(7) = -UG(1)
            UG(8) =  UG(2)
         ENDIF
C
C              --- CALCUL DES FORCES INDUITES ---
         DO 20 I = 1 , 6
            DO 22 J = 1 , 6
               FL(I)   = FL(I)   - KLC(I,J)     * UG(J)
               FL(I+6) = FL(I+6) - KLC(I+6,J+6) * UG(J+6)
 22         CONTINUE
 20      CONTINUE
      ENDIF
C
C --- ARCHIVAGE ---
      IF ( NPG .EQ. 2 ) THEN
         DO 702 I = 1 , 6
            ZR(JEFFO+I-1)   = -FL(I)
            ZR(JEFFO+I+6-1) =  FL(I+6)
702      CONTINUE
      ELSE
C        DANS LE CAS 3 POINTS DE GAUSS
C        IL NE FAUT PAS METTRE 0 SUR UN DES POINTS DE GAUSS
C        CF DOC ASTER POUR LA NUMEROTATION OU ELREGA
C              NOEUD        N1       N2       N3
C              POSITION   (-0.7777 , 0.0000 , 0.7777)
C        C'EST LINEAIRE : (N2) = ( (N1) + (N3) )/2
         DO 705 I = 1 , 6
            ZR(JEFFO+I-1)    =  -FL(I)
            ZR(JEFFO+I+6-1)  = (-FL(I) + FL(I+6))*0.5D0
            ZR(JEFFO+I+12-1) =   FL(I+6)
705      CONTINUE
      ENDIF
C
      END
