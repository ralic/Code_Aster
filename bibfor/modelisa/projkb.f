      SUBROUTINE PROJKB(MAILLA,X3DCA,LNUMA,LICNX,
     &                  NUMAIL,NBCNX,CXMA,XYZMA,NORMAL,
     &                  ITRIA,XBAR,IPROJ,EXCENT)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C-----------------------------------------------------------------------
C  DESCRIPTION : TENTATIVE DE PROJECTION D'UN NOEUD CABLE SUR LES BORDS
C  -----------   DES MAILLES AUXQUELLES APPARTIENT LE NOEUD BETON LE
C                PLUS PROCHE
C                APPELANT : PROJCA
C
C  IN     : MAILLA : CHARACTER*8 , SCALAIRE
C                    NOM DU CONCEPT MAILLAGE ASSOCIE A L'ETUDE
C  IN     : X3DCA  : REAL*8 , VECTEUR DE DIMENSION 3
C                    COORDONNEES DU NOEUD CABLE CONSIDERE
C  IN     : LNUMA  : CHARACTER*19 , SCALAIRE
C                    NOM D'UN VECTEUR D'ENTIERS CONTENANT LES NUMEROS
C                    DES MAILLES AUXQUELLES APPARTIENT LE NOEUD BETON
C                    LE PLUS PROCHE DU NOEUD CABLE
C  IN     : LICNX  : CHARACTER*19 , SCALAIRE
C                    NOM D'UN VECTEUR D'ENTIERS CONTENANT LES RANGS DU
C                    NOEUD BETON LE PLUS PROCHE DANS LES TABLES DE
C                    CONNECTIVITE DES MAILLES AUXQUELLES IL APPARTIENT
C  OUT    : NUMAIL : INTEGER , SCALAIRE
C                    SI PROJECTION REUSSIE : NUMERO DE LA MAILLE
C                    A LAQUELLE APPARTIENT LE BORD SUR LEQUEL EST
C                    REALISEE LA PROJECTION
C  OUT    : NBCNX  : INTEGER , SCALAIRE
C                    SI PROJECTION REUSSIE : NOMBRE DE NOEUDS DE LA
C                    MAILLE A LAQUELLE APPARTIENT LE BORD SUR LEQUEL
C                    EST REALISEE LA PROJECTION
C  OUT    : CXMA   : INTEGER , VECTEUR DE DIMENSION AU PLUS NNOMAX
C                    SI PROJECTION REUSSIE : NUMEROS DES NOEUDS DE LA
C                    MAILLE A LAQUELLE APPARTIENT LE BORD SUR LEQUEL
C                    EST REALISEE LA PROJECTION
C                    (TABLE DE CONNECTIVITE)
C  OUT    : XYZMA  : REAL*8 , TABLEAU DE DIMENSIONS (3,NNOMAX)
C                    SI PROJECTION REUSSIE : TABLEAU DES COORDONNEES
C                    DES NOEUDS DE LA MAILLE A LAQUELLE APPARTIENT LE
C                    BORD SUR LEQUEL EST REALISEE LA PROJECTION
C  OUT    : NORMAL : REAL*8 , VECTEUR DE DIMENSION 3
C                    SI PROJECTION REUSSIE : COORDONNEES DANS LE REPERE
C                    GLOBAL DU VECTEUR NORMAL AU BORD SUR LEQUEL EST
C                    REALISEE LA PROJECTION
C  OUT    : ITRIA  : INTEGER , SCALAIRE
C                    SI PROJECTION REUSSIE : INDICATEUR DU SOUS-DOMAINE
C                    AUQUEL APPARTIENT LE POINT PROJETE :
C                    ITRIA = 1 : TRIANGLE 1-2-3
C                    ITRIA = 2 : TRIANGLE 3-4-1
C  OUT    : XBAR   : REAL*8 , VECTEUR DE DIMENSION 3
C                    SI PROJECTION REUSSIE : COORDONNEES BARYCENTRIQUES
C                    DU POINT PROJETE (BARYCENTRE DES SOMMETS DU
C                    TRIANGLE 1-2-3 OU 3-4-1)
C  OUT    : IPROJ  : INTEGER , SCALAIRE
C                    INDICE DE PROJECTION
C                    IPROJ = -1  PROJECTION NON REUSSIE
C                    IPROJ =  1X NUMERO DU BORD SUR LEQUEL EST REALISEE
C                                LA PROJECTION + 10
C                                (NUMERO D'APPARTENANCE SUR LA MAILLE)
C                    IPROJ =  2  LE POINT PROJETE COINCIDE AVEC UN DES
C                                NOEUDS EXTREMITES DU BORD
C  OUT    : EXCENT : REAL*8 , SCALAIRE
C                    SI PROJECTION REUSSIE : EXCENTRICITE DU NOEUD
C                    CABLE PAR RAPPORT AU BORD SUR LEQUEL EST REALISEE
C                    LA PROJECTION
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
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
      CHARACTER*32  JEXNUM
C     ----- FIN   COMMUNS NORMALISES  JEVEUX  --------------------------
C
C ARGUMENTS
C ---------
      CHARACTER*8   MAILLA
      CHARACTER*19  LNUMA, LICNX
      INTEGER       NUMAIL, NBCNX, CXMA(*), ITRIA, IPROJ
      REAL*8        X3DCA(*), XYZMA(3,*), NORMAL(*), XBAR(*), EXCENT
C
C VARIABLES LOCALES
C -----------------
      INTEGER       I1, I2, I3, ICNX, IMA, IMAIL, INOMA, JCOOR, JCXMA,
     &              JLNUMA, JLICNX, JSOMNO, NBMAOK, NBSOM, NOE, SOMN12,
     &              SOMN23
      REAL*8        D, DX, DY, DZ, EPSG, NRM2, X3DP(3), XBW(2)
      CHARACTER*1   K1B
      CHARACTER*24  CONXMA, COORNO
      LOGICAL       DEJAVU
C
      REAL*8        DNRM2, R8PREM
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      CALL MATFPE(-1)
C
      CALL JEMARQ()
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 1   ACCES AUX OBJETS UTILES - CREATION DES OBJETS DE TRAVAIL
C     INITIALISATIONS
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      CONXMA = MAILLA//'.CONNEX'
      COORNO = MAILLA//'.COORDO    .VALE'
      CALL JEVEUO(COORNO,'L',JCOOR)
C
      CALL JELIRA(LNUMA,'LONUTI',NBMAOK,K1B)
      CALL JEVEUO(LNUMA,'L',JLNUMA)
      CALL JEVEUO(LICNX,'L',JLICNX)
C
      CALL WKVECT('&&PROJKB.SOMNO_BORD','V V I',2*NBMAOK,JSOMNO)
C
      EPSG = 1.0D+08 * R8PREM()
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 2   TENTATIVE DE PROJECTION DU NOEUD CABLE CONSIDERE SUR LES BORDS DES
C     MAILLES AUXQUELLES APPARTIENT LE NOEUD BETON LE PLUS PROCHE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C.... BOUCLE SUR LES MAILLES AUXQUELLES APPARTIENT LE NOEUD BETON LE
C.... PLUS PROCHE, AFIN DE DETERMINER LES BORDS SUR LESQUELS ON TENTE
C.... LA PROJECTION DU NOEUD CABLE
C
      DO 10 IMAIL = 1, NBMAOK
C
         ICNX = ZI(JLICNX+IMAIL-1)
C....... SI LE NOEUD BETON LE PLUS PROCHE EST LE NOEUD CENTRAL D'UNE
C....... MAILLE QUAD9, PAS DE TENTATIVE DE PROJECTION SUR UN BORD
         IF ( ICNX.EQ.9 ) THEN
            IPROJ = -1
            GO TO 10
         ENDIF
         NUMAIL = ZI(JLNUMA+IMAIL-1)
C
C 2.1    RECUPERATION DES INFORMATIONS CARACTERISANT LA TOPOLOGIE
C ---    DE LA MAILLE
C
         CALL JELIRA(JEXNUM(CONXMA,NUMAIL),'LONMAX',NBCNX,K1B)
         CALL JEVEUO(JEXNUM(CONXMA,NUMAIL),'L',JCXMA)
         IF ( (NBCNX.EQ.3).OR.(NBCNX.EQ.6) ) THEN
            NBSOM = 3
         ELSE
            NBSOM = 4
         ENDIF
C
C 2.2    RECUPERATION DES NUMEROS ET DES COORDONNEES DES NOEUDS
C ---    DE LA MAILLE
C
         DO 20 INOMA = 1, NBCNX
            NOE = ZI(JCXMA+INOMA-1)
            CXMA(INOMA) = NOE
            XYZMA(1,INOMA) = ZR(JCOOR+3*(NOE-1)  )
            XYZMA(2,INOMA) = ZR(JCOOR+3*(NOE-1)+1)
            XYZMA(3,INOMA) = ZR(JCOOR+3*(NOE-1)+2)
  20     CONTINUE
C
C 2.3    SI LE NOEUD BETON LE PLUS PROCHE EST UN NOEUD MILIEU D'UN BORD
C ---    DE LA MAILLE, LA PROJECTION NE PEUT ETRE TENTEE QUE SUR LE BORD
C        DONT CE NOEUD EST LE MILIEU
C
         IF ( ICNX.GT.NBSOM ) THEN
            I1 = ICNX - NBSOM
            I2 = I1 + 1
            IF ( I2.GT.NBSOM ) I2 = 1
            SOMN12 = CXMA(I1) + CXMA(I2)
            ZI(JSOMNO+2*(IMAIL-1)  ) = SOMN12
            ZI(JSOMNO+2*(IMAIL-1)+1) = 0
C
C 2.3.1     ON VERIFIE QUE LA TENTATIVE DE PROJECTION SUR LE BORD N'A
C .....     PAS DEJA ETE EFFECTUEE (SUR UNE MAILLE LIMITROPHE)
C
            DEJAVU = .FALSE.
            IF ( IMAIL.GT.1 ) THEN
               DO 30 IMA = 1, IMAIL-1
                  IF ( (ZI(JSOMNO+2*(IMA-1)  ).EQ.SOMN12).OR.
     &                 (ZI(JSOMNO+2*(IMA-1)+1).EQ.SOMN12) )
     &               DEJAVU = .TRUE.
                  IF ( DEJAVU ) GO TO 10
  30           CONTINUE
            ENDIF
C
C 2.3.2     TENTATIVE DE PROJECTION SUR LE BORD
C .....
C
C 2.3.2.1   TENTATIVE DE PROJECTION
C .......
            CALL PROJSG(X3DCA(1),XYZMA(1,I1),XYZMA(1,I2),
     &                  NORMAL(1),X3DP(1),XBW(1),IPROJ,EXCENT)
C
C 2.3.2.2   REAJUSTEMENT DE IPROJ, AFFECTATION DE ITRIA ET XBAR
C .......   SI PROJECTION REUSSIE
C
            IF ( IPROJ.EQ.0 ) THEN
               IPROJ = I1 + 10
C............. TEST DE COINCIDENCE AVEC LE NOEUD MILIEU DU BORD (NOEUD
C............. BETON LE PLUS PROCHE)
               NRM2 = DNRM2(3,XYZMA(1,ICNX),1)
               IF ( NRM2.EQ.0.0D0 ) NRM2 = 1.0D0
               DX = XYZMA(1,ICNX) - X3DP(1)
               DY = XYZMA(2,ICNX) - X3DP(2)
               DZ = XYZMA(3,ICNX) - X3DP(3)
               D  = DBLE ( SQRT ( DX*DX + DY*DY + DZ*DZ ) )
               IF ( D/NRM2.LT.EPSG ) IPROJ = 2
C............. AFFECTATION DE ITRIA ET XBAR
               IF ( NBSOM.EQ.3 ) THEN
                  ITRIA = 1
                  IF ( I1.EQ.1 ) THEN
                     XBAR(1) = XBW(1)
                     XBAR(2) = XBW(2)
                     XBAR(3) = 0.0D0
                  ELSE IF ( I1.EQ.2 ) THEN
                     XBAR(1) = 0.0D0
                     XBAR(2) = XBW(1)
                     XBAR(3) = XBW(2)
                  ELSE
                     XBAR(1) = XBW(2)
                     XBAR(2) = 0.0D0
                     XBAR(3) = XBW(1)
                  ENDIF
               ELSE
                  IF ( I1.LT.3 ) THEN
                     ITRIA = 1
                  ELSE
                     ITRIA = 2
                  ENDIF
                  IF ( MOD(I1,2).EQ.1 ) THEN
                     XBAR(1) = XBW(1)
                     XBAR(2) = XBW(2)
                     XBAR(3) = 0.0D0
                  ELSE
                     XBAR(1) = 0.0D0
                     XBAR(2) = XBW(1)
                     XBAR(3) = XBW(2)
                  ENDIF
               ENDIF
            ENDIF
C
C 2.3.2.3   SORTIE SI PROJECTION REUSSIE
C .......
            IF ( IPROJ.GT.0 ) GO TO 9999
C
C 2.4    SI LE NOEUD BETON LE PLUS PROCHE EST UN NOEUD SOMMET DE LA
C ---    MAILLE, LA PROJECTION PEUT ETRE TENTEE SUR DEUX BORDS
C
         ELSE
            I1 = ICNX - 1
            IF ( I1.LT.1 ) I1 = NBSOM
            I2 = ICNX
            I3 = ICNX + 1
            IF ( I3.GT.NBSOM ) I3 = 1
            SOMN12 = CXMA(I1) + CXMA(I2)
            SOMN23 = CXMA(I2) + CXMA(I3)
            ZI(JSOMNO+2*(IMAIL-1)  ) = SOMN12
            ZI(JSOMNO+2*(IMAIL-1)+1) = SOMN23
C
C 2.4.1     ON VERIFIE QUE LA TENTATIVE DE PROJECTION SUR LE PREMIER
C .....     BORD N'A PAS DEJA ETE EFFECTUEE (SUR UNE MAILLE LIMITROPHE)
C
            DEJAVU = .FALSE.
            IF ( IMAIL.GT.1 ) THEN
               DO 40 IMA = 1, IMAIL-1
                  IF ( (ZI(JSOMNO+2*(IMA-1)  ).EQ.SOMN12).OR.
     &                 (ZI(JSOMNO+2*(IMA-1)+1).EQ.SOMN12) )
     &               DEJAVU = .TRUE.
                  IF ( DEJAVU ) GO TO 41
  40           CONTINUE
  41           CONTINUE
            ENDIF
C
C 2.4.2     TENTATIVE DE PROJECTION SUR LE PREMIER BORD
C .....
            IF ( .NOT.DEJAVU ) THEN
C
C 2.4.2.1      TENTATIVE DE PROJECTION
C .......
               CALL PROJSG(X3DCA(1),XYZMA(1,I1),XYZMA(1,I2),
     &                     NORMAL(1),X3DP(1),XBW(1),IPROJ,EXCENT)
C
C 2.4.2.2      REAJUSTEMENT DE IPROJ, AFFECTATION DE ITRIA ET XBAR
C .......      SI PROJECTION REUSSIE
C
               IF ( IPROJ.EQ.0 ) THEN
                  IPROJ = I1 + 10
C................ PAS DE TEST DE COINCIDENCE AVEC LE NOEUD MILIEU :
C................ SI TEL ETAIT LE CAS, ON SERAIT PASSE EN 2.3
C................ AFFECTATION DE ITRIA ET XBAR
                  IF ( NBSOM.EQ.3 ) THEN
                     ITRIA = 1
                     IF ( I1.EQ.1 ) THEN
                        XBAR(1) = XBW(1)
                        XBAR(2) = XBW(2)
                        XBAR(3) = 0.0D0
                     ELSE IF ( I1.EQ.2 ) THEN
                        XBAR(1) = 0.0D0
                        XBAR(2) = XBW(1)
                        XBAR(3) = XBW(2)
                     ELSE
                        XBAR(1) = XBW(2)
                        XBAR(2) = 0.0D0
                        XBAR(3) = XBW(1)
                     ENDIF
                  ELSE
                     IF ( I1.LT.3 ) THEN
                        ITRIA = 1
                     ELSE
                        ITRIA = 2
                     ENDIF
                     IF ( MOD(I1,2).EQ.1 ) THEN
                        XBAR(1) = XBW(1)
                        XBAR(2) = XBW(2)
                        XBAR(3) = 0.0D0
                     ELSE
                        XBAR(1) = 0.0D0
                        XBAR(2) = XBW(1)
                        XBAR(3) = XBW(2)
                     ENDIF
                  ENDIF
               ENDIF
C
C 2.4.2.3      SORTIE SI PROJECTION REUSSIE
C .......
               IF ( IPROJ.GT.0 ) GO TO 9999
C
            ENDIF
C
C 2.4.3     ON VERIFIE QUE LA TENTATIVE DE PROJECTION SUR LE DEUXIEME
C .....     BORD N'A PAS DEJA ETE EFFECTUEE (SUR UNE MAILLE LIMITROPHE)
C
            DEJAVU = .FALSE.
            IF ( IMAIL.GT.1 ) THEN
               DO 50 IMA = 1, IMAIL-1
                  IF ( (ZI(JSOMNO+2*(IMA-1)  ).EQ.SOMN23).OR.
     &                 (ZI(JSOMNO+2*(IMA-1)+1).EQ.SOMN23) )
     &               DEJAVU = .TRUE.
                  IF ( DEJAVU ) GO TO 10
  50           CONTINUE
            ENDIF
C
C 2.4.4     TENTATIVE DE PROJECTION SUR LE DEUXIEME BORD
C .....
C
C 2.4.4.1   TENTATIVE DE PROJECTION
C .......
            CALL PROJSG(X3DCA(1),XYZMA(1,I2),XYZMA(1,I3),
     &                  NORMAL(1),X3DP(1),XBW(1),IPROJ,EXCENT)
C
C 2.4.4.2   REAJUSTEMENT DE IPROJ, AFFECTATION DE ITRIA ET XBAR
C .......   SI PROJECTION REUSSIE
C
            IF ( IPROJ.EQ.0 ) THEN
               IPROJ = I2 + 10
C............. PAS DE TEST DE COINCIDENCE AVEC LE NOEUD MILIEU :
C............. SI TEL ETAIT LE CAS, ON SERAIT PASSE EN 2.3
C............. AFFECTATION DE ITRIA ET XBAR
               IF ( NBSOM.EQ.3 ) THEN
                  ITRIA = 1
                  IF ( I2.EQ.1 ) THEN
                     XBAR(1) = XBW(1)
                     XBAR(2) = XBW(2)
                     XBAR(3) = 0.0D0
                  ELSE IF ( I2.EQ.2 ) THEN
                     XBAR(1) = 0.0D0
                     XBAR(2) = XBW(1)
                     XBAR(3) = XBW(2)
                  ELSE
                     XBAR(1) = XBW(2)
                     XBAR(2) = 0.0D0
                     XBAR(3) = XBW(1)
                  ENDIF
               ELSE
                  IF ( I2.LT.3 ) THEN
                     ITRIA = 1
                  ELSE
                     ITRIA = 2
                  ENDIF
                  IF ( MOD(I2,2).EQ.1 ) THEN
                     XBAR(1) = XBW(1)
                     XBAR(2) = XBW(2)
                     XBAR(3) = 0.0D0
                  ELSE
                     XBAR(1) = 0.0D0
                     XBAR(2) = XBW(1)
                     XBAR(3) = XBW(2)
                  ENDIF
               ENDIF
            ENDIF
C
C 2.4.4.3   SORTIE SI PROJECTION REUSSIE
C .......
            IF ( IPROJ.GT.0 ) GO TO 9999
C
         ENDIF
C
  10  CONTINUE
C
9999  CONTINUE
      CALL JEDETC('V','&&PROJKB',1)
      CALL JEDEMA()
C
C --- FIN DE PROJKB.
      CALL MATFPE(1)
C
      END
