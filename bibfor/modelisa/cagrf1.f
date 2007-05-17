      SUBROUTINE CAGRF1 ( CHAR, NOMA, Z0, CDG, Z22, DIRGC, 
     +                    VGRAP, FFGR, NGR )
      IMPLICIT   NONE
      INTEGER             NGR
      REAL*8              Z0, CDG(3), Z22, DIRGC(3), VGRAP(*)
      CHARACTER*8         CHAR, NOMA, FFGR(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 16/05/2007   AUTEUR BOYERE E.BOYERE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C
C BUT : STOCKAGE DES DONNEES HYDRAULIQUES ET DONNEES GEOMETRIQUES
C       POUR LE CALCUL DES FORCES FLUIDES
C
C ARGUMENTS D'ENTREE:
C      CHAR   : NOM UTILISATEUR DU RESULTAT DE CHARGE
C      NOMA   : NOM DU MAILLAGE
C      Z0     : PRONFONDEUR D'ENFONCEMENT INITIALE DE LA GRAPPE 
C               DANS LE TUBE
C ----------------------------------------------------------------------
C     ----- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------
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
      CHARACTER*32       JEXNOM
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      INTEGER       N1, I, J, NBM, NBNO, JMAIL, IDNONO, JVMA, JVNO, 
     +              ICOOR, JVAL, JABSC, IAPL, INO, NIV, IFM
      INTEGER       IZONE, IPNOEU, IDNOEU, NG, NOEUDG, NM, INDIK8
      REAL*8        XM1, XM2, YM1, YM2, ZM1, ZM2, XM, YM, ZM, COTE
      REAL*8 VALR(4)
      REAL*8        S, X1, Y1, Z1, X2, Y2, Z2, X12, Y12, Z12, V1(3)
      REAL*8        PI, R8PI, MGRAPP, MTIGE, MARAIG, MCRAYO, DTMOY, 
     +              LTIGE, ROTIGE, VARAI, RORAI, DCRAY, LCRAY, ROCRAY,
     +              MGRAP2, ERR, LGR, LGR2
      CHARACTER*8   K8B, GRMA, PREFIX
      CHARACTER*16  MOTCLF, MOTCLE, TYPMCL
      CHARACTER*24  LISNOM
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      PI = R8PI()
      MOTCLF = 'GRAPPE_FLUIDE'
      MOTCLE = 'GROUP_MA'
      TYPMCL = 'GROUP_MA'
C
C --- RECUPERATION DU "GROUP_MA"
C
      CALL GETVEM (NOMA, 'GROUP_MA', MOTCLF, 'GROUP_MA',1,1,1, GRMA,N1)
C
      CALL JELIRA ( JEXNOM(NOMA//'.GROUPEMA',GRMA),'LONMAX',NBM,K8B)
      CALL JEVEUO ( JEXNOM(NOMA//'.GROUPEMA',GRMA),'L',JMAIL)
C
      PREFIX = '&&SSCGNO'
      CALL FONFIS ( PREFIX, NOMA, MOTCLF, 1, 1, MOTCLE, TYPMCL, 'V')
      LISNOM = PREFIX//'.FOND      .NOEU'
      CALL JELIRA ( LISNOM, 'LONMAX', NBNO, K8B )
      CALL JEVEUO ( LISNOM, 'L', IDNONO )
C
C --- ON STOCKE LES NUMEROS DE MAILLE
C
      CALL WKVECT ( CHAR//'.CHME.GRFLU.LIMA', 'G V I', NBM, JVMA )
C
      DO 10 I = 1 , NBM
         ZI(JVMA+I-1) = ZI(JMAIL+I-1)
 10   CONTINUE
C
C --- ON STOCKE LES NUMEROS DE NOEUD
C
      CALL WKVECT ( CHAR//'.CHME.GRFLU.LINO', 'G V I', NBNO, JVNO )
      DO 20 I = 1 , NBNO
         CALL JENONU ( JEXNOM(NOMA//'.NOMNOE',ZK8(IDNONO-1+I)),
     +                                                   ZI(JVNO+I-1))
 20   CONTINUE
C
      CALL JEDETR ( PREFIX//'.FOND      .NOEU' ) 
      CALL JEDETR ( PREFIX//'.FOND      .TYPE' ) 
C
      CALL JEVEUO ( NOMA//'.COORDO    .VALE', 'L', ICOOR )
C
C --- VECTEUR DIRECTEUR ELEMENTAIRE
C
      CALL WKVECT ( CHAR//'.CHME.GRFLU.VDIR', 'G V R', 3*NBNO+3, JVAL )
C
      DO 100 INO = 1, NBNO-1
         S = 0.D0
         DO 110 I = 1 , 3
            X1 = ZR(ICOOR+3*(ZI(JVNO-1+INO  )-1)+I-1)
            X2 = ZR(ICOOR+3*(ZI(JVNO-1+INO+1)-1)+I-1)
            V1(I) = X2 - X1
            S = S + V1(I)**2
 110     CONTINUE
         S = SQRT( S )
         DO 112 I = 1 , 3
            ZR(JVAL-1+3*(INO-1)+I) = V1(I) / S
 112     CONTINUE
 100  CONTINUE
      LGR = 0.D0
      DO 114 I = 1 , 3
         X1 = ZR(ICOOR+3*(ZI(JVNO-1+1   )-1)+I-1)
         X2 = ZR(ICOOR+3*(ZI(JVNO-1+NBNO)-1)+I-1)
         V1(I) = X2 - X1
         LGR = LGR + V1(I)**2
 114  CONTINUE
      LGR = SQRT( LGR )
      DO 116 I = 1 , 3
         ZR(JVAL-1+3*(NBNO-1)+I) = V1(I) / LGR
 116  CONTINUE
      ZR(JVAL-1+3*NBNO+1) = DIRGC(1)
      ZR(JVAL-1+3*NBNO+2) = DIRGC(2)
      ZR(JVAL-1+3*NBNO+3) = DIRGC(3)
C
C --- CALCUL DES ABSCISSES CURVILIGNES
C
      CALL WKVECT ( CHAR//'.CHME.GRFLU.ABSC', 'G V R', NBNO, JABSC )
C
      J = 1
      ZR(JABSC) = Z0
      DO 40 I = NBNO , 2, -1
         X1 = ZR(ICOOR+3*(ZI(JVNO+I-1)-1)+1-1)
         Y1 = ZR(ICOOR+3*(ZI(JVNO+I-1)-1)+2-1)
         Z1 = ZR(ICOOR+3*(ZI(JVNO+I-1)-1)+3-1)
         X2 = ZR(ICOOR+3*(ZI(JVNO+I-2)-1)+1-1)
         Y2 = ZR(ICOOR+3*(ZI(JVNO+I-2)-1)+2-1)
         Z2 = ZR(ICOOR+3*(ZI(JVNO+I-2)-1)+3-1)
         X12 = X2 - X1
         Y12 = Y2 - Y1
         Z12 = Z2 - Z1
         J = J + 1
         ZR(JABSC+J-1) = ZR(JABSC+J-2) -
     &                     SQRT(X12*X12 + Y12 *Y12 + Z12*Z12)
 40   CONTINUE
C
C --- RECHERCHE DU NOEUD LE PLUS PROCHE DU CDG
C
      CALL JEVEUO(CHAR//'.CHME.GRFLU.APPL','E',IAPL)
      IF (((ZI(IAPL-1+1)).EQ.2).OR.
     +    ((ZI(IAPL-1+2)).EQ.2).OR.
     +    ((ZI(IAPL-1+3)).EQ.2).OR.
     +    ((ZI(IAPL-1+4)).EQ.2)) THEN
         NG = NOEUDG(ICOOR,NBNO,JVNO,CDG(1),CDG(2),CDG(3))
         ZI(IAPL-1+6) = NG
      ENDIF
C
C
C --- RECHERCHE DU NOEUD LE PLUS PROCHE DU MILIEU DU 
C --- GUIDAGE CONTINU :
C     ---------------
      IZONE = 0
      DO 50 I = 1, NBNO
         COTE = ZR(JABSC-1+I)
         IF ( COTE.GE.Z22.AND. COTE.LE.0.0D0 )  IZONE = IZONE + 1
         IF ( IZONE.EQ.1)  IPNOEU = I
 50   CONTINUE
      IDNOEU = IPNOEU + IZONE
      XM1 = ZR(ICOOR+3*(ZI(JVNO+IPNOEU)-1)+1-1)
      YM1 = ZR(ICOOR+3*(ZI(JVNO+IPNOEU)-1)+2-1)
      ZM1 = ZR(ICOOR+3*(ZI(JVNO+IPNOEU)-1)+3-1)
      XM2 = ZR(ICOOR+3*(ZI(JVNO+IDNOEU)-1)+1-1)
      YM2 = ZR(ICOOR+3*(ZI(JVNO+IDNOEU)-1)+2-1)
      ZM2 = ZR(ICOOR+3*(ZI(JVNO+IDNOEU)-1)+3-1)
      XM = 0.5D0*(XM1+XM2)
      YM = 0.5D0*(YM1+YM2)
      ZM = 0.5D0*(ZM1+ZM2)
      NM = NOEUDG(ICOOR,NBNO,JVNO,XM,YM,ZM)
      ZI(IAPL-1+5) = NM
C
C ----------------------------------------------------------------------
C     VERIFICATION DE QUELQUES GRANDEURS
C ----------------------------------------------------------------------
C
      CALL INFNIV ( IFM , NIV )
C --- VERIFICATION QUE LA MASSE ET LES MASSES VOLUMIQUES DONNEES SONT
C            COHERENTES
C
      J = INDIK8 ( FFGR, 'M', 1, NGR )
      MGRAPP = VGRAP(J)
C
      J = INDIK8 ( FFGR, 'DTMOY', 1, NGR )
      DTMOY  = VGRAP(J)
      J = INDIK8 ( FFGR, 'LTIGE', 1, NGR )
      LTIGE  = VGRAP(J)
      J = INDIK8 ( FFGR, 'ROTIGE', 1, NGR )
      ROTIGE = VGRAP(J)
      MTIGE  = ROTIGE * ( LTIGE * PI * DTMOY**2 / 4 )
C
      J = INDIK8 ( FFGR, 'VARAI', 1, NGR )
      VARAI  = VGRAP(J)
      J = INDIK8 ( FFGR, 'RORAI', 1, NGR )
      RORAI  = VGRAP(J)
      MARAIG = RORAI * VARAI
C
      J = INDIK8 ( FFGR, 'DCRAY', 1, NGR )
      DCRAY  = VGRAP(J)
      J = INDIK8 ( FFGR, 'LCRAY', 1, NGR )
      LCRAY  = VGRAP(J)
      J = INDIK8 ( FFGR, 'ROCRAY', 1, NGR )
      ROCRAY = VGRAP(J)
      MCRAYO = ROCRAY * ( 24 * LCRAY * PI * DCRAY**2 / 4 )
C
      MGRAP2 = MTIGE + MARAIG + MCRAYO
      ERR = ( MGRAPP - MGRAP2 ) / MGRAPP
          
       IF ( ABS(ERR).GT.1.D-03 ) THEN
         WRITE(IFM,*) '---------------ATTENTION: --------------------'
         VALR (1) = MTIGE
         VALR (2) = MARAIG
         VALR (3) = MCRAYO
         VALR (4) = MGRAP2
        CALL U2MESG('A', 'MODELISA8_41',0,' ',0,0,4,VALR)
         WRITE(IFM,*) '---------------------------------------------'
       ENDIF
C
      LGR2 = LTIGE + LCRAY
      ERR = ( LGR - LGR2 ) / LGR
      IF ( ABS(ERR).GT.1.D-03 ) THEN
         WRITE(IFM,*) '---------------ATTENTION: --------------------'
         VALR (1) = LGR
         VALR (2) = LGR2
         CALL U2MESG('F', 'MODELISA8_42',0,' ',0,0,2,VALR)
      ENDIF
C
      CALL JEDEMA()
      END
