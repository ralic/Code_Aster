      SUBROUTINE CAGRF1 ( CHAR, NOMA, Z0 )
      IMPLICIT   NONE
      REAL*8              Z0
      CHARACTER*8         CHAR, NOMA
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 18/11/2003   AUTEUR CIBHHLV L.VIVAN 
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
     +              ICOOR, JVAL, JABSC
      REAL*8        S, X1, Y1, Z1, X2, Y2, Z2, X12, Y12, Z12, V1(3)
      CHARACTER*8   K8B, GRMA, PREFIX
      CHARACTER*16  MOTCLF, MOTCLE, TYPMCL
      CHARACTER*24  LISNOM
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
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
      S = 0.D0
      DO 30 I = 1 , 3
         X1 = ZR(ICOOR+3*(ZI(JVNO       )-1)+I-1)
         X2 = ZR(ICOOR+3*(ZI(JVNO+NBNO-1)-1)+I-1)
         V1(I) = X2 - X1
         S = S + V1(I)**2
 30   CONTINUE
      S = SQRT( S )
      DO 32 I = 1 , 3
         V1(I) = V1(I) / S
 32   CONTINUE
C
      CALL WKVECT ( CHAR//'.CHME.GRFLU.VDIR', 'G V R', 3, JVAL )
      DO 34 I = 1 , 3
         ZR(JVAL+I-1) = V1(I)
 34   CONTINUE
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
      CALL JEDEMA()
      END
