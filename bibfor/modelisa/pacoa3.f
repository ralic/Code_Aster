      SUBROUTINE PACOA3(NOEUD1,NOEUD2,LONLI1,LONLI2,DMIN0,NOMAZ,
     +                  LISO1Z,LISO2Z,LONLIS)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           NOEUD1(*), NOEUD2(*),LONLI1,LONLI2,LONLIS
      CHARACTER*(*) NOMAZ, LISO1Z, LISO2Z
      REAL*8        DMIN0
C---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 07/12/1999   AUTEUR CIBHHBC B.CIREE 
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
C---------------------------------------------------------------------
C     BUT: TRIER 2 LISTES DE NOEUDS LISI1Z ET LISI2Z DE MANIERE A
C     METTRE EN VIS A VIS LES NOEUDS DES 2 LISTES. MAIS L'UNE DES 2
C     LISTES PEUT ETRE PLUS PETITE (CAS DES MAILLES POI1). ON CHERCHE
C     DONC LES NOEUDS DE LA PLUS GRANDE LISTE IMAGES DES NOEUDS DE LA
C     PLUS PETITE LISTE (INJECTION) DONT LA DISTANCE EST INFERIEURE A
C     DMIN.
C     LES LISTES TRIEES OBTENUES A PARTIR DE LISI1Z ET LISI2Z
C     SONT RESPECTIVEMENT LISO1Z ET LISO2Z, LA CORRESPONDANCE
C     ENTRE LES NOEUDS DES 2 LISTES EST ASSUREE DE LA MANIERE
C     SUIVANTE :
C          POUR I =1, LONLIS < MIN(LONLI1,LONLI2)
C          LISO1Z(I) EST EN VIS-AVIS AVEC LISO2Z(I)
C
C     LES LISTES LISI1Z, LISI2Z, LISO1Z ET LISO2Z CONTIENNENT
C     LES NOMS DES NOEUDS (CE SONT DES LISTES DE K8).
C
C---------------------------------------------------------------------
C ARGUMENTS D'ENTREE:
C IN   NOEUD1     I   : 1ERE LISTE DES NUMEROS DE NOEUDS
C IN   NOEUD2     I   : 2EME LISTE DES NUMEROS DE NOEUDS
C IN   LONLI1     I   : LONGUEUR DE LA LISTE LISIZ1
C IN   LONLI2     I   : LONGUEUR DE LA LISTE LISIZ2
C IN   DMIN0      R8  : DISTANCE MAXIMAL DE VIS A VIS DES NOEUDS
C IN   NOMAZ      K8  : NOM DU MAILLAGE
C OUT  LISO1Z     K24 : NOM DE LA 1ERE LISTE TRIEE
C OUT  LISO2Z     K24 : NOM DE LA 2EME LISTE TRIEE
C OUT  LONLIS     I   : LONGUEUR COMMUNE DE LISO1Z ET LISO2Z
C
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32      JEXNOM, JEXNUM
C     ------- FIN COMMUNS NORMALISES  JEVEUX  --------------------------
      REAL*8        X1(3), X2(3), D, DMIN, PADIST
      CHARACTER*8   NOMA
      CHARACTER*8   NOMNO1, NOMNO2, NOMNO3, NOMO1, NOMO2
      CHARACTER*24  LISIN1, LISIN2, LISOU1, LISOU2, NOMNOE
      INTEGER IRET,IDLOU1,IDLOU2,INO1, IAGEOM, LONLIM
      INTEGER LONMAX,IDLINV,I1,NUNO1,J2,I2,INO2,NUNO2,J1
C
      CALL JEMARQ()
      LISOU1 = LISO1Z
      LISOU2 = LISO2Z
      LONLIM = MIN(LONLI1, LONLI2)
      NOMA   = NOMAZ
C
      NOMNOE = NOMA//'.NOMNOE         '
      CALL JEVEUO ( NOMA//'.COORDO    .VALE', 'L', IAGEOM )
C
      CALL JEEXIN(LISOU1,IRET)
      IF (IRET.NE.0) THEN
          CALL JEDETR(LISOU1)
      ENDIF
      CALL JEEXIN(LISOU1,IRET)
      IF (IRET.NE.0) THEN
          CALL JEDETR(LISOU1)
      ENDIF
C
C --- CREATION SUR LA VOLATILE DES LISTES DE K8 LISOU1 ET LISOU2
C --- DE LONGUEUR LONLIM
C
      CALL JEEXIN(LISOU1, IRET)
      IF (IRET.NE.0) THEN
          CALL JEDETR(LISOU1)
      ENDIF
      CALL JEEXIN(LISOU2, IRET)
      IF (IRET.NE.0) THEN
          CALL JEDETR(LISOU2)
      ENDIF
      CALL WKVECT(LISOU1,'V V I',LONLIM,IDLOU1)
      CALL WKVECT(LISOU2,'V V I',LONLIM,IDLOU2)
C
C --- VECTEURS DE TRAVAIL
C
      LONMAX = MAX(LONLI1,LONLI2)
      CALL WKVECT('&&PACOAP.LISINV','V V I',LONMAX,IDLINV)
C
C --- CONSTITUTION DE LA PREMIERE CORRESPONDANCE ENTRE LES LISTES
C --- DE NOEUDS LISIN1 ET LISIN2 ENTRE NO1 DONNE ET NO2 SELON LE
C --- CRITERE : NO2 = NO DANS LISIN2 / D(NO1,NO2) = MIN D(NO1,NO)
C
      LONLIS = 0
      IF (LONLI1.LE.LONLI2) THEN
         DO 10 I1 = 1, LONLI1
           NUNO1  = NOEUD1(I1)
           X1(1)  = ZR(IAGEOM-1+3*(NUNO1-1)+1)
           X1(2)  = ZR(IAGEOM-1+3*(NUNO1-1)+2)
           X1(3)  = ZR(IAGEOM-1+3*(NUNO1-1)+3)
           DMIN = DMIN0
           J2 = 0
           DO 20 I2 = 1, LONLI2
             INO2  = NOEUD2(I2)
             X2(1) = ZR(IAGEOM-1+3*(INO2-1)+1)
             X2(2) = ZR(IAGEOM-1+3*(INO2-1)+2)
             X2(3) = ZR(IAGEOM-1+3*(INO2-1)+3)
             D = PADIST( 3, X1, X2 )
             IF (D.LT.DMIN) THEN
               DMIN   = D
               NUNO2  = INO2
               J2     = I2
             ENDIF
20         CONTINUE
C   
           IF (J2.EQ.0) THEN
               NUNO2  = 0
               J2     = 0
           ENDIF
C   
           IF (J2.GT.0) THEN
              IF (ZI(IDLINV+J2-1).EQ.0) THEN
                  LONLIS = LONLIS + 1
                  ZI(IDLOU1+LONLIS-1) = NUNO1
                  ZI(IDLOU2+LONLIS-1) = NUNO2
                  ZI(IDLINV+J2-1) = NUNO1
              ELSE
                  CALL JENUNO ( JEXNUM(NOMNOE,NUNO1), NOMNO1)
                  CALL JENUNO ( JEXNUM(NOMNOE,NUNO2), NOMNO2)
                  CALL JENUNO ( JEXNUM(NOMNOE,ZI(IDLINV+J2-1)), NOMNO3)
                  CALL UTDEBM('F','PACOA2','CONFLIT DANS LES '//
     +                     'VIS_A_VIS DES NOEUDS')
                  CALL UTIMPK('L','LE NOEUD ',1,NOMNO2)
                  CALL UTIMPK('S','EST LE VIS-A-VIS DES NOEUDS ',1,
     +                         NOMNO1)
                  CALL UTIMPK('S','ET ',1,NOMNO3)
                  CALL UTFINM()
              ENDIF
           ENDIF
10       CONTINUE
      ELSE
         DO 30 I2 = 1, LONLI2
           NUNO2  = NOEUD2(I2)
           X2(1)  = ZR(IAGEOM-1+3*(NUNO2-1)+1)
           X2(2)  = ZR(IAGEOM-1+3*(NUNO2-1)+2)
           X2(3)  = ZR(IAGEOM-1+3*(NUNO2-1)+3)
           DMIN = DMIN0
           J1 = 0
           DO 40 I1 = 1, LONLI1
             INO1  = NOEUD1(I1)
             X1(1) = ZR(IAGEOM-1+3*(INO1-1)+1)
             X1(2) = ZR(IAGEOM-1+3*(INO1-1)+2)
             X1(3) = ZR(IAGEOM-1+3*(INO1-1)+3)
             D = PADIST( 3, X1, X2 )
             IF (D.LT.DMIN) THEN
               DMIN   = D
               NUNO1  = INO1
               J1     = I1
             ENDIF
40         CONTINUE
C   
           IF (J1.EQ.0) THEN
               NUNO1  = 0
               J1     = 0
           ENDIF
C   
           IF (J1.GT.0) THEN
              IF (ZI(IDLINV+J1-1).EQ.0) THEN
                  LONLIS = LONLIS + 1
                  ZI(IDLOU1+LONLIS-1) = NUNO1
                  ZI(IDLOU2+LONLIS-1) = NUNO2
                  ZI(IDLINV+J1-1) = NUNO2
              ELSE
                  CALL JENUNO ( JEXNUM(NOMNOE,NUNO1), NOMNO1)
                  CALL JENUNO ( JEXNUM(NOMNOE,NUNO2), NOMNO2)
                  CALL JENUNO ( JEXNUM(NOMNOE,ZI(IDLINV+J1-1)), NOMNO3)
                  CALL UTDEBM('F','PACOAP','CONFLIT DANS LES '//
     +                    'VIS_A_VIS DES NOEUDS')
                  CALL UTIMPK('L','LE NOEUD ',1,NOMNO2)
                  CALL UTIMPK('S','EST LE VIS-A-VIS DES NOEUDS ',1,
     +                        NOMNO1)
                  CALL UTIMPK('S','ET ',1,NOMNO3)
                  CALL UTFINM()
              ENDIF
           ENDIF
30       CONTINUE
      ENDIF
C
      CALL JEDETR ('&&PACOAP.LISINV')
C
      CALL JEDEMA()
      END
