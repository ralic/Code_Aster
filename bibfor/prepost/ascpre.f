      SUBROUTINE ASCPRE(RM, RC, ALPHA, LT, LGV, NBSEP, ISCP, ILONC,
     +                  ISLP, ICIRP, ILONP, DENC, DENL, NBEC, NBEL,
     +                  INDBG, INDBD, BG, BD, INDBI, INDBS, BI, BS, DNX,
     +                  DNY, INDSEX, INDSEY, IABSC1, IABSC2, IORDO1,
     +                  IORDO2, CORXG, CORXD, CORYI, CORYS, TAMPON, 
     +                  NZONEX, NZONEY,NZMAX,SYME)
C 
      IMPLICIT NONE  
      REAL*8  RM,RC,ALPHA,LT,LGV,CORXG(*),CORXD(*),TAMPON(*),
     +        BG(*),BD(*),BI(*),BS(*),DNX(2,*),DNY(2,*),CORYI(*),
     +        CORYS(*),DENC(*),DENL(*)
      INTEGER ISCP,ICIRP,ILONP,ISLP,INDBD(*),INDBG(*),INDBI(*),INDBS(*),
     +        INDSEX(*),INDSEY(*),IABSC1(*),IABSC2(*), NBSEP, ILONC,
     +        IORDO1(*),IORDO2(*),NBEC(*),NBEL(*),NZONEX, NZONEY, NZMAX
      CHARACTER*8 SYME
C
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 14/03/2002   AUTEUR F1BHHAJ J.ANGLES 
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
C TOLE  CRP_20   
C TOLE  CRP_21   
C     MACR_ASCOUF_MAIL
C
C PREPARATION DES DONNEES POUR LE MAILLAGE DE PLAQUE 
C  AVEC SOUS-EPAISSEURS :
C
C  - CALCUL TABLEAU TRIE DES ABSCISSES ET ORDONNEES DES CENTRES 
C  - CALCUL TABLEAU DES ZONES COUVERTES PAR LES SOUS-EPAISSEURS    
C
C-----------------DONNEES FOURNIES PAR L'UTILISATEUR--------------------
C
C     RM    = RAYON MOYEN DU COUDE
C     RC    = RAYON DE CINTRAGE DU COUDE
C     ALPHA = ANGLE DU COUDE
C     LT    = LONGUEUR DE L'EMBOUT DU COTE CHARGEMENT
C     LGV   = LONGUEUR DE L'EMBOUT DU COTE CONDITIONS AUX LIMITES
C     NBSEP  = NOMBRE DE SOUS-EPAISSEURS
C     ISCP = ABSC. CIRCONF. CENTRE SOUS-EPAISSEUR SUR LA PLAQUE
C     ILONC = TAILLE LONGI SUR LE COUDE DE LA SOUS-EPAISSEUR
C     ISLP =  ABSC. LONGIT. CENTRE SOUS-EPAISSEUR SUR LA PLAQUE
C     ICIRP = TAILLE CIRCONF. SUR LA PLAQUE DE LA SOUS-EPAISSEUR
C     ILONP = TAILLE LONGIT. SUR LA PLAQUE DE LA SOUS-EPAISSEUR
C     DENC = DENSITE CIRCONF. DE LA SOUS-EPAISSEUR
C     DENL = DENSITE LONGIT. DE LA SOUS-EPAISSEUR
C     NBEC = NOMBRE D'ELEMENTS CIRCONF. DE LA SOUS-EPAISSEUR
C     NBEL = NOMBRE D'ELEMENTS LONGI DE LA SOUS-EPAISSEUR
C     COORXG = ABSCISSE DU BORD GAUCHE DE LA SOUS-EPAISSEUR I
C     COORXD = ABSCISSE DU BORD DROIT DE LA SOUS-EPAISSEUR I
C     COORYI = ORDONNEE DU BORD INFERIEUR DE LA SOUS-EPAISSEUR I
C     COORYS = ORDONNEE DU BORD SUPERIEUR DE LA SOUS-EPAISSEUR I
C     IABSC1 = CORRESPONDANCE ABSCISSE CURVILIGNE CIRCONF. SOUS-EP. I
C     IORDO1 = CORRESPONDANCE ABSCISSE CURVILIGNE LONGIT. SOUS-EP. I
C     IABSC2 = CORRESPONDANCE ABSC. GAUCHE ET DROITE CIRCONF. SOUS-EP. I
C     IORDO2 = CORRESPONDANCE ORDO. INF ET SUP. LONGIT. SOUS-EP. I
C     
C----------------------DONNEES FOURNIES PAR ASTER-----------------------
C
C     INDBG = INDICATEUR BORD GAUCHE DE LA ZONE CIRCONF J
C     INDBD = INDICATEUR BORD DROIT DE LA ZONE CIRCONF J
C     BG = ABSCISSE DU BORD GAUCHE DE LA ZONE CIRCONF J
C     BD = ABSCISSE DU BORD DROIT DE LA ZONE CIRCONF J
C     BI = ORDONNEE DU BORD INFERIEUR DE LA ZONE LONGI J
C     BS = ORDONNEE DU BORD SUPERIEUR DE LA ZONE LONGI J
C     INDBI = INDICATEUR BORD INFERIEUR DE LA ZONE LONGI J
C     INDBS = INDICATEUR BORD SUPERIEUR DE LA ZONE LONGI J
C     INDSEX = NUMERO DE SOUS-EPAISSEUR CONTENU DANS LA ZONE CIRCONF J
C     INDSEY = NUMERO DE SOUS-EPAISSEUR CONTENU DANS LA ZONE LONGI J
C     DNX = DENSITE ET NOMBRE D'ELEMENTS CIRCONF. DE LA ZONE J
C     DNY = DENSITE ET NOMBRE D'ELEMENTS LONGIT. DE LA ZONE J
C     NZONEX = NOMBRE DE ZONES CIRCONFERENTIELLES
C     NZONEY = NOMBRE DE ZONES LONGITUDINALES 
C     NZMAX  = NOMBRE MAXIMAL DE ZONES 
C ----------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      REAL*8    PI,R8PI,MIND,MING,MINI,MINS,ALPHAR,RAFMIN,DERAFC,DERAFL,
     +          EPSI
      INTEGER   TYPG,TYPD,TYPI,TYPS,IFM,IUNIFI,I,J,NBGAU,NBDRO,ICE,INDC,
     +          NUMG,NUMD,NUMI,NUMS,NBSUP,NBINF,INDSE
C
      IFM = IUNIFI('MESSAGE')     
      PI = R8PI()
      ALPHAR = 2.D0*ALPHA*PI/360.D0
      DERAFC = 18.D0
      DERAFL = 5.D0
      EPSI = 1.D-3
C
      WRITE(IFM,*) 'RECHERCHE DES ZONES DE SOUS-EPAISSEURS DANS'
     &            // ' LE COUDE'      
C
      DO 5 I=1,NZMAX
         INDSEX(I) = 0
         INDSEY(I) = 0
 5    CONTINUE
C
C --- TRI DES DONNEES SOUS-EPAISSEURS EN CIRCONFERENTIEL
C
      CALL ASCTCI(RM,NBSEP,ISCP,ICIRP,IABSC1,IABSC2,TAMPON,
     +            CORXG,CORXD)
C
C --- CALCUL DES RECOUVREMENTS DE ZONES EN CIRCONFERENTIEL
C
      J = 0
      ICE = 1
      NZONEX = 0
      NBGAU = 0
      NBDRO = 0
      TYPG=0
      TYPD=0
10    CONTINUE
      J = J+1
C
C  DEFINITION DE LA ZONE COURANTE (BORNE GAUCHE,BORNE DROITE)
C
C  TYPG = TYPE DE LA BORNE 
C         0 : BORNE GAUCHE SOUS-EPAISSEUR
C         1 : BORNE DROITE SOUS-EPAISSEUR
C         2 : CENTRE SOUS-EPAISSEUR
C
      IF ((J).GT.2*NBSEP.AND.(ICE.LT.NBSEP)) THEN
C
C  CAS OU IL NE RESTE PLUS QUE DES CENTRES A CASER
C
        MING = MIND
        TYPG = TYPD
        NUMG = NUMD
        MIND = 2.D0*PI*RM+1
C
      ELSE IF (TYPD.EQ.2) THEN
C
C  CAS OU LA BORNE DROITE DE LA ZONE PRECEDENTE ETAIT UN CENTRE
C
        MING = MIND
        TYPG = TYPD
        NUMG = NUMD
        MIND = TAMPON(J)
        IF (MOD(IABSC2(J),2).NE.0) THEN
           TYPD = 0
           NUMD = IABSC1(IABSC2(J)/2+1)
        ELSE
           TYPD = 1
           NUMD = IABSC1(IABSC2(J)/2)
        END IF  
        J = J-1                      
      ELSE     
        MING = TAMPON(J)
        MIND = TAMPON(J+1)
        IF (J.GE.2*NBSEP) MIND = TAMPON(2*NBSEP) 
C        WRITE(IFM,*) 'BUG : ',MIND,J+1
        IF (MOD(IABSC2(J),2).NE.0) THEN
           TYPG = 0
           NUMG = IABSC1(IABSC2(J)/2+1)
        ELSE
           TYPG = 1
           NUMG = IABSC1(IABSC2(J)/2)
        END IF
        IF (MOD(IABSC2(J+1),2).NE.0) THEN
           TYPD = 0
           NUMD = IABSC1(IABSC2(J+1)/2+1)
        ELSE
           TYPD = 1 
           NUMD = IABSC1(IABSC2(J+1)/2)
        END IF        
      END IF
      IF (ABS(MING-MIND).LT.EPSI) GOTO 10
C      WRITE(IFM,*) 'BORNES ZONES INIT : ',MING,MIND,J
      IF ((J.GT.2*NBSEP).AND.(ICE.GE.NBSEP)) GOTO 30

20    CONTINUE
      I = ICE
C      WRITE(IFM,*) 'CENTRE ',I
      IF (I.LE.NBSEP) THEN
C
C  RECHERCHE DES CENTRES A INTERCALER
C
         INDC = IABSC1(I)
         IF (I.GT.1) THEN
C          LE CENTRE EST DEJA LE MEME QUE LE PRECEDENT 
           IF ( ABS(ZR(ISCP+INDC-1)-ZR(ISCP+IABSC1(I-1)-1))
     +          .LT.EPSI ) THEN
              ICE = ICE+1
              GOTO 20
           END IF  
         END IF   
C             
         IF (ZR(ISCP+INDC-1) .LT. MING) THEN
C
C  LE CENTRE EST LA NOUVELLE BORNE GAUCHE
C
            J = J-1
            MIND = MING
            TYPD = TYPG
            NUMD = NUMG
            MING = ZR(ISCP+INDC-1)
            TYPG = 2
            NUMG = INDC
            ICE = ICE+1
C            WRITE(IFM,*) ' CENTRE BORNE GAUCHE :',MING,ICE,J
C
         ELSE IF (ZR(ISCP+INDC-1) .LT. MIND) THEN
C
C  LE CENTRE EST LA NOUVELLE BORNE DROITE
C
            MIND = ZR(ISCP+INDC-1)
            TYPD = 2
            NUMD = INDC
            ICE = ICE + 1
C            WRITE(IFM,*) ' CENTRE BORNE DROITE :',MIND,ICE,J          
            GOTO 20
         END IF
      END IF
      NZONEX = NZONEX+1
C      WRITE(IFM,*) 'ZONE ',NZONEX,MING,MIND,J
C
C  CODES D'INTERVALLES DE ZONES : 
C
C       0 0  = ZONE SOUS-EPAISSEUR
C       0 1  = SOUS-EPAISSEUR A DROITE DE LA ZONE
C       1 0  = SOUS-EPAISSEUR A GAUCHE DE LA ZONE
C       1 1  = SOUS EPAISSEUR A DROITE ET A GAUCHE DE LA ZONE
C
C  CAS OU LA PREMIERE ZONE NE COMMENCE PAS AU BORD DE LA PLAQUE
C
      IF ((MING.GT.0.D0) .AND. (NZONEX.EQ.1)) THEN
C
          BG(NZONEX) = 0.D0
          BD(NZONEX) = MING
          IF (TYPG.EQ.0) THEN
             INDBG(NZONEX) = 0
             INDBD(NZONEX) = 1
             DNX(1,NZONEX) = DERAFC
             DNX(2,NZONEX) = 0
          ELSE IF (TYPG.EQ.1) THEN
             INDBG(NZONEX) = 0
             INDBD(NZONEX) = 0
             DNX(1,NZONEX) = DENC(NUMG)
             DNX(2,NZONEX) = 0  
             INDSEX(NZONEX) = NUMG
          ELSE IF (TYPG.EQ.2) THEN                                
             INDBG(NZONEX) = 0
             INDBD(NZONEX) = 0
             DNX(1,NZONEX) = DENC(NUMG)
             DNX(2,NZONEX) = 0  
             INDSEX(NZONEX) = NUMG
          END IF
          NZONEX = NZONEX+1
C
      END IF
C
      BG(NZONEX) = MING
      BD(NZONEX) = MIND
C
      IF (TYPG.EQ.0) THEN
C
C BORNE GAUCHE ZONE = BORNE GAUCHE SSEP
C
          NBGAU = NBGAU+1
          INDBG(NZONEX) = 0
          INDBD(NZONEX) = 0   
          IF (TYPD.EQ.0) THEN
C     BORNE DROITE ZONE = BORNE GAUCHE SSEP          
             DNX(1,NZONEX) = DENC(NUMG)
             DNX(2,NZONEX) = 0
             INDSEX(NZONEX) = NUMG             
          ELSE IF (TYPD.EQ.1) THEN 
C     BORNE DROITE ZONE = BORNE DROITE SSEP
             RAFMIN = MIN(DENC(NUMG),DENC(NUMD)) 
             INDSE = NUMD    
             IF (DENC(NUMG).EQ.RAFMIN) INDSE = NUMG  
             DNX(1,NZONEX) = RAFMIN
             DNX(2,NZONEX) = 0
             INDSEX(NZONEX) = INDSE
          ELSE IF (TYPD.EQ.2) THEN                                
C     BORNE DROITE ZONE = CENTRE SSEP
             RAFMIN = MIN(DENC(NUMG),DENC(NUMD)) 
             INDSE = NUMD    
             IF (DENC(NUMG).EQ.RAFMIN) INDSE = NUMG  
             DNX(1,NZONEX) = RAFMIN
             DNX(2,NZONEX) = 0
             INDSEX(NZONEX) = INDSE 
          END IF       
C
      ELSE IF (TYPG.EQ.1) THEN
C
C BORNE GAUCHE ZONE = BORNE DROITE SSEP
C 
          NBDRO = NBDRO+1  
          IF (TYPD.EQ.0) THEN
C     BORNE DROITE ZONE = BORNE GAUCHE SSEP
             IF (NBDRO.EQ.NBGAU) THEN                       
                INDBG(NZONEX) = 1
                INDBD(NZONEX) = 1  
                DNX(1,NZONEX) = DERAFC
                DNX(2,NZONEX) = 0
             ELSE
C          CAS TORDU : UNE SOUS-EP ENVELOPPE LE TOUT
                       INDBG(NZONEX) = 0
                INDBD(NZONEX) = 0  
                DNX(1,NZONEX) = DENC(NUMG)
                DNX(2,NZONEX) = 0
                INDSEX(NZONEX) = NUMG 
             END IF            
          ELSE IF (TYPD.EQ.1) THEN 
C     BORNE DROITE ZONE = BORNE DROITE SSEP
             INDBG(NZONEX) = 0
             INDBD(NZONEX) = 0
             DNX(1,NZONEX) = DENC(NUMD)
             DNX(2,NZONEX) = 0
             INDSEX(NZONEX) = NUMD
          ELSE IF (TYPD.EQ.2) THEN                                
C     BORNE DROITE ZONE = CENTRE SSEP
             INDBG(NZONEX) = 0
             INDBD(NZONEX) = 0
             DNX(1,NZONEX) = DENC(NUMD)
             DNX(2,NZONEX) = 0
             INDSEX(NZONEX) = NUMD  
          END IF 
C    
      ELSE IF (TYPG.EQ.2) THEN
C
C BORNE GAUCHE ZONE = CENTRE SSEP
C   
          INDBG(NZONEX) = 0
          INDBD(NZONEX) = 0 
          IF (TYPD.EQ.0) THEN
C     BORNE DROITE ZONE = BORNE GAUCHE SSEP          
             DNX(1,NZONEX) = DENC(NUMG)
             DNX(2,NZONEX) = 0
             INDSEX(NZONEX) = NUMG
          ELSE IF (TYPD.EQ.1) THEN 
C     BORNE DROITE ZONE = BORNE DROITE SSEP
             RAFMIN = MIN(DENC(NUMG),DENC(NUMD)) 
             INDSE = NUMD    
             IF (DENC(NUMG).EQ.RAFMIN) INDSE = NUMG  
             DNX(1,NZONEX) = RAFMIN
             DNX(2,NZONEX) = 0
             INDSEX(NZONEX) = INDSE
          ELSE IF (TYPD.EQ.2) THEN                                
C     BORNE DROITE ZONE = CENTRE SSEP
             RAFMIN = MIN(DENC(NUMG),DENC(NUMD)) 
             INDSE = NUMD    
             IF (DENC(NUMG).EQ.RAFMIN) INDSE = NUMG  
             DNX(1,NZONEX) = RAFMIN
             DNX(2,NZONEX) = 0
             INDSEX(NZONEX) = INDSE  
          END IF     
C
      END IF     
C
C      WRITE(IFM,*) 'SORTIE : ',J,ICE,TYPD
      IF (J.LE.(2*NBSEP-2).OR.(ICE.LE.NBSEP).OR.
     &     ((TYPD.EQ.2).AND.(J.LT.2*NBSEP)) ) GOTO 10     
C
C  CAS OU LA DERNIERE ZONE NE FINIT PAS AU BORD DE LA PLAQUE
C
 30    CONTINUE
       IF (MIND.LT.(2.D0*PI*RM)) THEN
C
          NZONEX = NZONEX+1 
          BG(NZONEX) = MIND
          BD(NZONEX) = 2.D0*PI*RM
          IF (TYPD.EQ.0) THEN
             INDBG(NZONEX) = 0
             INDBD(NZONEX) = 0
             DNX(1,NZONEX) = DENC(NUMD)
             DNX(2,NZONEX) = 0  
             INDSEX(NZONEX) = NUMD
          ELSE IF (TYPD.EQ.1) THEN
             INDBG(NZONEX) = 1
             INDBD(NZONEX) = 0
             DNX(1,NZONEX) = DERAFC
             DNX(2,NZONEX) = 0  
          ELSE IF (TYPD.EQ.2) THEN                                
             INDBG(NZONEX) = 0
             INDBD(NZONEX) = 0
             DNX(1,NZONEX) = DENC(NUMD)
             DNX(2,NZONEX) = 0  
             INDSEX(NZONEX) = NUMD
          END IF
C
      END IF       
C
C AU CAS OU 2*PI*RM CORRESPOND A UNE BORNE D'INTERVALLE DE SOUS-EP. OU
C UN CENTRE DE SOUS-EP.
C
      IF (ABS(BG(NZONEX)-BD(NZONEX)).LT.EPSI) NZONEX = NZONEX-1
C
      WRITE(IFM,*)   
      WRITE(IFM,*) 'ZONES APRES RECOUVREMENT ABSC. CURV. CIRCONF. :'
      WRITE(IFM,*) '---------------------------------------------'
      DO 25 J=1,NZONEX
        IF ((INDBG(J).EQ.0).AND.(INDBD(J).EQ.0)) THEN
          WRITE(IFM,101) 'ZONE NO',J,'<> BORNE GAUCHE = ',BG(J),
     &               ' / BORNE DROITE = ',BD(J),' * SOUS-EPAISSEUR'
        ELSE
          WRITE(IFM,101) 'ZONE NO',J,'<> BORNE GAUCHE = ',BG(J),
     &               ' / BORNE DROITE = ',BD(J)
        END IF       
 25   CONTINUE
C
C --- TRI DES DONNEES SOUS-EPAISSEURS EN AXIAL
C
      CALL ASCTLO(RC,ALPHA,LT,LGV,NBSEP,ILONC,ISLP,ILONP,IORDO1,
     &            IORDO2,TAMPON,CORYI,CORYS)
C
      IF (SYME(1:4).EQ.'DEMI') THEN
C
C --- CALCUL DES ZONES EN AXIAL
C
C     ZONE (0 , BORD INFERIEUR)
C
      NZONEY = 1
      BI(NZONEY) = 0.D0
      BS(NZONEY) = ZR(ISLP) - ZR(ILONP)/2.D0
      INDBI(NZONEY) = 0
      INDBS(NZONEY) = 1
      DNY(1,NZONEY) = DERAFL
      DNY(2,NZONEY) = 0
C  
C     ZONE (BORD INFERIEUR, CENTRE SOUS-EP)
C
      NZONEY = 2
      BI(NZONEY) = BS(NZONEY-1)
      BS(NZONEY) = ZR(ISLP) 
      INDBI(NZONEY) = 0
      INDBS(NZONEY) = 0
      DNY(1,NZONEY) = DENL(1)
      DNY(2,NZONEY) = 0
      INDSEY(NZONEY) = 1
C             
      ELSE      
C
C --- CALCUL DES RECOUVREMENTS DE ZONES EN AXIAL
C
      J = 0
      ICE = 1
      NZONEY = 0
      NBINF = 0
      NBSUP = 0
      TYPI=0
      TYPS=0
40    CONTINUE
      J = J+1
C
C  DEFINITION DE LA ZONE COURANTE (BORNE INFERIEURE,BORNE SUPERIEURE)
C
C
C  TYPI = TYPE DE LA BORNE 
C         0 : BORNE INFERIEURE SOUS-EPAISSEUR
C         1 : BORNE SUPERIEURE SOUS-EPAISSEUR
C         2 : CENTRE SOUS-EPAISSEUR
C
      IF (TYPS.EQ.2) THEN
C
C  CAS OU LA BORNE SUPERIEURE DE LA ZONE PRECEDENTE ETAIT UN CENTRE
C
        MINI = MINS
        TYPI = TYPS
        NUMI = NUMS
        MINS = TAMPON(J)
        IF (MOD(IORDO2(J),2).NE.0) THEN
           TYPS = 0
           NUMS = IORDO1(IORDO2(J)/2+1)
        ELSE
           TYPS = 1
           NUMS = IORDO1(IORDO2(J)/2)
        END IF  
        J = J-1       
      ELSE     
        MINI = TAMPON(J)
        MINS = TAMPON(J+1)
        IF (MOD(IORDO2(J),2).NE.0) THEN
           TYPI = 0
           NUMI = IORDO1(IORDO2(J)/2+1)
        ELSE
           TYPI = 1
           NUMI = IORDO1(IORDO2(J)/2)
        END IF
        IF (MOD(IORDO2(J+1),2).NE.0) THEN
           TYPS = 0
           NUMS = IORDO1(IORDO2(J+1)/2+1)
        ELSE
           TYPS = 1 
           NUMS = IORDO1(IORDO2(J+1)/2)
        END IF        
      END IF
      IF (ABS(MINI-MINS).LT.EPSI) GOTO 40
C      WRITE(IFM,*) 'BORNES ZONES INIT : ',MINI,MINS,J

50    CONTINUE
      I = ICE
      IF (I.LE.NBSEP) THEN
C
C  RECHERCHE DES CENTRES A INTERCALER
C
         INDC = IORDO1(I)
         IF (I.GT.1) THEN
C          LE CENTRE EST DEJA LE MEME QUE LE PRECEDENT 
           IF ( ABS(ZR(ISLP+INDC-1)-ZR(ISLP+IORDO1(I-1)-1))
     +          .LT.EPSI) THEN
              ICE = ICE+1
              GOTO 50
           END IF  
         END IF   
C             
         IF (ZR(ISLP+INDC-1) .LT. MINI) THEN
C
C  LE CENTRE EST LA NOUVELLE BORNE INFERIEURE
C
            J = J-1
            MINS = MINI
            TYPS = TYPI
            NUMS = NUMI
            MINI = ZR(ISLP+INDC-1)
            TYPI = 2
            NUMI = INDC
            ICE = ICE+1
C            WRITE(IFM,*) ' CENTRE BORNE INFERIEURE :',MINI,ICE,J
C
         ELSE IF (ZR(ISLP+INDC-1) .LT. MINS) THEN
C
C  LE CENTRE EST LA NOUVELLE BORNE SUPERIEURE
C
            MINS = ZR(ISLP+INDC-1)
            TYPS = 2
            NUMS = INDC
            ICE = ICE + 1
C            WRITE(IFM,*) ' CENTRE BORNE SUPERIEURE :',MINS,ICE,J 
            GOTO 50
         END IF
      END IF
      NZONEY = NZONEY+1
C      WRITE(IFM,*) 'ZONE ',NZONEY,MINI,MINS,J
C
C  CODES D'INTERVALLES DE ZONES : 
C
C       0 0  = ZONE SOUS-EPAISSEUR
C       0 1  = SOUS-EPAISSEUR A SUPERIEURE DE LA ZONE
C       1 0  = SOUS-EPAISSEUR A INFERIEURE DE LA ZONE
C       1 1  = SOUS EPAISSEUR A SUPERIEURE ET A INFERIEURE DE LA ZONE
C
C  CAS OU LA PREMIERE ZONE NE COMMENCE PAS AU BORD DE LA PLAQUE
C
      IF ((MINI.GT.0.D0) .AND. (NZONEY.EQ.1)) THEN
C
          BI(NZONEY) = 0.D0
          BS(NZONEY) = MINI
          IF (TYPI.EQ.0) THEN
             INDBI(NZONEY) = 0
             INDBS(NZONEY) = 1
             DNY(1,NZONEY) = DERAFL
             DNY(2,NZONEY) = 0
          ELSE IF (TYPI.EQ.1) THEN
             INDBI(NZONEY) = 0
             INDBS(NZONEY) = 0
             DNY(1,NZONEY) = DENL(NUMI)
             DNY(2,NZONEY) = 0  
             INDSEY(NZONEY) = NUMI
          ELSE IF (TYPI.EQ.2) THEN                                
             INDBI(NZONEY) = 0
             INDBS(NZONEY) = 0
             DNY(1,NZONEY) = DENL(NUMI)
             DNY(2,NZONEY) = 0  
             INDSEY(NZONEY) = NUMI
          END IF
          NZONEY = NZONEY+1
C
      END IF
C
      BI(NZONEY) = MINI
      BS(NZONEY) = MINS
C
      IF (TYPI.EQ.0) THEN
C
C BORNE INFERIEURE ZONE = BORNE INFERIEURE SSEP
C
          NBINF = NBINF+1
          INDBI(NZONEY) = 0
          INDBS(NZONEY) = 0   
          IF (TYPS.EQ.0) THEN
C     BORNE SUPERIEURE ZONE = BORNE INFERIEURE SSEP          
             DNY(1,NZONEY) = DENL(NUMI)
             DNY(2,NZONEY) = 0
             INDSEY(NZONEY) = NUMI             
          ELSE IF (TYPS.EQ.1) THEN 
C     BORNE SUPERIEURE ZONE = BORNE SUPERIEURE SSEP
             RAFMIN = MIN(DENL(NUMI),DENL(NUMS)) 
             INDSE = NUMS    
             IF (DENL(NUMI).EQ.RAFMIN) INDSE = NUMI  
             DNY(1,NZONEY) = RAFMIN
             DNY(2,NZONEY) = 0
             INDSEY(NZONEY) = INDSE
          ELSE IF (TYPS.EQ.2) THEN                                
C     BORNE SUPERIEURE ZONE = CENTRE SSEP
             RAFMIN = MIN(DENL(NUMI),DENL(NUMS)) 
             INDSE = NUMS    
             IF (DENL(NUMI).EQ.RAFMIN) INDSE = NUMI  
             DNY(1,NZONEY) = RAFMIN
             DNY(2,NZONEY) = 0
             INDSEY(NZONEY) = INDSE 
          END IF       
C
      ELSE IF (TYPI.EQ.1) THEN
C
C BORNE INFERIEURE ZONE = BORNE SUPERIEURE SSEP
C 
          NBSUP = NBSUP+1  
          IF (TYPS.EQ.0) THEN
C     BORNE SUPERIEURE ZONE = BORNE INFERIEURE SSEP
             IF (NBSUP.EQ.NBINF) THEN                       
                INDBI(NZONEY) = 1
                INDBS(NZONEY) = 1  
                DNY(1,NZONEY) = DERAFL
                DNY(2,NZONEY) = 0
             ELSE
C          CAS TORDU : UNE SOUS-EP ENVELOPPE LE TOUT
                       INDBI(NZONEY) = 0
                INDBS(NZONEY) = 0  
                DNY(1,NZONEY) = DENL(NUMI)
                DNY(2,NZONEY) = 0
                INDSEY(NZONEY) = NUMI 
             END IF            
          ELSE IF (TYPS.EQ.1) THEN 
C     BORNE SUPERIEURE ZONE = BORNE SUPERIEURE SSEP
             INDBI(NZONEY) = 0
             INDBS(NZONEY) = 0
             DNY(1,NZONEY) = DENL(NUMS)
             DNY(2,NZONEY) = 0
             INDSEY(NZONEY) = NUMS
          ELSE IF (TYPS.EQ.2) THEN                                
C     BORNE SUPERIEURE ZONE = CENTRE SSEP
             INDBI(NZONEY) = 0
             INDBS(NZONEY) = 0
             DNY(1,NZONEY) = DENL(NUMS)
             DNY(2,NZONEY) = 0
             INDSEY(NZONEY) = NUMS  
          END IF 
C    
      ELSE IF (TYPI.EQ.2) THEN
C
C BORNE INFERIEURE ZONE = CENTRE SSEP
C   
          INDBI(NZONEY) = 0
          INDBS(NZONEY) = 0 
          IF (TYPS.EQ.0) THEN
C     BORNE SUPERIEURE ZONE = BORNE INFERIEURE SSEP          
             DNY(1,NZONEY) = DENL(NUMI)
             DNY(2,NZONEY) = 0
             INDSEY(NZONEY) = NUMI
          ELSE IF (TYPS.EQ.1) THEN 
C     BORNE SUPERIEURE ZONE = BORNE SUPERIEURE SSEP
             RAFMIN = MIN(DENL(NUMI),DENL(NUMS)) 
             INDSE = NUMS    
             IF (DENL(NUMI).EQ.RAFMIN) INDSE = NUMI  
             DNY(1,NZONEY) = RAFMIN
             DNY(2,NZONEY) = 0
             INDSEY(NZONEY) = INDSE
          ELSE IF (TYPS.EQ.2) THEN                                
C     BORNE SUPERIEURE ZONE = CENTRE SSEP
             RAFMIN = MIN(DENL(NUMI),DENL(NUMS)) 
             INDSE = NUMS    
             IF (DENL(NUMI).EQ.RAFMIN) INDSE = NUMI  
             DNY(1,NZONEY) = RAFMIN
             DNY(2,NZONEY) = 0
             INDSEY(NZONEY) = INDSE  
          END IF     
C
      END IF     
C
      IF (J.LE.(2*NBSEP-2).OR.TYPS.EQ.2) GOTO 40     
C
C  CAS OU LA DERNIERE ZONE NE FINIT PAS AU BORD DE LA PLAQUE
C
      IF (MINS.LT.(ALPHAR*RC)) THEN
C
          NZONEY = NZONEY+1 
          BI(NZONEY) = MINS
          BS(NZONEY) = ALPHAR*RC
          IF (TYPS.EQ.0) THEN
             INDBI(NZONEY) = 0
             INDBS(NZONEY) = 0
             DNY(1,NZONEY) = DENL(NUMS)
             DNY(2,NZONEY) = 0  
             INDSEY(NZONEY) = NUMS
          ELSE IF (TYPS.EQ.1) THEN
             INDBI(NZONEY) = 1
             INDBS(NZONEY) = 0
             DNY(1,NZONEY) = DERAFL
             DNY(2,NZONEY) = 0 
          ELSE IF (TYPS.EQ.2) THEN                                
             INDBI(NZONEY) = 0
             INDBS(NZONEY) = 0
             DNY(1,NZONEY) = DENL(NUMS)
             DNY(2,NZONEY) = 0  
             INDSEY(NZONEY) = NUMS
          END IF
C
      END IF
C      
      END IF
C  
      WRITE(IFM,*)   
      WRITE(IFM,*) 'ZONES APRES RECOUVREMENT ABSC. CURV. LONGIT. :'
      WRITE(IFM,*) '--------------------------------------------'
      DO 35 J=1,NZONEY
        IF ((INDBI(J).EQ.0).AND.(INDBS(J).EQ.0)) THEN
          WRITE(IFM,101) 'ZONE NO',J,'<> BORNE INF. = ',BI(J),
     &               ' / BORNE SUP. = ',BS(J),' * SOUS-EPAISSEUR'
        ELSE
          WRITE(IFM,101) 'ZONE NO',J,'<> BORNE INF. = ',BI(J),
     &               ' / BORNE SUP. = ',BS(J)
        END IF       
 35   CONTINUE
C
      CALL ASCNBE(RM,RC,NBSEP,CORXG,CORXD,BG,BD,CORYI,CORYS,
     +            BI,BS,INDSEX,INDSEY,IABSC1,IORDO1,DNX,DNY,
     +            NZONEX,NZONEY,NBEC,NBEL)
C
101   FORMAT(A,I3,A,F8.2,A,F8.2,A)
C
      END
