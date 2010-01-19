      SUBROUTINE MDFCHO (NBMODE,DEPGEN,VITGEN,ACCGEN,FEXGEN,MASGEN,
     +                   PHICAR,PULSA2,AMOGEN,
     +                   NBCHOC,LOGCHO,DPLMOD,PARCHO,NOECHO,SAUCHO,
     +                   TEMPS,NOFDEP,NOFVIT,NOFACC,NBEXCI,PSIDEL,
     +                   NONMOT)
      IMPLICIT  REAL*8  (A-H,O-Z)
      INTEGER            LOGCHO(NBCHOC,*)
      REAL*8             DEPGEN(*),VITGEN(*),FEXGEN(*),
     +                   ACCGEN(*),PARCHO(NBCHOC,*),SAUCHO(NBCHOC,*),
     +                   MASGEN(*),PHICAR(*),PULSA2(*),AMOGEN(*)
      REAL*8             DPLMOD(NBCHOC,NBMODE,*)
      CHARACTER*8        NOECHO(NBCHOC,*)
      CHARACTER*8        NONMOT
      INTEGER            NBEXCI
      CHARACTER*8        NOFDEP(NBEXCI),NOFVIT(NBEXCI),NOFACC(NBEXCI)
      REAL*8             TEMPS,PSIDEL(NBCHOC,NBEXCI,*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/01/2010   AUTEUR MACOCCO K.MACOCCO 
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
C TOLE CRP_21
C TOLE CRP_20
C
C     CALCUL LES FORCES DE CHOC DE LA STRUCTURE
C     ------------------------------------------------------------------
C IN  : NBMODE : NOMBRE DE MODES
C IN  : DEPGEN : DEPLACEMENTS GENERALISES
C IN  : VITGEN : VITESSES GENERALISEES
C IN  : ACCGEN : ACCELERATIONS GENERALISEES
C VAR : FEXGEN : FORCES GENERALISEES
C VAR : MASGEN : MASSES GENERALISEES (VAR SI LAME FLUIDE)
C VAR : PULSA2 : CARRES DES PULSATIONS (VAR SI LAME FLUIDE)
C VAR : AMOGEN : AMORTISSEMENT GENERALISES
C IN  : NBCHOC : NOMBRE DE NOEUDS DE CHOC
C VAR : LOGCHO : INDICATEUR D'ADHERENCE ET DE FORCE FLUIDE ET DE
C                PRESENCE D UN DISPOSITIF ANTI SISMIQUE
C IN  : DPLMOD : TABLEAU DES DEPL MODAUX AUX NOEUDS DE CHOC
C IN  : PARCHO : PARAMETRES DES NOEUDS DE CHOC
C IN  : NOECHO : NOM DES NOEUDS DE CHOC ET TYPE D'OBSTACLE
C OUT : SAUCHO : SAUVEGARDE DES VALEURS DE CHOC
C
C IN  : TEMPS  : INSTANT DE CALCUL DES DEPL_IMPO
C IN  : NOFDEP : NOM DE LA FONCTION DEPL_IMPO
C IN  : NOFVIT : NOM DE LA FONCTION VITE_IMPO
C IN  : NOFACC : NOM DE LA FONCTION ACCE_IMPO
C IN  : NBEXCI : NOMBRE D'ACCELERO DIFFERENTS
C IN  : PSIDEL : TABLEAU DE VALEURS DE PSI*DELTA
C IN  : NONMOT : = OUI SI MULTI-APPUIS
C ----------------------------------------------------------------------
      REAL*8      KNORM, KTANG, DEPLOC(6), DEPGLO(6), FLOCAL(3),
     +            FGLOBA(3), VITGLO(6), VITLOC(6), ORIG(3), ORIGOB(3),
     +            ACCGLO(6), ACCLOC(6),
     +            FTANGE(2), VTANG(2), DDEPLO(3),
     +            OLDFT(2), OLDXL(3), OLDVT(2), POND,
     +            SIGNE(2), FDISPO
C     ------------------------------------------------------------------
      INTEGER       IEX, I, J
      CHARACTER*8   NOMPAR
      REAL*8        COEDEP(NBEXCI), COEVIT(NBEXCI), COEACC(NBEXCI)
C
      ZERO = 0.D0
      ORIG(1) = ZERO
      ORIG(2) = ZERO
      ORIG(3) = ZERO
C
      NOMPAR = 'INST'
      IF (NONMOT(1:3).NE.'NON') THEN
         DO 11 IEX=1,NBEXCI
           COEDEP(IEX) = ZERO
           COEVIT(IEX) = ZERO
           COEACC(IEX) = ZERO
           IF (NOFDEP(IEX).NE.' ')
     +      CALL FOINTE('F',NOFDEP(IEX),1,NOMPAR,TEMPS,COEDEP(IEX),IER)
           IF (NOFVIT(IEX).NE.' ')
     +      CALL FOINTE('F',NOFVIT(IEX),1,NOMPAR,TEMPS,COEVIT(IEX),IER)
           IF (NOFACC(IEX).NE.' ')
     +      CALL FOINTE('F',NOFACC(IEX),1,NOMPAR,TEMPS,COEACC(IEX),IER)
 11      CONTINUE
      ENDIF

C
      DO 10 I = 1,NBCHOC
C
         FN = ZERO
         FTANGE(1) = ZERO
         FTANGE(2) = ZERO
         FFLUID = ZERO
         FDISPO = ZERO
         VNORM  = ZERO
         ANORM  = ZERO
         VTANG(1) = ZERO
         VTANG(2) = ZERO
         DEFPLA = ZERO
         DO 12 J = 1,6
            DEPLOC(J) = ZERO
            VITLOC(J) = ZERO
 12      CONTINUE
C
         ORIGOB(1) = PARCHO(I,14)
         ORIGOB(2) = PARCHO(I,15)
         ORIGOB(3) = PARCHO(I,16)
         SINA = PARCHO(I,17)
         COSA = PARCHO(I,18)
         SINB = PARCHO(I,19)
         COSB = PARCHO(I,20)
         SING = PARCHO(I,21)
         COSG = PARCHO(I,22)
         SIGNE(1) = PARCHO(I,37)
         SIGNE(2) = PARCHO(I,38)
C
C        --- CONVERSION DDLS GENERALISES DDLS PHYSIQUES ---
C        POUR LE NOEUD 1
C
         IF (NONMOT(1:3).EQ.'NON') THEN
            CALL TOPHYS(I,0,DPLMOD,NBCHOC,NBMODE,DEPGEN,UX1,UY1,UZ1)
         ELSE
            CALL TOPHY3(I,0,DPLMOD,NBCHOC,NBMODE,DEPGEN,UX1,UY1,UZ1,
     +                  NBEXCI,PSIDEL,COEDEP)
         ENDIF
C        POSITION DU NOEUD 1 DANS LE REPERE GLOBAL
         DEPGLO(1) = UX1 + PARCHO(I,8)
         DEPGLO(2) = UY1 + PARCHO(I,9)
         DEPGLO(3) = UZ1 + PARCHO(I,10)
C        VITESSE DU NOEUD 1 DANS LE REPERE GLOBAL
         IF (NONMOT(1:3).EQ.'NON') THEN
            CALL TOPHYS(I,0,DPLMOD,NBCHOC,NBMODE,VITGEN,VX1,VY1,VZ1)
         ELSE
            CALL TOPHY3(I,0,DPLMOD,NBCHOC,NBMODE,VITGEN,VX1,VY1,VZ1,
     +                  NBEXCI,PSIDEL,COEVIT)
         ENDIF
         VITGLO(1) = VX1
         VITGLO(2) = VY1
         VITGLO(3) = VZ1
C        ACCELERATION DU NOEUD 1 DANS LE REPERE GLOBAL
         IF (NONMOT(1:3).EQ.'NON') THEN
            CALL TOPHYS(I,0,DPLMOD,NBCHOC,NBMODE,ACCGEN,AX1,AY1,AZ1)
         ELSE
            CALL TOPHY3(I,0,DPLMOD,NBCHOC,NBMODE,ACCGEN,AX1,AY1,AZ1,
     +                  NBEXCI,PSIDEL,COEACC)
         ENDIF
         ACCGLO(1) = AX1
         ACCGLO(2) = AY1
         ACCGLO(3) = AZ1
C
C        --- PASSAGE DANS LE REPERE LOCAL ---
C        POUR LE NOEUD 1
         CALL GLOLOC(DEPGLO,ORIGOB,SINA,COSA,SINB,COSB,SING,COSG,DEPLOC)
         CALL GLOLOC(VITGLO,ORIG  ,SINA,COSA,SINB,COSB,SING,COSG,VITLOC)
         CALL GLOLOC(ACCGLO,ORIG  ,SINA,COSA,SINB,COSB,SING,COSG,ACCLOC)
C        DEPLACEMENT DIFFERENTIEL = DEPLOC SI 1 NOEUD
         DDEPLO(1) = DEPLOC(1)
         DDEPLO(2) = DEPLOC(2)
         DDEPLO(3) = DEPLOC(3)      
C
         IF ( NOECHO(I,9)(1:2).EQ.'BI') THEN
C
C           MEME TRAVAIL POUR LE NOEUD 2
C
            IF (NONMOT(1:3).EQ.'NON') THEN
               CALL TOPHYS(I,3,DPLMOD,NBCHOC,NBMODE,DEPGEN,UX2,UY2,UZ2)
            ELSE
               CALL TOPHY3(I,3,DPLMOD,NBCHOC,NBMODE,DEPGEN,UX2,UY2,UZ2,
     +                     NBEXCI,PSIDEL,COEDEP)
            ENDIF
C           POSITION DU NOEUD 2 DANS LE REPERE GLOBAL
            DEPGLO(4) = UX2 + PARCHO(I,11)
            DEPGLO(5) = UY2 + PARCHO(I,12)
            DEPGLO(6) = UZ2 + PARCHO(I,13)
C           VITESSE DU NOEUD 2 DANS LE REPERE GLOBAL
            IF (NONMOT(1:3).EQ.'NON') THEN
               CALL TOPHYS(I,3,DPLMOD,NBCHOC,NBMODE,VITGEN,VX2,VY2,VZ2)
            ELSE
               CALL TOPHY3(I,3,DPLMOD,NBCHOC,NBMODE,VITGEN,VX2,VY2,VZ2,
     +                     NBEXCI,PSIDEL,COEVIT)
            ENDIF
            VITGLO(4) = VX2
            VITGLO(5) = VY2
            VITGLO(6) = VZ2
C           ACCELERATION DU NOEUD 2 DANS LE REPERE GLOBAL
            IF (NONMOT(1:3).EQ.'NON') THEN
               CALL TOPHYS(I,3,DPLMOD,NBCHOC,NBMODE,ACCGEN,AX2,AY2,AZ2)
            ELSE
               CALL TOPHY3(I,3,DPLMOD,NBCHOC,NBMODE,ACCGEN,AX2,AY2,AZ2,
     +                     NBEXCI,PSIDEL,COEACC)
            ENDIF
            ACCGLO(4) = AX2
            ACCGLO(5) = AY2
            ACCGLO(6) = AZ2
C           --- PASSAGE DANS LE REPERE LOCAL --- POUR LE NOEUD 2
            CALL GLOLOC(DEPGLO(4),ORIGOB,SINA,COSA,SINB,COSB,SING,COSG,
     +                  DEPLOC(4))
            CALL GLOLOC(VITGLO(4),ORIG  ,SINA,COSA,SINB,COSB,SING,COSG,
     +                  VITLOC(4))
            CALL GLOLOC(ACCGLO(4),ORIG  ,SINA,COSA,SINB,COSB,SING,COSG,
     +                  ACCLOC(4))
C           ACCELERATION DIFFERENTIELLE ENTRE NOEUD1 ET NOEUD2
            ACCLOC(1) = ACCLOC(1)-ACCLOC(4)
            ACCLOC(2) = ACCLOC(2)-ACCLOC(5)
            ACCLOC(3) = ACCLOC(3)-ACCLOC(6)
C           VITESSE DIFFERENTIELLE ENTRE NOEUD1 ET NOEUD2
            VITLOC(1) = VITLOC(1)-VITLOC(4)
            VITLOC(2) = VITLOC(2)-VITLOC(5)
            VITLOC(3) = VITLOC(3)-VITLOC(6)
C           DEPLACEMENT DIFFERENTIEL ENTRE NOEUD1 ET NOEUD2
            DDEPLO(1) = DEPLOC(1)-DEPLOC(4)
            DDEPLO(2) = DEPLOC(2)-DEPLOC(5)
            DDEPLO(3) = DEPLOC(3)-DEPLOC(6)
         ENDIF
C
         XJEU  = PARCHO(I,1)
         KNORM = PARCHO(I,2)
         CNORM = PARCHO(I,3)
         KTANG = PARCHO(I,4)
         CTANG = PARCHO(I,5)
         CFROTD = PARCHO(I,6)
         CFROTS = PARCHO(I,7)
         DIST1 = PARCHO(I,30)
         DIST2 = PARCHO(I,31)
         COEFA = PARCHO(I,32)
         COEFB = PARCHO(I,33)
         COEFC = PARCHO(I,34)
         COEFD = PARCHO(I,35)
         CL    = PARCHO(I,36)
         COEFK1 = PARCHO(I,39)
         COEFK2 = PARCHO(I,40)
         COEFPY = PARCHO(I,41)
         COEFCC = PARCHO(I,42)
         COEFAD = PARCHO(I,43)
         XMAX = PARCHO(I,44)
C        --- PARAMETRES DE FLAMBAGE ---
         FLIM   = PARCHO(I,50)
         FSEUIL = PARCHO(I,51)
         RIGIFL = PARCHO(I,52)
         DEFPLA = SAUCHO(I,14)
C
C        ---  CALCUL DE LA DISTANCE NORMALE ---
         CALL DISTNO( DEPLOC,SIGNE,NOECHO(I,9),XJEU,DIST1,DIST2,
     +                DNORM,COST,SINT )
C
         IF ( LOGCHO(I,2).EQ.1) THEN
C
C        --- CAS LAME FLUIDE ---
C
            IF (DNORM .GT. ZERO) THEN
C
C              --- CALCUL DE LA FORCE FLUIDE REPERE LOCAL ---
               CALL MDFFLU( DNORM,VNORM,ANORM,VITLOC,ACCLOC,COST,SINT,
     +                   COEFA, COEFB, COEFC, COEFD, FFLUID, FLOCAL )
C
C               --- MISE A JOUR COUCHE LIMITE ET INDIC CHOC SEC ---
C
                IF (LOGCHO(I,3).EQ.1) THEN
                   IF ( DNORM .GE. CL ) THEN
                        LOGCHO(I,3) = 0
                        PARCHO(I,36) = ZERO
                        CL = ZERO
                   ENDIF
                ELSE
                   CL = FFLUID/KNORM
                   IF ( DNORM .LE. CL) THEN
                      LOGCHO(I,3)=1
                      PARCHO(I,36)=CL
                   ENDIF
                ENDIF

                CALL PONDER(DNORM,CL,POND)
C  
C               --- PONDERATION ---
                FLOCAL(1) = FLOCAL(1) * POND
                FLOCAL(2) = FLOCAL(2) * POND
                FLOCAL(3) = FLOCAL(3) * POND
                FFLUID    = FFLUID    * POND
C
C
C               --- PASSAGE DE LA FORCE DANS LE REPERE GLOBAL ---
                CALL LOCGLO(FLOCAL,SINA,COSA,SINB,COSB,SING,COSG,FGLOBA)
C
C               --- PASSAGE A LA FORCE GENERALISEE NOEUD_1 ---
                CALL TOGENE(I,0,DPLMOD,NBCHOC,NBMODE,
     +                  FGLOBA(1),FGLOBA(2),FGLOBA(3),FEXGEN)
C               --- LA FORCE OPPOSEE SUR NOEUD_2 ---
                IF ( NOECHO(I,9)(1:2).EQ.'BI') THEN
                  CALL TOGENE(I,3,DPLMOD,NBCHOC,NBMODE,
     +                     -FGLOBA(1),-FGLOBA(2),-FGLOBA(3),FEXGEN)
                ENDIF
                CALL MDMASF(I,DNORM,MASGEN,NBMODE,PHICAR,FEXGEN,ACCGEN,
     +                      PULSA2,AMOGEN,COEFA*POND )
C
           ELSE
              FFLUID = ZERO
           ENDIF
C

         IF ( DNORM .LE. CL ) THEN
C
C           --- CALCUL DE LA FORCE NORMALE REPERE LOCAL ---
            CALL FNORM( DNORM-CL,VITLOC,KNORM,CNORM,COST,SINT,
     +                                           FN,FLOCAL,VNORM )
C

              CALL PONDER(DNORM,CL,POND)

C           --- PONDERATION ---
            FLOCAL(1) = FLOCAL(1)*(1.D0-POND)
            FLOCAL(2) = FLOCAL(2)*(1.D0-POND)
            FLOCAL(3) = FLOCAL(3)*(1.D0-POND)
            FN        =    FN    *(1.D0-POND)
C

            IF ((( CFROTS .NE. ZERO ).OR.( CFROTD .NE. ZERO )) 
     +             .AND. (DNORM .LE. ZERO )) THEN
               OLDFT(1) = PARCHO(I,26)
               OLDFT(2) = PARCHO(I,27)
               OLDXL(1) = PARCHO(I,23)
               OLDXL(2) = PARCHO(I,24)
               OLDXL(3) = PARCHO(I,25)
               OLDVT(1) = PARCHO(I,28)
               OLDVT(2) = PARCHO(I,29)
               CALL FTANG(FN,DDEPLO,VITLOC,CFROTD,CFROTS,KTANG,CTANG,
     +                    LOGCHO(I,1),OLDVT,OLDFT,OLDXL,
     +                    COST,SINT,FTANGE,FLOCAL,VTANG )
               PARCHO(I,26) = OLDFT(1)
               PARCHO(I,27) = OLDFT(2)
               PARCHO(I,23) = OLDXL(1)
               PARCHO(I,24) = OLDXL(2)
               PARCHO(I,25) = OLDXL(3)
               PARCHO(I,28) = OLDVT(1)
               PARCHO(I,29) = OLDVT(2)
            ENDIF
C
C           --- PASSAGE DE LA FORCE DANS LE REPERE GLOBAL ---
            CALL LOCGLO(FLOCAL,SINA,COSA,SINB,COSB,SING,COSG,FGLOBA)
C
C           --- PASSAGE A LA FORCE GENERALISEE NOEUD_1 ---
            CALL TOGENE(I,0,DPLMOD,NBCHOC,NBMODE,
     +                  FGLOBA(1),FGLOBA(2),FGLOBA(3),FEXGEN)
C           --- LA FORCE OPPOSEE SUR NOEUD_2 ---
            IF ( NOECHO(I,9)(1:2).EQ.'BI') THEN
               CALL TOGENE(I,3,DPLMOD,NBCHOC,NBMODE,
     +                     -FGLOBA(1),-FGLOBA(2),-FGLOBA(3),FEXGEN)
            ENDIF
C
         ELSE
            FN = ZERO
         ENDIF

         PARCHO(I,23) = DDEPLO(1)
         PARCHO(I,24) = DDEPLO(2)
         PARCHO(I,25) = DDEPLO(3)
         PARCHO(I,26) = ZERO
         PARCHO(I,27) = ZERO
         LOGCHO(I,1) = 0
C
C        DE FACON PROVISOIRE ON STOCKE FFLUID DANS FN POUR VISU
C
         FN = FFLUID +FN
C
         ELSEIF (LOGCHO(I,4).EQ.1) THEN
C
C        --- CAS DISPOSITIF ANTI SISMIQUE ----
C
C**           IF ( DNORM .LE. ZERO ) THEN
C
C             --- CALCUL DE LA FORCE NORMALE REPERE LOCAL
C                 DU AU DISPOSITIF ANTI SISMIQUE  ---
              CALL MDFDAS(DNORM,VNORM,VITLOC,COST,SINT,COEFK1,COEFK2,
     +                    COEFPY,COEFCC,COEFAD,XMAX,FDISPO,FLOCAL)
C 
C             --- PASSAGE DE LA FORCE DANS LE REPERE GLOBAL ---
              CALL LOCGLO(FLOCAL,SINA,COSA,SINB,COSB,SING,COSG,FGLOBA)
C
C             --- PASSAGE A LA FORCE GENERALISEE NOEUD_1 ---
              CALL TOGENE(I,0,DPLMOD,NBCHOC,NBMODE,
     +                    FGLOBA(1),FGLOBA(2),FGLOBA(3),FEXGEN)
C             --- LA FORCE OPPOSEE SUR NOEUD_2 ---
              IF (NOECHO(I,9)(1:2).EQ.'BI') THEN
                 CALL TOGENE(I,3,DPLMOD,NBCHOC,NBMODE,
     +                       -FGLOBA(1),-FGLOBA(2),-FGLOBA(3),FEXGEN)
              ENDIF
C**           ELSE
C**              FDISPO = ZERO
C**           ENDIF
           PARCHO(I,23) = DDEPLO(1)
           PARCHO(I,24) = DDEPLO(2)
           PARCHO(I,25) = DDEPLO(3)
           PARCHO(I,26) = ZERO
           PARCHO(I,27) = ZERO
C           LOGCHO(I,1) = 0
C
C        DE FACON PROVISOIRE ON STOCKE FDISPO DANS FN POUR VISU
C
         FN = FDISPO
C
        ELSEIF (LOGCHO(I,5).EQ.1) THEN
C
C        --- CAS DU FLAMBAGE ----
C
          IF ( DNORM .LE. ZERO ) THEN
C
C           --- CALCUL DE LA FORCE NORMALE REPERE LOCAL ---
            CALL MDFLAM(DNORM,VITLOC,KNORM,COST,SINT,FLIM,FSEUIL,
     +                  RIGIFL,DEFPLA,FN,FLOCAL,VNORM )
            IF (( CFROTS .NE. ZERO ).OR.( CFROTD .NE. ZERO )) THEN
               OLDFT(1) = PARCHO(I,26)
               OLDFT(2) = PARCHO(I,27)
               OLDXL(1) = PARCHO(I,23)
               OLDXL(2) = PARCHO(I,24)
               OLDXL(3) = PARCHO(I,25)
               OLDVT(1) = PARCHO(I,28)
               OLDVT(2) = PARCHO(I,29)
               CALL FTANG(FN,DDEPLO,VITLOC,CFROTD,CFROTS,KTANG,CTANG,
     +                    LOGCHO(I,1),OLDVT,OLDFT,OLDXL,
     +                    COST,SINT,FTANGE,FLOCAL,VTANG )
               PARCHO(I,26) = OLDFT(1)
               PARCHO(I,27) = OLDFT(2)
               PARCHO(I,23) = OLDXL(1)
               PARCHO(I,24) = OLDXL(2)
               PARCHO(I,25) = OLDXL(3)
               PARCHO(I,28) = OLDVT(1)
               PARCHO(I,29) = OLDVT(2)
            ENDIF
C
C           --- PASSAGE DE LA FORCE DANS LE REPERE GLOBAL ---
            CALL LOCGLO(FLOCAL,SINA,COSA,SINB,COSB,SING,COSG,FGLOBA)
C
C           --- PASSAGE A LA FORCE GENERALISEE NOEUD_1 ---
            CALL TOGENE(I,0,DPLMOD,NBCHOC,NBMODE,
     +                  FGLOBA(1),FGLOBA(2),FGLOBA(3),FEXGEN)
C           --- LA FORCE OPPOSEE SUR NOEUD_2 ---
            IF ( NOECHO(I,9)(1:2).EQ.'BI') THEN
               CALL TOGENE(I,3,DPLMOD,NBCHOC,NBMODE,
     +                     -FGLOBA(1),-FGLOBA(2),-FGLOBA(3),FEXGEN)
            ENDIF
          ELSE
            PARCHO(I,23) = DDEPLO(1)
            PARCHO(I,24) = DDEPLO(2)
            PARCHO(I,25) = DDEPLO(3)
            PARCHO(I,26) = ZERO
            PARCHO(I,27) = ZERO
            LOGCHO(I,1) = 0
            FTANGE(1) = ZERO
            FTANGE(2) = ZERO
            VTANG(1) = ZERO
            VTANG(2) = ZERO
          ENDIF
        ELSE
C
C        --- CAS DU CHOC SEC ----
C
         IF ( DNORM .LE. ZERO ) THEN
C
C           --- CALCUL DE LA FORCE NORMALE REPERE LOCAL ---
            CALL FNORM( DNORM,VITLOC,KNORM,CNORM,COST,SINT,
     +                                           FN,FLOCAL,VNORM )
            IF (( CFROTS .NE. ZERO ).OR.( CFROTD .NE. ZERO )) THEN
               OLDFT(1) = PARCHO(I,26)
               OLDFT(2) = PARCHO(I,27)
               OLDXL(1) = PARCHO(I,23)
               OLDXL(2) = PARCHO(I,24)
               OLDXL(3) = PARCHO(I,25)
               OLDVT(1) = PARCHO(I,28)
               OLDVT(2) = PARCHO(I,29)
               CALL FTANG(FN,DDEPLO,VITLOC,CFROTD,CFROTS,KTANG,CTANG,
     +                    LOGCHO(I,1),OLDVT,OLDFT,OLDXL,
     +                    COST,SINT,FTANGE,FLOCAL,VTANG )
               PARCHO(I,26) = OLDFT(1)
               PARCHO(I,27) = OLDFT(2)
               PARCHO(I,23) = OLDXL(1)
               PARCHO(I,24) = OLDXL(2)
               PARCHO(I,25) = OLDXL(3)
               PARCHO(I,28) = OLDVT(1)
               PARCHO(I,29) = OLDVT(2)
            ENDIF
C
C           --- PASSAGE DE LA FORCE DANS LE REPERE GLOBAL ---
            CALL LOCGLO(FLOCAL,SINA,COSA,SINB,COSB,SING,COSG,FGLOBA)
C
C           --- PASSAGE A LA FORCE GENERALISEE NOEUD_1 ---
            CALL TOGENE(I,0,DPLMOD,NBCHOC,NBMODE,
     +                  FGLOBA(1),FGLOBA(2),FGLOBA(3),FEXGEN)
C           --- LA FORCE OPPOSEE SUR NOEUD_2 ---
            IF ( NOECHO(I,9)(1:2).EQ.'BI') THEN
               CALL TOGENE(I,3,DPLMOD,NBCHOC,NBMODE,
     +                     -FGLOBA(1),-FGLOBA(2),-FGLOBA(3),FEXGEN)
            ENDIF
           ELSE
            PARCHO(I,23) = DDEPLO(1)
            PARCHO(I,24) = DDEPLO(2)
            PARCHO(I,25) = DDEPLO(3)
            PARCHO(I,26) = ZERO
            PARCHO(I,27) = ZERO
            LOGCHO(I,1) = 0
            FTANGE(1) = ZERO
            FTANGE(2) = ZERO
            VTANG(1) = ZERO
            VTANG(2) = ZERO
           ENDIF
         ENDIF

         SAUCHO(I,1) = FN
         SAUCHO(I,2) = FTANGE(1)
         SAUCHO(I,3) = FTANGE(2)
C        DEPLACEMENT LOCAL DU NOEUD NOEUD_1
         SAUCHO(I,4) = DEPLOC(1)
         SAUCHO(I,5) = DEPLOC(2)
         SAUCHO(I,6) = DEPLOC(3)
         SAUCHO(I,7) = VNORM
         SAUCHO(I,8) = VTANG(1)
         SAUCHO(I,9) = VTANG(2)
C        DEPLACEMENT LOCAL DU NOEUD NOEUD_2
         SAUCHO(I,10) = DEPLOC(4)
         SAUCHO(I,11) = DEPLOC(5)
         SAUCHO(I,12) = DEPLOC(6)
C        INDICATEUR ADHERENCE
         SAUCHO(I,13) = LOGCHO(I,1)
C        FLAMBAGE : ECRASEMENT CUMULE (VARIABLE INTERNE)
         SAUCHO(I,14) = DEFPLA
C
 10   CONTINUE
C
      END
