      SUBROUTINE ASCSDO ( UN, RM, RC, ALPHA, EP, LTCHAR, LTCLIM,
     +                    GEOM, SYME, NZONEX, NZONEY, INDBG, INDBD,
     +                    BG, BD, INDBI, INDBS, BI, BS, DNX, DNY,
     +                    NBSEP, SLP, SCP, CIRP, LONP, PROF, POSIT,
     +                    TYPE, SLC, BETC, LONC, NBEP, EVID, IPCL)
      IMPLICIT    NONE
      INTEGER     UN, NZONEX, NZONEY, INDBG(*), INDBD(*), INDBI(*), 
     +            INDBS(*) ,NBSEP, NBEP, IPCL(*)
      REAL*8      RM, RC, ALPHA, EP, LTCHAR, LTCLIM, BG(*), BD(*), 
     +            BI(*), BS(*), DNX(2,*), DNY(2,*), SLP(*), SCP(*),
     +            CIRP(*), LONP(*), PROF(*), SLC(*), LONC(*), BETC(*)
      CHARACTER*8 GEOM,SYME,POSIT(*),TYPE(*),EVID(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 29/01/2004   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
C TOLE  CRP_21
C     MACR_ASCOUF_MAIL
C
C     ECRIT DANS UN FICHIER  LES DONNEES GIBI DE LA PROCEDURE
C                                               "PLAQUE SOUS-EPAISSEURS"
C
C-----------------DONNEES FOURNIES PAR L'UTILISATEUR--------------------
C
C     UN    = UNITE LOGIQUE DU FICHIER DGIB
C     RM    = RAYON MOYEN DU COUDE
C     RC    = RAYON DE CINTRAGE DU COUDE
C     ALPHA = ANGLE DU COUDE
C     EP    = EPAISSEUR DU COUDE
C     LTCHAR = LONGUEUR DE L'EMBOUT DU COTE CHARGEMENT
C     LCLIM  = LONGUEUR DE L'EMBOUT DU COTE CONDITIONS AUX LIMITES
C     GEOM  = TYPE DE GEOMETRIE MODELISEE (COUDE OU TUBE)  
C     PROF  = PROFONDEUR DEFAUT (DEMI PETIT AXE) 
C     POSIT = POSITION EN PEAU (EXTERNE OU INTERNE)
C     SYME = QUART DE STRUCTURE SI 'OUI'
C     EVID = CREUSEMENT SOUS-EPAISSEUR SI 'OUI'
C     NBSEP  = NOMBRE DE SOUS-EPAISSEURS
C     SCP = ABSC. CIRCONF. CENTRE SOUS-EPAISSEUR SUR LA PLAQUE
C     BETC = POSITION ANGULAIRE DU CENTRE DE LA SOUS-EPAISSEUR
C     LONC = TAILLE LONGI SUR LE COUDE DE LA SOUS-EPAISSEUR
C     SLP =  ABSC. LONGIT. CENTRE SOUS-EPAISSEUR SUR LA PLAQUE
C     SLC =  ABSC. LONGIT. CENTRE SOUS-EPAISSEUR SUR LE COUDE
C     CIRP = TAILLE CIRCONF. SUR LA PLAQUE DE LA SOUS-EPAISSEUR
C     LONP = TAILLE LONGIT. SUR LA PLAQUE DE LA SOUS-EPAISSEUR
C     DENC = DENSITE CIRCONF. DE LA SOUS-EPAISSEUR
C     DENL = DENSITE LONGIT. DE LA SOUS-EPAISSEUR
C     NBEC = NOMBRE D'ELEMENTS CIRCONF. DE LA SOUS-EPAISSEUR
C     NBEL = NOMBRE D'ELEMENTS LONGI DE LA SOUS-EPAISSEUR
C     COORXG = ABSCISSE DU BORD GAUCHE DE LA SOUS-EPAISSEUR I
C     COORXD = ABSCISSE DU BORD DROIT DE LA SOUS-EPAISSEUR I
C     COORYI = ORDONNEE DU BORD INFERIEUR DE LA SOUS-EPAISSEUR I
C     COORYS = ORDONNEE DU BORD SUPERIEUR DE LA SOUS-EPAISSEUR I
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
C     IPCL = INDICATEUR SUR LA POSITION LONGI (0=ANGLE,1=ABS.CURV.)
C ----------------------------------------------------------------------
C 
      REAL*8        PI, R8PI, AZIMC, DZC, R8BID, CZ, DELTAY, DELTAZ, 
     +              DENSTU, DENSGV
      INTEGER       J, N1, NY, NZC, NZT, NZGV
C 
      PI = R8PI()
C      CZ = ALPHA*RC*PI/180.D0
      NY = 20      
C      NZC = INT((ALPHA+1.D-5)/5.D0)
      DELTAY = 2.D0*PI*RM/NY
C      DELTAZ = CZ/NZC
      DENSTU = NINT(LTCHAR/DELTAY)/4.D0*DELTAY
      DENSGV = NINT(LTCHAR/DELTAY)/4.D0*DELTAY
C
C     CONVERSION DES DENSITES DE DERAFFINEMENT DES EMBOUTS EN
C     DEGRES PAR RAPPORT A L'ANGLE DU COUDE
C      
      DENSTU = DENSTU*360.D0/(2.D0*PI*RC)
      DENSGV = DENSGV*360.D0/(2.D0*PI*RC)
      NZT = 0
      NZGV= 0      
C
      WRITE(UN,10) '* DEBUT PARAMETRES UTILISATEUR'
      WRITE(UN,10) '*'
      WRITE(UN,10) '* Parametres generaux'
      WRITE(UN,10) '*'
      WRITE(UN,*) 'rm      = ',RM,';'
      WRITE(UN,*) 'rc      = ',RC,';'
      WRITE(UN,*) 'alphac  = ',ALPHA,';'
      WRITE(UN,*) 'epc     = ',EP,';'
      WRITE(UN,*) 'pirad   = ',PI,';'
      WRITE(UN,*) 'lgv     = ',LTCLIM,';'
      WRITE(UN,*) 'lt      = ',LTCHAR,';'
      WRITE(UN,*) 'lcoude = ',ALPHA*PI/180.D0*RC,';'
      IF (GEOM.EQ.'COUDE') THEN
        WRITE(UN,*) 'zcoude = ''oui'' ;'
      ELSE
        WRITE(UN,*) 'zcoude = ''non'' ;'
      END IF
      IF (SYME(1:6).EQ.'ENTIER') THEN
        WRITE(UN,*) 'zsyme = ''entier'' ;'
      ELSE IF (SYME(1:5).EQ.'QUART') THEN
        WRITE(UN,*) 'zsyme = ''quart'' ;'
      ELSE
        WRITE(UN,*) 'zsyme = ''demi'' ;'
      END IF    
      WRITE(UN,*) 'nxep   = ',NBEP,';'
      WRITE(UN,*) 'nzt    =',NZT,';'
      WRITE(UN,*) 'nzgv   =',NZGV,';'
      WRITE(UN,*) 'daxhtu =',DENSTU,';'
      WRITE(UN,*) 'daxhgv =',DENSGV,';'
C
      WRITE(UN,10) '*'
      WRITE(UN,10) '* Zones couvertes en circonference'
      WRITE(UN,10) '*'
C
      DO 20 J=1,NZONEX
C
         WRITE(UN,*) 'bg . ',J,' = ',BG(J)-PI*RM,';'
         WRITE(UN,*) 'bd . ',J,' = ',BD(J)-PI*RM,';'
         WRITE(UN,*) 'indbg . ',J,' = ',INDBG(J),';'
         WRITE(UN,*) 'indbd . ',J,' = ',INDBD(J),';'
         WRITE(UN,*) 'deny  . ',J,' = ',DNX(1,J),';'
         WRITE(UN,*) 'nbely . ',J,' = ',INT(DNX(2,J)),';'
         WRITE(UN,10) '*'
C
 20   CONTINUE
C
      WRITE(UN,10) '* Zones couvertes longitudinalement'
      WRITE(UN,10) '*' 
C
      DO 30 J=1,NZONEY
C
            WRITE(UN,*) 'bi . ',J,' = ',BI(J),';'
            WRITE(UN,*) 'bs . ',J,' = ',BS(J),';'
            WRITE(UN,*) 'indbi . ',J,' = ',INDBI(J),';'
            WRITE(UN,*) 'indbs . ',J,' = ',INDBS(J),';'
            WRITE(UN,*) 'denz  . ',J,' = ',DNY(1,J),';'
            WRITE(UN,*) 'nbelz . ',J,' = ',INT(DNY(2,J)),';'
            WRITE(UN,10) '*'
C
 30   CONTINUE   
C
      WRITE(UN,10) '* Caracteristiques des sous-epaisseurs'
      WRITE(UN,10) '*'
C
      DO 40 J=1,NBSEP
C
        CALL LXMINS(POSIT(J))
        WRITE(UN,*) 'axecir . ',J,' = ',CIRP(J),';'
        WRITE(UN,*) 'axelon . ',J,' = ',LONP(J),';'
        WRITE(UN,*) 'prof . ',J,' = ',PROF(J),';'
        WRITE(UN,*) 'coory . ',J,' = ',SCP(J)-PI*RM,';'
        WRITE(UN,*) 'coorz . ',J,' = ',SLP(J),';'
        WRITE(UN,*) 'posit . ',J,' = ''',POSIT(J)(1:7),''';'
C
        IF (IPCL(J).EQ.0) THEN
C
C  centre sous-ep sur le flanc gauche pour sous-ep axi
C
          AZIMC = PI/2.D0
          DZC = BETC(J)*PI/180.D0*
     &        (RC+(RM+EP/2.D0)*COS(AZIMC))
        ELSE
           DZC = SLC(J)
        END IF
        WRITE(UN,*) 'axelonc . ',J,' = ',LONC(J),';'
        WRITE(UN,*) 'coorzc . ',J,' = ',DZC,';'
        IF (TYPE(J).EQ.'AXIS') THEN
           WRITE(UN,*) 'axisym . ',J,' = ''oui'';'
        ELSE
           WRITE(UN,*) 'axisym . ',J,' = ''non'';'
        END IF
        IF (EVID(J).EQ.'OUI') THEN
          WRITE(UN,*) 'sousep . ',J,' = ''oui'';'
        ELSE
          WRITE(UN,*) 'sousep . ',J,' = ''non'';'
        END IF        
        WRITE(UN,10) '*'
C
 40   CONTINUE  
      WRITE(UN,10) '* FIN PARAMETRES UTILISATEUR'
C
 10   FORMAT(T1,A)
C
      END
