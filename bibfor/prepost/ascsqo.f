      SUBROUTINE ASCSQO ( UN, TYPELE, RM, RC, ALPHA, NBTRAN, EP1, EP2,
     +                    EPI, TETA1, TETA2, LTRAN, LTCHAR, LTCLIM,
     +                    GEOM, SYME, SLP, SCP, CIRP, LONP, PROF, POSIT,
     +                    TYPE, SLC, BETC, LONC, NBEP, EVID, NBXSE, 
     +                    NBEC, NBEL, AZIM, NIVMAG,IPCL)
      IMPLICIT    NONE
      INTEGER     UN, NBTRAN, NBEP, NBXSE(*), NBEC(*), NBEL(*), NIVMAG,
     +            IPCL(*)
      REAL*8      RM, RC, ALPHA, EP1, EP2, EPI, TETA1, TETA2, LTRAN, 
     +            LTCHAR, LTCLIM, SLP(*), SCP(*), CIRP(*), LONP(*),
     +            PROF(*), SLC(*), LONC(*), BETC(*), AZIM(*)
      CHARACTER*8 GEOM,SYME,POSIT(*),TYPE(*),EVID(*)
      CHARACTER*4 TYPELE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 21/10/2002   AUTEUR F1BHHAJ J.ANGLES 
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
C     NBTRAN = NOMBRE DE TRANSITION D'EPAISSEUR (0, 1 OU 2)
C     EP1   = EPAISSEUR DU COUDE (COTE EMBOUT 1 SI TRANSITION)
C     EP2   = EPAISSEUR DU COUDE (COTE EMBOUT 2 SI TRANSITION)
C     EPI   = EPAISSEUR DU COUDE INTERMEDIAIRE SI TRANSITION A 2 PENTES
C     TETA1  = ANGLE DE LA PREMIERE TRANSITION D'EPAISSEUR EVENTUELLE
C     TETA2  = ANGLE DE LA DEUXIEME TRANSITION D'EPAISSEUR EVENTUELLE
C     LTRAN  = LONGUEUR ENTRE FIN DE L'EMBOUT 1 ET DEBUT DE TRANSITION
C     LTCHAR = LONGUEUR DE L'EMBOUT DU COTE CHARGEMENT
C     LCLIM  = LONGUEUR DE L'EMBOUT DU COTE CONDITIONS AUX LIMITES
C     GEOM  = TYPE DE GEOMETRIE MODELISEE (COUDE OU TUBE)  
C     SYME = "QUART" DE STRUCTURE, "DEMI" STRUCTURE OU BIEN "ENTIER"
C     SLP =  ABSC. LONGIT. CENTRE SOUS-EPAISSEUR SUR LA PLAQUE
C     SCP = ABSC. CIRCONF. CENTRE SOUS-EPAISSEUR SUR LA PLAQUE
C     CIRP = TAILLE CIRCONF. SUR LA PLAQUE DE LA SOUS-EPAISSEUR
C     LONP = TAILLE LONGIT. SUR LA PLAQUE DE LA SOUS-EPAISSEUR
C     PROF  = PROFONDEUR DEFAUT (DEMI PETIT AXE) 
C     POSIT = POSITION EN PEAU (EXTERNE OU INTERNE)
C     SLC =  ABSC. LONGIT. CENTRE SOUS-EPAISSEUR SUR LE COUDE
C     BETC = POSITION ANGULAIRE DU CENTRE DE LA SOUS-EPAISSEUR
C     LONC = TAILLE LONGI SUR LE COUDE DE LA SOUS-EPAISSEUR
C     NBEP = NOMBRE D'ELEMENTS DANS LE COUDE
C     EVID = CREUSEMENT SOUS-EPAISSEUR SI 'OUI'
C     NBXSE = NOMBRE D'ELEMENTS DANS LA SOUS-EPAISSEUR
C     NBEC = NOMBRE D'ELEMENTS CIRCONF. DE LA SOUS-EPAISSEUR
C     NBEL = NOMBRE D'ELEMENTS LONGI DE LA SOUS-EPAISSEUR
C     AZIM = AZIMUT DE LA SOUS-EPAISSEUR EN DEGRES
C     IPCL = INDICATEUR SUR LA POSITION LONGI (0=ANGLE,1=ABS.CURV.)
C ----------------------------------------------------------------------
C 
      REAL*8    PI, R8PI, AZIMC, DZC, R8BID
      INTEGER   J, N1, LNOM
      CHARACTER*128 NOMREP
C      
      PI = R8PI()
C
      WRITE(UN,*) 'nivmag   = ',NIVMAG,';'
      CALL ASPECR(UN,
     +' option dime 3 elem '//TYPELE//' nive nivmag echo 0;')
      CALL ASPECR(UN,
     +'*')
      CALL ASPECR(UN,
     +'coory = table;')
      CALL ASPECR(UN,
     +'coorz = table;')
      CALL ASPECR(UN,
     +'prof  = table;')
      CALL ASPECR(UN,
     +'posit = table;')
      CALL ASPECR(UN,
     +'axisym = table;')
      CALL ASPECR(UN,
     +'axecir = table;')
      CALL ASPECR(UN,
     +'axelon = table;')
      CALL ASPECR(UN,
     +'sousep = table;')
      CALL ASPECR(UN,
     +'coorzc = table;')
      CALL ASPECR(UN,
     +'axelonc = table;')
      CALL ASPECR(UN,
     +'*')
      WRITE(UN,10) '* DEBUT PARAMETRES UTILISATEUR'
      WRITE(UN,10) '*'
      WRITE(UN,10) '* Parametres generaux'
      WRITE(UN,10) '*'
      WRITE(UN,*) 'pirad    = ',PI,';'
      WRITE(UN,*) 'rm       = ',RM,';'
      WRITE(UN,*) 'rc       = ',RC,';'
      WRITE(UN,*) 'alpha    = ',ALPHA,';'
      WRITE(UN,*) 'lt1      = ',LTCHAR,';'
      WRITE(UN,*) 'lt2      = ',LTCLIM,';'
      WRITE(UN,*) 'nbtranep = ',NBTRAN,';'
      WRITE(UN,*) 'ep1      = ',EP1,';'
      WRITE(UN,*) 'ep2      = ',EP2,';'
      WRITE(UN,*) 'epI      = ',EPI,';'
      WRITE(UN,*) 'teta1    = ',TETA1,';'
      WRITE(UN,*) 'teta2    = ',TETA2,';'
      WRITE(UN,*) 'ltran    = ',LTRAN,';'
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
      IF (TYPELE.EQ.'CU20') THEN
        WRITE(UN,*) 'zquad = ''oui'' ;'
      ELSE
        WRITE(UN,*) 'zquad = ''non'' ;'
      END IF
      WRITE(UN,*) 'nxep   = ',NBEP,';'   
C
      WRITE(UN,10) '*'
      WRITE(UN,10) '* Caracteristiques de la sous-epaisseur'
      WRITE(UN,10) '*'
      J = 1
      SCP(J) = PI*RM
C
      CALL LXMINS(POSIT(J))
      WRITE(UN,*) 'tysep = ',CIRP(J),';'
      WRITE(UN,*) 'tzsep = ',LONP(J),';'     
      WRITE(UN,*) 'prof . ',J,' = ',PROF(J),';'
      WRITE(UN,*) 'ycsep = ',SCP(J)-PI*RM,';'
      WRITE(UN,*) 'theta = ',AZIM(J),';'
      WRITE(UN,*) 'zcsep = ',SLP(J),';'
      WRITE(UN,*) 'posit . ',J,' = ''',POSIT(J)(1:7),''';'
      WRITE(UN,*) 'nby = ',NBEC(J),';'
      WRITE(UN,*) 'nbz = ',NBEL(J),';'
      WRITE(UN,*) 'nbxse = ',NBXSE(J),';'             

      IF (IPCL(J).EQ.0) THEN
C
C  centre sous-ep sur le flanc gauche pour sous-ep axi
C
          AZIMC = PI/2.D0
          DZC = BETC(J)*PI/180.D0*(RC+(RM+EP1/2.D0)*COS(AZIMC))
      ELSE
           DZC = SLC(J)
      END IF
      WRITE(UN,*) 'axelonc . ',J,' = ',LONC(J),';'
      WRITE(UN,*) 'coorzc . ',J,' = ',DZC,';'
C
      IF (TYPE(J).EQ.'AXIS') THEN
         WRITE(UN,*) 'zaxis = ''oui'';'
      ELSE
         WRITE(UN,*) 'zaxis = ''non'';'
      END IF        
      IF (EVID(J).EQ.'OUI') THEN
        WRITE(UN,*) 'sousep . ',J,' = ''oui'';'
      ELSE
        WRITE(UN,*) 'sousep . ',J,' = ''non'';'
      END IF        
      WRITE(UN,10) '*'
C
      WRITE(UN,10) '* FIN PARAMETRES UTILISATEUR'
C
      CALL REPDEX(1,LNOM,NOMREP)
      WRITE (UN,10)
     +'opti donn '
      WRITE (UN,*)
     + ' '''//NOMREP(1:LNOM)//'ascouf_ssep_mono_v1.datg''; '
C      WRITE (UN,10)
C     +' ''/home06/f1bhhaj/uaster/ASCOUF1/ascouf_ssep_mono_v1.datg'';'
C
 10   FORMAT(T1,A)
C
      END
