      SUBROUTINE TFVERI(CMD,NOMMCF,NBOCC,ITYPFL)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 16/01/2002   AUTEUR DURAND C.DURAND 
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
C-----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
C-----------------------------------------------------------------------
C     VERIFICATIONS DE PREMIER NIVEAU
C     APPELANT : OP0143 , OPERATEUR DEFI_FLUI_STRU
C-----------------------------------------------------------------------
C  IN   : CMD    : NOM DE LA COMMANDE
C  IN   : NOMMCF : NOM DU MOT-CLE FACTEUR UTILISE
C  IN   : NBOCC  : NOMBRE D'OCCURENCES DU MOT-CLE FACTEUR UTILISE
C  IN   : ITYPFL : INDICE CARACTERISTIQUE DE LA CONFIGURATION ETUDIEE
C-----------------------------------------------------------------------
      INTEGER       ITYPFL
      CHARACTER*16  CMD, NOMMCF
C ----------------------------------------------------------------------
      INTEGER       COUNT1,COUNT2,COUNT3,COUNT4,COUNT5
      INTEGER       TRES,OCGRIL
      CHARACTER*2   CARAPA(4)
      CHARACTER*3   OUINON
      CHARACTER*8   K8BID
      CHARACTER*9   TPAS
C      CHARACTER*9   TYPAS(2)     
      CHARACTER*16  TYPENC
      REAL*8        VECT(3),VALEPA(4)
C
C      DATA TYPAS   /'CARRE_LIGN ','TRIA_LIGN'/
C ----------------------------------------------------------------------
C
C
C
C ----1.CAS D'UN FAISCEAU_TRANS
C       -----------------------
C
      IF (ITYPFL.EQ.1) THEN
C ---    VERIFICATION DE LA PRESENCE D AU MOINS UNE OCCURENCE DU
C        MOT-CLE COUPLAGE
         NCOUP = 0
         DO 10 IOCC = 1,NBOCC
            CALL GETVTX(NOMMCF,'COUPLAGE',IOCC,1,0,K8BID,ICOUP)
            IF(ICOUP.NE.0) THEN
               NCOUP = NCOUP + 1
               JCOUP = IOCC
            ENDIF
  10     CONTINUE
         IF(NCOUP.EQ.0) THEN
            CALL UTMESS('E',CMD,'<FAISCEAU_TRANS> LE MOT CLE '//
     &                  '<COUPLAGE> DOIT ETRE RENSEIGNE AU MOINS '//
     &                  'UNE FOIS SOUS L UNE DES OCCURENCE DU MOT-'//
     &                  'CLE FACTEUR <FAISCEAU_TRANS>')
            GOTO 9999
         ENDIF
         NCARA = 0
         NRHOI = 0
         NRHOE = 0
         NCMP = 0
         DO 20 IOCC = 1,NBOCC
            CALL GETVID(NOMMCF,'CARA_ELEM'     ,IOCC,1,0,K8BID,ICARA)
            IF(ICARA.NE.0) NCARA = NCARA + 1
            CALL GETVID(NOMMCF,'PROF_RHO_F_INT',IOCC,1,0,K8BID,IRHOI)
            IF(IRHOI.NE.0) NRHOI = NRHOI + 1
            CALL GETVID(NOMMCF,'PROF_RHO_F_EXT',IOCC,1,0,K8BID,IRHOE)
            IF(IRHOE.NE.0) NRHOE = NRHOE + 1
            CALL GETVTX(NOMMCF,'NOM_CMP       ',IOCC,1,0,K8BID,ICMP)
            IF(ICMP.NE.0)  NCMP = NCMP + 1
  20     CONTINUE
C
         CALL GETVTX(NOMMCF,'COUPLAGE',JCOUP,1,1,OUINON,IBID)
C
C -------1.1.SI PRISE EN COMPTE DU COUPLAGE
C
         IF (OUINON.EQ.'OUI') THEN
            NTPAS = 0
            NTRES = 0
            NPAS = 0
            NCM = 0
            DO 30 IOCC = 1,NBOCC
               CALL GETVTX(NOMMCF,'TYPE_PAS',IOCC,1,0,TPAS,ITPAS)
               IF(ITPAS.NE.0) THEN
                  NTPAS = NTPAS + 1
               ENDIF
               CALL GETVIS(NOMMCF,'TYPE_RESEAU',IOCC,1,0,IBID,ITRES)
               IF(ITRES.NE.0) THEN
                  NTRES = NTRES + 1
               ENDIF
               CALL GETVR8(NOMMCF,'PAS',IOCC,1,0,RBID,IPAS)
               IF(IPAS.NE.0) THEN
C                  JPAS = IOCC
                  NPAS = NPAS + 1
               ENDIF
  30        CONTINUE
            IF(NTPAS.EQ.0.OR.NTRES.NE.NBOCC.OR.NPAS.EQ.0) THEN
               CALL UTMESS('E',CMD,'<FAISCEAU_TRANS> : SI COUPLAGE'//
     &         ' <TYPE_PAS> , <TYPE_RESEAU> ET <PAS> MOTS-CLES'//
     &         ' OBLIGATOIRES DANS AU MOINS L UNE DES OCCURENCES'//
     &         ' DU MOT-CLE FACTEUR' )
            ENDIF
C            CALL GETVTX(NOMMCF,'TYPE_PAS',JPAS,1,1,TPAS,IBID)
C            IF (TPAS .EQ. TYPAS(2)) THEN
C               DO 40 IOCC = 1,NBOCC
C                  CALL GETVIS(NOMMCF,'TYPE_RESEAU',IOCC,1,1,TRES,IBID)
C                  IF (TRES .GT. 4) THEN
C                     CALL UTMESS('E',CMD,'<FAISCEAU_TRANS> : MAUVAIS'//
C     &                          ' CHOIX DE  TYPE DE RESEAU POUR UN'//
C     &                          ' PAS TRIANGULAIRE')
C                  ENDIF
C  40           CONTINUE
C            ENDIF
C
C ------1.2.SI NON PRISE EN COMPTE DU COUPLAGE
C
         ELSE
            NCM = 0
            DO 50 IOCC = 1,NBOCC
               CALL GETVR8(NOMMCF,'COEF_MASS_AJOU',IOCC,1,0,RBID,ICM)
               IF(ICM.NE.0) THEN
                  NCM = NCM + 1
               ENDIF
  50        CONTINUE
            IF (NCM.EQ.0) THEN
              CALL UTMESS('E',CMD,'FAISCEAU_TRANS : SI PAS DE COUPLAGE '
     &       // '<COEF_MASS_AJOU> MOT-CLE OBLIGATOIRE DANS AU MOINS '//
     &       'L UNE DES OCCURENCES DU MOT CLE FACTEUR <FAISCEAU_TRANS>')
            ENDIF
        ENDIF
C
C ------1.3.VERIFICATION DE LA PRESENCE  DES MOT-CLE DEVANT APPARAITRE
C       AU MOINS UNE FOIS DANS L UNE DES OCCURENCES DU MOT-CLE FACTEUR
C
        IF(NCARA.EQ.0) THEN
           CALL UTMESS('E',CMD,'<FAISCEAU_TRANS> : LE MOT-CLE'//
     &            ' <CARA_ELEM> DOIT ETRE RENSEIGNE AU MOINS UNE'//
     &            ' FOIS DANS L UNE DES OCCURENCES DU MOT-CLE FACTEUR'//
     &            ' <FAISCEAU_TRANS>')
        ENDIF
        IF(NRHOI.EQ.0) THEN
           CALL UTMESS('E',CMD,'<FAISCEAU_TRANS> : LE MOT-CLE'//
     &            ' <PROF_RHO_F_INT> DOIT ETRE RENSEIGNE AU MOINS UNE'//
     &            ' FOIS DANS L UNE DES OCCURENCES DU MOT-CLE FACTEUR'//
     &            ' <FAISCEAU_TRANS>')
        ENDIF
        IF(NRHOE.EQ.0) THEN
           CALL UTMESS('E',CMD,'<FAISCEAU_TRANS> : LE MOT-CLE'//
     &            ' <PROF_RHO_F_EXT> DOIT ETRE RENSEIGNE AU MOINS UNE'//
     &            ' FOIS DANS L UNE DES OCCURENCES DU MOT-CLE FACTEUR'//
     &            ' <FAISCEAU_TRANS>')
        ENDIF
        IF(NCARA.EQ.0) THEN
           CALL UTMESS('E',CMD,'<FAISCEAU_TRANS> : LE MOT-CLE'//
     &            ' <NOM_CMP> DOIT ETRE RENSEIGNE AU MOINS UNE'//
     &            ' FOIS DANS L UNE DES OCCURENCES DU MOT-CLE FACTEUR'//
     &            ' <FAISCEAU_TRANS>')
        ENDIF
C
C ----2.CAS D'UNE GRAPPE
C       ----------------
C
      ELSE IF (ITYPFL.EQ.2) THEN
C
        CALL GETVTX(NOMMCF,'COUPLAGE',1,1,1,OUINON,IBID)
        IF (OUINON.EQ.'OUI') THEN
          CALL GETVTX(NOMMCF,'GRAPPE_2',1,1,0,K8BID,IGRA2)
          IF (IGRA2.EQ.0) THEN
            CALL UTMESS('E',CMD,'GRAPPE : SI PRISE EN COMPTE DU'//
     &        ' COUPLAGE, LES MOTS-CLES <GRAPPE_2>, <NOEUD>,'//
     &        ' <CARA_ELEM>, <MODELE> ET <RHO_FLUI> DOIVENT ETRE'//
     &        ' RENSEIGNES')
          ENDIF
        ENDIF
C
C
C ----3.CAS D'UN FAISCEAU_AXIAL
C       -----------------------
C
      ELSE IF (ITYPFL.EQ.3) THEN
C
        COUNT1 = 0
        COUNT2 = 0
        COUNT3 = 0
        COUNT4 = 0
        COUNT5 = 0
        OCGRIL = 0
C
        DO 80 IOCC = 1,NBOCC
C
C --------3.1.SI PLUSIEURS OCCURENCES <RAYON_TUBE> ET <COOR_TUBE>
C --------    OBLIGATOIRES A CHAQUE OCCURENCE
C --------    VERIFICATION DES DONNEES POUR <COOR_TUBE>
C
          CALL GETVR8(NOMMCF,'RAYON_TUBE',IOCC,1,0,RBID,IRAYON)
          IF (IRAYON.EQ.0) THEN
            IF (NBOCC.GT.1) THEN
              CALL UTMESS('E',CMD,'FAISCEAU_AXIAL : PLUSIEURS'//
     &          ' OCCURENCES POUR LE MOT-CLE FACTEUR => FAISCEAU'//
     &          ' EQUIVALENT => MOTS-CLES <RAYON_TUBE> ET <COOR_TUBE>'//
     &          ' OBLIGATOIRES A CHAQUE OCCURENCE')
            ENDIF
          ELSE
            CALL GETVR8(NOMMCF,'COOR_TUBE',IOCC,1,0,RBID,NBCOOR)
            NBCOOR = ABS(NBCOOR)
            NBTUB  = INT(NBCOOR/2)
            NBTUB2 = 2*NBTUB
            IF (NBTUB2.NE.NBCOOR) THEN
              CALL UTMESS('E',CMD,'FAISCEAU_AXIAL : ON ATTEND UN'//
     &          ' NOMBRE PAIR D ARGUMENTS POUR LE MOT-CLE'//
     &          ' <COOR_TUBE>. IL FAUT FOURNIR DEUX COORDONNEES'//
     &          ' POUR DEFINIR LA POSITION DE CHACUN DES TUBES DU'//
     &          ' FAISCEAU REEL')
            ENDIF
          ENDIF
C
C --------3.2.INCREMENTATION DU COMPTEUR POUR <VECT_X> ET VERIFICATION
C --------    DES DONNEES SI PRESENCE
C
          CALL GETVR8(NOMMCF,'VECT_X',IOCC,1,0,RBID,IVECT)
          IF (IVECT.NE.0) THEN
            COUNT1 = COUNT1 + 1
            IF (ABS(IVECT).NE.3) THEN
              CALL UTMESS('E',CMD,'FAISCEAU_AXIAL : IL FAUT TROIS'//
     &          ' COMPOSANTES POUR <VECT_X>')
            ELSE
              IER2 = 0
              CALL GETVR8(NOMMCF,'VECT_X',IOCC,1,3,VECT(1),IBID)
              IF (VECT(1).EQ.1.D0) THEN
                IF (VECT(2).NE.0.D0 .OR. VECT(3).NE.0.D0) IER2 = 1
              ELSE IF (VECT(2).EQ.1.D0) THEN
                IF (VECT(1).NE.0.D0 .OR. VECT(3).NE.0.D0) IER2 = 1
              ELSE IF (VECT(3).EQ.1.D0) THEN
                IF (VECT(1).NE.0.D0 .OR. VECT(2).NE.0.D0) IER2 = 1
              ELSE
                IER2 = 1
              ENDIF
              IF (IER2.EQ.1) CALL UTMESS('E',CMD,'FAISCEAU_AXIAL :'//
     &          ' LE VECTEUR DIRECTEUR DU FAISCEAU DOIT ETRE L UN DES'//
     &          ' VECTEURS UNITAIRES DE LA BASE LIEE AU REPERE GLOBAL')
            ENDIF
          ENDIF
C
C --------3.3.INCREMENTATION DES COMPTEURS POUR <PROF_RHO_FLUI>,
C --------    <PROF_VISC_CINE> ET <RUGO_TUBE>
C
          CALL GETVID(NOMMCF,'PROF_RHO_FLUI',IOCC,1,0,K8BID,IRHO)
          IF (IRHO.NE.0) COUNT2 = COUNT2 + 1
C
          CALL GETVID(NOMMCF,'PROF_VISC_CINE',IOCC,1,0,K8BID,IVISC)
          IF (IVISC.NE.0) COUNT3 = COUNT3 + 1
C
          CALL GETVR8(NOMMCF,'RUGO_TUBE',IOCC,1,0,RBID,IRUGO)
          IF (IRUGO.NE.0) COUNT4 = COUNT4 + 1
C
C --------3.4.VERIFICATION DES DONNEES POUR <PESANTEUR> SI PRESENCE
C
          CALL GETVR8(NOMMCF,'PESANTEUR',IOCC,1,0,RBID,IPESAN)
          IPESAN = ABS(IPESAN)
          IF (IPESAN.NE.0 .AND. IPESAN.NE.4) THEN
            CALL UTMESS('E',CMD,'FAISCEAU_AXIAL : IL FAUT 4 DONNEES'//
     &        ' POUR LE MOT-CLE <PESANTEUR> : LA NORME DU VECTEUR ET'//
     &        ' SES COMPOSANTES DANS LE REPERE GLOBAL, DANS CET ORDRE')
          ENDIF
C
C --------3.5.INCREMENTATION DU COMPTEUR POUR <CARA_PAROI>
C --------    VERIFICATION DES DONNEES POUR <CARA_PAROI>, <VALE_PAROI>
C --------    ET <ANGL_VRIL> SI PRESENCE
C
          CALL GETVTX(NOMMCF,'CARA_PAROI',IOCC,1,0,K8BID,ICAPA)
          ICAPA = ABS(ICAPA)
          IF (ICAPA.NE.0) THEN
            COUNT5 = COUNT5 + 1
            IF (ICAPA.NE.3 .AND. ICAPA.NE.4) THEN
              CALL UTMESS('E',CMD,'FAISCEAU_AXIAL : IL FAUT 3 OU 4'//
     &          ' DONNEES POUR LE MOT-CLE <CARA_PAROI> : 3 POUR UNE'//
     &          ' ENCEINTE CIRCULAIRE : <YC>,<ZC>,<R>. 4 POUR UNE'//
     &          ' ENCEINTE RECTANGULAIRE : <YC>,<ZC>,<HY>,<HZ>')
            ELSE
              CALL GETVR8(NOMMCF,'VALE_PAROI',IOCC,1,0,RBID,IVAPA)
              IVAPA = ABS(IVAPA)
              IF (IVAPA.NE.ICAPA) THEN
                CALL UTMESS('E',CMD,'FAISCEAU_AXIAL : POUR DEFINIR'//
     &            ' UNE ENCEINTE, IL FAUT AUTANT D ARGUMENTS POUR'//
     &            ' LES MOTS-CLES <CARA_PAROI> ET <VALE_PAROI>')
              ELSE
                CALL GETVTX(NOMMCF,'CARA_PAROI',IOCC,1,ICAPA,
     &                                                 CARAPA(1),IBID)
                CALL GETVR8(NOMMCF,'VALE_PAROI',IOCC,1,IVAPA,
     &                                                 VALEPA(1),IBID)
                NBYC = 0
                NBZC = 0
                NBR  = 0
                NBHY = 0
                NBHZ = 0
                IF (ICAPA.EQ.3) THEN
                  DO 60 ICARA = 1,ICAPA
                    IF (CARAPA(ICARA).EQ.'YC') NBYC = NBYC + 1
                    IF (CARAPA(ICARA).EQ.'ZC') NBZC = NBZC + 1
                    IF (CARAPA(ICARA)(1:1).EQ.'R') THEN
                      NBR = NBR + 1
                      IR = ICARA
                    ENDIF
  60              CONTINUE
                  IF (NBYC.NE.1 .OR. NBZC.NE.1 .OR. NBR.NE.1) THEN
                    CALL UTMESS('E',CMD,'FAISCEAU_AXIAL : MOT-CLE'//
     &                ' <CARA_PAROI>. DONNEES INCOHERENTES POUR UNE'//
     &                ' ENCEINTE CIRCULAIRE')
                  ELSE IF (VALEPA(IR).LE.0.D0) THEN
                    CALL UTMESS('E',CMD,'FAISCEAU_AXIAL : VALEUR'//
     &                ' INACCEPTABLE POUR LE RAYON DE L ENCEINTE'//
     &                ' CIRCULAIRE')
                  ENDIF
                ELSE
                  DO 70 ICARA = 1,ICAPA
                    IF (CARAPA(ICARA).EQ.'YC') NBYC = NBYC + 1
                    IF (CARAPA(ICARA).EQ.'ZC') NBZC = NBZC + 1
                    IF (CARAPA(ICARA).EQ.'HY') THEN
                      NBHY = NBHY + 1
                      IHY = ICARA
                    ENDIF
                    IF (CARAPA(ICARA).EQ.'HZ') THEN
                      NBHZ = NBHZ + 1
                      IHZ = ICARA
                    ENDIF
  70              CONTINUE
                  IF (NBYC.NE.1 .OR. NBZC.NE.1 .OR. NBHY.NE.1 .OR.
     &                NBHZ.NE.1) THEN
                    CALL UTMESS('E',CMD,'FAISCEAU_AXIAL : MOT-CLE'//
     &                ' <CARA_PAROI>. DONNEES INCOHERENTES POUR UNE'//
     &                ' ENCEINTE RECTANGULAIRE')
                  ELSE IF (VALEPA(IHY).LE.0.D0 .OR.
     &                     VALEPA(IHZ).LE.0.D0) THEN
                    CALL UTMESS('E',CMD,'FAISCEAU_AXIAL : VALEUR(S)'//
     &                ' INACCEPTABLE(S) POUR L UNE OU(ET) L AUTRE DES'//
     &                ' DIMENSIONS DE L ENCEINTE RECTANGULAIRE')
                  ELSE
                    CALL GETVR8(NOMMCF,'ANGL_VRIL',IOCC,1,0,RBID,IANGL)
                    IF (IANGL.EQ.0) THEN
                      CALL UTMESS('E',CMD,'FAISCEAU_AXIAL : LE'//
     &                  ' MOT-CLE <ANGL_VRIL> EST OBLIGATOIRE QUAND'//
     &                  ' ON DEFINIT UNE ENCEINTE RECTANGULAIRE')
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
C
C --------3.6.DETECTION DE LA DERNIERE OCCURENCE POUR LAQUELLE LES
C --------    OPERANDES ASSOCIEES AUX CARACTERISTIQUES DES GRILLES
C --------    SONT PRESENTES
C
          CALL GETVR8(NOMMCF,'LONG_TYPG',IOCC,1,0,RBID,NTYPG)
          IF (NTYPG.NE.0) THEN
             OCGRIL = IOCC
          ENDIF
C
  80    CONTINUE
C
C ------3.7.VERIFICATION DES COMPTEURS
C
        IF (COUNT1.EQ.0) THEN
          CALL UTMESS('E',CMD,'FAISCEAU_AXIAL : LE MOT-CLE <VECT_X>'//
     &      ' EST OBLIGATOIRE SI IL N Y A QU UNE SEULE OCCURENCE POUR'//
     &      ' LE MOT-CLE FACTEUR. SINON, IL DOIT APPARAITRE DANS AU'//
     &      ' MOINS UNE DES OCCURENCES')
        ELSE IF (COUNT2.EQ.0) THEN
          CALL UTMESS('E',CMD,'FAISCEAU_AXIAL : LE MOT-CLE'//
     &      ' <PROF_RHO_FLUI> EST OBLIGATOIRE SI IL N Y A QU UNE'//
     &      ' SEULE OCCURENCE POUR LE MOT-CLE FACTEUR. SINON, IL DOIT'//
     &      ' APPARAITRE DANS AU MOINS UNE DES OCCURENCES')
        ELSE IF (COUNT3.EQ.0) THEN
          CALL UTMESS('E',CMD,'FAISCEAU_AXIAL : LE MOT-CLE'//
     &      ' <PROF_VISC_CINE> EST OBLIGATOIRE SI IL N Y A QU UNE'//
     &      ' SEULE OCCURENCE POUR LE MOT-CLE FACTEUR. SINON, IL DOIT'//
     &      ' APPARAITRE DANS AU MOINS UNE DES OCCURENCES')
        ELSE IF (COUNT4.EQ.0) THEN
         CALL UTMESS('E',CMD,'FAISCEAU_AXIAL : LE MOT-CLE <RUGO_TUBE>'//
     &      ' EST OBLIGATOIRE SI IL N Y A QU UNE SEULE OCCURENCE POUR'//
     &      ' LE MOT-CLE FACTEUR. SINON, IL DOIT APPARAITRE DANS AU'//
     &      ' MOINS UNE DES OCCURENCES')
        ELSE IF (COUNT5.EQ.0) THEN
          CALL UTMESS('E',CMD,'FAISCEAU_AXIAL : LES MOTS-CLES '//
     &      '<CARA_PAROI> ET <VALE_PAROI> SONT OBLIGATOIRES SI IL '//
     &      'N Y A QU UNE SEULE OCCURENCE POUR LE MOT-CLE FACTEUR. '//
     &      'SINON, ILS DOIVENT APPARAITRE ENSEMBLE DANS AU MOINS '//
     &      'UNE DES OCCURENCES. LE MOT-CLE <ANGL_VRIL> DOIT '//
     &      'EGALEMENT ETRE PRESENT SI L ON DEFINIT UNE ENCEINTE '//
     &      'RECTANGULAIRE')
        ENDIF
C
C ------3.8.VERIFICATION DES DONNEES CARACTERISTIQUES DES GRILLES
C
        IF (OCGRIL.NE.0) THEN
           CALL TFVEGR(CMD,NOMMCF,OCGRIL)
        ENDIF
C
C
C ----4.CAS DE COQUE_COAX
C       -----------------
C
      ELSE
C
        CALL GETVR8(NOMMCF,'VECT_X',1,1,0,RBID,IVECT)
        IF (ABS(IVECT).NE.3) THEN
          CALL UTMESS('E',CMD,'COQUE_COAX : IL FAUT TROIS'//
     &      ' COMPOSANTES POUR <VECT_X>')
        ELSE
          IER2 = 0
          CALL GETVR8(NOMMCF,'VECT_X',1,1,3,VECT(1),IBID)
          IF (VECT(1).EQ.1.D0) THEN
            IF (VECT(2).NE.0.D0 .OR. VECT(3).NE.0.D0) IER2 = 1
          ELSE IF (VECT(2).EQ.1.D0) THEN
            IF (VECT(1).NE.0.D0 .OR. VECT(3).NE.0.D0) IER2 = 1
          ELSE IF (VECT(3).EQ.1.D0) THEN
            IF (VECT(1).NE.0.D0 .OR. VECT(2).NE.0.D0) IER2 = 1
          ELSE
            IER2 = 1
          ENDIF
          IF (IER2.EQ.1) CALL UTMESS('E',CMD,'COQUE_COAX : L AXE'//
     &      ' DE REVOLUTION DES COQUES DOIT AVOIR POUR VECTEUR'//
     &      ' DIRECTEUR L UN DES VECTEURS UNITAIRES DE LA BASE LIEE'//
     &      ' AU REPERE GLOBAL')
        ENDIF
C
      ENDIF
C
 9999 CONTINUE
C
      END
