      SUBROUTINE PRCCM1 ( MCF, NBOCC, TYPCO, COURBE, SM, NBORDM, 
     +                   INDINS, XNOMCP, XNUMCP, NCHEFF, NOMA, NLSNAC )
      IMPLICIT   NONE
      INTEGER             NBOCC, NBORDM 
      REAL*8              SM
      CHARACTER*8         TYPCO, COURBE, NOMA
      CHARACTER*16        NCHEFF
      CHARACTER*24        XNUMCP, XNOMCP, NLSNAC
      CHARACTER*(*)       MCF
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 26/11/2002   AUTEUR CIBHHLV L.VIVAN 
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
C     POST_RCCM: CALCUL SN ET SP  EN COMBINANT TOUS LES
C                TRANSITOIRES ENTRE EUX   ( OPTION "FATIGUE_ZH210" )
C ======================================================================
C     OPERATEUR POST_RCCM: CALCUL DU PMPB, DU SN, DU SP
C ======================================================================
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER      NBORD2, IDSNO, IDSPO, IDSNE, IDSPE, IDNBC1, IDNBC2,
     +             IDNBC3, IDNBC4, IDTEM1, IDTEM2, INDINS, IOCC, ICHEF1,
     +             JSNCY1, NBCHEF, JCHEF, N1, IDSNOM, IDSNEM,
     +             ICHEF2, IOCC2, JCHEF2, JINST2, JSNCY2, NBCHE2,
     +             IDOCC1, IDOCC2, JINST, I, INOM(3)
      REAL*8       SN1O(6), SN1E(6), SN2O(6), SN2E(6),SN12O(6),SN12E(6),
     +             SP1O(6), SP1E(6), SP2O(6), SP2E(6),SP12O(6),SP12E(6),
     +             SNFL(6), TEMP1, TEMP2, SNO, SNE, SPO, SPE,
     +             RNOM(2), EQUI(6)
      COMPLEX*16   C16B
      CHARACTER*8  K8B, RESMEC, RESME2, SNMAX
      CHARACTER*10 NOMTAB(5)
      CHARACTER*19 NCH19
      CHARACTER*24 NLSMAC
C ======================================================================
      CALL JEMARQ()
      NLSMAC = '&&PRCCM1.MAILLES.ACTIVES'
      NBORD2 = (NBORDM*(NBORDM+1)) / 2
C ======================================================================
C --- CREATION D'UN TABLEAU DE TRAVAIL DESTINE A CONTENIR LE TRESCA
C     DES CONTRAINTES LINEARISEES A L'ORIGINE DU CHEMIN :
C ======================================================================
      CALL WKVECT('&&OP0165.SNO','V V R',NBORD2,IDSNO)
C ======================================================================
C --- CREATION D'UN TABLEAU DE TRAVAIL DESTINE A CONTENIR LE TRESCA
C     DES CONTRAINTES LINEARISEES A L'EXTREMITE DU CHEMIN :
C ======================================================================
      CALL WKVECT('&&OP0165.SNE','V V R',NBORD2,IDSNE)
C ======================================================================
C --- CREATION D'UN TABLEAU DE TRAVAIL DESTINE A CONTENIR LE MAX
C     DE TRESCA DES CONTRAINTES LINEARISEES A L'ORIGINE DU CHEMIN :
C ======================================================================
      CALL WKVECT('&&OP0165.SNOMAX','V V R',NBORD2,IDSNOM)
C ======================================================================
C --- CREATION D'UN TABLEAU DE TRAVAIL DESTINE A CONTENIR LE MAX
C     DE TRESCA DES CONTRAINTES LINEARISEES A L'EXTREMITE DU CHEMIN :
C ======================================================================
      CALL WKVECT('&&OP0165.SNEMAX','V V R',NBORD2,IDSNEM)
C ======================================================================
C --- CREATION D'UN TABLEAU DE TRAVAIL DESTINE A CONTENIR LE TRESCA
C     DES CONTRAINTES A L'ORIGINE DU CHEMIN :
C ======================================================================
      CALL WKVECT('&&OP0165.SPO','V V R',NBORD2,IDSPO)
C ======================================================================
C --- CREATION D'UN TABLEAU DE TRAVAIL DESTINE A CONTENIR LE TRESCA
C     DES CONTRAINTES A L'EXTREMITE DU CHEMIN :
C ======================================================================
      CALL WKVECT('&&OP0165.SPE','V V R',NBORD2,IDSPE)
C ======================================================================
C --- CREATION D'UN TABLEAU DE TRAVAIL DESTINE A CONTENIR LE 
C --- NOMBRE DE CYLCLES RELATIF A L'INSTANT COURANT DU PREMIER 
C --- TRANSITOIRE POUR L'ORIGINE DU CHEMIN :
C ======================================================================
      CALL WKVECT('&&OP0165.NBCYCL1','V V I',NBORD2,IDNBC1)
C ======================================================================
C --- CREATION D'UN TABLEAU DE TRAVAIL DESTINE A CONTENIR LE 
C --- NOMBRE DE CYLCLES RELATIF A L'INSTANT COURANT DU SECOND 
C --- TRANSITOIRE POUR L'ORIGINE DU CHEMIN :
C ======================================================================
      CALL WKVECT('&&OP0165.NBCYCL2','V V I',NBORD2,IDNBC2)
C ======================================================================
C --- CREATION D'UN TABLEAU DE TRAVAIL DESTINE A CONTENIR LE 
C --- NOMBRE DE CYLCLES RELATIF A L'INSTANT COURANT DU PREMIER 
C --- TRANSITOIRE POUR L'EXTREMITE DU CHEMIN :
C ======================================================================
      CALL WKVECT('&&OP0165.NBCYCL3','V V I',NBORD2,IDNBC3)
C ======================================================================
C --- CREATION D'UN TABLEAU DE TRAVAIL DESTINE A CONTENIR LE 
C --- NOMBRE DE CYLCLES RELATIF A L'INSTANT COURANT DU SECOND 
C --- TRANSITOIRE POUR L'EXTREMITE DU CHEMIN :
C ======================================================================
      CALL WKVECT('&&OP0165.NBCYCL4','V V I',NBORD2,IDNBC4)
C ======================================================================
C --- CREATION D'UN TABLEAU DE TRAVAIL DESTINE A CONTENIR L'INSTANT 
C --- DU PREMIER CHARGEMENT POUR LA COMBINAISON COURANTE :
C ======================================================================
      CALL WKVECT('&&OP0165.NUMINST1','V V R',NBORD2,IDTEM1)
C ======================================================================
C --- CREATION D'UN TABLEAU DE TRAVAIL DESTINE A CONTENIR L'INSTANT 
C --- DU SECOND CHARGEMENT POUR LA COMBINAISON COURANTE :
C ======================================================================
      CALL WKVECT('&&OP0165.NUMINST2','V V R',NBORD2,IDTEM2)
C ======================================================================
C --- CREATION D'UN TABLEAU DE TRAVAIL DESTINE A CONTENIR LE NOM DU 
C --- TRANSITOIRE DU PREMIER CHARGEMENT POUR LA COMBINAISON COURANTE :
C ======================================================================
      CALL WKVECT('&&OP0165.NOMRESU1','V V K8',NBORD2,IDOCC1)
C ======================================================================
C --- CREATION D'UN TABLEAU DE TRAVAIL DESTINE A CONTENIR LE NOM DU 
C --- TRANSITOIRE DU SECOND CHARGEMENT POUR LA COMBINAISON COURANTE :
C ======================================================================
      CALL WKVECT('&&OP0165.NOMRESU2','V V K8',NBORD2,IDOCC2)
C ======================================================================
C --- CREATION DE LA TABLE TEMPORAIRE 
C ======================================================================
      SNMAX     = '&&SSNMAX'
      NOMTAB(1) = 'INDICAT'
      NOMTAB(2) = 'TRANSIT1'
      NOMTAB(3) = 'TRANSIT2'
      NOMTAB(4) = 'SNOTRANS'
      NOMTAB(5) = 'SNETRANS'
      CALL TBCRSD ( SNMAX, 'V' )
      CALL TBAJPA ( SNMAX, 1, NOMTAB(1), 'I' )
      CALL TBAJPA ( SNMAX, 1, NOMTAB(2), 'I'  )
      CALL TBAJPA ( SNMAX, 1, NOMTAB(3), 'I'  )
      CALL TBAJPA ( SNMAX, 1, NOMTAB(4), 'R'  )
      CALL TBAJPA ( SNMAX, 1, NOMTAB(5), 'R'  )
C ======================================================================
C --- INDICE DE LA COMBINAISON DE CHARGEMENTS :
C ======================================================================
      INDINS = 0
C ======================================================================
C --- BOUCLE SUR LES TRANSITOIRES :
C ======================================================================
      DO 10 IOCC = 1, NBOCC
         CALL GETVID ( MCF, 'RESULTAT',  IOCC,1,1, RESMEC, N1 )
C ======================================================================
C ---  RECUPERATION DU NOMBRE DE CYCLES DU TRANSITOIRE COURANT :
C ======================================================================
         CALL JEVEUO(JEXNUM(NCHEFF//'.NBCYCLE',IOCC),'L',JSNCY1)
         CALL JELIRA (JEXNUM(NCHEFF//'.LSCHEFF',IOCC),'LONMAX',
     +                                                   NBCHEF,K8B)
         CALL JEVEUO (JEXNUM(NCHEFF//'.LSCHEFF',IOCC),'L',JCHEF)
         CALL JEVEUO (JEXNUM(NCHEFF//'.VALACCE',IOCC),'L',JINST)
C ======================================================================
C --- BOUCLE SUR LES INDICES DES NUMEROS D'ORDRE DU TRANSITOIRE COURANT
C ======================================================================
         DO 20 ICHEF1 = 1 , NBCHEF 
C ======================================================================
C ---    SI LE NOMBRE DE CYCLES ASSOCIE A L'INSTANT COURANT EST NUL 
C ---    ON NE PREND PLUS EN COMPTE L'ETAT DE CONTRAINTES CORRESPONDANT:
C ======================================================================
            IF (ZI(JSNCY1+ICHEF1-1).EQ.0) GO TO 20
C ======================================================================
C ---    NOM DU CHAMP DE CONTRAINTES ASSOCIE AU NUMERO D'ORDRE COURANT :
C ======================================================================
            NCH19 = ZK24(JCHEF+ICHEF1-1)(1:19)
C ======================================================================
C ---    RECUPERATION DU CHEMIN ET CONSTRUCTION DE LA LISTE DE MAILLES
C ---    DE NOM NLSMAC CONSTITUANT CE CHEMIN :
C ======================================================================
            CALL PROUEX ( TYPCO, COURBE, NCH19, NLSMAC, NLSNAC, NOMA )
C ======================================================================
C ---    RECUPERATION DE L'INSTANT DU CHAMP DE CONTRAINTES COURANT :
C ======================================================================
            TEMP1 = ZR(JINST+ICHEF1-1)
            TEMP2 = 0.0D0
C ======================================================================
C ---    EXTRACTION DU CHAMP DE CONTRAINTES AUX EXTREMITES DU CHEMIN
C ---    --> SP1O ET SP1E ET CALCUL DES CONTRAINTES LINEARISEES SN1O
C ---    ET SN1E EN CES MEMES NOEUDS :
C ======================================================================
            CALL PRSNSP ( MCF, IOCC, TYPCO, COURBE, XNOMCP, XNUMCP,  
     +                    NLSMAC, NLSNAC, NCH19, SM,
     +                    SN1O, SN1E, SNFL, SP1O, SP1E, ICHEF1 )
C ======================================================================
C --- CALCUL DES VALEURS PROPRES DU TENSEUR DE CONTRAINTES LINEARISEES
C     SN1O A L'ORIGINE DU CHEMIN :
C --- CALCUL DE LA DIFFERENCE SUP SNO DES VALEURS PROPRES (LE TRESCA) :
C ======================================================================
            CALL FGEQUI ( SN1O, 'SIGM', 3, EQUI )
            SNO = EQUI(2)
C ======================================================================
C --- CALCUL DES VALEURS PROPRES DU TENSEUR DE CONTRAINTES LINEARISEES
C     SN1E A L'AUTRE EXTREMITE DU CHEMIN :
C --- CALCUL DE LA DIFFERENCE SUP SNE DES VALEURS PROPRES (LE TRESCA) :
C ======================================================================
            CALL FGEQUI ( SN1E, 'SIGM', 3, EQUI )
            SNE = EQUI(2)
C ======================================================================
C --- CALCUL DES VALEURS PROPRES DU TENSEUR DE CONTRAINTES SP1O 
C     A L'ORIGINE DU CHEMIN :
C --- CALCUL DE LA DIFFERENCE SUP SPO DES VALEURS PROPRES (LE TRESCA) :
C ======================================================================
            CALL FGEQUI ( SP1O, 'SIGM', 3, EQUI )
            SPO = EQUI(2)
C ======================================================================
C --- CALCUL DES VALEURS PROPRES DU TENSEUR DE CONTRAINTES SP1E 
C     A L'AUTRE EXTREMITE DU CHEMIN :
C --- CALCUL DE LA DIFFERENCE SUP SPE DES VALEURS PROPRES (LE TRESCA) :
C ======================================================================
            CALL FGEQUI ( SP1E, 'SIGM', 3, EQUI )
            SPE = EQUI(2)
C ======================================================================
C ---    BOUCLE SUR LES INDICES SUIVANT L'INDICE COURANT DES NUMEROS  
C ---    D'ORDRE DU TRANSITOIRE COURANT :
C ======================================================================
            DO 30 ICHEF2 = ICHEF1+1 , NBCHEF 
C ======================================================================
C ---    SI LE NOMBRE DE CYCLES ASSOCIE A L'INSTANT COURANT EST NUL ON
C ---    NE PREND PLUS EN COMPTE L'ETAT DE CONTRAINTES CORRESPONDANT :
C ======================================================================
               IF (ZI(JSNCY1+ICHEF2-1).EQ.0) GO TO 30
C ======================================================================
C ---    NOM DU CHAMP DE CONTRAINTES ASSOCIE AU NUMERO D'ORDRE COURANT
C ======================================================================
               NCH19 = ZK24(JCHEF+ICHEF2-1)(1:19)
C ======================================================================
C ---      RECUPERATION DE L'INSTANT DU CHAMP DE CONTRAINTES COURANT :
C ======================================================================
               TEMP2 = ZR(JINST+ICHEF2-1)
C ======================================================================
C ---       EXTRACTION DU CHAMP DE CONTRAINTES AUX EXTREMITES DU CHEMIN
C ---       --> SP2O ET SP2E ET CALCUL DES CONTRAINTES LINEARISEES SN2O
C ---       ET SN2E EN CES MEMES NOEUDS :
C ======================================================================
               CALL PRSNSP ( MCF, IOCC, TYPCO, COURBE, XNOMCP, XNUMCP,
     +                       NLSMAC, NLSNAC, NCH19, SM,
     +                       SN2O, SN2E, SNFL, SP2O, SP2E, ICHEF2 )
C ======================================================================
C ---      COMBINAISON DES CONTRAINTES AUX 2 INSTANTS TEMP1 ET TEMP2 :
C ======================================================================
               DO 40 I = 1 , 6 
                  SN12O(I) = SN1O(I) - SN2O(I)
                  SN12E(I) = SN1E(I) - SN2E(I)
                  SP12O(I) = SP1O(I) - SP2O(I)
                  SP12E(I) = SP1E(I) - SP2E(I)
 40            CONTINUE
C ======================================================================
C ---      CALCUL DES VALEURS PROPRES DE LA DIFFERENCE DES TENSEURS 
C ---      DE CONTRAINTES LINEARISEES 
C ---      SN12O = SNO(TEMP1)-SNO(TEMP2) A L'ORIGINE DU CHEMIN :
C ---  CALCUL DE LA DIFFERENCE SUP SNO DES VALEURS PROPRES ( LE TRESCA)
C ======================================================================
               CALL FGEQUI ( SN12O, 'SIGM', 3, EQUI )
               SNO = EQUI(2)
C ======================================================================
C ---      CALCUL DES VALEURS PROPRES DE LA DIFFERENCE DES TENSEURS 
C ---      DE CONTRAINTES LINEARISEES 
C ---      SN12E = SNE(TEMP1)-SNE(TEMP2) A L'AUTRE EXTREMITE DU CHEMIN :
C ---   CALCUL DE LA DIFFERENCE SUP SNE DES VALEURS PROPRES (LE TRESCA)
C ======================================================================
               CALL FGEQUI ( SN12E, 'SIGM', 3, EQUI )
               SNE = EQUI(2)
C ======================================================================
C ---      CALCUL DES VALEURS PROPRES DE LA DIFFERENCE DES TENSEURS 
C ---      DE CONTRAINTES SP12O = SPO(TEMP1)-SPO(TEMP2)
C ---      A L'ORIGINE DU CHEMIN :
C ---   CALCUL DE LA DIFFERENCE SUP SPO DES VALEURS PROPRES (LE TRESCA)
C ======================================================================
               CALL FGEQUI ( SP12O, 'SIGM', 3, EQUI )
               SPO = EQUI(2)
C ======================================================================
C ---      CALCUL DES VALEURS PROPRES DE LA DIFFERENCE DES TENSEURS 
C ---      DE CONTRAINTES SP12E = SPE(TEMP1)-SPE(TEMP2)
C ---      A L'AUTRE EXTREMITE DU CHEMIN :
C ---   CALCUL DE LA DIFFERENCE SUP SPE DES VALEURS PROPRES (LE TRESCA)
C ======================================================================
               CALL FGEQUI ( SP12E, 'SIGM', 3, EQUI )
               SPE = EQUI(2)
C ======================================================================
C ---      ECRITURE DANS LES TABLES :
C ======================================================================
               INDINS = INDINS + 1
               ZK8(IDOCC1+INDINS-1) = RESMEC
               ZK8(IDOCC2+INDINS-1) = RESMEC
               ZR(IDTEM1+INDINS-1) = TEMP1
               ZR(IDTEM2+INDINS-1) = TEMP2
               ZR(IDSPO+INDINS-1)  = SPO
               ZR(IDSPE+INDINS-1)  = SPE
               ZR(IDSNO+INDINS-1)  = SNO
               ZR(IDSNE+INDINS-1)  = SNE
               ZI(IDNBC1+INDINS-1) = ZI(JSNCY1+ICHEF1-1)
               ZI(IDNBC2+INDINS-1) = ZI(JSNCY1+ICHEF1-1)
               ZI(IDNBC3+INDINS-1) = ZI(JSNCY1+ICHEF2-1)
               ZI(IDNBC4+INDINS-1) = ZI(JSNCY1+ICHEF2-1)
C ======================================================================
C ---      ECRITURE DANS LA TABLE TEMPORAIRE :
C ======================================================================
               INOM(1) = INDINS
               INOM(2) = IOCC
               INOM(3) = IOCC
               RNOM(1) = SNO
               RNOM(2) = SNE
               CALL TBAJLI (SNMAX,5,NOMTAB(1),INOM,RNOM,C16B,K8B,0)
 30         CONTINUE
            IF ( NBCHEF .EQ. 1 ) THEN
               INDINS = INDINS + 1
               ZK8(IDOCC1+INDINS-1) = RESMEC
               ZK8(IDOCC2+INDINS-1) = ' '
               ZR(IDTEM1+INDINS-1) = TEMP1
               ZR(IDTEM2+INDINS-1) = TEMP2
               ZR(IDSPO+INDINS-1)  = SPO
               ZR(IDSPE+INDINS-1)  = SPE
               ZR(IDSNO+INDINS-1)  = SNO
               ZR(IDSNE+INDINS-1)  = SNE
               ZI(IDNBC1+INDINS-1) = ZI(JSNCY1)
               ZI(IDNBC2+INDINS-1) = ZI(JSNCY1)
               ZI(IDNBC3+INDINS-1) = ZI(JSNCY1)
               ZI(IDNBC4+INDINS-1) = ZI(JSNCY1)
C ======================================================================
C ---      ECRITURE DANS LA TABLE TEMPORAIRE :
C ======================================================================
               INOM(1) = INDINS
               INOM(2) = IOCC
               INOM(3) = IOCC
               RNOM(1) = SNO
               RNOM(2) = SNE
               CALL TBAJLI (SNMAX,5,NOMTAB(1),INOM,RNOM,C16B,K8B,0)
            ENDIF
C ======================================================================
C ---       BOUCLE SUR LES TRANSITOIRES SUIVANT LE TRANSITOIRE COURANT :
C ======================================================================
            DO 50 IOCC2 = IOCC+1, NBOCC 
              CALL GETVID ( MCF, 'RESULTAT',  IOCC2,1,1, RESME2, N1 )
              CALL JEVEUO (JEXNUM(NCHEFF//'.LSCHEFF',IOCC2),'L',JCHEF2)
              CALL JEVEUO (JEXNUM(NCHEFF//'.VALACCE',IOCC2),'L',JINST2)
C ======================================================================
C ---         RECUPERATION DU NOMBRE DE CYCLES DU TRANSITOIRE COURANT :
C ======================================================================
              CALL JEVEUO(JEXNUM(NCHEFF//'.NBCYCLE',IOCC2),'L',JSNCY2)
C ======================================================================
C ---         RECUPERATION DU NOMBRE D'INSTANTS DU TRANSITOIRE COURANT :
C ======================================================================
              CALL JELIRA (JEXNUM(NCHEFF//'.LSCHEFF',IOCC2),'LONMAX',
     +                                                   NBCHE2,K8B)
C ======================================================================
C ---         BOUCLE SUR LES INSTANTS DU TRANSITOIRE COURANT :  
C ======================================================================
              DO 60 ICHEF2 = 1 , NBCHE2
C ======================================================================
C ---  SI LE NOMBRE DE CYCLES ASSOCIE A L'INSTANT COURANT EST NUL ON NE
C ---  PREND PLUS EN COMPTE L'ETAT DE CONTRAINTES CORRESPONDANT :
C ======================================================================
               IF (ZI(JSNCY2+ICHEF2-1).EQ.0) GO TO 60
C ======================================================================
C ---        NOM DU CHAMP DE CONTRAINTES ASSOCIE AU NUMERO D'ORDRE  
C ---        COURANT DU TRANSITOIRE :
C ======================================================================
               NCH19 = ZK24(JCHEF2+ICHEF2-1)(1:19)
C ======================================================================
C ---        RECUPERATION DE L'INSTANT DU CHAMP DE CONTRAINTES COURANT :
C ======================================================================
               TEMP2 = ZR(JINST2+ICHEF2-1)
C ======================================================================
C ---  EXTRACTION DU CHAMP DE CONTRAINTES AUX EXTREMITES DU CHEMIN
C ---  --> SP2O ET SP2E ET CALCUL DES CONTRAINTES LINEARISEES SN2O ET
C ---  SN2E EN CES MEMES NOEUDS :
C ======================================================================
               CALL PRSNSP ( MCF, IOCC2, TYPCO, COURBE, XNOMCP, XNUMCP,
     +                       NLSMAC, NLSNAC, NCH19, SM,
     +                       SN2O, SN2E, SNFL, SP2O, SP2E, ICHEF2 )
C ======================================================================
C ---       COMBINAISON DES CONTRAINTES AUX 2 INSTANTS TEMP1 ET TEMP2 
C ---       RESPECTIVEMENT DES TRANSITOIRES IOCC ET IOCC2 :
C ======================================================================
               DO 70 I = 1 , 6 , 1
                  SN12O(I) = SN1O(I) - SN2O(I)
                  SN12E(I) = SN1E(I) - SN2E(I)
                  SP12O(I) = SP1O(I) - SP2O(I)
                  SP12E(I) = SP1E(I) - SP2E(I)
 70           CONTINUE
C ======================================================================
C ---      CALCUL DES VALEURS PROPRES DE LA DIFFERENCE DES TENSEURS 
C ---      DE CONTRAINTES LINEARISEES 
C ---      SN12O = SNO(TEMP1)-SNO(TEMP2) A L'ORIGINE DU CHEMIN :
C ---   CALCUL DE LA DIFFERENCE SUP SNO DES VALEURS PROPRES (LE TRESCA)
C ======================================================================
               CALL FGEQUI ( SN12O, 'SIGM', 3, EQUI )
               SNO = EQUI(2)
C ======================================================================
C ---      CALCUL DES VALEURS PROPRES DE LA DIFFERENCE DES TENSEURS 
C ---      DE CONTRAINTES LINEARISEES 
C ---      SN12E = SNE(TEMP1)-SNE(TEMP2) A L'AUTRE EXTREMITE DU CHEMIN :
C ---   CALCUL DE LA DIFFERENCE SUP SNO DES VALEURS PROPRES (LE TRESCA)
C ======================================================================
               CALL FGEQUI ( SN12E, 'SIGM', 3, EQUI )
               SNE = EQUI(2)
C ======================================================================
C ---      CALCUL DES VALEURS PROPRES DE LA DIFFERENCE DES TENSEURS
C ---      DE CONTRAINTES SP12O = SPO(TEMP1)-SPO(TEMP2)
C ---      A L'ORIGINE DU CHEMIN :
C ---   CALCUL DE LA DIFFERENCE SUP SNO DES VALEURS PROPRES (LE TRESCA)
C ======================================================================
               CALL FGEQUI ( SP12O, 'SIGM', 3, EQUI )
               SPO = EQUI(2)
C ======================================================================
C ---      CALCUL DES VALEURS PROPRES DE LA DIFFERENCE DES TENSEURS DE 
C ---      CONTRAINTES SP12E = SPE(TEMP1)-SPE(TEMP2)
C ---      A L'AUTRE EXTREMITE DU CHEMIN :
C ---   CALCUL DE LA DIFFERENCE SUP SNO DES VALEURS PROPRES (LE TRESCA)
C ======================================================================
               CALL FGEQUI ( SP12E, 'SIGM', 3, EQUI )
               SPE = EQUI(2)
C ======================================================================
C ---      ECRITURE DANS LES TABLES :
C ======================================================================
              INDINS = INDINS + 1
              ZK8(IDOCC1+INDINS-1) = RESMEC
              ZK8(IDOCC2+INDINS-1) = RESME2
              ZR(IDTEM1+INDINS-1) = TEMP1
              ZR(IDTEM2+INDINS-1) = TEMP2
              ZR(IDSPO+INDINS-1)  = SPO
              ZR(IDSPE+INDINS-1)  = SPE
              ZR(IDSNO+INDINS-1)  = SNO
              ZR(IDSNE+INDINS-1)  = SNE
              ZI(IDNBC1+INDINS-1) = ZI(JSNCY1+ICHEF1-1)
              ZI(IDNBC2+INDINS-1) = ZI(JSNCY2+ICHEF1-1)
              ZI(IDNBC3+INDINS-1) = ZI(JSNCY1+ICHEF2-1)
              ZI(IDNBC4+INDINS-1) = ZI(JSNCY2+ICHEF2-1)
C ======================================================================
C ---      ECRITURE DANS LA TABLE TEMPORAIRE :
C ======================================================================
               INOM(1) = INDINS
               INOM(2) = IOCC
               INOM(3) = IOCC2
               RNOM(1) = SNO
               RNOM(2) = SNE
               CALL TBAJLI (SNMAX,5,NOMTAB,INOM,RNOM,C16B,K8B,0)
 60           CONTINUE
 50         CONTINUE
            CALL JEEXIN(NLSMAC,N1)
            IF ( N1 .NE. 0 ) CALL JEDETR(NLSMAC)
 20      CONTINUE
 10   CONTINUE
      CALL RCSNMA(SNMAX,NBOCC)
      CALL DETRSD('TABLE', SNMAX)
 9999 CONTINUE
C ======================================================================
      CALL JEDEMA()
      END
