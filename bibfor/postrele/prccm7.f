      SUBROUTINE PRCCM7 ( MCF, NBOCC, TYPCO, COURBE, SM, XNOMCP, 
     +                    XNUMCP, NCHEFF, RCCMPM, RCCMSN, FATISP, 
     +                    SNTHER, NCHEFT, NOMA, NLSNAC )
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
      IMPLICIT   NONE
      INTEGER             NBOCC
      REAL*8              SM
      LOGICAL             RCCMPM, RCCMSN, FATISP, SNTHER
      CHARACTER*8         TYPCO, COURBE, NOMA
      CHARACTER*16        NCHEFF, NCHEFT
      CHARACTER*24        XNUMCP, XNOMCP, NLSNAC
      CHARACTER*(*)       MCF
C     ------------------------------------------------------------------
C MODIF POSTRELE  DATE 26/11/2002   AUTEUR CIBHHLV L.VIVAN 
C
C     POST_RCCM: CALCUL DU PMPB, SN ET SP  POUR CHAQUE TRANSITOIRE
C     ------------------------------------------------------------------
C
C     OPERATEUR POST_RCCM: CALCUL DU PMPB, DU SN, DU SP
C
C     ------------------------------------------------------------------
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
C
      INTEGER      IDSNO, IDSPO, IDSNE, IDSPE, INDINS, JINST, I, IDSNOE,
     +             IDTEM1, IDTEM2, IOCC, ICHEF1, ICHEF2, IDSNEE, 
     +             NBCHEF, JCHEF, JCHET, JPM, JPB, JPMPO, JPMPE, N1,
     +             JPMO1, JPMO2, JPMO3, JPMO4, JPMO5, JPMO6,
     +             JPME1, JPME2, JPME3, JPME4, JPME5, JPME6
      REAL*8       SN1O(6), SN1E(6), SN2O(6), SN2E(6),SN12O(6),SN12E(6),
     +             SP1O(6), SP1E(6), SP2O(6), SP2E(6),SP12O(6),SP12E(6),
     +             TEMP1, TEMP2, PMPB(4), SNO, SNE, SPO, SPE,
     +             SNT1O(6), SNT1E(6), SNT2O(6), SNT2E(6), SNT12O(6),
     +             SNT12E(6), SNTO, SNTE, SPT1O(6), SPT1E(6), SPT2O(6),
     +             SPT2E(6), TOMAX(6), TEMAX(6),
     +             SNFL1E(6), SNFL2E(6), EQUI(6)
      CHARACTER*8  K8B
      CHARACTER*19 NCH19
      CHARACTER*24 NLSMAC
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      NLSMAC = '&&PRCCM1.MAILLES.ACTIVES'
C
C
C --- BOUCLE SUR LES TRANSITOIRES :
C     ---------------------------
      DO 10 IOCC = 1, NBOCC
C
         INDINS = 0
C
         CALL JELIRA (JEXNUM(NCHEFF//'.LSCHEFF',IOCC),'LONMAX',
     +                                                   NBCHEF,K8B)
         CALL JEVEUO (JEXNUM(NCHEFF//'.LSCHEFF',IOCC),'L',JCHEF)
         CALL JEVEUO (JEXNUM(NCHEFF//'.VALACCE',IOCC),'L',JINST)
         IF ( RCCMPM ) THEN
            CALL JEVEUO (JEXNUM(NCHEFF//'.VALPM  ',IOCC),'E',JPM  )
            CALL JEVEUO (JEXNUM(NCHEFF//'.VALPB  ',IOCC),'E',JPB  )
            CALL JEVEUO (JEXNUM(NCHEFF//'.VALPMPO',IOCC),'E',JPMPO)
            CALL JEVEUO (JEXNUM(NCHEFF//'.VALPMPE',IOCC),'E',JPMPE)
            CALL JEVEUO (JEXNUM(NCHEFF//'.VPMPBO1',IOCC),'E',JPMO1)
            CALL JEVEUO (JEXNUM(NCHEFF//'.VPMPBO2',IOCC),'E',JPMO2)
            CALL JEVEUO (JEXNUM(NCHEFF//'.VPMPBO3',IOCC),'E',JPMO3)
            CALL JEVEUO (JEXNUM(NCHEFF//'.VPMPBO4',IOCC),'E',JPMO4)
            CALL JEVEUO (JEXNUM(NCHEFF//'.VPMPBO5',IOCC),'E',JPMO5)
            CALL JEVEUO (JEXNUM(NCHEFF//'.VPMPBO6',IOCC),'E',JPMO6)
            CALL JEVEUO (JEXNUM(NCHEFF//'.VPMPBE1',IOCC),'E',JPME1)
            CALL JEVEUO (JEXNUM(NCHEFF//'.VPMPBE2',IOCC),'E',JPME2)
            CALL JEVEUO (JEXNUM(NCHEFF//'.VPMPBE3',IOCC),'E',JPME3)
            CALL JEVEUO (JEXNUM(NCHEFF//'.VPMPBE4',IOCC),'E',JPME4)
            CALL JEVEUO (JEXNUM(NCHEFF//'.VPMPBE5',IOCC),'E',JPME5)
            CALL JEVEUO (JEXNUM(NCHEFF//'.VPMPBE6',IOCC),'E',JPME6)
         ENDIF
         IF ( RCCMSN .OR. FATISP ) THEN
            CALL JEVEUO (JEXNUM(NCHEFF//'.INST1  ',IOCC),'E',IDTEM1)
            CALL JEVEUO (JEXNUM(NCHEFF//'.INST2  ',IOCC),'E',IDTEM2)
            CALL JEVEUO (JEXNUM(NCHEFF//'.VALSNO ',IOCC),'E',IDSNO )
            CALL JEVEUO (JEXNUM(NCHEFF//'.VALSNE ',IOCC),'E',IDSNE )
            CALL JEVEUO (JEXNUM(NCHEFF//'.VALSPO ',IOCC),'E',IDSPO )
            CALL JEVEUO (JEXNUM(NCHEFF//'.VALSPE ',IOCC),'E',IDSPE )
         ENDIF
         IF ( SNTHER ) THEN
            CALL JEVEUO (JEXNUM(NCHEFT//'.LSCHEFF',IOCC),'L',JCHET)
            CALL JEVEUO (JEXNUM(NCHEFF//'.VALSNTO',IOCC),'E',IDSNOE)
            CALL JEVEUO (JEXNUM(NCHEFF//'.VALSNTE',IOCC),'E',IDSNEE)
         ENDIF
C
C ---  BOUCLE SUR LES INDICES DES NUMEROS D'ORDRE DU TRANSITOIRE 
C ---  COURANT :
C      -------
         DO 20 ICHEF1 = 1 , NBCHEF 
C
C ---    NOM DU CHAMP DE CONTRAINTES ASSOCIE AU NUMERO D'ORDRE  
C ---    COURANT DU TRANSITOIRE :
C        ----------------------
            NCH19 = ZK24(JCHEF+ICHEF1-1)(1:19)
C
C ---    RECUPERATION DU CHEMIN ET CONSTRUCTION DE LA LISTE DE MAILLES
C ---    DE NOM NLSMAC CONSTITUANT CE CHEMIN :
C        -----------------------------------
            CALL PROUEX ( TYPCO, COURBE, NCH19, NLSMAC, NLSNAC, NOMA )
C
C ---    RECUPERATION DE L'INSTANT DU CHAMP DE CONTRAINTES COURANT :
C        ---------------------------------------------------------
            TEMP1 = ZR(JINST+ICHEF1-1)
            TEMP2 = 0.0D0
C
C ---    CONTRAINTE EQUIVALENTE PRIMAIRE DE MEMBRANE  PM
C ---    CONTRAINTE EQUIVALENTE PRIMAIRE DE FLEXION   PB
C ---    CONTRAINTE EQUIVALENTE PRIMAIRE DE MEMBRANE+FLEXION  PMPB
C        ---------------------------------------------------------
C
            IF ( RCCMPM ) THEN
               CALL PRPMPB ( MCF, IOCC, XNOMCP, XNUMCP, NLSMAC, NLSNAC,
     +                       NCH19, SM, PMPB, ICHEF1, TYPCO, COURBE,
     +                       TOMAX, TEMAX )
               ZR(JPM  +ICHEF1-1) = PMPB(1)
               ZR(JPB  +ICHEF1-1) = PMPB(2)
               ZR(JPMPO+ICHEF1-1) = PMPB(3)
               ZR(JPMPE+ICHEF1-1) = PMPB(4)
               ZR(JPMO1+ICHEF1-1) = TOMAX(1)
               ZR(JPMO2+ICHEF1-1) = TOMAX(2)
               ZR(JPMO3+ICHEF1-1) = TOMAX(3)
               ZR(JPMO4+ICHEF1-1) = TOMAX(4)
               ZR(JPMO5+ICHEF1-1) = TOMAX(5)
               ZR(JPMO6+ICHEF1-1) = TOMAX(6)
               ZR(JPME1+ICHEF1-1) = TEMAX(1)
               ZR(JPME2+ICHEF1-1) = TEMAX(2)
               ZR(JPME3+ICHEF1-1) = TEMAX(3)
               ZR(JPME4+ICHEF1-1) = TEMAX(4)
               ZR(JPME5+ICHEF1-1) = TEMAX(5)
               ZR(JPME6+ICHEF1-1) = TEMAX(6)
            ENDIF
C
            IF ( .NOT. RCCMSN .AND. .NOT. FATISP ) GOTO 22
C
C ---    EXTRACTION DU CHAMP DE CONTRAINTES AUX EXTREMITES DU CHEMIN
C ---    --> SP1O ET SP1E
C ---    ET CALCUL DES CONTRAINTES LINEARISEES SN1O ET SN1E EN CES 
C ---    MEMES NOEUDS :
C        ------------
            CALL PRSNSP ( MCF, IOCC, TYPCO, COURBE, XNOMCP, XNUMCP,  
     +                    NLSMAC, NLSNAC, NCH19, SM, SN1O, SN1E, 
     +                    SNFL1E, SP1O, SP1E, ICHEF1 )
C
            CALL FGEQUI ( SN1O, 'SIGM', 3, EQUI )
            SNO = EQUI(2)
            CALL FGEQUI ( SN1E, 'SIGM', 3, EQUI )
            SNE = EQUI(2)
            CALL FGEQUI ( SP1O, 'SIGM', 3, EQUI )
            SPO = EQUI(2)
            CALL FGEQUI ( SP1E, 'SIGM', 3, EQUI )
            SPE = EQUI(2)
C
            IF ( SNTHER ) THEN
               NCH19 = ZK24(JCHET+ICHEF1-1)(1:19)
               CALL PRSNSP ( MCF, IOCC, TYPCO, COURBE, XNOMCP, XNUMCP,
     +                       NLSMAC, NLSNAC, NCH19, SM, SNT1O, SNT1E, 
     +                       SNFL1E, SPT1O, SPT1E, ICHEF1+1 )
               DO 24 I = 1 , 6 
                  SNT12O(I) = SN1O(I)-SNT1O(I)
                  SNT12E(I) = SN1E(I)-SNT1E(I)
 24            CONTINUE
               CALL FGEQUI ( SNT12O, 'SIGM', 3, EQUI )
               SNTO = EQUI(2)
               CALL FGEQUI ( SNT12E, 'SIGM', 3, EQUI )
               SNTE = EQUI(2)
            ENDIF
C
C ---    BOUCLE SUR LES INDICES SUIVANT L'INDICE COURANT DES NUMEROS  
C ---    D'ORDRE DU TRANSITOIRE COURANT :
C        ------------------------------
            DO 30 ICHEF2 = ICHEF1+1 , NBCHEF 
C
C ---       NOM DU CHAMP DE CONTRAINTES ASSOCIE AU NUMERO D'ORDRE  
C ---       COURANT DU TRANSITOIRE :
C           ----------------------
               NCH19 = ZK24(JCHEF+ICHEF2-1)(1:19)
C
C ---      RECUPERATION DE L'INSTANT DU CHAMP DE CONTRAINTES COURANT :
C          ---------------------------------------------------------
               TEMP2 = ZR(JINST+ICHEF2-1)
C
C ---       EXTRACTION DU CHAMP DE CONTRAINTES AUX EXTREMITES DU CHEMIN
C ---       --> SP2O ET SP2E
C ---       ET CALCUL DES CONTRAINTES LINEARISEES SN2O ET SN2E EN CES 
C ---       MEMES NOEUDS :
C           ------------
               CALL PRSNSP ( MCF, IOCC, TYPCO, COURBE, XNOMCP, XNUMCP,
     +                       NLSMAC, NLSNAC, NCH19, SM, SN2O, SN2E, 
     +                       SNFL2E, SP2O, SP2E, ICHEF2 )
C
C ---      COMBINAISON DES CONTRAINTES AUX 2 INSTANTS TEMP1 ET TEMP2 :
C          ---------------------------------------------------------
               DO 40 I = 1 , 6 
                  SN12O(I) = SN1O(I) - SN2O(I)
                  SN12E(I) = SN1E(I) - SN2E(I)
                  SP12O(I) = SP1O(I) - SP2O(I)
                  SP12E(I) = SP1E(I) - SP2E(I)
 40            CONTINUE
C
               CALL FGEQUI ( SN12O, 'SIGM', 3, EQUI )
               SNO = EQUI(2)
               CALL FGEQUI ( SN12E, 'SIGM', 3, EQUI )
               SNE = EQUI(2)
               CALL FGEQUI ( SP12O, 'SIGM', 3, EQUI )
               SPO = EQUI(2)
               CALL FGEQUI ( SP12E, 'SIGM', 3, EQUI )
               SPE = EQUI(2)
C
               IF ( SNTHER ) THEN
                  NCH19 = ZK24(JCHET+ICHEF2-1)(1:19)
                  CALL PRSNSP ( MCF, IOCC, TYPCO, COURBE, XNOMCP, 
     +                         XNUMCP, NLSMAC, NLSNAC, NCH19, SM, SNT2O,
     +                          SNT2E, SNFL2E, SPT2O, SPT2E, ICHEF2 )
                  DO 42 I = 1 , 6 
                     SNT12O(I)=(SN1O(I)-SNT1O(I))-(SN2O(I)-SNT2O(I))
                     SNT12E(I)=(SN1E(I)-SNT1E(I))-(SN2E(I)-SNT2E(I))
 42               CONTINUE
                  CALL FGEQUI ( SNT12O, 'SIGM', 3, EQUI )
                  SNTO = EQUI(2)
                  CALL FGEQUI ( SNT12E, 'SIGM', 3, EQUI )
                  SNTE = EQUI(2)
               ENDIF
C
C ---      AFFECTATION DES TABLEAUX CONTENANT LES VALEURS A
C ---      ECRIRE DANS LES TABLES :
C          ----------------------
               INDINS = INDINS + 1
               ZR(IDTEM1+INDINS-1) = TEMP1
               ZR(IDTEM2+INDINS-1) = TEMP2
               ZR(IDSPO+INDINS-1)  = SPO
               ZR(IDSPE+INDINS-1)  = SPE
               ZR(IDSNO+INDINS-1)  = SNO
               ZR(IDSNE+INDINS-1)  = SNE
               IF ( SNTHER ) THEN
                  ZR(IDSNOE+INDINS-1)  = SNTO
                  ZR(IDSNEE+INDINS-1)  = SNTE
               ENDIF
C
 30         CONTINUE
C
            IF ( NBCHEF .EQ. 1 ) THEN
               INDINS = INDINS + 1
               ZR(IDTEM1+INDINS-1) = TEMP1
               ZR(IDTEM2+INDINS-1) = TEMP2
               ZR(IDSPO+INDINS-1)  = SPO
               ZR(IDSPE+INDINS-1)  = SPE
               ZR(IDSNO+INDINS-1)  = SNO
               ZR(IDSNE+INDINS-1)  = SNE
               IF ( SNTHER ) THEN
                  ZR(IDSNOE+INDINS-1)  = SNTO
                  ZR(IDSNEE+INDINS-1)  = SNTE
               ENDIF
            ENDIF
C
 22         CONTINUE
C
            CALL JEEXIN(NLSMAC,N1)
            IF ( N1 .NE. 0 ) CALL JEDETR(NLSMAC)
C
 20      CONTINUE
C
 10   CONTINUE
C
      CALL JEDEMA()
C
      END
