      SUBROUTINE CTCRTB (NOMTB,TYCH,RESU,NKCHA,TYPAC,TOUCMP,NBCMP,NBVAL,
     &                   NKCMP,NDIM)
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      INTEGER      NBCMP,NDIM,NBVAL
      CHARACTER*4  TYCH
      CHARACTER*8  NOMTB,TYPAC,RESU
      CHARACTER*24 NKCHA,NKCMP
      LOGICAL      TOUCMP
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 12/02/2013   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ----- OPERATEUR CREA_TABLE , MOT-CLE FACTEUR RESU   --------------
C
C        BUT : CREATION DE LA TABLE
C
C        IN     : TYCH   (K4)  : TYPE DE CHAMP (=NOEU,ELNO,ELGA)
C                 NKCHA (K24)  : OBJET DES NOMS DE CHAMP
C                 RESU  (K8)   : NOM DU RESULTAT (SI RESULTAT,SINON ' ')
C                 NKCMP  (K24) : OBJET DES NOMS DE COMPOSANTES
C                 TOUCMP (L)   : INDIQUE SI TOUT_CMP EST RENSEIGNE
C                 NBCMP (I)    : NOMBRE DE COMPOSANTES LORSQUE
C                                NOM_CMP EST RENSEIGNE, 0 SINON
C                 TYPAC (K8)   : ACCES (ORDRE,MODE,FREQ,INST)
C                 NBVAL (I)    : NOMBRE DE VALEURS D'ACCES
C                 NDIM  (I)    : DIMENSION GEOMETRIQUE
C        IN/OUT : NOMTB (K24)  : OBJET TABLE
C
C ----------------------------------------------------------------------

      INTEGER NBPARA,N,JKCHA,JCNSD,JCNSC,JCESD,JCESC,JPARAK,JTYPEK
      INTEGER KK,I,J,JCMP,IRET
      CHARACTER*19 CHAMNS,CHAMES
C     ------------------------------------------------------------------

      CALL JEMARQ()
      CALL ASSERT(TYCH(1:2).EQ.'EL'.OR.TYCH.EQ.'CART'.OR.TYCH.EQ.'NOEU')
C
C --- 0. INITIALISATION
C     -----------------
      CHAMNS='&&CTCRTB.CHAM_NO_S'
      CHAMES='&&CTCRTB.CHAM_EL_S'
      CALL JEVEUO(NKCMP,'L',JCMP)


C     ----------------------------------------------------
C --- 1. DETERMINATION DU NOMBRE DE PARAMETRES DE LA TABLE
C     ----------------------------------------------------
      KK=0
      IF(RESU.EQ.' ')THEN
        KK=KK+1
      ELSE
        KK=KK+2
      ENDIF

      IF(RESU.NE.' ')THEN
        IF(TYPAC.NE.'ORDRE')THEN
          KK=KK+1
        ENDIF
        KK=KK+1
      ENDIF

      IF(TYCH.EQ.'NOEU')THEN
C        -- NOEUD
         KK=KK+1
      ENDIF

      IF(TYCH.EQ.'ELNO')THEN
C        -- MAILLE + NOEUD + SOUS_POINT
         KK=KK+3
      ENDIF

      IF(TYCH.EQ.'ELGA')THEN
C        -- MAILLE + POINT + SOUS_POINT
         KK=KK+3
      ENDIF

      IF(TYCH.EQ.'ELEM')THEN
C        -- MAILLE + SOUS_POINT
         KK=KK+2
      ENDIF

      IF(TYCH.EQ.'CART')THEN
C        -- MAILLE
         KK=KK+1
      ENDIF

C     -- COOR_X, ...
      KK=KK+1
      IF(NDIM.GE.2)THEN
         KK=KK+1
      ENDIF
      IF(NDIM.EQ.3)THEN
         KK=KK+1
      ENDIF

C     -- CMPS :
      N=NBCMP
      CALL JEVEUO(NKCHA,'L',JKCHA)
C     -- JE NE COMPRENDS PAS LA BOUCLE I=1,NBVAL (J. PELLET)
      DO 60 I=1,NBVAL
         IF(ZK24(JKCHA+I-1)(1:18).NE.'&&CHAMP_INEXISTANT')THEN
           IF(TOUCMP)THEN
              IF(TYCH.EQ.'NOEU')THEN
                 CALL CNOCNS(ZK24(JKCHA+I-1),'V',CHAMNS)
                 CALL JEVEUO(CHAMNS//'.CNSD','L',JCNSD)
                 CALL JEVEUO(CHAMNS//'.CNSC','L',JCNSC)
                 N=ZI(JCNSD+1)
              ELSEIF(TYCH(1:2).EQ.'EL')THEN
                 CALL CELCES(ZK24(JKCHA+I-1),'V',CHAMES)
                 CALL JEVEUO(CHAMES//'.CESD','L',JCESD)
                 CALL JEVEUO(CHAMES//'.CESC','L',JCESC)
                 N=ZI(JCESD+1)
              ELSEIF(TYCH.EQ.'CART')THEN
                 CALL CARCES(ZK24(JKCHA+I-1),'ELEM',' ','V',CHAMES,
     &                       ' ',IRET)
                 CALL ASSERT(IRET.EQ.0)
                 CALL JEVEUO(CHAMES//'.CESD','L',JCESD)
                 CALL JEVEUO(CHAMES//'.CESC','L',JCESC)
                 N=ZI(JCESD+1)
              ELSE
                 CALL ASSERT(.FALSE.)
              ENDIF
           ENDIF
         ENDIF
 60   CONTINUE
      KK=KK+N

      NBPARA=KK


C    ------------------------------------------------------------------
C --- 2. DETERMINATION DES NOMS ET DES TYPES DES PARAMETRES DE LA TABLE
C        DE LA TABLE
C     ------------------------------------------------------------------
      CALL WKVECT('&&CTCRTB.TABLE_PARAK','V V K16',NBPARA,JPARAK)
      CALL WKVECT('&&CTCRTB.TABLE_TYPEK','V V K8',NBPARA,JTYPEK)

      KK=0
      IF(RESU.EQ.' ')THEN
        ZK16(JPARAK+KK)='CHAM_GD'
        ZK8(JTYPEK+KK)='K8'
        KK=KK+1
      ELSE
        ZK16(JPARAK+KK)='RESULTAT'
        ZK8(JTYPEK+KK)='K8'
        KK=KK+1
        ZK16(JPARAK+KK)='NOM_CHAM'
        ZK8(JTYPEK+KK)='K16'
        KK=KK+1
      ENDIF

      IF(RESU.NE.' ')THEN
        IF(TYPAC.NE.'ORDRE')THEN
          ZK16(JPARAK+KK)=TYPAC
          ZK8(JTYPEK+KK)='R'
          IF(TYPAC.EQ.'MODE')ZK8(JTYPEK+KK)='I'
          KK=KK+1
        ENDIF
        ZK16(JPARAK+KK)='NUME_ORDRE'
        ZK8(JTYPEK+KK)='I'
        KK=KK+1
      ENDIF

      IF(TYCH(1:2).EQ.'EL'.OR.TYCH.EQ.'CART')THEN
         ZK16(JPARAK+KK)='MAILLE'
         ZK8(JTYPEK+KK)='K8'
         KK=KK+1
      ENDIF
      IF(TYCH.EQ.'ELNO' .OR. TYCH.EQ.'NOEU')THEN
         ZK16(JPARAK+KK)='NOEUD'
         ZK8(JTYPEK+KK)='K8'
         KK=KK+1
      ELSEIF(TYCH.EQ.'ELGA')THEN
         ZK16(JPARAK+KK)='POINT'
         ZK8(JTYPEK+KK)='I'
         KK=KK+1
      ENDIF
      IF(TYCH(1:2).EQ.'EL')THEN
C        -- TOUS LES CHAMPS ELXX PEUVENT AVOIR DES SOUS_POINT :
         ZK16(JPARAK+KK)='SOUS_POINT'
         ZK8(JTYPEK+KK)='I'
         KK=KK+1
      ENDIF

      ZK16(JPARAK+KK)='COOR_X'
      ZK8(JTYPEK+KK)='R'
      KK=KK+1
      IF(NDIM.GE.2)THEN
         ZK16(JPARAK+KK)='COOR_Y'
         ZK8(JTYPEK+KK)='R'
         KK=KK+1
      ENDIF
      IF(NDIM.EQ.3)THEN
         ZK16(JPARAK+KK)='COOR_Z'
         ZK8(JTYPEK+KK)='R'
         KK=KK+1
      ENDIF
      IF(TOUCMP)THEN
          IF(TYCH.EQ.'NOEU')THEN
             DO 90 J=1,N
                ZK16(JPARAK+KK)=ZK8(JCNSC+J-1)
                ZK8(JTYPEK+KK)='R'
                KK=KK+1
 90          CONTINUE
          ELSE IF(TYCH(1:2).EQ.'EL'.OR.TYCH.EQ.'CART')THEN
             DO 91 J=1,N
                ZK16(JPARAK+KK)=ZK8(JCESC+J-1)
                ZK8(JTYPEK+KK)='R'

                KK=KK+1
 91          CONTINUE
          ENDIF
      ELSE
          DO 95 J=1,N
              ZK16(JPARAK+KK)=ZK8(JCMP+J-1)
              ZK8(JTYPEK+KK)='R'
              KK=KK+1
 95       CONTINUE
      ENDIF

C    ------------------------------------------------------------------
C --- 3. CREATION DE LA TABLE
C     ------------------------------------------------------------------
      CALL TBCRSV(NOMTB,'G',NBPARA,ZK16(JPARAK),ZK8(JTYPEK),0)


      CALL JEDETR('&&CTCRTB.TABLE_PARAK')
      CALL JEDETR('&&CTCRTB.TABLE_TYPEK')

      CALL JEDEMA()

      END
