      SUBROUTINE ACEVDI(NBOCC,NOMAZ,NOMOZ,NLM,NLG,NLN,NLJ,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           NBOCC,            NLM,NLG,NLN,NLJ,IER
      CHARACTER*(*)           NOMAZ,NOMOZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 03/08/2001   AUTEUR CIBHHLV L.VIVAN 
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
C ----------------------------------------------------------------------
C     AFFE_CARA_ELEM
C     VERIFICATION DES MOTS CLES POUR L'ELEMENT DISCRET
C ----------------------------------------------------------------------
C IN  : NBOCC  : NOMBRE D'OCCURENCE
C OUT : NLM    : NOMBRE TOTAL DE MAILLE
C OUT : NLG    : NOMBRE TOTAL DE GROUPE DE MAILLE
C ----------------------------------------------------------------------
C     NCD    : NOMBRE D'ARGUMENTS ADMIS POUR "CARA"
C     NRD    : NOMBRE D'ARGUMENTS ADMIS POUR "REPERE"
C     NID    : DIMENSION DU TABLEAU DES ARGUMENTS INCOMPATIBLES
C               SOUS  UN  MEME  MOT  CLE  "CARA"
C     COMDIS : TABLEAU DES ARGUMENTS INCOMPATIBLES SOUS "CARA" :
C               ( A M K N S T TR )
C ----------------------------------------------------------------------
      PARAMETER  ( NBCAR = 100 , NBVAL = 1000               )
      PARAMETER  ( NCD   = 22  , NID = 7     )
      INTEGER       COMDIS(NID), NVALDI(NCD), NVALD2(NCD)
      REAL*8        R8B, VAL(NBVAL)
      CHARACTER*6   KIOC, KI
      CHARACTER*8   K8B, CAR(NBCAR), CARDIS(NCD), NOMU, NOMA, NOMO
      CHARACTER*16  REP, TOU, CONCEP, CMD
      DATA CARDIS /'K_T_D_N ','K_TR_D_N','K_T_D_L ','K_TR_D_L',
     +             'K_T_N   ','K_TR_N  ','K_T_L   ','K_TR_L  ',
     +             'M_T_D_N ','M_TR_D_N','M_T_N   ','M_TR_N  ',
     +             'M_T_L   ','M_TR_L  ','A_T_D_N ','A_TR_D_N',
     +             'A_T_D_L ','A_TR_D_L','A_T_N   ','A_TR_N  ',
     +             'A_T_L   ','A_TR_L  '/
      DATA NVALDI /3,6,3,6,6,21,21,78,1,10,6,21,21,78,3,6,3,6,6,
     +                          21,21,78/
      DATA NVALD2 /2,3,2,3,3,6,10,21,1,4,3,6,10,21,2,3,2,3,3,
     +                          6,10,21/
C     ------------------------------------------------------------------
      CALL GETRES(NOMU,CONCEP,CMD)
C
      NOMA = NOMAZ
      NOMO = NOMOZ
      NLM = 0
      NLG = 0
      NLN = 0
      NLJ = 0
      I3D = 0
      I2D = 0
C
C --- RECUPERATION DE LA DIMENSION DU MAILLAGE
      NDIM = 3
      CALL DISMOI('F','Z_CST',NOMOZ,'MODELE',IBID,K8B,IER)
      IF ( K8B(1:3) .EQ. 'OUI' )  NDIM = 2
C
C --- ON REGARDE SI LE MODELE COMPORTE DES ELEMENTS DISCRETS 3D
      CALL MODEXI(NOMOZ,'DIS_',I3D)
C
C --- ON REGARDE SI LE MODELE COMPORTE DES ELEMENTS DISCRETS 2D
      CALL MODEXI(NOMOZ,'2D_DIS_',I2D)
C
C --- ON INTERDIT SUR UN MAILLAGE 2D D'AVOIR DES ELEMENTS DISCRETS
C --- 2D ET 3D
      IF (I2D.EQ.1.AND.I3D.EQ.1.AND.NDIM.EQ.2) THEN
          CALL UTMESS('E',CMD,'ON INTERDIT D''AVOIR SUR UN MAILLAGE '
     +              //'2D DES ELEMENTS DISCRETS 2D ET 3D .')
          IER = IER + 1
      ENDIF
C
C --- ON INTERDIT SUR UN MAILLAGE 3D D'AVOIR DES ELEMENTS DISCRETS
C --- 2D
      IF (I2D.EQ.1.AND.NDIM.EQ.3) THEN
          CALL UTMESS('E',CMD,'ON INTERDIT D''AVOIR SUR UN MAILLAGE '
     +              //'3D DES ELEMENTS DISCRETS 2D .')
          IER = IER + 1
      ENDIF
C
C
      DO 10 IOC = 1,NBOCC
         CALL CODENT(IOC,'G',KIOC)
         CALL GETVEM(NOMA,'GROUP_MA','DISCRET','GROUP_MA',
     +             IOC,1,0,K8B,NG)
         CALL GETVEM(NOMA,'MAILLE','DISCRET','MAILLE',
     +           IOC,1,0,K8B,NM)
         CALL GETVEM(NOMA,'GROUP_NO','DISCRET','GROUP_NO',
     +             IOC,1,0,K8B,NJ)
         CALL GETVEM(NOMA,'NOEUD','DISCRET','NOEUD',
     +          IOC,1,0,K8B,NN)
         CALL GETVTX('DISCRET','CARA'     ,IOC,1,0    ,K8B ,NC)
         CALL GETVTX('DISCRET','CARA'     ,IOC,1,NBCAR,CAR ,NCAR)
         CALL GETVTX('DISCRET','REPERE'   ,IOC,1,0    ,K8B ,NR)
         CALL GETVTX('DISCRET','REPERE'   ,IOC,1,1    ,REP ,NREP)
         CALL GETVR8('DISCRET','VALE'     ,IOC,1,0    ,R8B ,NV)
         CALL GETVR8('DISCRET','VALE'     ,IOC,1,NBVAL,VAL ,NVAL)
C
C -- IOC = 1
         IF (IOC.EQ.1) THEN
            IF (NV.EQ.0) THEN
               CALL UTMESS('E',CMD,'DISCRET : OCCURENCE 1 : '//
     +                              'LE MOT CLE "VALE" EST OBLIGATOIRE')
               IER = IER + 1
            ENDIF
            IF (NC.EQ.0) THEN
               CALL UTMESS('E',CMD,'DISCRET : OCCURENCE 1 : '//
     +                            'LE MOT CLE "CARA" EST OBLIGATOIRE')
               IER = IER + 1
            ENDIF
         ENDIF
C
C -- CARA
         IF (NCAR.GT.0) THEN
C-DEL       NCARA = NCAR
            IF (NVAL.EQ.0) THEN
               CALL UTMESS('E',CMD,'DISCRET : OCCURENCE '//KIOC//' : '//
     +          'PRESENCE DE "VALE" OBLIGATOIRE SI "CARA" EST PRESENT')
               IER = IER + 1
            ENDIF
            IF (NCAR.GT.3) THEN
               CALL UTMESS('E',CMD,'DISCRET : OCCURENCE '//KIOC//
     +                                 ' : "CARA" : 3 ARGUMENTS MAXI')
               IER = IER + 1
            ENDIF
            DO 20 I = 1 , NID
               COMDIS(I) = 0
 20         CONTINUE
            NBV  = 0
            DO 30 I = 1 , NCAR
               CALL CODENT(I,'G',KI)
               NUDI = 0
               DO 40 J = 1 , NCD
                  IF (CAR(I).EQ.CARDIS(J)) THEN
                     NUDI = J
                     IF (CAR(I)(1:1).EQ.'K') THEN
                        IF (COMDIS(2).EQ.1) THEN
                           CALL UTMESS('E',CMD,'DISCRET : OCCURENCE '//
     +                         KIOC//' : ARGUMENT '//KI//' DE "CARA"'//
     +                       ' : MOT CLE MATRICE RIGIDITE DEJA PRESENT')
                           IER = IER + 1
                        ELSE
                           COMDIS(2) = 1
                        ENDIF
                     ELSEIF (CAR(I)(1:1).EQ.'A') THEN
                        IF (COMDIS(1).EQ.1) THEN
                           CALL UTMESS('E',CMD,'DISCRET : OCCURENCE '//
     +                      KIOC//' : ARGUMENT '//KI//' DE "CARA" : '//
     +                     'MOT CLE MATRICE AMORTISSEMENT DEJA PRESENT')
                           IER = IER + 1
                        ELSE
                           COMDIS(1) = 1
                        ENDIF
                     ELSEIF (CAR(I)(1:1).EQ.'M') THEN
                        IF (COMDIS(3).EQ.1) THEN
                           CALL UTMESS('E',CMD,'DISCRET : OCCURENCE '//
     +                        KIOC//' : ARGUMENT '//KI//' DE "CARA"'//
     +                          ' : MOT CLE MATRICE MASSE DEJA PRESENT')
                           IER = IER + 1
                        ELSE
                           COMDIS(3) = 1
                        ENDIF
                     ENDIF
                     IF (CAR(I)(3:4).EQ.'T_') THEN
                        IF (COMDIS(7).EQ.1) THEN
                           CALL UTMESS('E',CMD,'DISCRET : OCCURENCE '//
     +                      KIOC//' : ARGUMENT '//KI//' DE "CARA" : '//
     +                     'MATRICE (T) INCOMPATIBLE AVEC MATRICE (TR)')
                           IER = IER + 1
                        ELSE
                           COMDIS(6) = 1
                        ENDIF
                     ELSEIF(CAR(I)(3:4).EQ.'TR')THEN
                        IF (COMDIS(6).EQ.1) THEN
                           CALL UTMESS('E',CMD,'DISCRET : OCCURENCE '//
     +                      KIOC//' : ARGUMENT '//KI//' DE "CARA" : '//
     +                     'MATRICE (TR) INCOMPATIBLE AVEC MATRICE (T)')
                           IER = IER + 1
                        ELSE
                           COMDIS(7) = 1
                        ENDIF
                     ENDIF
                     IF (CAR(I)(5:5).EQ.'L'.OR.CAR(I)(6:6).EQ.'L'.OR.
     +                   CAR(I)(7:7).EQ.'L'.OR.CAR(I)(8:8).EQ.'L') THEN
                       IF (COMDIS(4).EQ.1) THEN
                          CALL UTMESS('E',CMD,'DISCRET : OCCURENCE '//
     +                      KIOC//' : ARGUMENT '//KI//' DE "CARA" : '//
     +                      'MATRICE (L) INCOMPATIBLE AVEC MATRICE (N)')
                           IER = IER + 1
                        ELSE
                           COMDIS(5) = 1
                        ENDIF
                     ELSEIF(CAR(I)(5:5).EQ.'N'.OR.CAR(I)(6:6).EQ.'N'.OR.
     +                    CAR(I)(7:7).EQ.'N'.OR.CAR(I)(8:8).EQ.'N') THEN
                        IF (COMDIS(5).EQ.1) THEN
                           CALL UTMESS('E',CMD,'DISCRET : OCCURENCE '//
     +                      KIOC//' : ARGUMENT '//KI//' DE "CARA" : '//
     +                      'MATRICE (N) INCOMPATIBLE AVEC MATRICE (L)')
                           IER = IER + 1
                        ELSE
                           COMDIS(4) = 1
                        ENDIF
                     ENDIF
                  ENDIF
 40            CONTINUE
               IF (I3D.EQ.1) THEN
                 NBV = NBV + NVALDI(NUDI)
               ELSEIF (I2D.EQ.1) THEN
                 NBV = NBV + NVALD2(NUDI)
               ENDIF
 30         CONTINUE
         ENDIF
C
C -- VALE
         IF (NVAL.GT.0) THEN
            IF (NVAL.NE.NBV) THEN
               CALL CODENT(NBV,'G',KI)
               CALL UTMESS('E',CMD,'DISCRET : OCCURENCE '//KIOC//
     +            ' : "VALE" : NOMBRE DE VALEURS ENTREES INCORRECT :'//
     +                                              ' IL EN FAUT '//KI)
               IER = IER + 1
            ENDIF
         ENDIF
C
C -- GROUP_MA + GROUP_NO + MAILLE + NOEUD
         NSOM = NG + NM + NJ + NN
         IF (NSOM.EQ.NG .OR. NSOM.EQ.NM .OR. NSOM.EQ.NJ
     +                                  .OR. NSOM.EQ.NN) THEN
            NLM = MAX(NLM,-NM)
            NLG = MAX(NLG,-NG)
            NLN = MAX(NLN,-NN)
            NLJ = MAX(NLJ,-NJ)
         ENDIF
C
 10   CONTINUE
C
      LMAX2=MAX(1,NLM,NLG,NLN,NLJ)
      CALL ACEVD2(NOMA,NOMO,LMAX2,NBOCC)
      END
