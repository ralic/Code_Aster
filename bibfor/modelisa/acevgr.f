      SUBROUTINE ACEVGR(NBOCC,NLM,NLG,IER)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 01/03/2000   AUTEUR CIBHHPD P.DAVID 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           NBOCC,NLM,NLG,IER
C ----------------------------------------------------------------------

C ----------------------------------------------------------------------
C     AFFE_CARA_ELEM
C     VERIFICATION DES MOTS CLES POUR ASSE_GRIL
C ----------------------------------------------------------------------
C IN  : NBOCC  : NOMBRE D'OCCURENCE
C OUT : NLM    : NOMBRE TOTAL DE MAILLE
C OUT : NLG    : NOMBRE TOTAL DE GROUPE DE MAILLE
C ----------------------------------------------------------------------
C     NCD    : NOMBRE D'ARGUMENTS ADMIS POUR "CARA"
C ----------------------------------------------------------------------
      PARAMETER  ( NBCAR = 3 , NBVAL = 20 , NCD = 3 )
      INTEGER       NVALDI(NCD),INDICE(NCD)
      REAL*8        R8B, VAL(NBVAL)
      CHARACTER*6   KIOC, KI
      CHARACTER*10  K10B, CAR(NBCAR), CARDIS(NCD), NOMU
      CHARACTER*16  REP, TOU, CONCEP, CMD
      DATA CARDIS /'K_TR_D_N  ','K_TR_D_L_T','K_TR_D_L_N'/
      DATA NVALDI /6,6,6/
C     ------------------------------------------------------------------
      CALL GETRES(NOMU,CONCEP,CMD)
C
      NLM = 0
      NLG = 0
      DO 10 IOC = 1,NBOCC
         CALL CODENT(IOC,'G',KIOC)
         CALL GETVID('ASSE_GRIL','GROUP_MA' ,IOC,1,0    ,K10B ,NG)
         CALL GETVID('ASSE_GRIL','MAILLE'   ,IOC,1,0    ,K10B ,NM)
         CALL GETVTX('ASSE_GRIL','CARA'     ,IOC,1,0    ,K10B ,NC)
         CALL GETVTX('ASSE_GRIL','CARA'     ,IOC,1,NBCAR,CAR ,NCAR)
         CALL GETVR8('ASSE_GRIL','VALE'     ,IOC,1,0    ,R8B ,NV)
         CALL GETVR8('ASSE_GRIL','VALE'     ,IOC,1,NBVAL,VAL ,NVAL)
C
C -- IOC = 1
         IF (IOC.EQ.1) THEN
            IF (NV.EQ.0) THEN
               CALL UTMESS('E',CMD,'ASSE_GRIL : OCCURENCE 1 : '//
     +                              'LE MOT CLE "VALE" EST OBLIGATOIRE')
               IER = IER + 1
            ENDIF
            IF (NC.EQ.0) THEN
               CALL UTMESS('E',CMD,'ASSE_GRIL : OCCURENCE 1 : '//
     +                            'LE MOT CLE "CARA" EST OBLIGATOIRE')
               IER = IER + 1
            ENDIF
         ENDIF
C
C -- CARA
         IF (NCAR.GT.0) THEN
            IF (NVAL.EQ.0) THEN
             CALL UTMESS('E',CMD,'ASSE_GRIL : OCCURENCE '//KIOC//' : '//
     +       'PRESENCE DE "VALE" OBLIGATOIRE SI "CARA" EST PRESENT')
             IER = IER + 1
            ENDIF
            IF (NCAR.GT.3) THEN
               CALL UTMESS('E',CMD,'ASSE_GRIL : OCCURENCE '//KIOC//
     +                                 ' : "CARA" : 3 ARGUMENTS MAXI')
               IER = IER + 1
            ENDIF
            DO 20 I = 1, NCD
               INDICE(I) = 0
 20         CONTINUE
            NBV  = 0
            DO 30 I = 1 , NCAR
               CALL CODENT(I,'G',KI)
               NUDI = 0
               DO 40 J = 1 , NCD
                  IF (CAR(I).EQ.CARDIS(J)) THEN
                     NUDI = J
                     IF (INDICE(J).EQ.1) THEN
                         CALL UTMESS('E',CMD,'ASSE_GRIL : OCCURENCE '//
     +                      KIOC//' : ARGUMENT '//KI//' DE "CARA"'//
     +                    ' : MOT CLE MATRICE RIGIDITE DEJA PRESENT')
                         IER = IER + 1
                     ELSE
                         INDICE(J) = 1
                     ENDIF
                  ENDIF
 40            CONTINUE
               NBV = NBV + NVALDI(NUDI)
 30         CONTINUE
         ENDIF
C
C -- VALE
         IF (NVAL.GT.0) THEN
            IF (NVAL.NE.NBV) THEN
               CALL CODENT(NBV,'G',KI)
               CALL UTMESS('E',CMD,'ASSE_GRIL : OCCURENCE '//KIOC//
     +            ' : "VALE" : NOMBRE DE VALEURS ENTREES INCORRECT :'//
     +                                              ' IL EN FAUT '//KI)
               IER = IER + 1
            ENDIF
         ENDIF
C
C -- GROUP_MA + MAILLE
         NSOM = NG + NM
         IF (NSOM.EQ.NG .OR. NSOM.EQ.NM ) THEN
            NLM = MAX(NLM,-NM)
            NLG = MAX(NLG,-NG)
         ENDIF
C
 10   CONTINUE
C
      END
