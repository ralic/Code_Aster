      SUBROUTINE AFFDEF(TMP,NOM,NEL,NTEL,TAB,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                       NTEL(*)
      CHARACTER*8           NOM,         TAB(*)
      CHARACTER*24      TMP
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/03/2004   AUTEUR REZETTE C.REZETTE 
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
C       ----------------------------------------------------------------
C        - VERIFICATION DE LA COMPLETUDE DES DONNES OBLIGATOIRES A
C          ENTRER , ET DE LA POSITIVITE DE CERTAINES VALEURS
C        - AFFECTATION DES VALEURS PAR DEFAUT AUX CARACTERISTIQUES
C          GENERALES ET GEOMETRIQUES ABSENTES
C       RQ : NTEL(1) = NUMERO DU TYPE ELEMENT MECA_POU_D_T
C            NTEL(2) = NUMERO DU TYPE ELEMENT MECA_POU_D_E
C            NTEL(4) = NUMERO DU TYPE ELEMENT MECA_POU_C_T
C            NTEL(5) = NUMERO DU TYPE ELEMENT MEFS_POU_D_T
C            NTEL(6) = NUMERO DU TYPE ELEMENT MECA_POU_D_TG
C       ----------------------------------------------------------------
C        TAB  1    2    3    4    5    6    7    8    9    10
C        0    A1   IY1  IZ1  AY1  AZ1  EY1  EZ1  JX1  RY1  RZ1
C        1    RT1  A2   IY2  IZ2  AY2  AZ2  EY2  EZ2  JX2  RY2
C        2    RZ2  RT2  TVAR HY1  HZ1  EPY1 EPZ1 HY2  HZ2  EPY2
C        3    EPZ2 R1   E1   R2   E2   TSEC AI1  AI2  JG1  JG2
C        4    IYR21 IYR22 IZR21 IZR22
C       ----------------------------------------------------------------
C       ----- DEBUT COMMUNS NORMALISES  JEVEUX  ------------------------
        INTEGER            ZI
        COMMON  / IVARJE / ZI(1)
        REAL*8             ZR
        COMMON  / RVARJE / ZR(1)
        COMPLEX*16         ZC
        COMMON  / CVARJE / ZC(1)
        LOGICAL            ZL
        COMMON  / LVARJE / ZL(1)
        CHARACTER*8        ZK8
        CHARACTER*16                ZK16
        CHARACTER*24                          ZK24
        CHARACTER*32                                    ZK32
        CHARACTER*80                                             ZK80
        COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1), ZK80(1)
        CHARACTER*32     JEXNOM,        JEXNUM
C       -----  FIN  COMMUNS NORMALISES  JEVEUX  ------------------------
        PARAMETER       ( NR = 4 ,      NC = 2,    NG = 8 )
        PARAMETER       ( NT = 4 ,      NE =12,    ND = 6 )
        PARAMETER       ( NX = 10,      NY = 8,    NZ = 4 )
        CHARACTER*16    CMD
        REAL*8          R8MAEM, R8PI,   TST,    PI
        INTEGER         OGEN(NG),       OREC(NR),       OCER(NC)
        INTEGER         OTPE(NT)
        INTEGER         DEXC(NE),       DDFX(ND),       DREC(NR)
        INTEGER         DCER(NC)
        INTEGER         PGEN(NX),       PREC(NY),       PCER(NZ)
        DATA OGEN       /1,2,3,8,12,13,14,19/
        DATA OREC       /24,25,28,29/
        DATA OCER       /32,34/
        DATA OTPE       /4,5,15,16/
        DATA DEXC       /6,7,17,18,37,38,39,40,41,42,43,44/
        DATA DDFX       /9,10,11,20,21,22/
        DATA DREC       /26,27,30,31/
        DATA DCER       /33,35/
        DATA PGEN       /1,9,10,11,12,20,21,22,37,38/
        DATA PREC       /24,25,26,27,28,29,30,31/
        DATA PCER       /32,33,34,35/
C       ----------------------------------------------------------------
C
      CALL JEMARQ()
        CMD = 'AFFE_CARA_ELEM'
        TST = R8MAEM()
C
        CALL JEVEUO(JEXNOM(TMP,NOM),'E',JDGE)
        ISEC = NINT(ZR(JDGE+35))
C
C -     COMPLETUDE DES DONNES GENERALES
C
          IF(ISEC.EQ.0)THEN
            DO 20 J = 1 , NG
            IF(ZR(JDGE+OGEN(J)-1).EQ.TST)THEN
            CALL UTMESS('A',CMD,'POUTRE'//
     +      ' : MAILLE '//NOM//' : SECTION GENERALE'//
     +      ' : IL MANQUE LA CARACTERISTIQUE '//TAB(OGEN(J)))
            IER = IER + 1
            ENDIF
 20         CONTINUE
C - TYMOSHENKO
            IF ( NEL.EQ.NTEL(1) .OR. NEL.EQ.NTEL(4) .OR.
     +           NEL.EQ.NTEL(5) .OR. NEL.EQ.NTEL(6) )THEN
              DO 50 J = 1 , NT
                IF(ZR(JDGE+OTPE(J)-1).EQ.TST)THEN
                CALL UTMESS('A',CMD,'POUTRE'//
     +          ' : MAILLE '//NOM//
     +          ' : SECTION GENERALE'//
     +          ' : ELEMENT POUTRE DE TIMOSHENKO'//
     +          ' : IL MANQUE LA CARACTERISTIQUE '//TAB(OTPE(J)))
                ENDIF
 50           CONTINUE
            ENDIF
          ENDIF
C
C -     COMPLETUDE DES DONNES GEOMETRIQUES RECTANGLE
C
          IF(ISEC.EQ.1)THEN
            DO 30 J = 1 , NR
            IF(ZR(JDGE+OREC(J)-1).EQ.TST)THEN
            CALL UTMESS('A',CMD,'POUTRE'//
     +      ' : MAILLE '//NOM//' : SECTION RECTANGLE'//
     +      ' : IL MANQUE  LA CARACTERISTIQUE '//TAB(OREC(J)))
            IER = IER + 1
            ENDIF
 30         CONTINUE
          ENDIF
C
C -     COMPLETUDE DES DONNES GEOMETRIQUES CERCLE
C
          IF(ISEC.EQ.2)THEN
            DO 40 J = 1 , NC
            IF(ZR(JDGE+OCER(J)-1).EQ.TST)THEN
            CALL UTMESS('A',CMD,'POUTRE'//
     +      ' : MAILLE '//NOM//' : SECTION CERCLE'//
     +      ' :  IL MANQUE  LA CARACTERISTIQUE '//TAB(OCER(J)))
            IER = IER + 1
            ENDIF
 40         CONTINUE
          ENDIF
C
C -     VERIFICATION DE LA STRICTE POSITIVITE DE  VALEURS GENERALE
C
        IF(ISEC.EQ.0)THEN
          DO 130 J = 1 , NX
            IF(ZR(JDGE+PGEN(J)-1).NE.TST)THEN
              IF(ZR(JDGE+PGEN(J)-1).LE.0.D0)THEN
              CALL UTMESS('A',CMD,'POUTRE'//
     +        ' : MAILLE '//NOM//' : SECTION GENERALE'//
     +        ' : LA VALEUR DE '//TAB(PGEN(J))//' DOIT'//
     +        ' ETRE  STRICTEMENT POSITIVE')
              IER = IER + 1
              ENDIF
            ENDIF
 130     CONTINUE
       ENDIF
C
C -     VERIFICATION DE LA STRICTE POSITIVITE DE VALEURS RECTANGLE
C
        IF(ISEC.EQ.1)THEN
          DO 110 J = 1 , NY
            IF(ZR(JDGE+PREC(J)-1).NE.TST)THEN
              IF(ZR(JDGE+PREC(J)-1).LE.0.D0)THEN
              CALL UTMESS('A',CMD,'POUTRE'//
     +        ' : MAILLE '//NOM//' : SECTION RECTANGLE'//
     +        ' : LA VALEUR DE '//TAB(PREC(J))//' DOIT'//
     +        ' ETRE STRICTEMENT POSITIVE')
              IER = IER + 1
              ENDIF
            ENDIF
 110     CONTINUE
       ENDIF
C
C -     VERIFICATION DE LA STRICTE POSITIVITE DE VALEURS CERCLE
C
        IF(ISEC.EQ.2)THEN
          DO 120 J = 1 , NZ
            IF(ZR(JDGE+PCER(J)-1).NE.TST)THEN
              IF(ZR(JDGE+PCER(J)-1).LE.0.D0)THEN
              CALL UTMESS('A',CMD,'POUTRE'//
     +        ' : MAILLE '//NOM//' : SECTION CERCLE'//
     +        ' :  LA VALEUR DE '//TAB(PCER(J))//' DOIT'//
     +        ' ETRE STRICTEMENT POSITIVE')
              IER = IER + 1
              ENDIF
            ENDIF
 120     CONTINUE
       ENDIF
C
        IF(IER.NE.0)GOTO 9999
C
C -     AFFECTATION DES VALEURS PAR DEFAUT POUR LES DONNEES GENERALES
C
        IF(ISEC.EQ.0)THEN
C - EXCENTRICITES , AIRES INTERIEURES , CONSTANTES DE GAUCHISSEMENT,...
          DO 60 J = 1 , NE
          IF(ZR(JDGE+DEXC(J)-1).EQ.TST)ZR(JDGE+DEXC(J)-1) = 0.D0
 60       CONTINUE
C - DIST. FIBRE EXT.+ RAYON TORSION
          DO 70 J = 1 , ND
          IF(ZR(JDGE+DDFX(J)-1).EQ.TST)ZR(JDGE+DDFX(J)-1) = 1.D0
 70       CONTINUE
C - EULER
          IF(NEL.EQ.NTEL(2))THEN
            DO 80 J = 1 , NT
C           IF(ZR(JDGE+OTPE(J)-1).NE.TST.AND.ZR(JDGE+OTPE(J)-1).NE.0.D0)
C     +      THEN

C    +      ' : MAILLE '//NOM//' : SECTION GENERALE'//
C    +      ' : POUTRE D EULER'//
C    +      ' : LA VALEUR DE '//TAB(OTPE(J))//' DOIT ETRE 0 !! GRR!!')
C           IER = IER + 1
C           ELSE
            ZR(JDGE+OTPE(J)-1) = 0.D0
C           ENDIF
 80         CONTINUE
          ENDIF
        ENDIF
C
C -     AFFECTATION DES VALEURS PAR DEFAUT POUR LES DONNEES RECTANGLE
C
        IF(ISEC.EQ.1)THEN
          DO 90 J = 1 , NR
            IF(ZR(JDGE+DREC(J)-1).EQ.TST)THEN
            ZR(JDGE+DREC(J)-1) = ZR(JDGE+OREC(J)-1) / 2.D0
            ELSE
              IF(ZR(JDGE+DREC(J)-1).GT.(ZR(JDGE+OREC(J)-1)/2.D0))THEN
              CALL UTMESS('A',CMD,'POUTRE'//
     +        ' : MAILLE '//NOM//' : SECTION RECTANGLE'//
     +        ' : LA VALEUR DE '//TAB(DREC(J))//' NE DOIT '//
     +        'PAS DEPASSER '//TAB(OREC(J))//'/2  !!! M ENFIN QUOI !')
              IER = IER + 1
              ENDIF
            ENDIF
 90      CONTINUE
       ENDIF
C
C -     AFFECTATION DES VALEURS PAR DEFAUT POUR LES DONNEES CERCLE
C
        IF(ISEC.EQ.2)THEN
          DO 100 J = 1 , NC
            IF(ZR(JDGE+DCER(J)-1).EQ.TST)THEN
            ZR(JDGE+DCER(J)-1) = ZR(JDGE+OCER(J)-1)
            ELSE
              IF(ZR(JDGE+DCER(J)-1).GT.ZR(JDGE+OCER(J)-1))THEN
              CALL UTMESS('A',CMD,'POUTRE'//
     +        ' : MAILLE '//NOM//' : SECTION CERCLE'//
     +        ' :  LA VALEUR DE '//TAB(DCER(J))//' NE DOIT '//
     +        'PAS DEPASSER CELLE DE '//TAB(OCER(J))//' !!! AARG !!')
              IER = IER + 1
              ENDIF
            ENDIF
 100     CONTINUE
       ENDIF
C
 9999   CONTINUE
      CALL JEDEMA()
        END
