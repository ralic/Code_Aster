      SUBROUTINE AFFBAR(TMP,TMPF,FCX,NOMMAI,
     +                  ISEC,CAR,VAL,EXP,NBO,KIOC,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                      ISEC,            NBO,     IER
      REAL*8                                VAL(*)
      CHARACTER*6                                       KIOC
      CHARACTER*8       FCX, NOMMAI,     CAR(*), EXP(*)
      CHARACTER*24      TMP,TMPF
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 28/03/2001   AUTEUR CIBHHLV L.VIVAN 
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
C     ------------------------------------------------------------------
C     VERIFICATION DE LA BONNE AFFECTATION DES SECTIONS DE BARRE :
C        - INTERDICTION D ECRASER UNE GEOMETRIE DE SECTION PAR UNE AUTRE
C     AFFECTATION DES CARACTERISTIQUES GENERALES ET GEOMETRIQUES
C                         AUX MAILLES DE TYPE BARRE DANS L OBJET TAMPON
C
C     L OBJET TAMPON (TMP) CONTIENT (8*NBBARRE) VALEURS
C     EXP  : A    HY   HZ   EPY  EPZ  EP   R   TSEC
C     TSEC : TYPE  GEOMETRIQUE DE SECTION : 0 = GENERALE
C                                           1 = RECTANGLE
C                                           2 = CERCLE
C-----------------------------------------------------------------------
C     ------- DEBUT COMMUNS NORMALISES  JEVEUX  ------------------------
      INTEGER              ZI
      COMMON    / IVARJE / ZI(1)
      REAL*8               ZR
      COMMON    / RVARJE / ZR(1)
      COMPLEX*16           ZC
      COMMON    / CVARJE / ZC(1)
      LOGICAL              ZL
      COMMON    / LVARJE / ZL(1)
      CHARACTER*8          ZK8
      CHARACTER*16                  ZK16
      CHARACTER*24                            ZK24
      CHARACTER*32                                      ZK32
      CHARACTER*80                                               ZK80
      COMMON    / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1), ZK80(1)
      CHARACTER*32     JEXNOM,        JEXNUM
C     -------  FIN  COMMUNS NORMALISES  JEVEUX  ------------------------
      REAL*8       TST, R8MAEM, PI, R8PI, ZERO
      REAL*8       HY, HZ, EPY, EPZ, HYI, HZI, E, RE, RI
      CHARACTER*8   RESU
      CHARACTER*16  CONCEP, CMD
      LOGICAL      SECPLE
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL GETRES(RESU,CONCEP,CMD)
      TST = R8MAEM()
      PI  = R8PI()
      ZERO = 0.D0
      SECPLE = .TRUE.
C
      CALL JENONU(JEXNOM(TMP,NOMMAI),NUM)
C
C --- TESTS D ECRASEMENT DE SECTION
      IF (NUM.NE.0) THEN
         CALL JEVEUO(JEXNOM(TMP,NOMMAI),'E',JDGE)
         IISEC = NINT(ZR(JDGE+NBO-1))
         IF (IISEC.NE.ISEC) THEN
            CALL UTMESS('A',CMD,'OCCURENCE '//KIOC//'DE "BARRE" ('//
     +                             'MAILLE '//NOMMAI//') ECRASEMENT '//
     +                'D UN TYPE DE GEOMETRIE DE SECTION PAR UN AUTRE')
            IER = IER + 1
            GOTO 9999
         ENDIF
      ELSE
         CALL JECROC(JEXNOM(TMP,NOMMAI))
         CALL JEVEUO(JEXNOM(TMP,NOMMAI),'E',JDGE)
         DO 5 I = 1 , NBO
            ZR(JDGE+I-1) = TST
 5       CONTINUE
      ENDIF

C     --- NOM DE LA FONCTION DU CX
      CALL JENONU(JEXNOM(TMPF,NOMMAI),NUM)
      IF ( NUM .EQ. 0 ) THEN
        CALL JECROC(JEXNOM(TMPF,NOMMAI))
      ENDIF
      CALL JEVEUO(JEXNOM(TMPF,NOMMAI),'E',JDGEF)
      ZK8(JDGEF) = FCX
C
      IF (ISEC.EQ.0) THEN
         DO 20 J = 1 , NBO
            IF (CAR(J).EQ.'A       ') ZR(JDGE) = VAL(J)
 20      CONTINUE
      ELSEIF (ISEC.EQ.1) THEN
         DO 22 J = 1 , NBO
            IF (CAR(J).EQ.'HY      ') THEN
               ZR(JDGE+1) = VAL(J)
            ELSEIF (CAR(J).EQ.'HZ      ') THEN
               ZR(JDGE+2) = VAL(J)
            ELSEIF (CAR(J).EQ.'EPY     ') THEN
               ZR(JDGE+3) = VAL(J)
               SECPLE = .FALSE.
            ELSEIF (CAR(J).EQ.'EPZ     ') THEN
               ZR(JDGE+4) = VAL(J)
               SECPLE = .FALSE.
            ELSEIF (CAR(J).EQ.'H       ') THEN
               ZR(JDGE+1) = VAL(J)
               ZR(JDGE+2) = VAL(J)
            ELSEIF (CAR(J).EQ.'EP      ') THEN
               ZR(JDGE+3) = VAL(J)
               ZR(JDGE+4) = VAL(J)
               SECPLE = .FALSE.
            ENDIF
 22      CONTINUE
      ELSEIF (ISEC.EQ.2) THEN
         DO 24 J = 1 , NBO
            IF (CAR(J).EQ.'R       ') THEN
               ZR(JDGE+5) = VAL(J)
            ELSEIF (CAR(J).EQ.'EP      ') THEN
               ZR(JDGE+6) = VAL(J)
               SECPLE = .FALSE.
            ENDIF
 24      CONTINUE
      ENDIF
      ZR(JDGE+7) = ISEC
C
C --- COMPLETUDE DES DONNES GENERALES
      IF (ISEC.EQ.0) THEN
         IF (ZR(JDGE).EQ.TST) THEN
            CALL UTMESS('A',CMD,'BARRE'//
     +                    ' : MAILLE '//NOMMAI//' : SECTION GENERALE'//
     +                      ' : IL MANQUE LA CARACTERISTIQUE '//EXP(1))
            IER = IER + 1
         ENDIF
         IF (ZR(JDGE).LE.ZERO) THEN
            CALL UTMESS('A',CMD,'BARRE'//
     +                    ' : MAILLE '//NOMMAI//' : SECTION GENERALE'//
     +                            ' : LA VALEUR DE '//EXP(1)//' DOIT'//
     +                                  ' ETRE  STRICTEMENT POSITIVE.')
            IER = IER + 1
         ENDIF
C
C --- COMPLETUDE DES DONNES GEOMETRIQUES RECTANGLE
      ELSEIF (ISEC.EQ.1) THEN
         DO 40 J = 1 , 2
            IF (ZR(JDGE+J).EQ.TST) THEN
               CALL UTMESS('A',CMD,'BARRE'//
     +                   ' : MAILLE '//NOMMAI//' : SECTION RECTANGLE'//
     +                   ' : IL MANQUE  LA CARACTERISTIQUE '//EXP(1+J))
               IER = IER + 1
            ENDIF
            IF (ZR(JDGE+J).LE.ZERO) THEN
               CALL UTMESS('A',CMD,'BARRE'//
     +                   ' : MAILLE '//NOMMAI//' : SECTION RECTANGLE'//
     +                          ' : LA VALEUR DE '//EXP(1+J)//' DOIT'//
     +                                  ' ETRE  STRICTEMENT POSITIVE.')
               IER = IER + 1
            ENDIF
 40      CONTINUE
         IF ( .NOT. SECPLE ) THEN
            DO 42 J = 3 , 4
               IF (ZR(JDGE+J).EQ.TST) THEN
                  CALL UTMESS('A',CMD,'BARRE'//
     +                   ' : MAILLE '//NOMMAI//' : SECTION RECTANGLE'//
     +                   ' : IL MANQUE  LA CARACTERISTIQUE '//EXP(1+J))
                  IER = IER + 1
               ENDIF
               IF (ZR(JDGE+J).LE.ZERO) THEN
                  CALL UTMESS('A',CMD,'BARRE'//
     +                   ' : MAILLE '//NOMMAI//' : SECTION RECTANGLE'//
     +                          ' : LA VALEUR DE '//EXP(1+J)//' DOIT'//
     +                                  ' ETRE  STRICTEMENT POSITIVE.')
                  IER = IER + 1
               ENDIF
 42         CONTINUE
         ENDIF
C
C --- COMPLETUDE DES DONNES GEOMETRIQUES CERCLE
      ELSEIF(ISEC.EQ.2)THEN
         IF (ZR(JDGE+5).EQ.TST) THEN
            CALL UTMESS('A',CMD,'BARRE'//
     +                      ' : MAILLE '//NOMMAI//' : SECTION CERCLE'//
     +                     ' : IL MANQUE  LA CARACTERISTIQUE '//EXP(5))
            IER = IER + 1
         ENDIF
         IF (ZR(JDGE+5).LE.ZERO) THEN
            CALL UTMESS('A',CMD,'BARRE'//
     +                      ' : MAILLE '//NOMMAI//' : SECTION CERCLE'//
     +                            ' : LA VALEUR DE '//EXP(5)//' DOIT'//
     +                                  ' ETRE  STRICTEMENT POSITIVE.')
            IER = IER + 1
         ENDIF
         IF ( .NOT. SECPLE ) THEN
            IF (ZR(JDGE+6).LE.ZERO) THEN
               CALL UTMESS('A',CMD,'BARRE'//
     +                      ' : MAILLE '//NOMMAI//' : SECTION CERCLE'//
     +                            ' : LA VALEUR DE '//EXP(6)//' DOIT'//
     +                                               ' ETRE POSITIVE.')
               IER = IER + 1
            ENDIF
         ENDIF
      ENDIF
C
C
      IF (IER.NE.0) GOTO 9999
C
C --- AFFECTATION DES VALEURS PAR DEFAUT POUR LES DONNEES RECTANGLE
      IF (ISEC.EQ.1) THEN
         HY  = ZR(JDGE+1)
         HZ  = ZR(JDGE+2)
         IF ( SECPLE ) THEN
            ZR(JDGE) = HY * HZ
         ELSE
            EPY = ZR(JDGE+3)
            EPZ = ZR(JDGE+4)
            HYI = HY - 2.D0*EPY
            HZI = HZ - 2.D0*EPZ
            ZR(JDGE) = HY * HZ - HYI * HZI
         ENDIF
         IF (ZR(JDGE+3).EQ.TST) ZR(JDGE+3) = ZR(JDGE+1)
         IF (ZR(JDGE+4).EQ.TST) ZR(JDGE+4) = ZR(JDGE+2)
C
C --- AFFECTATION DES VALEURS PAR DEFAUT POUR LES DONNEES CERCLE
      ELSEIF (ISEC.EQ.2) THEN
         RE = ZR(JDGE+5)
         IF ( SECPLE ) THEN
            ZR(JDGE) = PI * RE * RE
         ELSE
            E  = ZR(JDGE+6)
            RI = RE - E
            ZR(JDGE) = PI * ( RE*RE - RI*RI )
         ENDIF
         IF (ZR(JDGE+6).EQ.TST) ZR(JDGE+6) = ZR(JDGE+5)
      ENDIF
      DO 50 I = 1,NBO
         IF (ZR(JDGE+I-1).EQ.TST) ZR(JDGE+I-1) = ZERO
 50   CONTINUE
C
 9999 CONTINUE
      CALL JEDEMA()
      END
