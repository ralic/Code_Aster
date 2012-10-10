      SUBROUTINE UTITES (LABEL1,LABEL2,TYPE,NBREF,REFI,REFR,REFC,
     +                   VALI,VALR,VALC,EPSI,CRIT,IFIC,LLAB,SSIGNE)
      IMPLICIT       NONE
      INTEGER        VALI,NBREF,REFI(NBREF),IFIC
      CHARACTER*(*)  LABEL1,LABEL2,TYPE,CRIT,SSIGNE
      REAL*8         VALR,REFR(NBREF),EPSI
      COMPLEX*16     VALC,REFC(NBREF)
      LOGICAL        LLAB
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 10/10/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C  IN  : K1  : TYPE   : TYPE DE VALEUR A TESTER 'R', OU 'C'
C  IN  : I   : NBREF  : NOMBRE DE VALEURS DE REFERENCE (=1 SOUVENT)
C  IN  : R8  : REFR   : VALEUR(S) REELLE(S) DE REFERENCE
C  IN  : C16 : REFC   : VALEUR(S) COMPLEXE(S) DE REFERENCE
C  IN  : R8  : VALR   : VALEUR REELLE A TESTER ( ASTER )
C  IN  : C16 : VALC   : VALEUR COMPLEXE A TESTER ( ASTER )
C  IN  : K8  : CRIT   : PRECISION 'RELATIVE' OU 'ABSOLUE'
C  IN  : R8  : EPSI   : PRECISION ESPEREE
C  IN  : I   : IFIC   : NUMERO LOGIQUE DU FICHIER DE SORTIE
C  IN  : L   : LLAB   : FLAG IMPRESSION LABELS
C  OUT :                IMPRESSION SUR LISTING
C ----------------------------------------------------------------------
      INTEGER I, IMIN, MINVI, TMPI, NL, LXLGUT, TVALI(2),NL1,NL2
      PARAMETER   (NL=6)
      CHARACTER*4  TESTOK, RELA
      CHARACTER*16 K120,K170,TCHERR(2)
      CHARACTER*24 TCHVAL(2),TCHVAR(2),TCHVAC(2)
      CHARACTER*48 LIGN2(NL),TCHVA2(2)
      REAL*8       ERR, ZERO, CENT, TMPR, MINVR, MINVC, TMPC
      REAL*8       TVALR(2),TERRR(2)
      COMPLEX*16   ZEROC, VTC
      LOGICAL      LOK
      DATA LIGN2/'REFERENCE','LEGENDE','VALE_REFE','VALE_CALC','ERREUR',
     &           'TOLE'/
C     ------------------------------------------------------------------
C
      K120   = LABEL1
      K170   = LABEL2
      RELA   = CRIT
      CENT   = 100.0D0
      ZERO   = 0.D0
      ZEROC  = DCMPLX(0.D0,0.D0)
      TESTOK = 'NOOK'
      TCHVAR(1) = ' '
      TCHVAR(2) = ' '
      TCHVAC(1) = ' '
      TCHVAC(2) = ' '
      TCHVA2(1) = ' '
      TCHVA2(2) = ' '
C
C-----------------
C --- CAS REEL ---
C ----------------
C
      IF ( TYPE(1:1) .EQ. 'R' ) THEN
C
         IF ( SSIGNE .EQ. 'OUI' )  VALR = ABS(VALR)
         MINVR=ABS(VALR-REFR(1))
         IMIN=1
         DO 10 I=1,NBREF-1
           TMPR=ABS(VALR-REFR(I+1))
           IF(TMPR.LT.MINVR)THEN
             TMPR=MINVR
             IMIN=I+1
           ENDIF
 10      CONTINUE

         TVALR(1)=REFR(IMIN)
         TVALR(2)=VALR
         TVALI(1)=0
         TVALI(2)=0
         TCHVAL(1)=' '
         TCHVAL(2)=' '
         TCHERR(1)=' '
         TCHERR(2)=' '

         IF ( RELA .EQ. 'RELA' ) THEN
            LOK = ( ABS(VALR-REFR(IMIN)) .LE. EPSI * ABS(REFR(IMIN)) )
            IF ( REFR(IMIN) .NE. ZERO ) THEN
               ERR = (VALR - REFR(IMIN)) / REFR(IMIN) *CENT
            ELSE
               IF (LOK) THEN
                 ERR = 0.D0
               ELSE
                 ERR = 999.999999D0
               ENDIF
            ENDIF
            IF ( LOK ) TESTOK = ' OK '
             TERRR(1)=ERR
             TERRR(2)=EPSI*CENT
         ELSE
             LOK = ( ABS(VALR - REFR(IMIN)) .LE. EPSI )
             ERR =       VALR - REFR(IMIN)
             IF ( LOK ) TESTOK = ' OK '
             TERRR(1)=ERR
             TERRR(2)=EPSI
         ENDIF

         CALL UTCOVT('R',TVALR,TVALI,TERRR,RELA,TCHVAL,TCHERR)
         
         NL1=LXLGUT(TCHVAL(1))
         NL2=LXLGUT(TCHVAL(2))
       
         IF(NL1.LT.17)THEN
           IF(NL2.LT.17)THEN
              IF (LLAB) WRITE(IFIC,1616)(LIGN2(I), I=1,NL)
              WRITE(IFIC,5616)TESTOK,K120,K170,TCHVAL(1),TCHVAL(2),
     &                      TCHERR(1),TCHERR(2)

           ELSE
              IF (LLAB) WRITE(IFIC,1624)(LIGN2(I), I=1,NL)
              WRITE(IFIC,5624)TESTOK,K120,K170,TCHVAL(1),TCHVAL(2),
     &                      TCHERR(1),TCHERR(2)
           ENDIF
         ELSE
           IF(NL2.LT.17)THEN
              IF (LLAB) WRITE(IFIC,2416)(LIGN2(I), I=1,NL)
              WRITE(IFIC,6416)TESTOK,K120,K170,TCHVAL(1),TCHVAL(2),
     &                      TCHERR(1),TCHERR(2)

           ELSE
              IF (LLAB) WRITE(IFIC,2424)(LIGN2(I), I=1,NL)
              WRITE(IFIC,6424)TESTOK,K120,K170,TCHVAL(1),TCHVAL(2),
     &                      TCHERR(1),TCHERR(2)
           ENDIF
         ENDIF       
C
C-------------------
C --- CAS ENTIER ---
C ------------------
C
      ELSEIF ( TYPE(1:1) .EQ. 'I' ) THEN
C
         MINVI=ABS(VALI-REFI(1))
         IMIN=1
         DO 20 I=1,NBREF-1
           TMPI=ABS(VALI-REFI(I+1))
           IF(TMPI.LT.MINVI)THEN
             TMPI=MINVI
             IMIN=I+1
           ENDIF
 20      CONTINUE
         IF ( SSIGNE .EQ. 'OUI' ) VALI = ABS(VALI) 
         ERR = DBLE( VALI - REFI(IMIN) )

         TVALR(1)=0.D0
         TVALR(2)=0.D0
         TVALI(1)=REFI(IMIN)
         TVALI(2)=VALI
         TCHVAL(1)=' '
         TCHVAL(2)=' '
         TCHERR(1)=' '
         TCHERR(2)=' '

         IF ( RELA .EQ. 'RELA' ) THEN
            LOK = ( ABS( ERR ) .LE. EPSI*ABS(REFI(IMIN)) )
            IF ( REFI(IMIN) .NE. 0 ) THEN
               ERR = ( VALI - REFI(IMIN) )*CENT / REFI(IMIN)
            ELSE
               IF (LOK) THEN
                 ERR = 0.D0
               ELSE
                 ERR = 999.999999D0
               ENDIF
            ENDIF
            IF ( LOK ) TESTOK = ' OK '
             TERRR(1)=ERR
             TERRR(2)=EPSI*CENT
         ELSE
            LOK = ( ABS( ERR ) .LE. EPSI )
            IF ( LOK ) TESTOK = ' OK '
             TERRR(1)=ERR
             TERRR(2)=EPSI
         ENDIF

         CALL UTCOVT('I',TVALR,TVALI,TERRR,RELA,TCHVAL,TCHERR)

         NL1=LXLGUT(TCHVAL(1))
         NL2=LXLGUT(TCHVAL(2))
       
         IF(NL1.LT.17)THEN
           IF(NL2.LT.17)THEN
              IF (LLAB) WRITE(IFIC,1616)(LIGN2(I), I=1,NL)
              WRITE(IFIC,5616)TESTOK,K120,K170,TCHVAL(1),TCHVAL(2),
     &                      TCHERR(1),TCHERR(2)

           ELSE
              IF (LLAB) WRITE(IFIC,1624)(LIGN2(I), I=1,NL)
              WRITE(IFIC,5624)TESTOK,K120,K170,TCHVAL(1),TCHVAL(2),
     &                      TCHERR(1),TCHERR(2)
           ENDIF
         ELSE
           IF(NL2.LT.17)THEN
              IF (LLAB) WRITE(IFIC,2416)(LIGN2(I), I=1,NL)
              WRITE(IFIC,6416)TESTOK,K120,K170,TCHVAL(1),TCHVAL(2),
     &                      TCHERR(1),TCHERR(2)

           ELSE
              IF (LLAB) WRITE(IFIC,2424)(LIGN2(I), I=1,NL)
              WRITE(IFIC,6424)TESTOK,K120,K170,TCHVAL(1),TCHVAL(2),
     &                      TCHERR(1),TCHERR(2)
           ENDIF
         ENDIF       
C         
C---------------------
C --- CAS COMPLEXE ---
C --------------------
C
      ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
C
         VTC = REFC(1)
         IF ( SSIGNE .EQ. 'OUI' ) THEN
            VALC = ABS(VALC)
            VTC  = ABS(VTC)
         ENDIF
         MINVC=ABS(VALC-VTC)
         IMIN=1
C    --- NBREF > 1 N'EST PAS GERE PAR LE SUPERVISEUR...
         DO 30 I=1,NBREF-1
            VTC = REFC(I+1)
            IF ( SSIGNE .EQ. 'OUI' )  VTC = ABS(VTC)
            TMPC=ABS(VALC-VTC)
            IF(TMPC.LT.MINVC)THEN
               TMPC=MINVC
               IMIN=I+1
            ENDIF
 30      CONTINUE
         VTC = REFC(IMIN)
         IF ( SSIGNE .EQ. 'OUI' )  VTC = ABS(VTC)

         TVALI(1)=0
         TVALI(2)=0
         TCHVAL(1)=' '
         TCHVAL(2)=' '
         TCHERR(1)=' '
         TCHERR(2)=' '

         IF ( RELA .EQ. 'RELA' ) THEN
            LOK = ( ABS(VALC-VTC) .LE. EPSI * ABS(VTC))
            IF ( VTC .NE. ZEROC ) THEN
               ERR = ABS(VALC - VTC) /  ABS(VTC) *CENT
            ELSE
               IF (LOK) THEN
                 ERR = 0.D0
               ELSE
                 ERR = 999.999999D0
               ENDIF
            ENDIF
            IF ( LOK ) TESTOK = ' OK '
            TERRR(1)=ERR
            TERRR(2)=EPSI*CENT
         ELSE
            LOK = ( ABS(VALC - VTC) .LE. EPSI )
            ERR =   ABS(VALC - VTC)
            IF ( LOK ) TESTOK = ' OK '
            TERRR(1)=ERR
            TERRR(2)=EPSI
         ENDIF

         TVALR(1)=DBLE(VTC)
         TVALR(2)=DBLE(VALC)
         CALL UTCOVT('R',TVALR,TVALI,TERRR,RELA,TCHVAR,TCHERR)

         TVALR(1)=DIMAG(VTC)
         TVALR(2)=DIMAG(VALC)
         CALL UTCOVT('R',TVALR,TVALI,TERRR,RELA,TCHVAC,TCHERR)

         NL1=LXLGUT(TCHVAR(1))
         NL2=LXLGUT(TCHVAC(1))
         IF(TCHVAC(1)(1:1).EQ.'-')THEN
            TCHVA2(1)=TCHVAR(1)(1:NL1)//TCHVAC(1)(1:NL2)//'j'
         ELSE
            TCHVA2(1)=TCHVAR(1)(1:NL1)//'+'//TCHVAC(1)(1:NL2)//'j' 
         ENDIF
         NL1=LXLGUT(TCHVAR(2))
         NL2=LXLGUT(TCHVAC(2))
         IF(TCHVAC(2)(1:1).EQ.'-')THEN
            TCHVA2(2)=TCHVAR(2)(1:NL1)//TCHVAC(2)(1:NL2)//'j'
         ELSE
            TCHVA2(2)=TCHVAR(2)(1:NL1)//'+'//TCHVAC(2)(1:NL2)//'j' 
         ENDIF

         NL1=LXLGUT(TCHVA2(1))
         NL2=LXLGUT(TCHVA2(2))
         IF(NL1.GE.48 .OR. NL2.GE.48 )CALL ASSERT(.FALSE.)
         IF(NL1.LT.24)THEN
            IF(NL2.LT.24)THEN
               IF (LLAB) WRITE(IFIC,2424)(LIGN2(I), I=1,NL)
               WRITE(IFIC,6424)TESTOK,K120,K170,TCHVA2(1),TCHVA2(2),
     &                   TCHERR(1),TCHERR(2)
            ELSEIF(NL2.LT.36)THEN
               IF (LLAB) WRITE(IFIC,2436)(LIGN2(I), I=1,NL)
               WRITE(IFIC,6436)TESTOK,K120,K170,TCHVA2(1),TCHVA2(2),
     &                   TCHERR(1),TCHERR(2)
            ELSE
               IF (LLAB) WRITE(IFIC,2448)(LIGN2(I), I=1,NL)
               WRITE(IFIC,6448)TESTOK,K120,K170,TCHVA2(1),TCHVA2(2),
     &                      TCHERR(1),TCHERR(2)
            ENDIF
         ELSEIF(NL1.LT.36)THEN
            IF(NL2.LT.24)THEN
               IF (LLAB) WRITE(IFIC,3624)(LIGN2(I), I=1,NL)
               WRITE(IFIC,7624)TESTOK,K120,K170,TCHVA2(1),TCHVA2(2),
     &                   TCHERR(1),TCHERR(2)
            ELSEIF(NL2.LT.36)THEN
               IF (LLAB) WRITE(IFIC,3636)(LIGN2(I), I=1,NL)
               WRITE(IFIC,7636)TESTOK,K120,K170,TCHVA2(1),TCHVA2(2),
     &                   TCHERR(1),TCHERR(2)
            ELSE
               IF (LLAB) WRITE(IFIC,3648)(LIGN2(I), I=1,NL)
               WRITE(IFIC,7648)TESTOK,K120,K170,TCHVA2(1),TCHVA2(2),
     &                   TCHERR(1),TCHERR(2)   
            ENDIF
         ELSE
            IF(NL2.LT.24)THEN
               IF (LLAB) WRITE(IFIC,4824)(LIGN2(I), I=1,NL)
               WRITE(IFIC,8824)TESTOK,K120,K170,TCHVA2(1),TCHVA2(2),
     &                   TCHERR(1),TCHERR(2)
            ELSEIF(NL2.LT.36)THEN
               IF (LLAB) WRITE(IFIC,4836)(LIGN2(I), I=1,NL)
               WRITE(IFIC,8836)TESTOK,K120,K170,TCHVA2(1),TCHVA2(2),
     &                   TCHERR(1),TCHERR(2)          
            ELSE
               IF (LLAB) WRITE(IFIC,4848)(LIGN2(I), I=1,NL)
               WRITE(IFIC,8848)TESTOK,K120,K170,TCHVA2(1),TCHVA2(2),
     &                   TCHERR(1),TCHERR(2)                           
            ENDIF
         ENDIF
            
      ENDIF
C
C     IF (TESTOK.EQ.'NOOK') CALL ABORT()
1616  FORMAT(5X,6(1X,A16))
5616  FORMAT(A4,1X,6(1X,A16))
1624  FORMAT(5X,3(1X,A16),1(1X,A24),2(1X,A16))
5624  FORMAT(A4,1X,3(1X,A16),1(1X,A24),2(1X,A16))
2416  FORMAT(5X,2(1X,A16),1(1X,A24),3(1X,A16))
6416  FORMAT(A4,1X,2(1X,A16),1(1X,A24),3(1X,A16))
2424  FORMAT(5X,2(1X,A16),2(1X,A24),2(1X,A16))
6424  FORMAT(A4,1X,2(1X,A16),2(1X,A24),2(1X,A16))
2436  FORMAT(5X,2(1X,A16),1X,A24,1X,A36,2(1X,A16))
6436  FORMAT(A4,1X,2(1X,A16),1X,A24,1X,A36,2(1X,A16))
2448  FORMAT(5X,2(1X,A16),1X,A24,1X,A48,2(1X,A16))
6448  FORMAT(A4,1X,2(1X,A16),1X,A24,1X,A48,2(1X,A16))
3624  FORMAT(5X,2(1X,A16),1X,A36,1X,A24,2(1X,A16))
7624  FORMAT(A4,1X,2(1X,A16),1X,A36,1X,A24,2(1X,A16))
3636  FORMAT(5X,2(1X,A16),2(1X,A36),2(1X,A16))
7636  FORMAT(A4,1X,2(1X,A16),2(1X,A36),2(1X,A16))
3648  FORMAT(5X,2(1X,A16),1X,A36,1X,A48,2(1X,A16))
7648  FORMAT(A4,1X,2(1X,A16),1X,A36,1X,A48,2(1X,A16))
4824  FORMAT(5X,2(1X,A16),1X,A48,1X,A24,2(1X,A16))
8824  FORMAT(A4,1X,2(1X,A16),1X,A48,1X,A24,2(1X,A16))
4836  FORMAT(5X,2(1X,A16),1X,A48,1X,A36,2(1X,A16))
8836  FORMAT(A4,1X,2(1X,A16),1X,A48,1X,A36,2(1X,A16))
4848  FORMAT(5X,2(1X,A16),2(1X,A48),2(1X,A16))
8848  FORMAT(A4,1X,2(1X,A16),2(1X,A48),2(1X,A16))


C
      END
