      SUBROUTINE IBOPDC(CHAINE,NOM,DESCR,PTR1,IPOSIN,NBVAL,
     +                             PTR2,PTR3, CARG,IARG,RARG,TARG,LONG)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     CHAINE,NOM(*),DESCR(*)
      INTEGER                      PTR1(*),PTR2(*),PTR3(*),IPOSIN,NBVAL
      CHARACTER*(*)                           CARG(*),       TARG(*)
      INTEGER                                      IARG(*),     LONG(*)
      REAL*8                                            RARG(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 15/10/96   AUTEUR GIBHHCM C.MASSERET 
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
C     DECODAGE DES INFORMATIONS RELATIVES AUX OPERATEURS
C     ------------------------------------------------------------------
C IN  IPOSIN : IS : POSITION - 1 DU DESCRIPTEUR A REMPLIR
C OUT NBVAL  : IS : NOMBRE DE VALEURS ARGUMENTS
C                 SI NBVAL < 0  PROBLEME DE DECODAGE
C IN  NOM : CH16 : TABLEAU DE NOMS EXTERNES
C VAR DESCR: CH16 : TABLEAU DESCRIPTEUR DE CONTENU ............
C VAR PTR  : IS   : TABLEAU DES POINTEURS
C VAR CARG : CH16 : TABLEAU DES CONCEPTS ARGUMENTS
C VAR IARG : IS   : TABLEAU DES ARGUMENTS ENTIERS
C VAR RARG : R8   : TABLEAU DES ARGUMENTS REELS
C VAR TARG : CH500: TABLEAU DES ARGUMENTS TEXTES
C VAR LONG  : IS : TABLEAU DES LONGUEURS
C        LONG(1) ==> LONGUEUR DES SQUELETTES
C        LONG(2) ==> NOMBRE D'ARGUMENTS ENTIERS
C        LONG(3) ==> NOMBRE D'ARGUMENTS REELS
C        LONG(4) ==> NOMBRE D'ARGUMENTS CONCEPTS
C        LONG(5) ==> NOMBRE D'ARGUMENTS TEXTES
C     ------------------------------------------------------------------
C
      CHARACTER*80 ETOILE
      CHARACTER*80 CVAL
      INTEGER      IVAL
      REAL*8       RVAL(2)
C     ------------------------------------------------------------------
      CHARACTER*1  STATUT(4)
      CHARACTER*2  SHTTYP(6)
      DATA    SHTTYP/ 'IS', 'R8', 'CO', 'TX', 'C8', 'LS'/
      DATA    STATUT/ 'O', 'F', 'C', 'D' /
      DATA    ETOILE/ '***********************************************'/
C     ------------------------------------------------------------------
C
C
C     --- INITIALISATIONS DIVERSES ---
      IDEB   = 1
      NBVAL  = 0
      CALL LXCAPS( CHAINE )
C
C     --- BOUCLE SUR LES ITEMS  ---
   1  CONTINUE
      CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
C
C
      IF (ICLASS.EQ.-1) THEN
         GOTO 9999
      ELSEIF (ICLASS.EQ.3) THEN
C
          IF ( CVAL(1:IVAL).EQ.'STATUT' ) THEN
C
C
             CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
             IF (ICLASS.NE.5.OR.CVAL(1:1).NE.':') GOTO 908
             CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
             IF (ICLASS.NE.3) GOTO 901
             IF ( DESCR(IPOSIN+1)(1:1) .EQ. '*' ) THEN
                CALL UTREMT( CVAL(1:1) , STATUT , 4 , IER )
             ELSE
                CALL UTREMT( CVAL(1:1) , STATUT , 3 , IER )
             ENDIF
             IF (IER.EQ.0) GOTO 907
             DESCR(IPOSIN+1)(3:3) = CVAL(1:1)
C
          ELSEIF ( CVAL(1:IVAL).EQ.'NB_ARG' ) THEN
C
C
             CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
             IF (ICLASS.NE.5.OR.CVAL(1:1).NE.':') GOTO 908
             CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
             IF (ICLASS.NE.1) GOTO 902
             IF (IVAL.LT.01.OR.IVAL.GT.99) GOTO 903
             NBVAL = IVAL
             DO 10 IPOS = 1, NBVAL
                CALL CODENT( IPOS , 'D0' , CVAL(1:2) )
                DESCR(IPOSIN+IPOS)(1:4) = DESCR(IPOSIN+1)(1:4)
                DESCR(IPOSIN+IPOS)(5:6) = CVAL(1:2)
   10        CONTINUE
             DO 11 IPOS = 2, NBVAL
                NOM(IPOSIN+IPOS) = '   '
   11        CONTINUE
C
          ELSEIF ( CVAL(1:IVAL).EQ.'TYPE' ) THEN
C
C
             CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
             IPOS = 1
             IF (ICLASS.EQ.5.AND.CVAL(1:1).EQ.'(') THEN
                CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
                IF (ICLASS.NE.1) GOTO 902
                IPOS = IVAL
                CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
                IF (ICLASS.NE.5.OR.CVAL(1:1).NE.')') GOTO 904
                CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
             ENDIF
             IF (ICLASS.NE.5.OR.CVAL(1:1).NE.':') GOTO 908
             CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
             IF (ICLASS.NE.3) GOTO 901
             IF ( CVAL(1:2).EQ. 'L_' ) THEN
                DESCR(IPOSIN+IPOS)(7:7) = CVAL(1:1)
                IFIRST = 3
                IEND   = 4
             ELSE
                IFIRST = 1
                IEND   = 2
             ENDIF
             CALL UTREMT( CVAL(IFIRST:IEND) , SHTTYP , 6 , IER )
             IF ( IER .EQ. 0 ) GOTO 906
             DESCR(IPOSIN+IPOS)(8:9) = CVAL(IFIRST:IEND)
             IF (CVAL(IFIRST:IEND).EQ.'CO') THEN
                CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
                IF (ICLASS.EQ.5.AND.CVAL(1:1).EQ.'(') THEN
                   CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
                   IF (ICLASS.EQ.3) THEN
C                     --- DEPOT DU CONCEPT CVAL(1:16) ---
                      LONG(4) = LONG(4) + 1
C-ANC                 PTR1(IPOSIN+IPOS) = LONG(4)
                      CARG(LONG(4)) = CVAL(1:IVAL)
                      JLONG         = LONG(4)
                      IF( PTR2(IPOSIN+IPOS) .EQ.0 )
     +                    PTR2(IPOSIN+IPOS) = JLONG
                                              PTR3(IPOSIN+IPOS) = JLONG
                      CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
                   ENDIF
                   IF (ICLASS.NE.5.OR.CVAL(1:1).NE.')') GOTO 904
                ENDIF
             ELSEIF (CVAL(IFIRST:IEND).EQ.'TX') THEN
                IF ( IEND.LT.IVAL ) THEN
                   IEND = IEND + 1
                   IF ( CVAL(IEND:IEND) .EQ. 'M')
     +                                  DESCR(IPOSIN+IPOS)(10:10) = 'M'
                ENDIF
             ENDIF
C
          ELSEIF ( CVAL(1:IVAL).EQ.'UNITE' ) THEN
C
C
             CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
             IPOS = 1
             IF (ICLASS.EQ.5.AND.CVAL(1:1).EQ.'(') THEN
                CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
                IF (ICLASS.NE.1) GOTO 902
                IPOS = IVAL
                CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
                IF (ICLASS.NE.5.OR.CVAL(1:1).NE.')') GOTO 904
                CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
             ENDIF
             IF (ICLASS.NE.5.OR.CVAL(1:1).NE.':') GOTO 908
             CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
             DESCR(IPOSIN+IPOS)(11:14) = CVAL(1:MIN(4,IVAL))
C
          ELSEIF ( CVAL(1:IVAL).EQ.'MIN' ) THEN
C
             IF( DESCR(IPOSIN+1)(1:1) .NE. '*' ) GOTO 905
             CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
             IF (ICLASS.NE.5.OR.CVAL(1:1).NE.':') GOTO 908
             CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
             IF (ICLASS.NE.1) GOTO 902
             CALL CODENT(IVAL,'D0',DESCR(IPOSIN+1)(5:6) )
C
C
          ELSEIF ( CVAL(1:IVAL).EQ.'MAX' ) THEN
C
             IF( DESCR(IPOSIN+1)(1:1) .NE. '*' ) GOTO 905
             CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
             IF (ICLASS.NE.5.OR.CVAL(1:1).NE.':') GOTO 908
             CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
             IF (ICLASS.EQ.1) THEN
                CALL CODENT(IVAL,'D0',DESCR(IPOSIN+1)(8:9) )
             ELSEIF (ICLASS.EQ.5.AND.CVAL(1:1).EQ.'*') THEN
                CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
                IF (ICLASS.NE.5.OR.CVAL(1:1).NE.'*') GOTO 911
             ELSE
                GOTO 902
             ENDIF
C
          ELSEIF ( CVAL(1:IVAL).EQ.'DEF' ) THEN
C
C
             CALL LXSCAN( CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
             IPOS = 1
             IF (ICLASS.EQ.5.AND.CVAL(1:1).EQ.'(') THEN
                CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
                IF (ICLASS.NE.1) GOTO 902
                IPOS = IVAL
                CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
                IF (ICLASS.NE.5.OR.CVAL(1:1).NE.')') GOTO 904
                CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
             ENDIF
             IF (ICLASS.NE.5.OR.CVAL(1:1).NE.':') GOTO 908
             CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
             IF (ICLASS .EQ. 1 ) THEN
                LONG(2)            = LONG(2) + 1
                PTR1(IPOSIN+IPOS) = LONG(2)
                IARG(LONG(2))      = IVAL
             ELSEIF (ICLASS.EQ. 2 ) THEN
                LONG(3)            = LONG(3) + 1
                PTR1(IPOSIN+IPOS) = LONG(3)
                RARG(LONG(3))      = RVAL(1)
             ELSEIF (ICLASS.EQ. 3 ) THEN
C               TEXTES RECUPERES COMME IDENTIFICATEUR
                LONG(5)            = LONG(5) + 1
                PTR1(IPOSIN+IPOS) = LONG(5)
                TARG(LONG(5))      = CVAL(1:IVAL)
             ELSEIF (ICLASS.EQ. 4 ) THEN
C               TEXTES RECUPERES COMME TEXTE
                LONG(5)            = LONG(5) + 1
                PTR1(IPOSIN+IPOS)  = LONG(5)
                TARG(LONG(5))      = CVAL(1:IVAL)
             ELSE
                GOTO 912
             ENDIF
C
          ELSEIF ( CVAL(1:IVAL).EQ.'IN' ) THEN
C
C
             CALL LXSCAN( CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
             IPOS = 1
             IF (ICLASS.EQ.5.AND.CVAL(1:1).EQ.'(') THEN
                CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
                IF (ICLASS.NE.1) GOTO 902
                IPOS = IVAL
                CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
                IF (ICLASS.NE.5.OR.CVAL(1:1).NE.')') GOTO 904
                CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
             ENDIF
             IF (ICLASS.NE.5.OR.CVAL(1:1).NE.':') GOTO 908
             CALL LXSCAN(CHAINE,IDEB,ICLASS,IVAL,RVAL,CVAL)
             IF (ICLASS .EQ. 1 ) THEN
                LONG(2)       = LONG(2) + 1
                IARG(LONG(2)) = IVAL
                JLONG         = LONG(2)
                IF (PTR2(IPOSIN+IPOS) .GT. 0) THEN
                   DO 101 IP=PTR2(IPOSIN+IPOS),PTR3(IPOSIN+IPOS) 
                      IF (IARG(IP) .EQ. IVAL) GOTO 913
 101               CONTINUE   
                ENDIF                
             ELSEIF (ICLASS.EQ. 2 ) THEN
                LONG(3)       = LONG(3) + 1
                RARG(LONG(3)) = RVAL(1)
                JLONG         = LONG(3)
                IF (PTR2(IPOSIN+IPOS) .GT. 0) THEN
                   DO 102 IP=PTR2(IPOSIN+IPOS),PTR3(IPOSIN+IPOS) 
                      IF (RARG(IP) .EQ. RVAL(1)) GOTO 913
 102               CONTINUE   
                ENDIF                
             ELSEIF (ICLASS.EQ. 3 ) THEN
C               TEXTES SOUS FORME D'IDENTIFICATEUR
                LONG(5)       = LONG(5) + 1
                TARG(LONG(5)) = CVAL(1:IVAL)
                JLONG         = LONG(5)
                IF (PTR2(IPOSIN+IPOS) .GT. 0) THEN
                   DO 103 IP=PTR2(IPOSIN+IPOS),PTR3(IPOSIN+IPOS) 
                      IF (TARG(IP) .EQ. CVAL(1:IVAL)) GOTO 913
 103               CONTINUE   
                ENDIF                
             ELSEIF (ICLASS.EQ. 4 ) THEN
C               TEXTES SOUS FORME DE TEXTE
                LONG(5)       = LONG(5) + 1
                TARG(LONG(5)) = CVAL(1:IVAL)
                JLONG         = LONG(5)
                IF (PTR2(IPOSIN+IPOS) .GT. 0) THEN
                   DO 104 IP=PTR2(IPOSIN+IPOS),PTR3(IPOSIN+IPOS) 
                      IF (TARG(IP) .EQ. CVAL(1:IVAL)) GOTO 913
 104               CONTINUE   
                ENDIF                
             ELSE
                GOTO 912
             ENDIF
             IF( PTR2(IPOSIN+IPOS) .EQ.0 ) PTR2(IPOSIN+IPOS) = JLONG
             PTR3(IPOSIN+IPOS) = JLONG
C
          ELSE
             GOTO 910
          ENDIF
      ENDIF
      GOTO 1
C
C
C
C
C     -------------------- MESSAGE D'ERREURS ---------------------------
 901  CONTINUE
         CALL UTDEBM('E','COMPILATION DES COMMANDES (ERREUR 01)',
     +                   'ON ATTENDAIT UN IDENTIFICATEUR.')
         CALL UTIMPK('L',' ',1,CHAINE)
         CALL UTPOSI( IDEB-1 )
         CALL UTIMPK('S',' ',1,'*')
      GOTO 999
 902  CONTINUE
         CALL UTDEBM('E','COMPILATION DES COMMANDES (ERREUR 02)',
     +                   'ON ATTENDAIT UN ENTIER.')
         CALL UTIMPK('L',' ',1,CHAINE)
         CALL UTPOSI( IDEB-1 )
         CALL UTIMPK('S',' ',1,'*')
      GOTO 999
 903  CONTINUE
         CALL UTDEBM('E','COMPILATION DES COMMANDES (ERREUR 03)',
     +                   'ON ATTENDAIT UN ENTIER COMPRIS ENTRE '//
     +                   '1 ET 99 ')
         CALL UTIMPK('L',' ',1,CHAINE)
         CALL UTPOSI( IDEB-1 )
         CALL UTIMPK('S',' ',1,'*')
      GOTO 999
 904  CONTINUE
         CALL UTDEBM('E','COMPILATION DES COMMANDES (ERREUR 04)',
     +                   'ON ATTENDAIT LE SEPARATEUR ")"')
         CALL UTIMPK('L',' ',1,CHAINE)
         CALL UTPOSI( IDEB-1 )
         CALL UTIMPK('S',' ',1,'*')
      GOTO 999
 905  CONTINUE
         CALL UTDEBM('E','COMPILATION DES COMMANDES (ERREUR 05)',
     +                   '"'//CVAL(1:IVAL)//'" EST UNE '//
     +                   'OPTION RESERVEE AU MOT CLE FACTEUR')
         CALL UTIMPK('L',' ',1,CHAINE)
         CALL UTPOSI( IDEB-IVAL )
         CALL UTIMPK('S',' ',1,ETOILE(1:MIN(MAX(1,IVAL),70)) )
      GOTO 999
 906  CONTINUE
         CALL UTDEBM('E','COMPILATION DES COMMANDES (ERREUR 06)',
     +                   'ARGUMENT INVALIDE POUR "TYPE"')
         CALL UTIMPK('L',' ',1,CHAINE)
         CALL UTPOSI( IDEB-IVAL )
         CALL UTIMPK('S',' ',1,ETOILE(1:MIN(MAX(1,IVAL),70)) )
      GOTO 999
 907  CONTINUE
         CALL UTDEBM('E','COMPILATION DES COMMANDES (ERREUR 07)',
     +                   'ARGUMENT INVALIDE POUR "STATUT"' )
         CALL UTIMPK('L',' ',1,CHAINE)
         CALL UTPOSI( IDEB-IVAL )
         CALL UTIMPK('S',' ',1,ETOILE(1:MIN(MAX(1,IVAL),70)) )
      GOTO 999
 908  CONTINUE
         CALL UTDEBM('E','COMPILATION DES COMMANDES (ERREUR 08)',
     +                   'ON ATTENDAIT LE SEPARATEUR ":"')
         CALL UTIMPK('L',' ',1,CHAINE)
         CALL UTPOSI( IDEB-1 )
         CALL UTIMPK('S',' ',1,'*')
      GOTO 999
 910  CONTINUE
         CALL UTDEBM('E','COMPILATION DES COMMANDES (ERREUR 10)',
     +                   '"'//CVAL(1:IVAL)//'" EST UN MOT CLE INCONNU')
         CALL UTIMPK('L',' ',1,CHAINE)
         CALL UTPOSI( IDEB-IVAL )
         CALL UTIMPK('S',' ',1,ETOILE(1:MIN(MAX(1,IVAL),70)) )
      GOTO 999
 911  CONTINUE
         CALL UTDEBM('E','COMPILATION DES COMMANDES (ERREUR 11)',
     +                   '"'//CVAL(1:IVAL)//'" '//
     +                   'CHAINE INCORRECTE, ON ATTENDAIT "**" ')
         CALL UTIMPK('L',' ',1,CHAINE)
         CALL UTPOSI( IDEB-IVAL )
         CALL UTIMPK('S',' ',1,ETOILE(1:MIN(MAX(1,IVAL),70)) )
      GOTO 999
 912  CONTINUE
         CALL UTDEBM('E','COMPILATION DES COMMANDES (ERREUR 12)',
     +                'LE TYPE DE LA VALEUR PAR DEFAUT EST NON PREVU.')
      GOTO 999
 913  CONTINUE
         CALL UTDEBM('E','COMPILATION DES COMMANDES (ERREUR 13)',' ')
         CALL UTIMPK('L','LA LISTE DES VALEURS ADMISSIBLES DU MOT CLE'
     +               ,1,NOM(IPOSIN+1)(1:16))
         IF (ICLASS .EQ. 1) THEN
            CALL UTIMPI('L','CONTIENT PLUSIEURS FOIS :',1,IVAL)
         ELSE IF (ICLASS .EQ. 2) THEN
            CALL UTIMPR('L','CONTIENT PLUSIEURS FOIS :',1,RVAL)
         ELSE 
            CALL UTIMPK('L','CONTIENT PLUSIEURS FOIS :',1,CVAL(1:IVAL))
         ENDIF
      GOTO 999
 999  CONTINUE
      CALL UTFINM()
      NBVAL = - 1
 9999 CONTINUE
      END
