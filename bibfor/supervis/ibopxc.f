      SUBROUTINE IBOPXC( MOTFAC,KEXCLU,NBSEM,NOM ,DESCR,PTR,NBINT,IARG,
     +                                                   IDEB,IFIN,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)      MOTFAC,KEXCLU(*),   NOM(*),DESCR(*)
      INTEGER                          NBSEM,       PTR(*),NBINT,IARG(*)
      INTEGER                                            IDEB,IFIN,IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 02/10/95   AUTEUR GIBHHAY A.Y.PORTABILITE 
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
C     TRAITEMENT DES INFORMATIONS EXCLUSIONS MUTUELLES
C     ------------------------------------------------------------------
C     CODAGE INTERNE: (-1) EXCLUS,
C                     (-2) UN_PARMI,
C                     (-3) ENSEMBLE,
C                     (-4) AU_MOINS_UN,
C                     (-5) PRESENT_PRESENT,
C                     (-6) PRESENT_ABSENT.
C     ------------------------------------------------------------------
C
C     --- RECHERCHE DES INFORMATIONS RELATIVES AU MOT CLE FACTEUR ---
C-DBG WRITE(6,*)  ' IBOPXC : FIN DU MOT CLE FACTEUR "',MOTFAC,'"'
      DO 10 I=1,NBSEM
C-DBG   WRITE(6,*) '  "',KEXCLU(I),'"  '
        IF (KEXCLU(I)(1:1) .EQ. '&') THEN
           IF (KEXCLU(MIN(NBSEM,I+1)) .EQ. MOTFAC ) THEN
              IDEB0 = I
              GOTO 20
           ENDIF
        ENDIF
   10 CONTINUE
C     --- IL N'Y A RIEN POUR LE MOT CLE FACTEUR COURANT ---
      GOTO 9999
C
C     --- TRAITEMENT DES INFORMATIONS ---
   20 CONTINUE
      IF ( MOTFAC .EQ. '  '  .AND. IDEB0 .NE. 1 ) THEN
         CALL UTMESS('E','COMPILATION DES COMMANDES (ERREUR 52)',
     +                   'DECLARATION DES EXCLUSIONS INCONSISTANTE')
         IER = IER + 1
      ELSE
         I = IDEB0
   31    CONTINUE
         IF ( I .LE. NBSEM ) THEN
C-DBG       WRITE(6,*) MOTFAC,'  "',KEXCLU(I),'"  '
            IF ( KEXCLU(I)(1:1) .EQ. '&' ) THEN
               NBINT = NBINT + 1
               IF ( IDEB .EQ. 0 ) IDEB = NBINT
               IF ( KEXCLU(I)(2:).EQ.'EXCLUS__  '    ) IARG(NBINT) = -1
               IF ( KEXCLU(I)(2:).EQ.'UN_PARMI__'    ) IARG(NBINT) = -2
               IF ( KEXCLU(I)(2:).EQ.'ENSEMBLE__'    ) IARG(NBINT) = -3
               IF ( KEXCLU(I)(2:).EQ.'AU_MOINS_UN__' ) IARG(NBINT) = -4
               IF ( KEXCLU(I)(2:).EQ.'PRESENT_PRESENT')IARG(NBINT) = -5
               IF ( KEXCLU(I)(2:).EQ.'PRESENT_ABSENT_')IARG(NBINT) = -6
               I = I + 1
            ELSE
               IF ( MOTFAC .EQ. '    ' ) THEN
                  NBSAUT = 0
                  DO 21 J=2, PTR(1)
                     IF ( NBSAUT .GT. 0 ) THEN
                        NBSAUT = NBSAUT - 1
                     ELSE
                        IF (DESCR(J)(1:1).EQ. '*' )  NBSAUT = PTR(J)
                        IF ( KEXCLU(I) .EQ. NOM(J) ) THEN
                           IPLACE = J
                           GOTO 23
                        ENDIF
                     ENDIF
   21             CONTINUE
                  IPLACE = 0
                  CALL UTDEBM('E','COMPILATION DES COMMANDES '//
     +                        '(ERREUR 50)','DECLARATION D''EXCLUSION')
                  CALL UTIMPK('L','MOT CLE (FACTEUR)INCONNU',1,
     +                                                       KEXCLU(I) )
                  CALL UTIMPK('L','LISTE DES MOTS CLES ENREGISTRES',
     +                                 0,' ')
                  NBSAUT = 0
                  DO 22 J=2, PTR(1)
                     IF ( NBSAUT .GT. 0 ) THEN
                        NBSAUT = NBSAUT - 1
                     ELSE
                        IF (DESCR(J)(1:1).EQ. '*' )  NBSAUT = PTR(J)
                        CALL UTIMPK('S',' ',1,NOM(J) )
                     ENDIF
   22             CONTINUE
                  CALL UTFINM()
                  IER = IER + 1
   23             CONTINUE
               ELSE
                  CALL UTREMT(KEXCLU(I),NOM(2),PTR(1)-1,IPLACE)
                  IF ( IPLACE .EQ. 0 ) THEN
                     CALL UTDEBM('E','COMPILATION DES COMMANDES '//
     +                        '(ERREUR 51)','DECLARATION D''EXCLUSION')
                     CALL UTIMPK('L','MOT CLE INCONNU',1, KEXCLU(I) )
                     CALL UTIMPK('L','LISTE DES MOTS CLES ENREGISTRES',
     +                                PTR(1)-1,NOM(2) )
                     CALL UTFINM()
                     IER = IER + 1
                  ELSE
                     IPLACE = IPLACE + 1
                  ENDIF
               ENDIF
C-DBG          WRITE(6,*) MOTFAC,'  "',KEXCLU(I),'" = ',IPLACE
               IF ( IPLACE .GT. 0 ) THEN
                  NBINT = NBINT + 1
                  IFIN  = NBINT
                  IARG(NBINT) = IPLACE
               ENDIF
            ENDIF
            I = I + 1
            GOTO 31
         ENDIF
      ENDIF
      NBSEM = IDEB0-1
 9999 CONTINUE
      END
