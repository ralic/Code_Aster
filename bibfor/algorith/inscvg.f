        SUBROUTINE INSCVG( DY,   DDY,    NR,    ITMAX, TOLER, ITER,
     &                      INTG, TYPESS, ESSAI, ICOMP, IRTETI)
C TOLE CRS_505 CRS_507
        IMPLICIT REAL*8 (A-H,O-Z)
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 30/03/2004   AUTEUR CIBHHLV L.VIVAN 
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
C       NADAI_BETON     : CONTROLE DE LA CONVERGENCE
C                                  DE LA CONFORMITE DE LA SOLUTION DP
C                                  ET DE LA RE-INTEGRATION
C                                  ET DU REDECOUPAGE DU PAS DE TEMPS
C       ----------------------------------------------------------------
C       IN   ITMAX  :  NB MAXI D ITERATIONS LOCALES
C            TOLER  :  TOLERANCE A CONVERGENCE
C            ITER   :  NUMERO ITERATION COURANTE
C            NR     :  DIMENSION DY DDY
C            DY     :  VECTEUR SOLUTION DY = ( DSIG DX1 DX2 DP (DEPS3))
C            DDY    :  VECTEUR CORRECTION SUR LA SOLUTION
C            ICOMP  :  COMPTEUR POUR LE REDECOUPAGE DU PAS DE TEMPS
C       VAR  INTG   :  NUMERO INTEGRATION COURANTE
C       OUT  ESSAI  :  SOLUTION D ESSAI
C            TYPESS :  TYPE DE SOLUTION D ESSAI
C                               0 = NUL(0)
C                               1 = ELASTIQUE
C                               2 = EXPLICITE
C                               3 = ESSAI
C            RETURN :  CONVERGENCE
C            RETURN1:  ITERATION SUIVANTE
C            RETURN2:  RE-INTEGRATION
C            RETURN3:  REDECOUPAGE DU PAS DE TEMPS
C       ----------------------------------------------------------------
        INTEGER         TYPESS, ITMAX,  ITER,   INTG, NR
        REAL*8          TOLER,  ESSAI,  DDY(*), DY(*)
C       ----------------------------------------------------------------
        REAL*8          TOLIM, DPLIM
        PARAMETER       ( DPLIM = 1.D-10 )
        PARAMETER       ( TOLIM = 1.D-3  )
C
        INTEGER         NDT ,     NDI
        INTEGER         ITSUP,    NDP,   ICOMP
        REAL*8          TER(100), ERR
        REAL*8          DER(10),  DP
        CHARACTER*10    CDP,      CTOL,  CITER, CINTG
        SAVE            ITSUP,TER
        COMMON /TDIM/   NDT , NDI
C       ----------------------------------------------------------------
        DATA ITSUP      /0/
C
C -- ICOMP = 0 ==> PAS DE REDECOUPAGE EN COURS
C -- ICOMP = 1 ==> 1 REDECOUPAGE EN COURS, UN DEUXIEME REDECOUPAGE EST
C                  POSSIBLE
C -- ICOMP = 2 ==> PAS DE REDECOUPAGE
C
        NDP       = NDT+1
        DP        = DY(NDP)
        IRTETI = 0
C
C -     EVALUATION  DE L'ERREUR RELATIVE EN DY, ERR =  !!DDY!!/!!DY!!
C
        CALL LCVERR ( DY, DDY, NR, 1, ERR  )
        TER(ITER) = ERR
C
C -     CAS DE DP NEGATIF
C       -----------------
C
        IF ( DP .LT. 0.D0 ) THEN
C
C -             SI -DP < 1.E-10 ET ERR < TOLER
C
                IF ( ABS(DP) .LT. DPLIM .AND. ERR .LT. TOLER ) THEN
                CALL CODREE(ABS(DP),'E',CDP)
                CALL UTMESS('A','NADAI_B',
     1          ' INCREMENT DE DEFORMATION CUMULEE (DP) = -'//CDP )
                IRTETI = 0
                GOTO 9999
                ENDIF
C
C -     SI ITER > 5 ,ON ESSAYE AVEC DIFFERENTES VALEURS POUR ESSAI
C
            IF ( ITER .GE. 5 ) THEN
            INTG      = INTG + 1
            TYPESS    = 3
C                       PRINT *,' --- REINTEGRATION ESSAI ',INTG
C
                IF     ( INTG .EQ. 1 ) THEN
                ESSAI = 1.D-15
                IRTETI = 2
                GOTO 9999
                ELSEIF ( INTG .EQ. 2 ) THEN
                TYPESS = 2
                IRTETI = 2
                GOTO 9999
                ELSEIF ( INTG .EQ. 3 ) THEN
                ESSAI = 1.D-10
                IRTETI = 2
                GOTO 9999
                ELSEIF ( INTG .EQ. 4 ) THEN
                ESSAI = 1.D-20
                IRTETI = 2
                GOTO 9999
                ELSEIF ( INTG .EQ. 5 ) THEN
                ESSAI = 1.D-5
                IRTETI = 2
                GOTO 9999
                ELSEIF ( INTG .EQ. 6 ) THEN
                ESSAI = 1.D-2
                IRTETI = 2
                GOTO 9999
                ELSEIF ( INTG .EQ. 7 ) THEN
C
                IF ( ICOMP .EQ. 0 .OR. ICOMP .EQ. 1) THEN
                CALL CODENT(INTG,'G',CINTG)
                CALL CODREE(ABS(DP),'E',CDP)
                CALL UTMESS('I','NADAI_B','ERREUR D INTEGRATION '//
     1          '- ESSAI D INTEGRATION  NUMERO '//CINTG//
     2          '- CONVERGENCE VERS  UNE SOLUTION NON CONFORME '//
     3          '- INCREMENT DE DEFORMATION CUMULEE NEGATIVE = -'//CDP//
     4          '- REDECOUPAGE DU PAS DE TEMPS')
                   IRTETI = 3
                   GOTO 9999
                ELSE
                CALL CODENT(INTG,'G',CINTG)
                CALL CODREE(ABS(DP),'E',CDP)
                CALL UTEXCP(23,'NADAI_B','ERREUR D INTEGRATION '//
     1          '- ESSAI D INTEGRATION  NUMERO '//CINTG//
     2          '- CONVERGENCE VERS  UNE SOLUTION NON CONFORME '//
     3          '- INCREMENT DE DEFORMATION CUMULEE NEGATIVE = -'//CDP//
     4          '- CHANGER LA TAILLE D INCREMENT')
                ENDIF
                ENDIF
C
C -         SINON ITERATION SUIVANTE
C
            ELSE
            IRTETI = 1
            GOTO 9999
            ENDIF
C
C -     CAS DE DP POSITIF
C       -----------------
C
        ELSEIF ( DP .GE. 0.D0 ) THEN
C
C -         ITER < ITMAX
C           ------------
C
            IF ( ITER .LT. ITMAX ) THEN
C
C -             CONVERGENCE
C
                IF ( ERR .LE. TOLER ) THEN
                IRTETI = 0
                GOTO 9999
                ELSE
C
C -             NON CONVERGENCE ITERATION SUIVANTE
C
                IRTETI = 1
                GOTO 9999
                ENDIF
C
C -         ITER >= ITMAX
C           ------------
C
            ELSEIF ( ITER .GE. ITMAX ) THEN
C
C -             NON CONVERGENCE ET ITMAX ATTEINT
C
                IF ( ERR .GT. TOLER ) THEN
C
C -               ITER >= 6
C
                  IF ( ITER .GE. 6 ) THEN
C
                    DO 20 I = 1,5
                    DER(I) = ABS(TER(ITER-I-1) - TER(ITER-I))
 20                 CONTINUE
C
C -                 CONVERGENCE REGULIERE SUR LES 5 DERNIERES ITERATIONS
C
                    IF ( ( TER(ITER)   .LT. TER(ITER-1) .AND.
     1                     TER(ITER-1) .LT. TER(ITER-2) .AND.
     2                     TER(ITER-2) .LT. TER(ITER-3) .AND.
     3                     TER(ITER-3) .LT. TER(ITER-4) .AND.
     4                     TER(ITER-4) .LT. TER(ITER-5)     ) .OR.
     5                   ( DER(1)      .LT. DER(2)      .AND.
     6                     DER(2)      .LT. DER(3)      .AND.
     7                     DER(3)      .LT. DER(4)      .AND.
     8                     DER(4)      .LT. DER(5)          )  )  THEN
C
                    ITSUP = ITSUP + 1
C
C -                     SI ERR < TOLIM ET DP < DPLIM , ON ACCEPTE
C
                        IF ( ERR .LT. TOLIM .AND. DP .LT. DPLIM ) THEN
                        IRTETI = 0
                        GOTO 9999
C
C -                     SINON ON ESSAIE ENCORE 10 ITERATIONS ..
C
                        ELSEIF ( ITSUP .LT. 10 ) THEN
                        IRTETI = 1
                        GOTO 9999
C
C -                     SINON STOP
C
                        ELSE
                        IF ( ICOMP .EQ. 0 .OR. ICOMP .EQ. 1 ) THEN
                        CALL CODENT(ITER,'G',CITER)
                        CALL CODREE(TOLER,'E',CTOL)
                        CALL UTMESS ('I','NADAI_B',' ERREUR'//
     1                  ' - NON CONVERGENCE A ITERATION MAXI '//CITER//
     2                  ' - CONVERGENCE REGULIERE MAIS TROP LENTE'//
     3                  ' - ERREUR > '//CTOL//
     4                  ' - REDECOUPAGE DU PAS DE TEMPS')
                           IRTETI = 3
                           GOTO 9999
                        ELSE
                        CALL CODENT(ITER,'G',CITER)
                        CALL CODREE(TOLER,'E',CTOL)
                        CALL UTEXCP(23,'NADAI_B',' ERREUR'//
     1                  ' - NON CONVERGENCE A ITERATION MAXI '//CITER//
     2                  ' - CONVERGENCE REGULIERE MAIS TROP LENTE'//
     3                  ' - ERREUR > '//CTOL//
     4                  ' - DIMINUER LA TAILLE D INCREMENT')
                        ENDIF
                        ENDIF
C
C -                 SINON STOP
C
                    ELSE
                    IF ( ICOMP .EQ. 0 .OR. ICOMP .EQ. 1 ) THEN
                    CALL CODENT(ITER,'G',CITER)
                    CALL CODREE(TOLER,'E',CTOL)
                    CALL UTMESS ('I','NADAI_B',' ERREUR'//
     1              ' - NON CONVERGENCE A ITERATION MAXI '//CITER//
     2              ' - CONVERGENCE IRREGULIERE & ERREUR > '//CTOL//
     4              ' - REDECOUPAGE DU PAS DE TEMPS')
                       IRTETI = 3
                       GOTO 9999
                    ELSE
                    CALL CODENT(ITER,'G',CITER)
                    CALL CODREE(TOLER,'E',CTOL)
                    CALL UTEXCP(23,'NADAI_B',' ERREUR'//
     1              ' - NON CONVERGENCE A ITERATION MAXI '//CITER//
     2              ' - CONVERGENCE IRREGULIERE & ERREUR > '//CTOL//
     4              ' - DIMINUER LA TAILLE D INCREMENT')
                    ENDIF
                    ENDIF
C
C -               ITER < 6 STOP
C
                  ELSE
                  IF ( ICOMP .EQ. 0 .OR. ICOMP .EQ. 1 ) THEN
                  CALL CODENT(ITER,'G',CITER)
                  CALL CODREE(TOLER,'E',CTOL)
                  CALL UTMESS ('I','NADAI_B',' ERREUR'//
     1            ' - NON CONVERGENCE A ITERATION MAXI '//CITER//
     2            ' - ERREUR > '//CTOL//
     4            ' - REDECOUPAGE DU PAS DE TEMPS')
                  IRTETI = 3
                  GOTO 9999
                  ELSE
                  CALL CODENT(ITER,'G',CITER)
                  CALL CODREE(TOLER,'E',CTOL)
                  CALL UTEXCP(23,'NADAI_B',' ERREUR'//
     1            ' - NON CONVERGENCE A ITERATION MAXI '//CITER//
     2            ' - ERREUR > '//CTOL//
     4            ' - DIMINUER LA TAILLE D INCREMENT')
                  ENDIF
                  ENDIF
C
C -           CONVERGENCE A ITMAX
C
              ELSE
              ITSUP = 0
              IRTETI = 0
              GOTO 9999
              ENDIF
            ENDIF
        ENDIF
C
 9999   CONTINUE
        END
