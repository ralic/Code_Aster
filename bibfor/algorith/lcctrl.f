        SUBROUTINE LCCTRL ( LOI, DY,   DDY,    NR,  ITMAX, TOLER, ITER,
     &                      IRTETI)
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
C       MODULE STANDARD DE CONTROLE DE LA CONVERGENCE
C       ON A DONC       - CONVERGENCE           (RETURN)
C                       - OU ITERATION SUIVANTE (RETURN 1)
C                       - OU STOP ERREUR
C       MAIS  PAS DE RE-INTEGRATION
C       ----------------------------------------------------------------
C       IN   ITMAX  :  NB MAXI D ITERATIONS LOCALES
C            TOLER  :  TOLERANCE A CONVERGENCE
C            ITER   :  NUMERO ITERATION COURANTE
C            NR     :  DIMENSION DY DDY
C            DY     :  VECTEUR SOLUTION DY = ( DSIG DX1 DX2 DP (DEPS3))
C            DDY    :  VECTEUR CORRECTION SUR LA SOLUTION
C            LOI    :  NOM DE LOI DE COMPORTEMENT
C        OUT RETURN :  CONVERGENCE
C            RETURN1:  ITERATION SUIVANTE
C       ----------------------------------------------------------------
        INTEGER         ITMAX,  ITER,   NR
        REAL*8          TOLER,  DDY(*), DY(*)
        CHARACTER*16    LOI
C       ----------------------------------------------------------------
        INTEGER         NDT ,     NDI
        INTEGER         ITSUP,    NDP
        REAL*8          TER(100), ERR
        REAL*8          DER(10),  DP
        CHARACTER*10    CDP,      CTOL,  CITER
        SAVE            ITSUP,TER
        COMMON /TDIM/   NDT , NDI
C       ----------------------------------------------------------------
        DATA ITSUP      /0/
C
C -     EVALUATION  DE L'ERREUR RELATIVE EN DY, ERR =  !!DDY!!/!!DY!!
C
        IRTETI = 0
        CALL LCVERR ( DY, DDY, NR, 1, ERR  )
C        PRINT *," --- ITERATION ",ITER," ERREUR = ",ERR
        TER(ITER) = ERR
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
C -               ITER >= 5
C
                  IF ( ITER .GE. 5 ) THEN
                    DO 20 I = 1,5
                    DER(I) = ABS(TER(ITER-I-1) - TER(ITER-I))
 20                 CONTINUE
C
C -                 CONVERGENCE REGULIERE SUR LES 4 DERNIERES ITERATIONS
C
                    IF ( ( TER(ITER)   .LT. TER(ITER-1) .AND.
     1                     TER(ITER-1) .LT. TER(ITER-2) .AND.
     2                     TER(ITER-2) .LT. TER(ITER-3) .AND.
     3                     TER(ITER-3) .LT. TER(ITER-4)    ) .OR.
     5                   ( DER(1)      .LT. DER(2)      .AND.
     6                     DER(2)      .LT. DER(3)      .AND.
     7                     DER(3)      .LT. DER(4)         )  )  THEN
C
                    ITSUP = ITSUP + 1
C
C -                     ON ESSAIE ENCORE 10 ITERATIONS ...
C
                        IF ( ITSUP .LT. 10 ) THEN
                        IRTETI = 1
                        GOTO 9999
C
C -                     SINON STOP
C
                        ELSE
                        CALL CODENT(ITER,'G',CITER)
                        CALL CODREE(TOLER,'E',CTOL)
                        CALL UTEXCP(23,LOI,' ERREUR'//
     1                  ' - NON CONVERGENCE A ITERATION MAXI '//CITER//
     2                  ' - CONVERGENCE REGULIERE MAIS TROP LENTE'//
     3                  ' - ERREUR > '//CTOL//
     4                  ' - DIMINUER LA TAILLE D INCREMENT')
                        ENDIF
C
C -                 SINON CONVERGENCE IRREGULIERE STOP
C
                    ELSE
                    CALL CODENT(ITER,'G',CITER)
                    CALL CODREE(TOLER,'E',CTOL)
                    CALL UTEXCP(23,LOI,' ERREUR'//
     1              ' - NON CONVERGENCE A ITERATION MAXI '//CITER//
     2              ' - CONVERGENCE IRREGULIERE ET ERREUR > '//CTOL//
     4              ' - DIMINUER LA TAILLE D INCREMENT')
                    ENDIF
C
C -               ITER < 5 STOP
C
                  ELSE
                  CALL CODENT(ITER,'G',CITER)
                  CALL CODREE(TOLER,'E',CTOL)
                  CALL UTEXCP(23,LOI,' ERREUR'//
     1            ' - NON CONVERGENCE A ITERATION MAXI '//CITER//
     2            ' - ERREUR > '//CTOL//
     4            ' - DIMINUER LA TAILLE D INCREMENT')
                  ENDIF
C
C -           CONVERGENCE A ITMAX
C
              ELSE
              ITSUP = 0
              IRTETI = 0
              GOTO 9999
              ENDIF
C
            ENDIF
 9999   CONTINUE
        END
