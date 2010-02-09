        SUBROUTINE LMACVG ( DY,   DDY,    NR,    ITMAX, TOLER, ITER,
     &                      INTG, TYPESS, ESSAI, ICOMP, IRTETI)
C TOLE CRS_505 CRS_507
        IMPLICIT REAL*8 (A-H,O-Z)
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/02/2010   AUTEUR SELLENET N.SELLENET 
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
C       MODELE VISCOPLASTIQUE DE BESANCON EN VITESSE
C                   : CONTROLE DE LA CONVERGENCE
C                                  DE LA CONFORMITE DE LA SOLUTION DV
C                                  DE LA RE-INTEGRATION
C                                  ET DU REDECOUPAGE DU PAS DE TEMPS
C       ----------------------------------------------------------------
C       IN   ITMAX  :  NB MAXI D ITERATIONS LOCALES
C            TOLER  :  TOLERANCE A CONVERGENCE
C            ITER   :  NUMERO ITERATION COURANTE
C            NR     :  DIMENSION DY DDY
C            DY     :  VECTEUR SOLUTION DY = ( DSIG DX DX1 DX2 DV )
C            DDY    :  VECTEUR CORRECTION SUR LA SOLUTION
C            ICOMP  :  COMPTEUR POUR LE REDECOUPAGE DU PAS DE TEMPS
C       VAR  INTG   :  NUMERO INTEGRATION COURANTE
C       OUT  ESSAI  :  SOLUTION D ESSAI
C            TYPESS :  TYPE DE SOLUTION D ESSAI
C                               0 = NUL(0)
C                               1 = ELASTIQUE
C                               2 = EXPLICITE
C                               3 = ESSAI
C            IRTETI = 0 :  CONVERGENCE
C            IRTETI = 1 :  ITERATION SUIVANTE
C            IRTETI = 2 :  RE-INTEGRATION
C            IRTETI = 3 :  REDECOUPAGE DU PAS DE TEMPS
C       ----------------------------------------------------------------
        INTEGER         TYPESS, ITMAX,  ITER,   INTG, NR, ICOMP
        REAL*8          TOLER,  ESSAI,  DDY(*), DY(*)
C       ----------------------------------------------------------------
        REAL*8          TOLIM, DVLIM
        PARAMETER       ( DVLIM = 1.D-10 )
        PARAMETER       ( TOLIM = 1.D-3  )
C
        INTEGER         NDT ,     NDI, VALI
        INTEGER         ITSUP,    NDP
        REAL*8          TER(100), ERR, DSIG,LCNRTE
        REAL*8          DER(10),  DV
        CHARACTER*10    CDV,      CTOL,  CITER, CINTG
        CHARACTER*24 VALK(2)
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
        NDP       = 4*NDT+1
        DV        = DY(NDP)
        IRTETI = 0
C
C -     EVALUATION  DE L'ERREUR RELATIVE EN DY, ERR =  !!DDY!!/!!DY!!
C
C -------------TEMPORAIRE-------------
C         ERR=DDY(NDP)/DY(NDP)
C
        CALL LCVERR ( DY, DDY, NR, 2, ERR  )
        TER(ITER) = ERR
C
C -------TEST SUR LA NORME DE DSIG AFIN DE REDECOUPER LE PAS DE
C        TEMPS SI RISQUE DE DIVERGENCE LORS DE L'INTEGRATION
C
        DSIG = LCNRTE ( DY(1) )
        IF ( DSIG . GE . 10000.D0 ) THEN
                IF ( ICOMP .EQ. 0 .OR. ICOMP .EQ. 1) THEN
                CALL CODENT(INTG,'G',CINTG)
                CALL U2MESK('I','ALGORITH5_13',1,CINTG)
                IRTETI = 3
                GOTO 9999
                ELSE
                VALI=INTG
                CALL UTEXCM(23,'ALGORITH16_72',0,' ',1,VALI,0,0.D0)

                ENDIF
        ENDIF

C
C
C -     CAS DE DV NEGATIF
C       -----------------
C
        IF ( DV .LT. 0.D0 ) THEN
C
C -             SI -DV < 1.E-10 ET ERR < TOLER
C
                IF ( ABS(DV) .LT. DVLIM .AND. ERR .LT. TOLER ) THEN
                CALL CODREE(ABS(DV),'E',CDV)
                CALL U2MESK('A','ALGORITH5_14',1,CDV)
                IRTETI = 0
                GOTO 9999
                ENDIF
C
C -     SI ITER > 3 ,ON ESSAYE AVEC UNE SOLUTION DE DEPART ELASTIQUE
C
            IF ( ITER .GE. 3 ) THEN
            INTG      = INTG + 1
                IF     ( INTG .EQ. 1 ) THEN
                TYPESS    = 1
                IRTETI = 2
                GOTO 9999
C
C -     SI ITER > 3 ,ON ESSAYE AVEC DIFFERENTES VALEURS POUR ESSAI
C
                ELSEIF ( INTG .EQ. 2 ) THEN
                ESSAI = 1.D-25
                TYPESS    = 3
                IRTETI = 2
                GOTO 9999
                ELSEIF ( INTG .EQ. 3 ) THEN
                ESSAI = 1.D-2
                TYPESS    = 3
                IRTETI = 2
                GOTO 9999
                ELSEIF ( INTG .EQ. 4 ) THEN
                ESSAI = 1.D-10
                TYPESS    = 3
                IRTETI = 2
                GOTO 9999
                ELSEIF ( INTG .EQ. 5 ) THEN
C
                IF ( ICOMP .EQ. 0 .OR. ICOMP .EQ. 1) THEN
                CALL CODENT(INTG,'G',CINTG)
                CALL CODREE(ABS(DV),'E',CDV)
                 VALK(1) = CINTG
                 VALK(2) = CDV
                 CALL U2MESK('I','ALGORITH2_55', 2 ,VALK)
                IRTETI = 3
                GOTO 9999
                ELSE
                VALI = INTG
                VALR = DV
                CALL UTEXCM(23,'ALGORITH16_73',0,' ',1,VALI,1,VALR)
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
C -     CAS DE DV POSITIF
C       -----------------
C
        ELSEIF ( DV .GE. 0.D0 ) THEN
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
     &                     TER(ITER-1) .LT. TER(ITER-2) .AND.
     &                     TER(ITER-2) .LT. TER(ITER-3) .AND.
     &                     TER(ITER-3) .LT. TER(ITER-4) .AND.
     &                     TER(ITER-4) .LT. TER(ITER-5)     ) .OR.
     &                   ( DER(1)      .LT. DER(2)      .AND.
     &                     DER(2)      .LT. DER(3)      .AND.
     &                     DER(3)      .LT. DER(4)      .AND.
     &                     DER(4)      .LT. DER(5)          )  )  THEN
C
                    ITSUP = ITSUP + 1
C
C -                     SI ERR < TOLIM ET DV < DVLIM , ON ACCEPTE
C
                        IF ( ERR .LT. TOLIM .AND. DV .LT. DVLIM ) THEN
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
                        IF ( ICOMP .EQ. 0 .OR. ICOMP .EQ. 1) THEN
                        CALL CODENT(ITER,'G',CITER)
                        CALL CODREE(TOLER,'E',CTOL)
                         VALK(1) = CITER
                         VALK(2) = CTOL
                         CALL U2MESK('I','ALGORITH2_56', 2 ,VALK)
                        IRTETI = 3
                        GOTO 9999
                        ELSE
                        VALI = ITER
                CALL UTEXCM(23,'ALGORITH16_74',0,' ',1,VALI,0,0.D0)

                        ENDIF
                        ENDIF
C
C -                 SINON STOP
C
                    ELSE
                    IF ( ICOMP .EQ. 0 .OR. ICOMP .EQ. 1) THEN
                    CALL CODENT(ITER,'G',CITER)
                    CALL CODREE(TOLER,'E',CTOL)
                     VALK(1) = CITER
                     VALK(2) = CTOL
                     CALL U2MESK('I','ALGORITH2_57', 2 ,VALK)
                    IRTETI = 3
                    GOTO 9999
                    ELSE
                      VALI = ITER
                      VALR = TOLER
                CALL UTEXCM(23,'ALGORITH16_75',0,' ',1,VALI,1,VALR)

                    ENDIF
                    ENDIF
C
C -               ITER < 6 STOP
C
                  ELSE
                  IF ( ICOMP .EQ. 0 .OR. ICOMP .EQ. 1) THEN
                  CALL CODENT(ITER,'G',CITER)
                  CALL CODREE(TOLER,'E',CTOL)
                   VALK(1) = CITER
                   VALK(2) = CTOL
                   CALL U2MESK('I','ALGORITH2_58', 2 ,VALK)
                  IRTETI = 3
                  GOTO 9999
                  ELSE
                    VALI = ITER
                    VALR = TOLER
                CALL UTEXCM(23,'ALGORITH16_76',0,' ',1,VALI,1,VALR)
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
