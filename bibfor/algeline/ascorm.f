      SUBROUTINE ASCORM(MONOAP,TYPCMO,NBSUP,NSUPP,NEQ,NBMODE,REPMOD,
     +                  AMORT,ZRVAL,ID,TEMPS,ZRCREP,TABS)
      IMPLICIT  REAL*8 (A-H,O-Z)
      INTEGER           NEQ,NBMODE,ID,NSUPP(*)
      REAL*8            REPMOD(NBMODE,NBSUP,NEQ,*),AMORT(*)
      REAL*8            ZRVAL(NBMODE,*),TABS(NBSUP,*)
      REAL*8            TEMPS,ZRCREP(NBSUP,NEQ,*)
      CHARACTER*(*)     TYPCMO
      LOGICAL           MONOAP
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 17/01/2001   AUTEUR CIBHHLV L.VIVAN 
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
C     COMMANDE : COMB_SISM_MODAL
C        RECOMBINAISON DES REPONSES MODALES
C     ------------------------------------------------------------------
C IN  : TYPCMO : TYPE DE COMBINAISON
C IN  : MONOAP : =.TRUE. , CAS DU MONO-APPUI
C IN  : NBSUP  : NOMBRE DE SUPPORT
C IN  : NSUPP  : MAX DU NOMBRE DE SUPPORT PAR DIRECTION
C IN  : NEQ    : NOMBRE D'EQUATIONS
C IN  : NBMODE : NOMBRE DE MODES
C IN  : REPMOD : VECTEUR DES REPONSES MODALES
C IN  : AMORT  : VECTEUR DES AMORTISSEMENTS MODAUX
C IN  : ZRVAL  : VECTEUR DES PARAMETRES MODAUX
C IN  : ID     : DIRECTION
C IN  : TEMPS  : DUREE DE LA PARTIE FORTE SU SEISME (TYPCMO='DSC')
C OUT : ZRCREP : VECTEUR DES COMBINAISONS DES REPONSES DES MODES
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16               ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                 ZK32
      CHARACTER*80                                          ZK80
      COMMON  / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      REAL*8  B1, B12, B2, B22, W0, W1, W12, W2, W22
      REAL*8  B1W1, B2W2, BP1, BP2, WP1, WP2, BP1W1, BP2W2
      REAL*8  XNU, XDE, XXX, XX1, XX2, TEST
      REAL*8  ZERO, DEMI, UN, DEUX, QUATRE, HUIT
      INTEGER NSUP
C
      CALL JEMARQ()
      ZERO   = 0.D0
      DEMI   = 0.5D0
      UN     = 1.D0
      DEUX   = 2.D0
      QUATRE = 4.D0
      HUIT   = 8.D0
C
C
      IF (MONOAP) THEN 
         NSUP=NBSUP
      ELSE
         NSUP=NSUPP(ID)
      ENDIF
C
      DO 2 IS = 1,NSUP      
         DO 3 IN = 1,NEQ
            ZRCREP(IS,IN,ID) = ZERO
 3       CONTINUE
 2    CONTINUE
C
C     --- COMBINAISON EN VALEURS ABSOLUES ---
      IF (TYPCMO(1:3).EQ.'ABS') THEN
         DO 4 IM = 1,NBMODE
            DO 5 IS = 1,NSUP 
               DO 6 IN = 1,NEQ
                  XXX = REPMOD(IM,IS,IN,ID)
                  ZRCREP(IS,IN,ID) = ZRCREP(IS,IN,ID) + ABS(XXX)
 6             CONTINUE
 5          CONTINUE
 4       CONTINUE
         DO 7 IS = 1,NSUP      
            DO 8 IN = 1,NEQ
               ZRCREP(IS,IN,ID) = ZRCREP(IS,IN,ID) * ZRCREP(IS,IN,ID)
 8          CONTINUE
 7       CONTINUE
C
C     --- AVEC REGLE DES "DIX POUR CENT" ---
      ELSEIF (TYPCMO(1:3).EQ.'DPC') THEN
         CALL WKVECT('&&ASCORM.DPC','V V R',NEQ*NSUP,JABS)
         IM = 0
 40      CONTINUE
         IM = IM + 1
         IF (IM.LE.NBMODE) THEN
            IM1 = IM
            DO 41 IS = 1,NSUP      
               DO 42 IN = 1,NEQ
                  TABS(IS,IN) = ABS(REPMOD(IM1,IS,IN,ID))  
 42            CONTINUE                                 
 41         CONTINUE
 52         CONTINUE
            IF (IM1.LE.NBMODE) THEN
               W1 = SQRT(ZRVAL(IM1,1))
               II = 0
               DO 44 IM2 = IM1+1,NBMODE
                  W2 = SQRT(ZRVAL(IM2,1))
                  W0 = DEMI * ( W1 + W2 )
                  TEST = ( W1 - W2 ) / W0
                  IF (ABS(TEST).LE.0.10D0) THEN
                     II = 1
                     IM = IM + 1
                     DO 45 IS = 1,NSUP      
                        DO 46 IN = 1,NEQ
                           XXX = ABS(REPMOD(IM2,IS,IN,ID))
                           TABS(IS,IN)= TABS(IS,IN)  + XXX
 46                     CONTINUE
 45                  CONTINUE
                  ELSE
                     IF (II.EQ.0) GOTO 48
                     IM1 = IM2 - 1
                     GOTO 52
                  ENDIF
 44            CONTINUE
            ENDIF
 48         CONTINUE
            DO 49 IS = 1,NSUP      
               DO 50 IN = 1,NEQ
                   XXX =  TABS(IS,IN)
                   ZRCREP(IS,IN,ID) = ZRCREP(IS,IN,ID) + XXX*XXX
 50            CONTINUE
 49         CONTINUE
            GOTO 40
         ENDIF
         CALL JEDETR('&&ASCORM.DPC')
      ELSE
C
C     --- COMBINAISON QUADRATIQUE SIMPLE ---
      DO 10 IM = 1,NBMODE
         DO 11 IS = 1,NSUP      
            DO 12 IN = 1,NEQ
               XXX = REPMOD(IM,IS,IN,ID)            
               ZRCREP(IS,IN,ID) = ZRCREP(IS,IN,ID) + ( XXX * XXX )
 12         CONTINUE
 11      CONTINUE
 10   CONTINUE
C
C     --- CQC AVEC FORMULE DE DER-KIUREGHIAN ---
      IF (TYPCMO(1:3).EQ.'CQC') THEN
         DO 20 IM1 = 1,NBMODE-1
            B1 = AMORT(IM1)
            W1 = SQRT(ZRVAL(IM1,1))
            B1W1 = B1 * W1
            B12 = B1 * B1
            W12 = W1 * W1
            DO 22 IM2 = IM1+1,NBMODE
               B2 = AMORT(IM2)
               W2 = SQRT(ZRVAL(IM2,1))
               B2W2 = B2 * W2
               B22 = B2 * B2
               W22 = W2 * W2
               XNU = (SQRT(B1W1*B2W2)) * (B1W1+B2W2) * W1 * W2 * HUIT
               XDE = (W12-W22)*(W12-W22) + B1W1*B2W2*(W12+W22)*QUATRE
     +                                   + (B12+B22)*W12*W22*QUATRE
               XXX = XNU / XDE
               DO 21 IS = 1,NSUP      
                  DO 24 IN = 1,NEQ
                     XX1 = REPMOD(IM1,IS,IN,ID)
                     XX2 = REPMOD(IM2,IS,IN,ID)
                      ZRCREP(IS,IN,ID) = ZRCREP(IS,IN,ID) 
     +                                + (DEUX*XX1*XX2*XXX)
 24               CONTINUE
 21            CONTINUE
 22         CONTINUE
 20      CONTINUE
C
C     --- DSC AVEC FORMULE DE ROSENBLUETH ---
      ELSEIF (TYPCMO(1:3).EQ.'DSC') THEN
         DO 30 IM1 = 1,NBMODE-1
            B1 = AMORT(IM1)
            W1 = SQRT(ZRVAL(IM1,1))
            BP1 = B1 + ( DEUX / ( TEMPS * W1 ) )
            WP1 = W1 * (SQRT( UN - (BP1*BP1) ) ) 
            BP1W1 = BP1 * W1    
            DO 31 IM2 = IM1+1,NBMODE
               B2 = AMORT(IM2)
               W2 = SQRT(ZRVAL(IM2,1))
               BP2 = B2 + ( DEUX / ( TEMPS * W2 ) )
               WP2 = W2 * (SQRT( UN - (BP2*BP2) ) )
               BP2W2 = BP2 * W2      
               XDE = ( WP1-WP2 ) / ( BP1W1 + BP2W2 )
               XXX = UN / ( UN + (XDE*XDE) )
               DO 32 IS = 1,NSUP      
                  DO 34 IN = 1,NEQ
                     XX1 = REPMOD(IM1,IS,IN,ID)
                     XX2 = REPMOD(IM2,IS,IN,ID)
                     ZRCREP(IS,IN,ID) = ZRCREP(IS,IN,ID) 
     +                                + (DEUX*XX1*XX2*XXX)
 34               CONTINUE
 32            CONTINUE
 31         CONTINUE
 30      CONTINUE
      ENDIF
      ENDIF
C
      CALL JEDEMA()
      END
