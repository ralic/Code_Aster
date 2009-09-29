      SUBROUTINE ASCARM ( NOMSY, MONOAP, NBSUP, NSUPP, NEQ, NBMODE,
     +                    VECMOD, PARMOD, ID, REASUP, SPECTR, REPMOD,
     +                    CORFRE, AMORT, MUAPDE, TCOSUP, IM, NBDIS)
      IMPLICIT  NONE
      INTEGER           NBSUP, NSUPP(*), NEQ, NBMODE, ID,
     +                  TCOSUP(NBSUP,*), IM, NBDIS(*)
      REAL*8            VECMOD(NEQ,*), SPECTR(*), AMORT(*)
      REAL*8            PARMOD(NBMODE,*), REPMOD(NBSUP,NEQ,*)
      REAL*8            REASUP(NBSUP,NBMODE,*)
      CHARACTER*16      NOMSY
      LOGICAL           MONOAP, CORFRE, MUAPDE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 29/09/2009   AUTEUR MACOCCO K.MACOCCO 
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
C        CALCUL DE LA REPONSE POUR CHAQUE MODE
C     ------------------------------------------------------------------
C IN  : NOMSY  : OPTION DE CALCUL
C IN  : MONOAP : =.TRUE. , CAS DU MONO-APPUI
C IN  : NBSUP  : NOMBRE DE SUPPORT
C IN  : NSUPP  : MAX DU NOMBRE DE SUPPORT PAR DIRECTION
C IN  : NEQ    : NOMBRE D'EQUATIONS
C IN  : NBMODE : NOMBRE DE MODES
C IN  : VECMOD : VECTEUR DES MODES
C IN  : PARMOD : VECTEUR DES PARAMETRES MODAUX
C IN  : ID     : DIRECTION
C IN  : REASUP : TABLEAU DES REACTIONS MODALES AUX SUPPORTS
C IN  : SPECTR : TABLEAU DES VALEURS DU SPECTRE
C OUT : REPMOD : VECTEUR DES REPONSES MODALES
C IN  : CORFRE : = .TRUE.  , CORRECTION DES FREQUENCES
C IN  : AMORT  : VECTEUR DES AMORTISSEMENTS MODAUX
C IN  : NBDIS  : APPARTENANCE DES SUPPORTS AUX INTRAGROUPES
C     ------------------------------------------------------------------
      INTEGER   IN, IS, IND, IOC
      REAL*8    UN, XAMO, OMEGA, OMEGA2, XXM, XXX, YYY
C     ------------------------------------------------------------------
C
      UN = 1.D0
C
C     --- CAS DU MONO-APPUI ---
C
      IF ( MONOAP ) THEN
            OMEGA = SQRT(PARMOD(IM,1))
            XAMO  = AMORT(IM)
            IF ( CORFRE ) OMEGA = OMEGA * SQRT( UN - XAMO*XAMO )
            OMEGA2 = OMEGA * OMEGA
            IND = ID + 3*(IM-1)
            XXX = ( PARMOD(IM,2+ID) * SPECTR(IND) ) / OMEGA2
            IF (NOMSY(1:4).EQ.'VITE') XXX = XXX * OMEGA
            IF (NOMSY(1:4).EQ.'ACCE') XXX = XXX * OMEGA2
            DO 12 IN = 1,NEQ
               REPMOD(NBSUP,IN,ID) = XXX * VECMOD(IN,IM)
 12         CONTINUE
C
C     --- CAS DU MULTI-APPUI ---
C
      ELSE
            DO 13 IS = 1,NBSUP
               DO 17 IN = 1,NEQ
                  REPMOD(IS,IN,ID) = 0.D0
 17            CONTINUE
 13         CONTINUE         
            OMEGA = SQRT(PARMOD(IM,1))
            XAMO  = AMORT(IM)
            IF ( CORFRE ) OMEGA = OMEGA * SQRT( UN - XAMO*XAMO )
            OMEGA2 = OMEGA * OMEGA
            XXM = -UN / ( PARMOD(IM,2) * OMEGA2 * OMEGA2 )
            IF (NOMSY(1:4).EQ.'VITE') XXM = XXM * OMEGA
            IF (NOMSY(1:4).EQ.'ACCE') XXM = XXM * OMEGA2
            DO 22 IS = 1,NSUPP(ID)
               IND = ID + 3*(IM-1) + 3*NBMODE*(IS-1)
               XXX = REASUP(IS,IM,ID) * XXM * SPECTR(IND)
               DO 24 IN = 1,NEQ
                  IOC = NBDIS(IS)
                  REPMOD(IOC,IN,ID) =  REPMOD(IOC,IN,ID)+
     &                                        XXX * VECMOD(IN,IM)
 24            CONTINUE
 22         CONTINUE
      ENDIF
C
C --- CAS CORRELE : ON RECOMBINE LES SUPPORTS 
C
      IF ( .NOT.MUAPDE ) THEN
         DO 100 IN = 1,NEQ
            YYY=0.D0
            DO 110 IS = 1,NSUPP(ID)
               IF (TCOSUP(IS,ID).EQ.1) THEN
C              --- COMBINAISON QUADRATIQUE ---
                     XXX = REPMOD(IS,IN,ID)
                     YYY=YYY+SQRT(XXX*XXX)
C
               ELSEIF (TCOSUP(IS,ID).EQ.2) THEN
C              --- COMBINAISON LINEAIRE ---
                     XXX = REPMOD(IS,IN,ID)
                     YYY=YYY+XXX
C
               ELSE
C              --- COMBINAISON VALEUR ABSOLUE ---
                     XXX = ABS( REPMOD(IS,IN,ID) )
                     YYY=YYY+XXX
               ENDIF
 110        CONTINUE
            REPMOD(1,IN,ID) = YYY
 100     CONTINUE
      ENDIF
C
      END
