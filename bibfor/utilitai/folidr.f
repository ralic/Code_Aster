      SUBROUTINE FOLIDR ( LISS, I, MAX, NBMAX, NBDEB, NBFRI, NBFR,
     +                    ENVEL, ELIM )
      IMPLICIT NONE
      INTEGER             I, MAX(*), NBMAX, NBFR, NBDEB, NBFRI, ELIM(*)
      REAL*8              LISS, ENVEL(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 18/09/2002   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C
C     ------------------------------------------------------------------
      INTEGER       IFR, INDEB , INFIN
      REAL*8        EPSI, B, M, YA, YP1, CRLIS
C     ------------------------------------------------------------------
C
      EPSI = 1.0D-6
     
C --- EQUATION DE LA DROITE P1 P2
C
      IF ( I .EQ. 1 ) THEN
C
         M = ( ENVEL(NBFR+MAX(1)) - ENVEL(NBFR+1) ) /
     +                                     ( ENVEL(MAX(1)) - ENVEL(1) )
         B = ENVEL(NBFR+1) - ( M * ENVEL(1) )
C
      ELSE IF ((I.GT.1).AND.(I.LE.NBMAX)) THEN
C
         M = ( ENVEL(NBFR+MAX(I)) - ENVEL(NBFR+MAX(I-1))) /
     +                              ( ENVEL(MAX(I)) - ENVEL(MAX(I-1)) )
         B = ENVEL(NBFR+MAX(I)-1) - ( M * ENVEL(MAX(I)) )
C
      ELSE IF (I.EQ.(NBMAX+1)) THEN
C
         M = ( ENVEL(NBFR+MAX(NBMAX)) - ENVEL(NBFR+NBFRI) ) /
     +                             ( ENVEL(MAX(NBMAX)) - ENVEL(NBFRI) )
         B = ENVEL(NBFR+MAX(NBMAX)) - ( M * ENVEL(MAX(NBMAX)) )
C
      ENDIF
C
C --- VERIFICATION QUE LE POINT A EST EN DESSOUS DE (P1,P2)
C
      IF ( I .EQ. 1 ) THEN 
         INDEB = NBDEB
      ELSE
         INDEB = MAX(I-1)
      ENDIF
C
      IF ( I .EQ. (NBMAX+1) ) THEN 
         INFIN = NBFRI
      ELSE
         INFIN = MAX(I)
      ENDIF
C
      DO 10 IFR = INDEB , INFIN
         IF ((IFR.NE.INDEB).AND.(IFR.NE.INFIN)) THEN
            YA  = M * ENVEL(IFR  ) + B
            YP1 = M * ENVEL(INDEB) + B
            IF (ENVEL(NBFR+IFR-1).LT.YA) THEN
               CRLIS = LOG(ENVEL(IFR)) + LOG(1.0D0+(LISS/100.0D0))
               IF ((LOG(YA).LE.CRLIS).OR.(LOG(YA).LT.LOG(YP1))) THEN
                  ELIM(IFR) = IFR
               ENDIF
            ELSEIF (ENVEL(NBFR+IFR).GT.YA) THEN
               IF ((ENVEL(NBFR+IFR).LT.(YA+EPSI)).AND.
     +                       (IFR.NE.INDEB).AND.(IFR.NE.INFIN)) THEN
                  ELIM(IFR) = IFR
               ENDIF
            ENDIF
         ENDIF
 10   CONTINUE
C
      END
