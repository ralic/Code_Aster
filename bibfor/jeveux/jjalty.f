      SUBROUTINE JJALTY ( TYPEI , LTYPI , CEL , INATB , JCTAB )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 13/11/2012   AUTEUR COURTOIS M.COURTOIS 
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
C TOLE CRS_508 CRP_18
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER                     LTYPI , INATB , JCTAB
      CHARACTER*(*)       TYPEI , CEL
C-----------------------------------------------------------------------
C ALLOUE LE SEGMENT DE VALEURS EN MEMOIRE ET LE POSITIONNE EN
C FONCTION DU TYPE ASSOCIE
C
C IN   TYPEI  : TYPE DE L'OBJET
C IN   LTYPI  : LONGUEUR DU TYPE
C IN   CEL    : 'L' OU 'E'
C IN   INATB  : TYPE D'OBJET 1:OS 2:CO 3:OC
C OUT  JCTAB  : ADRESSE PAR RAPPORT AU COMMUN DE REFERENCE
C
C-----------------------------------------------------------------------
      INTEGER         IZR,IZC,IZL,IZK8,IZK16,IZK24,IZK32,IZK80,IZI4
      EQUIVALENCE    (IZR,ZR),(IZC,ZC),(IZL,ZL),(IZK8,ZK8),(IZK16,ZK16),
     &               (IZK24,ZK24),(IZK32,ZK32),(IZK80,ZK80),(IZI4,ZI4)
C DEB ------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      JCTAB = 0
      IF ( TYPEI .EQ. 'I' ) THEN
        CALL JXVEUO ( CEL , ZI  , INATB , JCTAB )
      ELSE IF ( TYPEI .EQ. 'S' ) THEN
        CALL JXVEUO ( CEL , IZI4 , INATB , JCTAB )
      ELSE IF ( TYPEI .EQ. 'R' ) THEN
        CALL JXVEUO ( CEL , IZR , INATB , JCTAB )
      ELSE IF ( TYPEI .EQ. 'C' ) THEN
        CALL JXVEUO ( CEL , IZC , INATB , JCTAB )
      ELSE IF ( TYPEI .EQ. 'K' ) THEN
        IF ( LTYPI .EQ. 8 ) THEN
          CALL JXVEUO ( CEL , IZK8  , INATB , JCTAB )
        ELSE IF ( LTYPI .EQ. 16 ) THEN
          CALL JXVEUO ( CEL , IZK16 , INATB , JCTAB )
        ELSE IF ( LTYPI .EQ. 24 ) THEN
          CALL JXVEUO ( CEL , IZK24 , INATB , JCTAB )
        ELSE IF ( LTYPI .EQ. 32 ) THEN
          CALL JXVEUO ( CEL , IZK32 , INATB , JCTAB )
        ELSE IF ( LTYPI .EQ. 80 ) THEN
          CALL JXVEUO ( CEL , IZK80 , INATB , JCTAB )
        ELSE
          CALL JXVEUO ( CEL , IZK8  , INATB , JCTAB )
        ENDIF
      ELSE IF ( TYPEI .EQ. 'L' ) THEN
        CALL JXVEUO ( CEL , IZL , INATB , JCTAB )
      ENDIF
C FIN ------------------------------------------------------------------
      END
