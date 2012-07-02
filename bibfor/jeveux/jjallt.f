      SUBROUTINE JJALLT (LONOI, IC, GI, TYPEI, LTYPI, CI, JCTAB, JCDYN)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C TOLE CRP_18  CRS_508 CRP_4
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER             LONOI,IC,LTYPI,JCTAB
      CHARACTER*(*)       GI,TYPEI,CI
C-----------------------------------------------------------------------
C CHAPEAU A LA ROUTINE JJALLS POUR PLACER CORRECTEMENT LE SEGMENT DE
C VALEURS EN FONCTION DU TYPE ASSOCIE
C
C IN   LONOI  : LONGUEUR DU SEGMENT DE VALEURS
C IN   IC     : CLASSE DE L'OBJET
C IN   GI     : GENRE DE L'OBJET
C IN   TYPEI  : TYPE DE L'OBJET
C IN   LTYPI  : LONGUEUR DU TYPE
C IN   CI     : = 'INIT' POUR INITIALISER LE SEGMENT DE VALEUR
C OUT  JCTAB  : ADRESSE PAR RAPPORT AU COMMUN DE REFERENCE EN 
C               SEGMENTATION MEMOIRE
C OUT  JCDYN  : ADRESSE PAR RAPPORT AU COMMUN DE REFERENCE EN 
C               ALLOCATION DYNAMIQUE
C
      INTEGER        IZR,IZC,IZL,IZK8,IZK16,IZK24,IZK32,IZK80,JBID,IZI4,
     &               JCDYN
      EQUIVALENCE    (IZR,ZR),(IZC,ZC),(IZL,ZL),(IZK8,ZK8),(IZK16,ZK16),
     &               (IZK24,ZK24),(IZK32,ZK32),(IZK80,ZK80),(IZI4,ZI4)
C DEB ------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      JCTAB = 0
      IF ( TYPEI(1:1) .EQ. 'I' ) THEN
        CALL JJALLS (LONOI,IC,GI,TYPEI,LTYPI,CI,ZI ,JBID,JCTAB,JCDYN)
      ELSE IF ( TYPEI .EQ. 'S' ) THEN
        CALL JJALLS (LONOI,IC,GI,TYPEI,LTYPI,CI,IZI4,JBID,JCTAB,JCDYN)
      ELSE IF ( TYPEI(1:1) .EQ. 'R' ) THEN
        CALL JJALLS (LONOI,IC,GI,TYPEI,LTYPI,CI,IZR,JBID,JCTAB,JCDYN)
      ELSE IF ( TYPEI(1:1) .EQ. 'C' ) THEN
        CALL JJALLS (LONOI,IC,GI,TYPEI,LTYPI,CI,IZC,JBID,JCTAB,JCDYN)
      ELSE IF ( TYPEI(1:1) .EQ. 'K' ) THEN
        IF ( LTYPI .EQ. 8 ) THEN
          CALL JJALLS (LONOI,IC,GI,TYPEI,LTYPI,CI,IZK8,JBID,JCTAB,JCDYN)
        ELSE IF ( LTYPI .EQ. 16 ) THEN
          CALL JJALLS(LONOI,IC,GI,TYPEI,LTYPI,CI,IZK16,JBID,JCTAB,JCDYN)
        ELSE IF ( LTYPI .EQ. 24 ) THEN
          CALL JJALLS(LONOI,IC,GI,TYPEI,LTYPI,CI,IZK24,JBID,JCTAB,JCDYN)
        ELSE IF ( LTYPI .EQ. 32 ) THEN
          CALL JJALLS(LONOI,IC,GI,TYPEI,LTYPI,CI,IZK32,JBID,JCTAB,JCDYN)
        ELSE IF ( LTYPI .EQ. 80 ) THEN
          CALL JJALLS(LONOI,IC,GI,TYPEI,LTYPI,CI,IZK80,JBID,JCTAB,JCDYN)
        ENDIF
      ELSE IF ( TYPEI(1:1) .EQ. 'L' ) THEN
        CALL JJALLS (LONOI,IC,GI,TYPEI,LTYPI,CI,IZL,JBID,JCTAB,JCDYN)
      ENDIF
C FIN ------------------------------------------------------------------
      END
