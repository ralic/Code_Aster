      SUBROUTINE XMMFFD(ALIAS,XI,YI,
     &                  FF,DFF,DDFF,IRET)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/10/2007   AUTEUR NISTOR I.NISTOR 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      CHARACTER*8 ALIAS 
      REAL*8      XI
      REAL*8      YI
      REAL*8      FF(9)
      REAL*8      DFF(2,9)
      REAL*8      DDFF(3,9)
      INTEGER     IRET                  
C
C ----------------------------------------------------------------------
C ROUTINE UTILITAIRE (CONTACT X-FEM AVEC LA METHODE CONTINUE)
C ----------------------------------------------------------------------
C ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
C TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
C ----------------------------------------------------------------------
C CALCUL DES FONCTIONS DE FORMES ET DE LEURS DERIVEES
C AU POINT DE COORDONNEES XI,YI (COORDONNES PARAMETRIQUES)
C
C IN  ALIAS  : NOM D'ALIAS DE L'ELEMENT
C IN  XI     : POINT DE CONTACT SUIVANT KSI1 DES
C               FONCTIONS DE FORME ET LEURS DERIVEES
C IN  YI     : POINT DE CONTACT SUIVANT KSI2 DES
C               FONCTIONS DE FORME ET LEURS DERIVEES
C OUT FF     : FONCTIONS DE FORMES EN XI,YI
C OUT DFF    : DERIVEES PREMIERES DES FONCTIONS DE FORME EN XI YI
C OUT DDFF   : DERIVEES SECONDES DES FONCTIONS DE FORME EN XI YI
C OUT IRET   : RETOURNE UN CODE ERREUR
C                0  TOUT VA BIEN
C                1  ELEMENT INCONNU
C
C ----------------------------------------------------------------------
      INTEGER I
      REAL*8  UNS4
      REAL*8  A,B,C,D
C-----------------------------------------------------------------------
C
      IRET   = 0
      DO 10 I=1,9
        FF(I)     = 0.D0
        DFF(1,I)  = 0.D0  
        DFF(2,I)  = 0.D0                    
        DDFF(1,I) = 0.D0  
        DDFF(2,I) = 0.D0  
        DDFF(3,I) = 0.D0  
 10   CONTINUE
C      
      IF (ALIAS(1:3).EQ.'SG2') THEN
        FF(1) = 0.5D0* (1-XI)
        FF(2) = 0.5D0* (1+XI)
        DFF(1,1) = -0.5D0
        DFF(1,2) = 0.5D0
        DDFF(1,1) = 0.D0
        DDFF(1,2) = 0.D0
      ELSE IF (ALIAS(1:3).EQ.'TR3') THEN
        FF(1) = 0.5D0* (1+YI)
        FF(2) = -0.5D0* (XI+YI)
        FF(3) = 0.5D0* (1+XI)
        DFF(1,1) = 0.D0
        DFF(1,2) = -0.5D0
        DFF(1,3) = 0.5D0
        DFF(2,1) = 0.5D0
        DFF(2,2) = -0.5D0
        DFF(2,3) = 0.D+00
        DDFF(1,1) = 0.D0
        DDFF(1,2) = 0.D0
        DDFF(1,3) = 0.D0
        DDFF(2,1) = 0.D0
        DDFF(2,2) = 0.D0
        DDFF(2,3) = 0.D0
        DDFF(3,1) = 0.D0
        DDFF(3,2) = 0.D0
        DDFF(3,3) = 0.D0
      ELSE IF (ALIAS(1:3).EQ.'QU4') THEN
        UNS4 = 0.25D0
        A = 1.D0 + XI
        B = 1.D0 + YI
        C = 1.D0 - XI
        D = 1.D0 - YI
C     LES FFS
        FF(1) = C*B*UNS4
        FF(2) = C*D*UNS4
        FF(3) = A*D*UNS4
        FF(4) = A*B*UNS4
C     LES DD 1ER / XI
        DFF(1,1) = -B*UNS4
        DFF(1,2) = -D*UNS4
        DFF(1,3) = D*UNS4
        DFF(1,4) = B*UNS4
C     LES DD 1ER / YI
        DFF(2,1) = C*UNS4
        DFF(2,2) = -C*UNS4
        DFF(2,3) = -A*UNS4
        DFF(2,4) = A*UNS4
C     LES DD 2ER / XIXI
        DDFF(1,1) = 0.D0
        DDFF(1,2) = 0.D0
        DDFF(1,3) = 0.D0
        DDFF(1,4) = 0.D0
C     LES DD 2ER / YIYI
        DDFF(2,1) = 0.D0
        DDFF(2,2) = 0.D0
        DDFF(2,3) = 0.D0
        DDFF(2,4) = 0.D0
C                  / XIYI
        DDFF(3,1) = -UNS4
        DDFF(3,2) = UNS4
        DDFF(3,3) = -UNS4
        DDFF(3,4) = UNS4
       ELSE 
         IRET = 1
       END IF
       END
