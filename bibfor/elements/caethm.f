      SUBROUTINE CAETHM(NOMTE,AXI,TYPMOD,NSOM,NNOMAX,NVOMAX,
     +                  NSOMAX,VOISIN,NBVOS,P2P1)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ELEMENTS  DATE 31/01/2005   AUTEUR ROMEO R.FERNANDES 
C RESPONSABLE UFBHHLL C.CHAVANT
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
      IMPLICIT      NONE
      LOGICAL       AXI,P2P1
      INTEGER       NNOMAX,NVOMAX,NSOMAX,NSOM,VOISIN(NVOMAX,NNOMAX)
      INTEGER       NBVOS(NSOMAX)
      CHARACTER*8   TYPMOD(2)
      CHARACTER*16  NOMTE
C ======================================================================
C --- CETTE ROUTINE DETERMINE LE TYPE DE MODELISATION (AXI DPLAN 3D) ---
C --- ET CREE LE VECTEUR VOISIN POUR LES ELEMENTS D ORDRE 2 ------------
C ======================================================================
C IN NNOMAX : DIMENSIONNEMENT DE VOISIN
C IN  NVOMAX : NOMBRE MAXIMUM DE MILIEUX VOIISINS D UN SOMMET
C IN NSOMAX  : NOMBRE MAXIMUM DE SOMMETS
C IN  NOMTE  : NOM DU TYPE ELEMENT
C OUT AXI     :
C OUT P2P1   : TRUE SI P2P1
C OUT VOISIN
C  POUR UN SOMMET J DE 1 A NSOM : 
C         VOISIN(1:NDIM,J) =  LES NDIM NOEUDS MILIEUX VOSIN DU SOMMMET
C
C  POUR UN MILIEU J DE 1 A NSOM +1 A NNO: 
C         VOISIN(1:2,J) =  LES 2 NOEUDS DU SEGMENT DONT IL EST LE MILIEU
C
C OUT NBVOS  : NBVOS(1,NSOMAX) : NOMBRE DE VOISINS D UN SOMMET < NVOMAX
C ======================================================================
      INTEGER       IS,IADZI,IAZK24,NNO
      CHARACTER*8   MAILLE
C ======================================================================
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C ======================================================================
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C ======================================================================
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C ======================================================================
C --- INITIALISATIONS --------------------------------------------------
C ======================================================================
      P2P1      = .FALSE.
      AXI       = .FALSE.
      TYPMOD(2) = '        '
C ======================================================================
C --- RECUPERATION DU TYPE DE MAILLE POUR CREER VOISIN -----------------
C ======================================================================
      CALL TECAEL(IADZI,IAZK24)
      NNO    = ZI(IADZI-1+2)
      MAILLE = ZK24(IAZK24-1+3+NNO+3)
C ======================================================================
C --- TYPE DE MODELISATION? AXI DPLAN OU 3D ----------------------------
C ======================================================================
      IF ( NOMTE(6:9) .EQ.'AXIS' .OR.
     +     NOMTE(4:7) .EQ.'AXIS' .OR.
     +     NOMTE(5:8) .EQ.'AXIS' .OR.
     +     NOMTE(7:10).EQ.'AXIS' ) THEN
         AXI       = .TRUE.
         TYPMOD(1) = 'AXIS    '
      ELSE IF ( NOMTE(6:7) .EQ.'DP' .OR.
     +          NOMTE(7:8) .EQ.'DP' .OR.
     +          NOMTE(4:5) .EQ.'DP' .OR.
     +          NOMTE(5:6) .EQ.'DP') THEN
         TYPMOD(1) = 'D_PLAN  '
      ELSE
         TYPMOD(1) = '3D      '
      ENDIF
C ======================================================================
C --- CREATION DU VECTEUR VOISIN ---------------------------------------
C ======================================================================
      IF (MAILLE.EQ.'SEG3') THEN 
C ======================================================================
C --- LA MAILLE EST DE TYPE SEG3 ---------------------------------------
C ======================================================================
         P2P1 = .TRUE.
         VOISIN(1,3) = 1
         VOISIN(2,3) = 2
      ELSE IF (MAILLE.EQ.'TRIA6') THEN 
C ======================================================================
C --- LA MAILLE EST DE TYPE TRIA6 --------------------------------------
C ======================================================================
         P2P1 = .TRUE.
         DO 10 IS = 1,NSOM
            NBVOS(IS) = 2
 10      CONTINUE
         VOISIN(1,1) = 4
         VOISIN(2,1) = 6
         VOISIN(1,2) = 4
         VOISIN(2,2) = 5
         VOISIN(1,3) = 5
         VOISIN(2,3) = 6
C       
         VOISIN(1,4) = 1
         VOISIN(2,4) = 2
         VOISIN(1,5) = 2
         VOISIN(2,5) = 3
         VOISIN(1,6) = 3
         VOISIN(2,6) = 1 
      ELSE IF (MAILLE.EQ.'QUAD8') THEN
C ======================================================================
C --- LA MAILLE EST DE TYPE QUAD8 --------------------------------------
C ======================================================================
         P2P1 = .TRUE.
         DO 20 IS = 1,NSOM
            NBVOS(IS) = 2
 20      CONTINUE
         VOISIN(1,1) = 5
         VOISIN(2,1) = 8
         VOISIN(1,2) = 6
         VOISIN(2,2) = 5
         VOISIN(1,3) = 7
         VOISIN(2,3) = 6
         VOISIN(1,4) = 8
         VOISIN(2,4) = 7
C   
         VOISIN(1,5) = 1
         VOISIN(2,5) = 2
         VOISIN(1,6) = 2
         VOISIN(2,6) = 3
         VOISIN(1,7) = 3
         VOISIN(2,7) = 4
         VOISIN(1,8) = 4
         VOISIN(2,8) = 1
      ELSE IF (MAILLE.EQ.'TETRA10') THEN
C ======================================================================
C --- LA MAILLE EST DE TYPE TETRA10 ------------------------------------
C ======================================================================
         P2P1 = .TRUE.
         DO 30 IS = 1,NSOM
            NBVOS(IS) = 3
 30      CONTINUE
         VOISIN(1,1) = 5
         VOISIN(2,1) = 7
         VOISIN(3,1) = 8      
         VOISIN(1,2) = 5
         VOISIN(2,2) = 6
         VOISIN(3,2) = 9
         VOISIN(1,3) = 6
         VOISIN(2,3) = 7
         VOISIN(3,3) = 10
         VOISIN(1,4) = 8
         VOISIN(2,4) = 9
         VOISIN(3,4) = 10
 
         VOISIN(1,5) = 1
         VOISIN(2,5) = 2
         VOISIN(1,6) = 2
         VOISIN(2,6) = 3
         VOISIN(1,7) = 3
         VOISIN(2,7) = 1
         VOISIN(1,8) = 1
         VOISIN(2,8) = 4
         VOISIN(1,9) = 4
         VOISIN(2,9) = 2
         VOISIN(1,10) = 4
         VOISIN(2,10) = 3
      ELSE IF (MAILLE.EQ.'PYRAM13') THEN
C ======================================================================
C --- LA MAILLE EST DE TYPE PYRAM13 ------------------------------------
C ======================================================================
         P2P1 = .TRUE.
         DO 40 IS = 1,4
            NBVOS(IS) = 3
 40      CONTINUE
         NBVOS(5) = 4
         VOISIN(1,1) = 6
         VOISIN(2,1) = 9
         VOISIN(3,1) = 10  
         VOISIN(1,2) = 6
         VOISIN(2,2) = 7
         VOISIN(3,2) = 11
         VOISIN(1,3) = 7
         VOISIN(2,3) = 8
         VOISIN(3,3) = 12
         VOISIN(1,4) = 8
         VOISIN(2,4) = 9
         VOISIN(3,4) = 13
         VOISIN(1,5) = 10
         VOISIN(2,5) = 11
         VOISIN(3,5) = 12
         VOISIN(4,5) = 13
 
         VOISIN(1,6) = 1
         VOISIN(2,6) = 2
         VOISIN(1,7) = 2
         VOISIN(2,7) = 3
         VOISIN(1,8) = 3
         VOISIN(2,8) = 4
         VOISIN(1,9) = 4
         VOISIN(2,9) = 1
         VOISIN(1,10) = 1
         VOISIN(2,10) = 5
         VOISIN(1,11) = 2
         VOISIN(2,11) = 5
         VOISIN(1,12) = 3
         VOISIN(2,12) = 5
         VOISIN(1,13) = 4
         VOISIN(2,13) = 5
      ELSE IF (MAILLE.EQ.'PENTA15') THEN
C ======================================================================
C --- LA MAILLE EST DE TYPE PENTA15 ------------------------------------
C ======================================================================
         P2P1 = .TRUE.
         DO 50 IS = 1,NSOM
            NBVOS(IS) = 3
 50      CONTINUE
         VOISIN(1,1) = 7
         VOISIN(2,1) = 9
         VOISIN(3,1) = 10  
         VOISIN(1,2) = 7
         VOISIN(2,2) = 8
         VOISIN(3,2) = 11
         VOISIN(1,3) = 8
         VOISIN(2,3) = 9
         VOISIN(3,3) = 12
         VOISIN(1,4) = 10
         VOISIN(2,4) = 13
         VOISIN(3,4) = 15
         VOISIN(1,5) = 11
         VOISIN(2,5) = 13
         VOISIN(3,5) = 14
         VOISIN(1,6) = 12
         VOISIN(2,6) = 14
         VOISIN(3,6) = 15

         VOISIN(1,7)  = 1
         VOISIN(2,7)  = 2
         VOISIN(1,8)  = 2
         VOISIN(2,8)  = 3
         VOISIN(1,9)  = 3
         VOISIN(2,9)  = 1
         VOISIN(1,10) = 1
         VOISIN(2,10) = 4
         VOISIN(1,11) = 2
         VOISIN(2,11) = 5
         VOISIN(1,12) = 3
         VOISIN(2,12) = 6
         VOISIN(1,13) = 4
         VOISIN(2,13) = 5
         VOISIN(1,14) = 5
         VOISIN(2,14) = 6
         VOISIN(1,15) = 6
         VOISIN(2,15) = 4
      ELSE IF (MAILLE.EQ.'HEXA20') THEN
C ======================================================================
C --- LA MAILLE EST DE TYPE HEXA20 -------------------------------------
C ======================================================================
         DO 60 IS = 1,NSOM
            NBVOS(IS) = 3
 60      CONTINUE
         P2P1 = .TRUE.
         VOISIN(1,1) = 9
         VOISIN(2,1) = 12
         VOISIN(3,1) = 13       
         VOISIN(1,2) = 10
         VOISIN(2,2) = 9
         VOISIN(3,2) = 14
         VOISIN(1,3) = 11
         VOISIN(2,3) = 10
         VOISIN(3,3) = 15
         VOISIN(1,4) = 12
         VOISIN(2,4) = 11
         VOISIN(3,4) = 16
         VOISIN(1,5) = 17
         VOISIN(2,5) = 20
         VOISIN(3,5) = 13
         VOISIN(1,6) = 18
         VOISIN(2,6) = 17
         VOISIN(3,6) = 14
         VOISIN(1,7) = 19
         VOISIN(2,7) = 18
         VOISIN(3,7) = 15
         VOISIN(1,8) = 20
         VOISIN(2,8) = 19
         VOISIN(3,8) = 16

         VOISIN(1,9) =  1
         VOISIN(2,9) =  2
         VOISIN(1,10) = 2
         VOISIN(2,10) = 3
         VOISIN(1,11) = 3
         VOISIN(2,11) = 4
         VOISIN(1,12) = 4
         VOISIN(2,12) = 1
         VOISIN(1,13) = 1
         VOISIN(2,13) = 5
         VOISIN(1,14) = 2
         VOISIN(2,14) = 6
         VOISIN(1,15) = 3
         VOISIN(2,15) = 7
         VOISIN(1,16) = 4
         VOISIN(2,16) = 8
         VOISIN(1,17) = 5
         VOISIN(2,17) = 6
         VOISIN(1,18) = 6
         VOISIN(2,18) = 7
         VOISIN(1,19) = 7
         VOISIN(2,19) = 8
         VOISIN(1,20) = 8
         VOISIN(2,20) = 5
      ENDIF
C ======================================================================
      END
