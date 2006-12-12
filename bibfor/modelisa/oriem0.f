      SUBROUTINE ORIEM0(TYPE,NOMA,COOR,LINO1,NBNO1,LINO2,NBNO2,
     &                  LINO3,NBNO3,PREC,IE1,IE2)
      IMPLICIT   NONE
      INTEGER             LINO1(*),NBNO1, LINO2(*),NBNO2, LINO3(*),NBNO3
      INTEGER             IE1, IE2
      REAL*8              PREC, COOR(*)
      CHARACTER*8         TYPE
      CHARACTER*(*)       NOMA
C.======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 13/12/2006   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C IN  : NOMA   : NOM DU MAILLAGE
C IN  : LINO1  : LISTE DES NOEUDS DE LA MAILLE 1
C IN  : NBNO1  : NB DE NOEUDS DE LINO1
C IN  : LINO2  : LISTE DES NOEUDS DE LA MAILLE 2
C IN  : NBNO2  : NB DE NOEUDS DE LINO2
C IN  : LINO3  : LISTE DES NOEUDS DE LA MAILLE DE PEAU
C IN  : NBNO3  : NB DE NOEUDS DE DE LA MAILLE DE PEAU
C IN  : PREC   : PRECISION
C IN  : TYPE   : TYPE DE LA MAILLE DE PEAU (SEG,QUAD,..)
C OUT : IE1    : = 0  MAILLE 1 ET MAILLES 2 IDENTIQUES (DUPLIQUEES)
C                = 1  SINON
C OUT : IE2    : = 0  MAILLE 1 ET MAILLES 2 DU MEME COTE (PAR RAPPORT
C                     A LA MAILLE DE PEAU)
C                = 1  SINON
C
C APPELEE PAR : UTMASU
C.========================= DEBUT DES DECLARATIONS ====================
C
      INTEGER      INO, N1, N2, N3, IC, K, INDIIS, INDI
      REAL*8       X1, X2, NOR1(3), N1N2(3), N1N3(3), PS1, PS2, DDOT
C
C ========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ()

C --- INITIALISATIONS :
C     ---------------
      IE1  = 1
      IE2  = 1
C
      IF ( NBNO1.NE.NBNO2 ) GOTO 15
C
C --- VERIFICATION DES COORDONNEES (MAILLES DUPPLIQUEES ?):
C     ----------------------------
      DO 10 INO = 1 , NBNO1
         N1 = LINO1(INO)
         N2 = LINO2(INO)
         DO 12 IC = 1 , 3
            X1 = COOR(3*(N1-1)+IC)
            X2 = COOR(3*(N2-1)+IC)
            IF ( X2 .EQ. 0.D0 ) THEN
               IF ( ABS(X1) .GT. PREC ) GOTO 15
            ELSE
               IF ( ABS((X1-X2)/X2) .GT. PREC ) GOTO 15
            ENDIF
 12      CONTINUE
 10   CONTINUE
C
      IE1 = 0
C
 15   CONTINUE
C
C --- VERIFICATION DE LA POSITION DES MAILLES
C     PAR RAPPORT A LA MAILLE DE PEAU :
C     --------------------------------

C     DEFINITION DE LA NORMALE DE LA MAILLE DE PEAU: NOR1
      N1 = LINO3(1)
      N2 = LINO3(2)
      IF(TYPE(1:3).NE.'SEG')THEN
        N3 = LINO3(3)
        DO 19 IC = 1 , 3
          N1N2(IC)=COOR(3*(N2-1)+IC)-COOR(3*(N1-1)+IC)
          N1N3(IC)=COOR(3*(N3-1)+IC)-COOR(3*(N1-1)+IC)
 19     CONTINUE
        CALL PROVEC(N1N2,N1N3,NOR1)
      ELSE
        DO 20 IC = 1 , 3
          N1N2(IC)=COOR(3*(N2-1)+IC)-COOR(3*(N1-1)+IC)
 20     CONTINUE
        NOR1(1)=-N1N2(2)
        NOR1(2)=N1N2(1)
        NOR1(3)=N1N2(3)
      ENDIF

C     POSITION DE LA MAILLE 1 PAR RAPPORT A LA MAILLE DE PEAU      
      K=0
      DO 21 INO =  1 , NBNO1
         INDI = INDIIS(LINO3(1),LINO1(INO),1,NBNO3)
         IF ( INDI .EQ. 0 .AND. K.EQ.0)  THEN
           K=K+1
           N2=LINO1(INO)
           N1=LINO3(1)
           DO 22 IC = 1 , 3
             N1N2(IC)=COOR(3*(N2-1)+IC)-COOR(3*(N1-1)+IC)
 22        CONTINUE
           PS1=DDOT(3,N1N2,1,NOR1,1)
         ENDIF
 21   CONTINUE
      CALL ASSERT(K.GT.0)

C     POSITION DE LA MAILLE 2 PAR RAPPORT A LA MAILLE DE PEAU 
      K=0
      DO 23 INO =  1 , NBNO2
         INDI = INDIIS(LINO3(1),LINO2(INO),1,NBNO2)
         IF ( INDI .EQ. 0 .AND. K.EQ.0)  THEN
           K=K+1
           N2=LINO2(INO)
           N1=LINO3(1)
           DO 24 IC = 1 , 3
             N1N2(IC)=COOR(3*(N2-1)+IC)-COOR(3*(N1-1)+IC)
 24        CONTINUE
           PS2=DDOT(3,N1N2,1,NOR1,1)
         ENDIF
 23   CONTINUE
      CALL ASSERT(K.GT.0)

C     VERIFICATION QUE LA MAILLE 1 ET LA MAILLE 2 SONT DU
C     MEME COTE PAR RAPPORT A LA MAILLE DE PEAU
      IF( ( PS1.GT.0  .AND.  PS2.GT.0 ) .OR.
     &    ( PS1.LT.0  .AND.  PS2.LT.0 ) ) IE2=0

      CALL JEDEMA()

      END
