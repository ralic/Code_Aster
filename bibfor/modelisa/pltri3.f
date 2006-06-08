      SUBROUTINE PLTRI3(SC,NS,FS,NF,VM,ZL,TET,NT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 16/12/2004   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C     TETRAEDRISATION D'UN POLYEDRE ETOILE EN SON CENTRE DE GRAVITE
C ----------------------------------------------------------------------
C VARIABLES EN ENTREE
C REAL*8     SC(3,*)  : COORDONNEES DES SOMMETS DU POLYEDRE
C INTEGER    NS       : NOMBRE DE SOMMETS DU POLYEDREE
C INTEGER    FS(3,*)  : SOMMETS DES FACES TRIANGLES 
C                       SENS DE PARCOURS, NORMALE SORTANTE A DROITE  
C INTEGER    NF       : NOMBRE DE FACES TRIANGLES DU POLYEDRE
C
C VECTEUR DE TRAVAIL
C LOGICAL    ZL(*)    : NS
C
C VARIABLES DE SORTIE
C REAL*8     SC(3,*)  : COORDONNEES DES SOMMETS DES TETRAEDRES
C INTEGER    NS       : NOMBRE DE SOMMETS DES DES TETRAEDRES
C INTEGER    TET(4,*) : CONNECTIVITE DES TETRAEDRES
C INTEGER    NT       : NOMBRE DE TETRAEDRES
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- FONCTIONS
      REAL*8  DDOT,PLVOL3

C --- VARIABLES
      INTEGER FS(3,*),TET(4,*),NS,NS0,NF,F,A,B,C,S,NT
      REAL*8  SC(3,*),SI(3),N(3),V(3),R0,R1,R2,VM
      LOGICAL ZL(*),IR

      NT = 0
      NS0 = NS
      IR = .FALSE.

C --- SOMMET APPARTENANT AU POLYEDRE

      DO 10 S = 1, NS0
        ZL(S) = .TRUE.
 10   CONTINUE

      DO 20 F = 1, NF
        ZL(FS(1,F)) = .FALSE.
        ZL(FS(2,F)) = .FALSE.
        ZL(FS(3,F)) = .FALSE.
 20   CONTINUE

C --- CALCUL DU CENTRE DE GRAVITE (G)

      NS = NS + 1
      CALL PLCENT(3,SC,FS,NF,SC(1,NS))

C --- VERIFICATION QUE LE POLYEDRE EST ETOILE EN G

      DO 30 F = 1, NF

        A = FS(1,F)
        B = FS(2,F)
        C = FS(3,F)

        CALL PROVE3(SC(1,A),SC(1,B),SC(1,C),N)
        R0 = DDOT(3,N,1,SC(1,A),1)
        R1 = DDOT(3,N,1,SC(1,NS),1) - R0

        IF (ABS(R1).LT.VM) GOTO 30
 
        IF (R1.GT.0.D0) THEN
          IR = .TRUE.
          GOTO 50
        ENDIF

        DO 40 S = 1, NS0

          IF (ZL(S).OR.(S.EQ.A).OR.(S.EQ.B).OR.(S.EQ.C)) GOTO 40

          R2 = DDOT(3,N,1,SC(1,S),1) - R0
          IF (((R1.LT.0.D0).EQV.(R2.LT.0.D0)).OR.(ABS(R2).LT.VM))GOTO 40

          R2 = R1/(R1 - R2)
          SI(1) = (1-R2)*SC(1,NS) + R2*SC(1,S)
          SI(2) = (1-R2)*SC(2,NS) + R2*SC(2,S)
          SI(3) = (1-R2)*SC(3,NS) + R2*SC(3,S)

          CALL PROVE3(SC(1,A),SC(1,B),SI,V)
          IF (DDOT(3,N,1,V,1).LT.0.D0) GOTO 40

          CALL PROVE3(SC(1,B),SC(1,C),SI,V)
          IF (DDOT(3,N,1,V,1).LT.0.D0) GOTO 40

          CALL PROVE3(SC(1,C),SC(1,A),SI,V)
          IF (DDOT(3,N,1,V,1).LT.0.D0) GOTO 40

          IR = .TRUE.
      
 40     CONTINUE

C ----- ECRITURE DES TETRAEDRES

 50     CONTINUE

        NT = NT + 1
        TET(1,NT) = NS
        TET(2,NT) = FS(1,F)
        TET(3,NT) = FS(2,F)
        TET(4,NT) = FS(3,F)

 30   CONTINUE

C --- DIFFERENCE DE VOLUME
C SOLUTION : DECOUPER POLYEDRE PAR PLAN F A PARTIR DU VOISINAGE DE S  
C            ITERER SUR LES COMPOSANTES CONNEXES 

      IF (IR) THEN

        R0 = PLVOL3(SC,FS,NF)
        R1 = 0.D0
        DO 60 F = 1, NT
          S = TET(1,F)
          A = TET(2,F)
          B = TET(3,F)
          C = TET(4,F)
          N(1) = SC(1,A)-SC(1,S)
          N(2) = SC(2,A)-SC(2,S)
          N(3) = SC(3,A)-SC(3,S)
          CALL PROVE3(SC(1,S),SC(1,B),SC(1,C),V)
          R1 = R1 + DDOT(3,N,1,V,1)
 60     CONTINUE
        R1 = R1/6.D0
        CALL INFMAJ()
        CALL INFNIV(F,A)
        CALL UTMESS('A','PLTRI3',
     &              'POLYEDRE NON ETOILE AU CENTRE DE GRAVITE')     
        WRITE(F,*) 'VOLUME :',R1,' AU LIEU DE',R0,' DIFFE',R1-R0,IR

      ENDIF

      END
