      SUBROUTINE MUDIRX(NBSOM,GEOM,IDIM,AL1,AL2,AXE,ANG)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8 GEOM(IDIM,NBSOM),AXE(3,3),ANG(2),AL1,AL2
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 05/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
C.......................................................................
C .                                                                    .
C .  - FONCTION REALISEE : CALCULE LES COSINUS DIRECTEURS DE LA MATRICE.
C .                        DE PASSAGE DU REPERE DE L'ELEMENT AU REPERE .
C .                        DE REFERENCE AINSI QUE LES 3 DIRECTIONS     .
C .                        NORMEES DU REPERE DE L'ELEMENT.             .
C .  - ARGUMENTS :                                                     .
C .                                                                    .
C .      ENTREE :  NBSOM  --> NB DE SOMMETS DE L'ELEMENT (3 OU 4)      .
C .                GEOM   --> TABLEAU DES COORDONNEES DES SOMMETS      .
C .                IDIM   --> DIMENSION DE GEOM (2 OU 3)               .
C .                AL1    --> ANGLE 1 DU REPERE DE REFERENCE           .
C .                AL2    --> ANGLE 2 DU REPERE DE REFERENCE           .
C .      SORTIE :                                                      .
C .                AXE    <-- 3 DIRECTIONS NORMEES DU REPERE DE        .
C .                           L'ELEMENT                                .
C .                ANG    <-- COSINUS ET SINUS DE L'ANGLE (REF,ELEM)   .
C .                                                                    .
C .  - ROUTINES APPELEES:                                              .
C .                                                                    .
C .    JEFINI                                                          .
C.......................................................................
C
      REAL*8 XI1,XI2,XI3,X12,Y12,Z12,X13,Y13,Z13,X24,Y24,Z24,S
      REAL*8 PJXI1,PJXI2,PJXI3,S1,S2,S3,PSXIN,COEPI
C
      COEPI=R8PI()/180.D0
      IF (NBSOM.NE.3.AND.NBSOM.NE.4) THEN
         IFM=IUNIFI('MESSAGE')
         WRITE(IFM,*) 'NOMBRE DE SOMMETS DIFFERENT DE 3 OU 4'
         CALL JEFINI('ERREUR')
      ENDIF
      IF (IDIM.NE.2.AND.IDIM.NE.3) THEN
         IFM=IUNIFI('MESSAGE')
         WRITE(IFM,*) 'DIMENSION DIFFERENTE DE 2 OU 3'
         CALL JEFINI('ERREUR')
      ENDIF
      XI1=COS(COEPI*AL2)*COS(COEPI*AL1)
      XI2=COS(COEPI*AL2)*SIN(COEPI*AL1)
      XI3=SIN(COEPI*AL2)
      S=(XI1**2+XI2**2+XI3**2)**0.5D0
      XI1=XI1/S
      XI2=XI2/S
      XI3=XI3/S
      X12=GEOM(1,2)-GEOM(1,1)
      Y12=GEOM(2,2)-GEOM(2,1)
      Z12=0.D0
      IF (IDIM.EQ.3) Z12=GEOM(3,2)-GEOM(3,1)
      S=(X12**2+Y12**2+Z12**2)**0.5D0
      AXE(1,1)=X12/S
      AXE(2,1)=Y12/S
      AXE(3,1)=Z12/S
      X13=GEOM(1,3)-GEOM(1,1)
      Y13=GEOM(2,3)-GEOM(2,1)
      Z13=0.D0
      IF (IDIM.EQ.3) Z13=GEOM(3,3)-GEOM(3,1)
      IF (NBSOM.EQ.3) THEN
         S1=Y12*Z13-Z12*Y13
         S2=Z12*X13-X12*Z13
         S3=X12*Y13-Y12*X13
         S=(S1**2+S2**2+S3**2)**0.5D0
         AXE(1,3)=S1/S
         AXE(2,3)=S2/S
         AXE(3,3)=S3/S
      ENDIF
      IF (NBSOM.EQ.4) THEN
         X24=GEOM(1,4)-GEOM(1,2)
         Y24=GEOM(2,4)-GEOM(2,2)
         Z24=0.D0
         IF (IDIM.EQ.3) Z24=GEOM(3,4)-GEOM(3,2)
         S1=Y13*Z24-Z13*Y24
         S2=Z13*X24-X13*Z24
         S3=X13*Y24-Y13*X24
         S=(S1**2+S2**2+S3**2)**0.5D0
         AXE(1,3)=S1/S
         AXE(2,3)=S2/S
         AXE(3,3)=S3/S
      ENDIF
      AXE(1,2)=AXE(2,3)*AXE(3,1)-AXE(3,3)*AXE(2,1)
      AXE(2,2)=AXE(3,3)*AXE(1,1)-AXE(1,3)*AXE(3,1)
      AXE(3,2)=AXE(1,3)*AXE(2,1)-AXE(2,3)*AXE(1,1)
      PSXIN=XI1*AXE(1,3)+XI2*AXE(2,3)+XI3*AXE(3,3)
      PJXI1=XI1-PSXIN*AXE(1,3)
      PJXI2=XI2-PSXIN*AXE(2,3)
      PJXI3=XI3-PSXIN*AXE(3,3)
      S=(PJXI1**2+PJXI2**2+PJXI3**2)**0.5D0
      PJXI1=PJXI1/S
      PJXI2=PJXI2/S
      PJXI3=PJXI3/S
      ANG(1)=PJXI1*AXE(1,1)+PJXI2*AXE(2,1)+PJXI3*AXE(3,1)
      ANG(2)=PJXI1*AXE(1,2)+PJXI2*AXE(2,2)+PJXI3*AXE(3,2)
      END
