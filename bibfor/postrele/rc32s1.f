      SUBROUTINE RC32S1 ( SIJ, NBINST, STH, SEISME, SIJS, SNP )
      IMPLICIT   NONE
      INTEGER             NBINST
      LOGICAL             SEISME
      REAL*8              SIJ(6), STH(6*NBINST), SIJS(6), SNP
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 21/03/2005   AUTEUR CIBHHLV L.VIVAN 
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
C     ------------------------------------------------------------------
C
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
C     CALCUL DU SN MAX SUR LES INSTANTS ET LES 64 POSSIBILITES DE SIGNE
C     POUR LES CMP DE SEISME
C     ROUTINE INSPIREE DE RC32ST
C
C IN  : SIJ    : CONTRAINTES LINEARISEES OU EN PEAU (CHARGEMENTS MECA)
C IN  : NBINST : NOMBRE D'INTANTS DE CALCUL THERMOMECA
C IN  : STH    : CONTRAINTES LINEARISEES OU EN PEAU ( THERMOMECA)
C IN  : SEISME : =.FALSE. SI PAS DE SEISME, =.TRUE. SINON
C IN  : SIJS   : CONTRAINTES LINEARISEES OU EN PEAU ( SEISME)
C OUT : SNP    : AMPLITUDE DE VARIATION DES CONTRAINTES DE TRESCA
C     ------------------------------------------------------------------
C
      INTEGER  I1, I2, I3, I4, I5, I6, I, IT
      REAL*8   SIJT(6), SNP1, SIJM(6), SIJTH(6), EQUI(6)
      REAL*8   E1(2), E2(2), E3(2), E4(2), E5(2), E6(2)
C DEB ------------------------------------------------------------------
C
      SNP = 0.D0
C
      DO 10 I = 1 , 6
         SIJM(I) = SIJ(I)
   10 CONTINUE
      DO 11 I = 1 , 2
         I1 = 2*(I-2)+1
         E1(I) = I1
         E2(I) = I1
         E3(I) = I1
         E4(I) = I1
         E5(I) = I1
         E6(I) = I1
11    CONTINUE
C
C
C --- CALCUL DU SNP:
C     --------------
C
      IF (NBINST.EQ.0) THEN
        IF ( SEISME ) THEN
C CAS DU SEISME. ESSAI DES 64 POSSIBLITES DE SIGNE DE SIJS
          DO 70 I1 = 1,2
            SIJT(1) = SIJM(1) + SIJS(1)*E1(I1)
            DO 60 I2 = 1,2
              SIJT(2) = SIJM(2) + SIJS(2)*E2(I2)
              DO 50 I3 = 1,2
                SIJT(3) = SIJM(3) + SIJS(3)*E3(I3)
                DO 40 I4 = 1,2
                  SIJT(4) = SIJM(4) + SIJS(4)*E4(I4)
                  DO 30 I5 = 1,2
                    SIJT(5) = SIJM(5) + SIJS(5)*E5(I5)
                    DO 20 I6 = 1,2
                      SIJT(6) = SIJM(6) + SIJS(6)*E6(I6)
                      CALL FGEQUI ( SIJT, 'SIGM', 3, EQUI )
                      SNP1 = EQUI(2)
                      SNP = MAX( SNP , SNP1 )
   20               CONTINUE
   30             CONTINUE
   40           CONTINUE
   50         CONTINUE
   60       CONTINUE
   70     CONTINUE
        ELSE
          CALL FGEQUI ( SIJM, 'SIGM', 3, EQUI )
          SNP = EQUI(2)
        END IF
C
C --- CALCUL THERMOMECANIQUE (DEPENDANT DU TEMPS)
C
      ELSE

C ----- ON BOUCLE SUR LES INSTANTS DU THERMIQUE DE P

        DO 150 IT = 1,NBINST
          DO 80 I = 1,6
            SIJTH(I) = SIJM(I) + STH((IT-1)*6+I)
   80     CONTINUE
          IF ( SEISME ) THEN
C CAS DU SEISME. ESSAI DES 64 POSSIBLITES DE SIGNE DE SIJS
            DO 140 I1 = 1,2
              SIJT(1) = SIJTH(1) + SIJS(1)*E1(I1)
              DO 130 I2 = 1,2
                SIJT(2) = SIJTH(2) + SIJS(2)*E2(I2)
                DO 120 I3 = 1,2
                  SIJT(3) = SIJTH(3) + SIJS(3)*E3(I3)
                  DO 110 I4 = 1,2
                    SIJT(4) = SIJTH(4) + SIJS(4)*E4(I4)
                    DO 100 I5 = 1,2
                      SIJT(5) = SIJTH(5) + SIJS(5)*E5(I5)
                      DO 90 I6 = 1,2
                        SIJT(6) = SIJTH(6) + SIJS(6)*E6(I6)
                        CALL FGEQUI ( SIJT, 'SIGM', 3, EQUI )
                        SNP = MAX( SNP , EQUI(2) )
   90                 CONTINUE
  100               CONTINUE
  110             CONTINUE
  120           CONTINUE
  130         CONTINUE
  140       CONTINUE
          ELSE
            CALL FGEQUI ( SIJTH, 'SIGM', 3, EQUI )
            SNP = MAX( SNP , EQUI(2) )
          END IF
  150   CONTINUE
      END IF
C
      END
