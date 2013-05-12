        SUBROUTINE HUJPEL ( ETATD,MOD,CRIT,IMAT,NMAT,MATERF,ANGMAS,
     &                      DEPS,SIGD,NVI,VIND,SIGF,VINF,IRET )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 30/04/2013   AUTEUR FOUCAULT A.FOUCAULT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE FOUCAULT A.FOUCAULT
        IMPLICIT   NONE
C       ----------------------------------------------------------------
C       INTEGRATION ELASTIQUE SUR DT
C       IN  ETATD  :  ETAT MATERIAU A T (ELASTIC OU PLASTIC)
C           MOD    :  MODELISATION
C           CRIT   :  CRITERES LOCAUX LIES AU SCHEMA DE NEWTON
C           IMAT   :  NUMERO MATERIAU
C           NMAT   :  DIMENSION TABLEAU DONNEES MATERIAUX
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           DEPS   :  INCREMENT DE DEFORMATION
C           SIGD   :  CONTRAINTE  A T
C           NVI    :  DIMENSION VECTEUR VARIABLES INTERNES
C           VIND   :  VARIABLES INTERNES A T
C       OUT SIGF   :  CONTRAINTE A T+DT
C           VINF   :  VARIABLES INTERNES A T+DT
C           IRET   :  CODE RETOUR (O-->OK / 1-->NOOK)
C       ----------------------------------------------------------------
        INTEGER         NVI, IMAT, IRET,NMAT
        REAL*8          MATERF(NMAT,2),SIGD(6),SIGF(6),ANGMAS(3)
        REAL*8          VIND(*),VINF(*),DEPS(6),CRIT(*)
        CHARACTER*8     MOD
        CHARACTER*7     ETATD
C
        INTEGER         I
        REAL*8          ZERO,UN,R8VIDE,BID66(6,6),MATERT(22,2)
        LOGICAL         REORIE
        PARAMETER      (ZERO = 0.D0)
        PARAMETER      (UN   = 1.D0)
C       ----------------------------------------------------------------
        IF(MOD(1:6).EQ.'D_PLAN')THEN
          DO 10 I = 5, 6
            DEPS(I) = ZERO  
            SIGD(I) = ZERO
  10      CONTINUE  
        ENDIF

        IF (((VIND(24) .EQ. ZERO) .OR.
     &     (VIND(24) .EQ. -UN .AND. VIND(28) .EQ. ZERO)) .AND.
     &    ((VIND(25) .EQ. ZERO) .OR.
     &     (VIND(25) .EQ. -UN .AND. VIND(29) .EQ. ZERO)) .AND.
     &    ((VIND(26) .EQ. ZERO) .OR.
     &     (VIND(26) .EQ. -UN .AND. VIND(30) .EQ. ZERO)) .AND.
     &    ((VIND(27) .EQ. ZERO).OR.
     &     (VIND(27) .EQ. -UN .AND. VIND(31) .EQ. ZERO))) THEN
          ETATD = 'ELASTIC'
        ELSE
          ETATD = 'PLASTIC'
        ENDIF

C --- ORIENTATION DES CONTRAINTES SELON ANGMAS VERS REPERE LOCAL
        IF (ANGMAS(1).EQ.R8VIDE()) CALL U2MESS('F','ALGORITH8_20')
        REORIE =(ANGMAS(1).NE.ZERO) .OR. (ANGMAS(2).NE.ZERO)
     &          .OR. (ANGMAS(3).NE.ZERO)
        CALL HUJORI ('LOCAL', 1, REORIE, ANGMAS, SIGD, BID66)
        CALL HUJORI ('LOCAL', 1, REORIE, ANGMAS, DEPS, BID66)

        DO 20 I = 1, 22
          MATERT(I,1) = MATERF(I,1)
          MATERT(I,2) = MATERF(I,2)
  20    CONTINUE

        CALL HUJPRE (ETATD,MOD,CRIT,IMAT,MATERT,DEPS,SIGD,
     &                SIGF, VIND, IRET)
        CALL LCEQVN(NVI,VIND,VINF)

        END
