      SUBROUTINE  SOSULC (C0,MM,AF,LH,LT0,Q0,W0,RHOF,TR,SM0,IMATE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/09/96   AUTEUR JMBHH01 J.M.PROIX 
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
C       IMPLICIT REAL*8 (A-H,O-Z)
       IMPLICIT NONE
C
       INTEGER       IMATE
C
       REAL*8     C0,MM,AF,LH,LT0,Q0,W0,RHOF,SM0,TR
C.......................................................................
C
C     BUT: LOI DE COMPOTEMENT D'UN JOINT THERMO PORO ELASTIQUE
C           HYDRO-THERMIQUE
C.......................................................................
C IN  IMATE   : MATERIAU CODE
C OUT CE,MM,AF,LH,LT,QO,WO,RE : COEFFICIENTS EN
C                        THERMOPOROHYDRAULIQUE LINEAIRE
C......................................................................
C
      INTEGER IJK,NBPAR,NBRES
      REAL*8  VALPAR
      REAL*8  VALRES(10)
      CHARACTER*8 NOMPAR,NOMCRA(10)
      CHARACTER*16 PHENOM
      CHARACTER*2 CODRES(10),CODRET
C ....................................................................
      DATA NOMCRA /'C_0','ALPHA_M','BIOT_M',
     &'LAMBDA_T','LAMBDA_H','SOURCE_INIT','OMEGA_0','RHO_FLUI',
     &'T_R','ENTRO_FLUI'/
C

      CALL RCCOMA(IMATE,'PORO_JOINT',PHENOM,CODRET)
      IF (CODRET.EQ.'NO') THEN
         CALL UTMESS('F','SOSULC','PAS DE MATERIAU HM TROUVE')
      ELSE
         IF (PHENOM(1:10).EQ.'PORO_JOINT') THEN
C
C           RECUPERATION DES COEFFS DU MATERIAU PORO_THM
C
            NBRES = 10
            NBPAR = 0
            VALPAR = 0
            NOMPAR = ' '
            CALL RCVALA(IMATE,'PORO_JOINT',NBPAR,NOMPAR,VALPAR,
     &                  NBRES,NOMCRA,VALRES,CODRES,'  ')
            DO 77 IJK = 1,NBRES
               IF (CODRES(IJK).NE.'OK') THEN
                  CALL UTMESS('F','SOSULC','VALEUR DE '
     &                        // NOMCRA(IJK) // 'NON TROUVE')
               END IF
 77         CONTINUE
            C0 = VALRES(1)
            AF = VALRES(2)
            MM = VALRES(3)
            LT0 = VALRES(4)
            LH = VALRES(5)
            Q0 = VALRES(6)
            W0 = VALRES(7)
            RHOF = VALRES(8)
            TR = VALRES(9)
            SM0 = VALRES(10)
         ENDIF
      ENDIF
      END
