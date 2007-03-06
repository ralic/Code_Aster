      SUBROUTINE LCMAEI (NMATER,IMAT,NECRIS, NECOUL,NBVAL,NBPAR,NOMPAR,
     &            IMPEXP,VALPAR,VALRES,NMAT,HSR,IFA,NOMFAM,NBSYS)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/03/2007   AUTEUR ELGHARIB J.EL-GHARIB 
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
C     ----------------------------------------------------------------
C     MONOCRISTAL : RECUPERATION DU MATERIAU A T(TEMPD) ET T+DT(TEMPF)
C                  MATER(*,2) = COEF ECRO ISOT ET CALCUL DE LA 
C                  MATRICE D'INTERACTION HSR
C     ----------------------------------------------------------------
C     IN  IMAT   :  ADRESSE DU MATERIAU CODE
C         NMATER :  NOM DU MATERIAU
C         NMAT   :  DIMENSION  DE MATER
C         NECRIS :  NOM DE LA LOI D'ECOULEMENT
C         IFA    :  NUMERO DE LA FAMILLE DE GLISSEMENT
C         NBCOMM :  NOMBRE DE COEF MATERIAU PAR FAMILLE
C         VALPAR :  VALEUR DES PARAMETRES (TEMPERATURE)
C         NOMPAR :  NOM DES PARAMETRES (TEMPARATURE)
C     OUT VALRES :  COEFFICIENTS MATERIAU
C     OUT NBVAL  :  NB DE COEFFICIENTS MATERIAU
C     OUT HSR    :  MATRICE D'INTERACTION
C     ----------------------------------------------------------------
      INTEGER         NMAT,NBPAR,NBVAL,IMAT,I,NBSYS,IS,IR,IFA,J,IMPEXP
      REAL*8          MATER(NMAT,2),VALH(6)
      REAL*8          VALPAR(NMAT),VALRES(NMAT),HSRI(24,24),H
      REAL*8          HSR(5,24,24),A1(3,3),A2(3,3),A3(3,3),A4(3,3)
      CHARACTER*8     NOMPAR(NBPAR),NOMRES(NMAT)
      CHARACTER*2     CODRET(NMAT)
      CHARACTER*16    NMATER, NECRIS, NOMFAM,NECOUL
C     ----------------------------------------------------------------
C
      IF (NECRIS.EQ.'ECRO_ISOT1') THEN
          NBVAL=3
          NOMRES(1)='R_0'
          NOMRES(2)='Q'
          NOMRES(3)='B'
          CALL RCVALA (IMAT,NMATER, NECRIS,1, NOMPAR,VALPAR,3,
     &                 NOMRES, VALRES,CODRET,'FM')
      ELSEIF (NECRIS.EQ.'ECRO_ISOT2') THEN
          NBVAL=5
          NOMRES(1)='R_0'
          NOMRES(2)='Q1'
          NOMRES(3)='B1'
          NOMRES(4)='Q2'
          NOMRES(5)='B2'
          CALL RCVALA (IMAT,NMATER, NECRIS,1, NOMPAR,VALPAR,NBVAL,
     &                 NOMRES, VALRES,CODRET,'FM')
      ENDIF
C     DEFINITION DE LA MATRICE D'INTERACTION      
      NOMRES(1)='H' 
      NBVAL=NBVAL+1
      VALRES(NBVAL)=0.D0 
      CALL RCVALA (IMAT,NMATER, NECRIS,1, NOMPAR,VALPAR,1,   
     &              NOMRES, H,CODRET,' ')
      IF(NECOUL.EQ.'KOCKS_RAUCH') THEN                     
      CALL RCVALA (IMAT,NMATER, NECOUL,1, NOMPAR,VALPAR,1,   
     &              NOMRES, H,CODRET,' ')
      ENDIF                     
      IF (CODRET(1).EQ.'OK') THEN                            
          VALRES(NBVAL)=H                                        
C  DEFINITION DE LA MATRICE D INTERACTION
          DO 507 IS = 1, NBSYS             
             DO 508 IR = 1, NBSYS          
                IF (IS.EQ.IR) THEN         
                   HSR(IFA,IS,IR) = 1.D0   
                ELSE                       
                   HSR(IFA,IS,IR) = H      
                ENDIF                      
  508        CONTINUE                      
  507     CONTINUE                         
      ELSE
          IF (IMPEXP.NE.0) CALL U2MESS('F','ALGORITH4_64')
          NOMRES(1)='H1'                                        
          NOMRES(2)='H2'                                        
          NOMRES(3)='H3'                                        
          NOMRES(4)='H4'                                        
          NOMRES(5)='H5'                                        
          NOMRES(6)='H6'                                        

          IF(NECOUL.EQ.'KOCKS_RAUCH') THEN                     
          CALL RCVALA (IMAT,NMATER, NECOUL,1, NOMPAR,VALPAR,6,  
     &               NOMRES, VALH,CODRET,' ')                   
          ELSE
          CALL RCVALA (IMAT,NMATER, NECRIS,1, NOMPAR,VALPAR,6,  
     &               NOMRES, VALH,CODRET,' ')
          ENDIF
                             
          CALL R8INIR ( 3*3, VALH(1) , A1, 1 )                  
          CALL R8INIR ( 3*3, VALH(2) , A2, 1 )                  
          CALL R8INIR ( 3*3, VALH(3) , A3, 1 )                  
          CALL R8INIR ( 3*3, VALH(4) , A4, 1 )                  
          CALL R8INIR ( 24*24, 0.D0 , HSRI, 1 )                 

C         DEFINITION DE LA MATRICE D INTERACTION BCC24
          IF (NOMFAM.EQ.'BCC24') THEN
            CALL LCICMA(A1,3,3,3,3,1,1,HSRI,24,24,16,1)
            CALL LCICMA(A1,3,3,3,3,1,1,HSRI,24,24,19,1)
            CALL LCICMA(A1,3,3,3,3,1,1,HSRI,24,24,22,1)
            CALL LCICMA(A1,3,3,3,3,1,1,HSRI,24,24,13,4)
            CALL LCICMA(A1,3,3,3,3,1,1,HSRI,24,24,19,4)
            CALL LCICMA(A1,3,3,3,3,1,1,HSRI,24,24,22,4)
            CALL LCICMA(A1,3,3,3,3,1,1,HSRI,24,24,13,7)
            CALL LCICMA(A1,3,3,3,3,1,1,HSRI,24,24,16,7)
            CALL LCICMA(A1,3,3,3,3,1,1,HSRI,24,24,22,7)
            CALL LCICMA(A1,3,3,3,3,1,1,HSRI,24,24,13,10)
            CALL LCICMA(A1,3,3,3,3,1,1,HSRI,24,24,16,10)
            CALL LCICMA(A1,3,3,3,3,1,1,HSRI,24,24,19,10)


            CALL LCICMA(A2,3,3,3,3,1,1,HSRI,24,24,1,1)
            CALL LCICMA(A2,3,3,3,3,1,1,HSRI,24,24,4,4)
            CALL LCICMA(A2,3,3,3,3,1,1,HSRI,24,24,7,7)
            CALL LCICMA(A2,3,3,3,3,1,1,HSRI,24,24,10,10)
            CALL LCICMA(A2,3,3,3,3,1,1,HSRI,24,24,13,1)
            CALL LCICMA(A2,3,3,3,3,1,1,HSRI,24,24,16,4)
            CALL LCICMA(A2,3,3,3,3,1,1,HSRI,24,24,19,7)
            CALL LCICMA(A2,3,3,3,3,1,1,HSRI,24,24,22,10)

            CALL LCICMA(A3,3,3,3,3,1,1,HSRI,24,24,16,13)
            CALL LCICMA(A3,3,3,3,3,1,1,HSRI,24,24,19,13)
            CALL LCICMA(A3,3,3,3,3,1,1,HSRI,24,24,22,13)
            CALL LCICMA(A3,3,3,3,3,1,1,HSRI,24,24,19,16)
            CALL LCICMA(A3,3,3,3,3,1,1,HSRI,24,24,22,16)
            CALL LCICMA(A3,3,3,3,3,1,1,HSRI,24,24,22,19)

            CALL LCICMA(A4,3,3,3,3,1,1,HSRI,24,24,13,13)
            CALL LCICMA(A4,3,3,3,3,1,1,HSRI,24,24,16,16)
            CALL LCICMA(A4,3,3,3,3,1,1,HSRI,24,24,19,19)
            CALL LCICMA(A4,3,3,3,3,1,1,HSRI,24,24,22,22)

            CALL LCICMA(A1,3,3,3,3,1,1,HSRI,24,24,4,1)
            HSRI(4,1)=VALH(2)
            CALL LCICMA(A1,3,3,3,3,1,1,HSRI,24,24,7,1)
            HSRI(7,2)=VALH(2)
            CALL LCICMA(A1,3,3,3,3,1,1,HSRI,24,24,10,1)
            HSRI(10,3)=VALH(2)
            CALL LCICMA(A1,3,3,3,3,1,1,HSRI,24,24,7,4)
            HSRI(8,6)=VALH(2)
            CALL LCICMA(A1,3,3,3,3,1,1,HSRI,24,24,10,4)
            HSRI(12,5)=VALH(2)
            CALL LCICMA(A1,3,3,3,3,1,1,HSRI,24,24,10,7)
            HSRI(11,9)=VALH(2)

         ELSEIF((NOMFAM.EQ.'OCTAEDRIQUE').OR.
     &          (NOMFAM.EQ.'CUBIQUE1').OR.   
     &          (NOMFAM.EQ.'CUBIQUE2'))   
     &                      THEN
            DO 10 I=1,12
               HSRI(I,I)=VALH(1)
  10        CONTINUE
            HSRI(2,1)=VALH(2)
            HSRI(3,1)=VALH(2)
            HSRI(3,2)=VALH(2)
            HSRI(4,1)=VALH(4)
            HSRI(4,2)=VALH(5)
            HSRI(4,3)=VALH(5)
            HSRI(5,1)=VALH(5)
            HSRI(5,2)=VALH(3)
            HSRI(5,3)=VALH(6)
            HSRI(5,4)=VALH(2)
            HSRI(6,1)=VALH(5)
            HSRI(6,2)=VALH(6)
            HSRI(6,3)=VALH(3)
            HSRI(6,4)=VALH(2)
            HSRI(6,5)=VALH(2)
            HSRI(7,1)=VALH(5)
            HSRI(7,2)=VALH(4)
            HSRI(7,3)=VALH(5)
            HSRI(7,4)=VALH(6)
            HSRI(7,5)=VALH(3)
            HSRI(7,6)=VALH(5)
            HSRI(8,1)=VALH(6)
            HSRI(8,2)=VALH(5)
            HSRI(8,3)=VALH(3)
            HSRI(8,4)=VALH(5)
            HSRI(8,5)=VALH(5)
            HSRI(8,6)=VALH(4)
            HSRI(8,7)=VALH(2)
            HSRI(9,1)=VALH(3)
            HSRI(9,2)=VALH(5)
            HSRI(9,3)=VALH(6)
            HSRI(9,4)=VALH(3)
            HSRI(9,5)=VALH(6)
            HSRI(9,6)=VALH(5)
            HSRI(9,7)=VALH(2)
            HSRI(9,8)=VALH(2)
            HSRI(10,1)=VALH(5)
            HSRI(10,2)=VALH(5)
            HSRI(10,3)=VALH(4)
            HSRI(10,4)=VALH(6)
            HSRI(10,5)=VALH(5)
            HSRI(10,6)=VALH(3)
            HSRI(10,7)=VALH(6)
            HSRI(10,8)=VALH(3)
            HSRI(10,9)=VALH(5)
            HSRI(11,1)=VALH(3)
            HSRI(11,2)=VALH(6)
            HSRI(11,3)=VALH(5)
            HSRI(11,4)=VALH(3)
            HSRI(11,5)=VALH(5)
            HSRI(11,6)=VALH(6)
            HSRI(11,7)=VALH(5)
            HSRI(11,8)=VALH(5)
            HSRI(11,9)=VALH(4)
            HSRI(11,10)=VALH(2)
            HSRI(12,1)=VALH(6)
            HSRI(12,2)=VALH(3)
            HSRI(12,3)=VALH(5)
            HSRI(12,4)=VALH(5)
            HSRI(12,5)=VALH(4)
            HSRI(12,6)=VALH(5)
            HSRI(12,7)=VALH(3)
            HSRI(12,8)=VALH(6)
            HSRI(12,9)=VALH(5)
            HSRI(12,10)=VALH(2)
            HSRI(12,11)=VALH(2)
         ELSE
            CALL U2MESS('F','ALGORITH4_64')
         ENDIF
         DO 1 I=1,24                 
         DO 1 J=1,I                  
            HSRI(J,I)=HSRI(I,J)      
 1       CONTINUE                    
         DO 2 I=1,24                 
         DO 2 J=1,24                 
            HSR(IFA,I,J)=HSRI(I,J)   
 2       CONTINUE                    
      ENDIF

      END
