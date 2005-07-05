      SUBROUTINE DIAGP3(TENS,VECP,VALP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/07/2005   AUTEUR PBADEL P.BADEL 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
      REAL*8   TENS(6),VALP(3),VECP(3,3)
      
C ----------------------------------------------------------------------
C  DIAGONALISATION MATRICE 3x3 SYMETRIQUE PAR UNE METHODE DIRECTE
C    IN    TENS   : TENSEUR SOUS LA FORME
C                     (XX XY XZ YY YZ ZZ)
C ----------------------------------------------------------------------
      
      INTEGER I,J,NRAC,IND
      LOGICAL INVVP,TNULL
      REAL*8  TRACE,X(6),Y(6),DET(4),D12,D13,D23,RTEMP,NORM(3)
      REAL*8  A,B,C,R8PI,THETA,R8MIEM,R8PREM
      REAL*8  F,G
      REAL*8  TPS(6)
      
C      CALL UTTCPU(88,'DEBUT',6,TPS)
      

      CALL R8INIR(9,0.D0,VECP,1)

C      write (6,*) '-----------------------------------------------'
C      write (6,*) TENS(1),TENS(4),TENS(6),TENS(2),TENS(3),TENS(5)
C -- PASSAGE AU DEVIATEUR
      TRACE=(TENS(1)+TENS(4)+TENS(6))/3.D0
C      write (6,*) TRACE,R8PREM()
      TENS(1)=TENS(1)-TRACE
      TENS(4)=TENS(4)-TRACE
      TENS(6)=TENS(6)-TRACE
C      write (6,*) TENS(1),TENS(4),TENS(6),TENS(2),TENS(3),TENS(5)

C -- CALCUL DES COEFFICIENTS DU POLYNOME P3   
      
      DET(1)=TENS(4)*TENS(6)-TENS(5)**2
      DET(2)=TENS(1)*TENS(6)-TENS(3)**2
      DET(3)=TENS(1)*TENS(4)-TENS(2)**2

      
      DET(4)=TENS(1)*DET(1)-TENS(2)*(TENS(2)*TENS(6)-TENS(5)*TENS(3))
     &       +TENS(3)*(TENS(2)*TENS(5)-TENS(4)*TENS(3))
      
      CALL ZEROP3(0.D0,DET(1)+DET(2)+DET(3),-DET(4),VALP,NRAC)

C -- CAS DES RACINES DOUBLES QUI PASSENT MAL NUMERIQUEMENT      
      IF (NRAC.EQ.1) THEN
        RTEMP=(DET(1)+DET(2)+DET(3))
        IF (RTEMP.LE.0.D0) THEN
          VALP(2)=SQRT(-RTEMP/3.D0)
          VALP(3)=-SQRT(-RTEMP/3.D0)
          IF (ABS(VALP(2)**3+RTEMP*VALP(2)-DET(4)).LT.
     &          ABS(VALP(3)**3+RTEMP*VALP(3)-DET(4))) THEN
            VALP(3)=VALP(2)
          ELSE
            VALP(2)=VALP(3)
          ENDIF
        ELSE
          TNULL = .TRUE.
          DO 600 I=1,6
            IF (ABS(TENS(I)).GT.(R8PREM()*100*ABS(TRACE))) THEN
              TNULL = .FALSE.
            ENDIF
600       CONTINUE
          IF (TNULL) THEN
            VALP(1)=0.D0
            VALP(2)=0.D0
            VALP(3)=0.D0
          ELSE
            CALL UTMESS('F','DIAGP3','PAS DE VALEURS PROPRES TROUVEES')
          ENDIF
        ENDIF
        IF (VALP(2).GT.VALP(1)) THEN
            RTEMP=VALP(1)
            VALP(1)=VALP(2)
            VALP(3)=RTEMP
        ENDIF
      ENDIF   

      RTEMP=VALP(1)
      VALP(1)=VALP(3)
      VALP(3)=RTEMP
      
C      write (6,*) 'VALEURS PROPRES :',VALP(1),VALP(2),VALP(3)
        
C -- ON RECHERCHE LA VALEUR PROPRE LA PLUS SEPAREE
C    ON LA MET DANS VALP(1)
      IF ((VALP(2)-VALP(1)).GT.(VALP(3)-VALP(1))/2.D0) THEN
        INVVP=.FALSE.
      ELSE
        INVVP=.TRUE.
        RTEMP=VALP(1)
        VALP(1)=VALP(3)
        VALP(3)=RTEMP
      ENDIF
            
C -- VECP DE LA VAL PROPRE LA PLUS SEPAREE
C      write (6,*) 'VAL PROPRE APRES TRI : ',VALP(1),VALP(2),VALP(3)
C -- ON MULTIPLIE LES 3 VECT DE BASE PAR (A-LAMBDA_3.ID)(A-LAMBDA_2.ID)
C      ON PRENDRA CELUI DONT LA NORME EST LA PLUS GRANDE
      DO 4000 IND=1,3
        IF (IND.EQ.1) THEN
          X(1)= TENS(1)-VALP(2)      
          X(2)= TENS(2)      
          X(3)= TENS(3)      
        ELSE IF (IND.EQ.2) THEN
          X(1)= TENS(2)
          X(2)= TENS(4)-VALP(2)
          X(3)= TENS(5)
        ELSE
          X(1)= TENS(3)
          X(2)= TENS(5)
          X(3)= TENS(6)-VALP(2)
        ENDIF
        VECP(1,IND)=(TENS(1)-VALP(3))*X(1)+
     &                     TENS(2)*X(2)+TENS(3)*X(3)
        VECP(2,IND)=TENS(2)*X(1)+(TENS(4)-VALP(3))*X(2)+
     &                     TENS(5)*X(3)
        VECP(3,IND)=TENS(3)*X(1)+TENS(5)*X(2)+
     &                    (TENS(6)-VALP(3))*X(3)
        Y(IND)=(VECP(1,IND))**2+(VECP(2,IND))**2+
     &                    (VECP(3,IND))**2
4000  CONTINUE
      RTEMP=Y(1)
      IND=1
      IF (Y(2).GT.Y(1)) THEN
        IF (Y(3).GT.Y(2)) THEN
          IND=3
        ELSE
          IND=2
        ENDIF
      ELSE IF (Y(3).GT.Y(1)) THEN
        IND=3
      ENDIF
      A=SQRT(Y(IND))
C -- CAS DE 3 VALEURS PROPRES EGALES      
      IF (A.LT.R8MIEM()) THEN
        CALL R8INIR(9,0.D0,VECP,1)
        VECP(1,1)=1.D0
        VECP(2,2)=1.D0
        VECP(3,3)=1.D0
        GOTO 9999
      ENDIF
      DO 4010 I=1,3
        VECP(I,1)=VECP(I,IND)/A
4010  CONTINUE

C -- AUTRES VECTEURS PROPRES : ON PASSE DANS LE SOUS-ESPACE
C    ORTHOGONAL AU PREMIER VECTEUR PROPRE
        
C -- ON CHERCHE DEUX VECTEURS ORTHOGONAUX AU PREMIER VECT PROPRE 
C    ON COMMENCE PAR PRENDRE CELUI ORTHOGONAL A UN VECT DE BASE
C    DONT LA NORME EST LA PLUS GRANDE (POUR LE CAS OU LE PREMIER
C    VECTEUR PROPRE SERAIT UN VECTEUR DE BASE)       
      Y(1)=VECP(3,1)**2+VECP(2,1)**2
      Y(2)=VECP(3,1)**2+VECP(1,1)**2
      Y(3)=VECP(1,1)**2+VECP(2,1)**2      
      RTEMP=Y(1) 
      IND=1
      IF (Y(2).GT.Y(1)) THEN
        IF (Y(3).GT.Y(2)) THEN
          IND=3
        ELSE
          IND=2
        ENDIF
      ELSE IF (Y(3).GT.Y(1)) THEN
        IND=3
      ENDIF
      A=SQRT(Y(IND))
      IF (IND.EQ.1) THEN
        VECP(1,2)=0.D0
        VECP(2,2)=-VECP(3,1)/A
        VECP(3,2)=VECP(2,1)/A
      ELSE IF (IND.EQ.2) THEN      
        VECP(1,2)=VECP(3,1)/A
        VECP(2,2)=0.D0
        VECP(3,2)=-VECP(1,1)/A
      ELSE
        VECP(1,2)=-VECP(2,1)/A
        VECP(2,2)=VECP(1,1)/A
        VECP(3,2)=0.D0
      ENDIF
      VECP(1,3)=VECP(2,1)*VECP(3,2)-
     &                 VECP(3,1)*VECP(2,2)
      VECP(2,3)=VECP(3,1)*VECP(1,2)-
     &                 VECP(1,1)*VECP(3,2)
      VECP(3,3)=VECP(1,1)*VECP(2,2)-
     &                      VECP(2,1)*VECP(1,2)

C -- ON PASSE DANS LE SOUS-ESPACE ORTHOGONAL

      A=VECP(1,2)*(VECP(1,2)*TENS(1)+2.D0*VECP(2,2)*TENS(2))
     & +VECP(2,2)*(VECP(2,2)*TENS(4)+2.D0*VECP(3,2)*TENS(5))
     & +VECP(3,2)*(VECP(3,2)*TENS(6)+2.D0*VECP(1,2)*TENS(3))
      B=VECP(1,3)*(VECP(1,3)*TENS(1)+2.D0*VECP(2,3)*TENS(2))
     & +VECP(2,3)*(VECP(2,3)*TENS(4)+2.D0*VECP(3,3)*TENS(5))
     & +VECP(3,3)*(VECP(3,3)*TENS(6)+2.D0*VECP(1,3)*TENS(3))
      C=VECP(1,2)*(VECP(1,3)*TENS(1)+VECP(2,3)*TENS(2)+VECP(3,3)*TENS(3)
     &)+VECP(2,2)*(VECP(1,3)*TENS(2)+VECP(2,3)*TENS(4)+VECP(3,3)*TENS(5)
     &)+VECP(3,2)*(VECP(1,3)*TENS(3)+VECP(2,3)*TENS(5)+VECP(3,3)*TENS(6)
     &)
C -- ON CHERCHE L'ANGLE DONT EST TOURNE LE REPERE PROPRE

        F=2.D0*C
        G=A-B
        IF (ABS(F).LT.R8MIEM()) THEN
          THETA=0.D0
        ELSE
          THETA=ATAN2(F,G)
          THETA=THETA/2.D0
        ENDIF
        
C -- EST-CE THETA OU THETA+PI/2 ?

        RTEMP=(A-B)*COS(2.D0*THETA)+2.D0*C*SIN(2.D0*THETA)
        IF (RTEMP*(VALP(2)-VALP(3)).LT.0.D0)
     &     THETA=THETA+R8PI()/2.D0
     
C        write (6,*) 'THETA =',THETA
        A=COS(THETA)
        B=SIN(THETA)
     
        Y(1)=VECP(1,2)*A+VECP(1,3)*B        
        Y(2)=VECP(2,2)*A+VECP(2,3)*B        
        Y(3)=VECP(3,2)*A+VECP(3,3)*B        
        VECP(1,2)=Y(1)
        VECP(2,2)=Y(2)
        VECP(3,2)=Y(3)

        VECP(1,3)=VECP(2,1)*VECP(3,2)-
     &                VECP(3,1)*VECP(2,2)
        VECP(2,3)=VECP(3,1)*VECP(1,2)-
     &                VECP(1,1)*VECP(3,2)
        VECP(3,3)=VECP(1,1)*VECP(2,2)-
     &                      VECP(2,1)*VECP(1,2)

        IF (INVVP) THEN
          DO 5050 I=1,3            
            RTEMP=VECP(I,1)
            VECP(I,1)=VECP(I,3)
            VECP(I,3)=-RTEMP
5050      CONTINUE
          RTEMP=VALP(1)
          VALP(1)=VALP(3)
          VALP(3)=RTEMP
        ENDIF
      
C        Y(1)=TENS(1)
C        Y(2)=TENS(4)
C        Y(3)=TENS(6)
C        Y(4)=TENS(2)
C        Y(5)=TENS(3)
C        Y(6)=TENS(5)
C        CALL BGTOBP(Y,X,VECP)
       
C        IF ((ABS(X(1)-VALP(1)).GT.TOL).OR.
C     &      (ABS(X(2)-VALP(2)).GT.TOL).OR.
C     &      (ABS(X(3)-VALP(3)).GT.TOL)) THEN
C          write (6,*) 'X(1) = ',X(1),' ; VALP(1) = ',VALP(1)
C          write (6,*) 'X(2) = ',X(2),' ; VALP(2) = ',VALP(2)
C          write (6,*) 'X(3) = ',X(3),' ; VALP(3) = ',VALP(3)
C        ENDIF
      
      DO 200 I=1,3
        VALP(I)=VALP(I)+TRACE
200   CONTINUE
       
      
9999  CONTINUE
C      CALL UTTCPU(88,'FIN',6,TPS)
C      IF (MOD(INT(TPS(2)),100000).EQ.0) THEN
C        write (6,*) 'NB APP = ',TPS(2),' ; TOT = ',TPS(3),
C     &              ' ; MOY = ',TPS(4)
C      ENDIF
      END
