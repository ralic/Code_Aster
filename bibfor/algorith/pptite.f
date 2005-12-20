      FUNCTION PPTITE ( K, N, ARR )
      IMPLICIT   NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 25/10/2004   AUTEUR CIBHHLV L.VIVAN 
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
      INTEGER   K, N, I, IR, J, L, MID
      REAL*8    PPTITE, ARR(N), A, TEMP

      L  = 1
      IR = N
 1    CONTINUE
      IF (IR-L .LE. 1 )THEN 
         IF ( IR-L .EQ. 1 )THEN 
            IF ( ARR(IR) .LT. ARR(L) )THEN
               TEMP=ARR(L)
               ARR(L)=ARR(IR)
               ARR(IR)=TEMP
            ENDIF
         ENDIF
         PPTITE = ARR(K)
         GOTO 9999
      ELSE
         MID=(L+IR)/2 
         TEMP=ARR(MID)
         ARR(MID)=ARR(L+1)
         ARR(L+1)=TEMP
         IF(ARR(L).GT.ARR(IR))THEN
            TEMP=ARR(L)
            ARR(L)=ARR(IR)
            ARR(IR)=TEMP
         ENDIF
         IF(ARR(L+1).GT.ARR(IR))THEN
            TEMP=ARR(L+1)
            ARR(L+1)=ARR(IR)
            ARR(IR)=TEMP
         ENDIF
         IF(ARR(L).GT.ARR(L+1))THEN
            TEMP=ARR(L)
            ARR(L)=ARR(L+1)
            ARR(L+1)=TEMP
         ENDIF
         I=L+1 
         J=IR
         A=ARR(L+1) 
 3       CONTINUE 
            I=I+1 
         IF(ARR(I).LT.A) GOTO 3
 4       CONTINUE
            J = J-1 
         IF (ARR(J).GT.A) GOTO 4
         IF(J.LT.I) GOTO 5 
         TEMP=ARR(I) 
         ARR(I)=ARR(J)
         ARR(J)=TEMP
         GOTO 3 
 5       CONTINUE
         ARR(L+1)=ARR(J) 
         ARR(J)=A
         IF(J.GE.K)IR=J-1 
         IF(J.LE.K)L=I
      ENDIF
      GOTO 1
 9999 CONTINUE
      END
