        SUBROUTINE HMDERP(YATE,YAVP,T,R,KH,PVP,PAD,RHO11,RHO12,
     &                      H11,H12,CLIQ,ALPLIQ,
     &                      DP11P1,DP11P2,DP11T,DP12P1,DP12P2,DP12T,
     &                      DP21P1,DP21P2,DP21T,DP22P1,DP22P2,DP22T,
     &                      DP1PP1,DP2PP1,DTPP1,DP1PP2,DP2PP2,DTPP2,
     &                      DP1PT,DP2PT,DTPT
     &                       )
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/03/2010   AUTEUR ANGELINI O.ANGELINI 
C RESPONSABLE GRANET S. GRANET
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
C TOLE CRP_20
C TOLE CRP_21
C
C *********************************************************************
C ROUTINE HMDERP
C CALCULE LES DERIVEES PARTIELLES DES PRESSIONS DU PREMIER 
C ET SECOND ORDRE DANS LE CAS OU THMC = 'LIQU_AD_GAZ_VAPE'
C *********************************************************************
C
C OUT RETCOM : RETOUR LOI DE COMPORTEMENT
C  VARIABLES IN / OUT
C
      IMPLICIT NONE
      INTEGER         YATE
      LOGICAL         YAVP
      REAL*8          T,R,KH,PVP,PAD,RHO11
      REAL*8          RHO12,H11,H12,CLIQ,ALPLIQ
C
C DERIVEES PARTIELLES DES PRESSIONS PAR RAP A P1, P2 ET T
      REAL*8          DP11P1,DP11P2,DP11T,DP12P1,DP12P2,DP12T
      REAL*8          DP21P1,DP21P2,DP21T,DP22P1,DP22P2,DP22T
C
C DERIVEES PARTIELLES SECONDES DE PVP (1) ET PAD (2) 
C PAR RAP A P1, P2 ET T
      REAL*8          DP1PP1(2),DP2PP1(2),DTPP1(2)
      REAL*8          DP1PP2(2),DP2PP2(2),DTPP2(2)
      REAL*8          DP1PT(2),DP2PT(2),DTPT(2)
C
C  VARIABLES LOCALES
C

      REAL*8         A1,A2,A3,A4,L,ZERO
      PARAMETER(ZERO=0.D0)
C
C
      IF (YAVP) THEN 

C *********************************************************************
C CALCUL DES DERIVEES PARTIELLES DES PRESSIONS 
C 
       DP11P1 = 1.D0/((RHO12*R*T/RHO11/KH)-1.D0)
       DP11P2 = (R*T/KH - 1.D0)/((RHO12*R*T/RHO11/KH)-1.D0)
       DP12P1 = 1.D0/(R*T/KH-(RHO11/RHO12))
       DP12P2 = (R*T/KH-1.D0)*DP12P1
       DP21P1 = - DP12P1
       DP21P2 = 1.D0 - DP12P2
       DP22P1 = -1.D0- DP11P1
       DP22P2 = 1.D0- DP11P2
C
       IF ((YATE.EQ.1)) THEN
         L = (H12-H11)
C               
         DP11T = (-L*R*RHO12/KH+PAD/T)/
     &     ((RHO12*R*T/RHO11/KH)-1)
         DP12T = (-L*RHO11+PAD)/T*DP12P1
         DP21T =  - DP12T
         DP22T =  - DP11T
       ENDIF

C *********************************************************************
C CALCUL DES DERIVEES SECONDES DES  PRESSIONS 
       A1 = R*T/KH - RHO11/RHO12
       A2 = RHO11/RHO12*(R*T/KH - 1)
       A3 = DP12T/PVP-1/T-CLIQ*DP22T-3*ALPLIQ
       A4 = -DP12T/PVP+1/T+CLIQ*DP22T-3*ALPLIQ
C
C *********************************************************************
C 
C    DERIVEE DE LA DERIVEE PAR RAPPORT AU GAZ : DP1PP2,DP2PP2,    

       DP1PP2(1) = A2/A1/A1*(CLIQ*DP11P1-1/PVP*DP12P1)
       DP2PP2(1) = A2/A1/A1*(CLIQ*DP11P2-1/PVP*DP12P2)
       DP1PP2(2) = R*T*A2/A1/A1/KH*(-CLIQ*DP11P1+1/PVP*DP12P1)
       DP2PP2(2) = R*T*A2/A1/A1/KH*(-CLIQ*DP11P2+1/PVP*DP12P2)
      
      
C    DERIVEE DE LA DERIVEE PAR RAPPORT A PC : DP1PP1,DP2PP1,
C    

       DP1PP1(1) = -RHO11/RHO12/A1/A1*(DP12P1/PVP-CLIQ*DP11P1)
       DP2PP1(1) = -RHO11/RHO12/A1/A1*(DP12P2/PVP-CLIQ*DP11P2)
       DP1PP1(2) = R*T/KH*RHO11/RHO12/A1/A1*(DP12P1/PVP-CLIQ*DP11P1)
       DP2PP1(2) = R*T/KH*RHO11/RHO12/A1/A1*(DP12P2/PVP-CLIQ*DP11P2)
     
       IF ((YATE.EQ.1)) THEN
          DTPP2(1) = R/A1/KH - (R*T/KH-1)/A1/A1*(R/KH-A4*RHO11/RHO12)
          DTPP1(1) = -1.D0/A1/A1*(R/KH-RHO11/RHO12*A4)
          DTPP2(2) = R/A1/KH*RHO11/RHO12 + (RHO11/RHO12)*(RHO11/RHO12)*
     &           A2*R/KH/A1/A1*(1.D0+A3*T)
          DTPP1(2) = R/KH/A1/A1*RHO11/RHO12*(1.D0+A3*T)
          
C    DERIVEE DE LA DERIVEE PAR RAPPORT A T : DP1PT,DP2PT,DTPT
C    
          
          DP1PT(1) = -1.D0/T/A1/A1*(A1*(1.D0-DP11P1*(1.D0+L*RHO11*CLIQ))
     &           +(PAD-L*RHO11)*RHO11/RHO12*(CLIQ*DP11P1-DP12P1/PVP))
          DP2PT(1) = -1.D0/T/A1/A1*(A1*(1.D0-DP11P2*(1.D0+L*RHO11*CLIQ))
     &           +(PAD-L*RHO11)*RHO11/RHO12*(CLIQ*DP11P2-DP12P2/PVP))
          DTPT(1) = 
     >     +1.D0/T/A1*(DP22T-L*(RHO11*DP11T*CLIQ-3.D0*ALPLIQ*RHO11))-
     &     1.D0/T/T/A1/A1*(R*T/KH-RHO11/RHO12+T*(R/KH-RHO11/RHO12*A4))
     &     *(PAD-L*RHO11)
                
          DP1PT(2) = 
     >      1.D0/A1*RHO11/RHO12*(L*R/KH*RHO12/PVP*DP12P1-DP22P1/T)
     &     -R*T/KH*RHO11/RHO12/A1/A1*(L*R/KH*RHO12-PAD/T)*A3
          DP2PT(2) = 
     >      1.D0/A1*RHO11/RHO12*(L*R/KH*RHO12/PVP*DP12P2-DP22P2/T)
     &     -R*T/KH*RHO11/RHO12/A1/A1*(L*R/KH*RHO12-PAD/T)*A3
          DTPT(2) = 
     >      RHO11/RHO12/A1*(L*R*RHO12/KH*(DP12T/PVP-1.D0/T)+PAD/T/T
     &     -DP12T/T)-R*T/KH*RHO11/RHO12/A1/A1*
     &      (L*R*RHO12/KH-PAD/T)*(A3+1.D0/T)

       ENDIF
      ELSE
C *********************************************************************
C CALCUL DES DERIVEES PARTIELLES DES PRESSIONS 
C 
       DP11P1 = 1.D0/((RHO12*R*T/RHO11/KH)-1.D0)
       DP11P2 = (R*T/KH - 1.D0)/((RHO12*R*T/RHO11/KH)-1.D0)
       DP12P1 = ZERO
       DP12P2 = ZERO
       DP21P1 = - DP12P1
       DP21P2 = 1.D0 - DP12P2
       DP22P1 = -1.D0- DP11P1
       DP22P2 = 1.D0- DP11P2
C
       IF ((YATE.EQ.1)) THEN
         L = (H12-H11)
C               
         DP11T = (-L*R*RHO12/KH+PAD/T)/
     &     ((RHO12*R*T/RHO11/KH)-1)
         DP12T = ZERO
         DP21T =  - DP12T
         DP22T =  - DP11T
       ENDIF

C *********************************************************************
C CALCUL DES DERIVEES SECONDES DES  PRESSIONS 

C
C *********************************************************************
C 
C    DERIVEE DE LA DERIVEE PAR RAPPORT AU GAZ : DP1PP2,DP2PP2,    

       DP1PP2(1) = ZERO
       DP2PP2(1) = ZERO
       DP1PP2(2) = ZERO
       DP2PP2(2) = ZERO
      
      
C    DERIVEE DE LA DERIVEE PAR RAPPORT A PC : DP1PP1,DP2PP1,
C    

       DP1PP1(1) = ZERO
       DP2PP1(1) = ZERO
       DP1PP1(2) = ZERO
       DP2PP1(2) = ZERO
     
       IF ((YATE.EQ.1)) THEN
          DTPP2(1) = ZERO
          DTPP1(1) = ZERO
          DTPP2(2) = ZERO
          DTPP1(2) = ZERO
          
C    DERIVEE DE LA DERIVEE PAR RAPPORT A T : DP1PT,DP2PT,DTPT
C    
          
          DP1PT(1) = ZERO
          DP2PT(1) = ZERO
          DTPT(1)  = ZERO
          DP1PT(2) = ZERO
          DP2PT(2) = ZERO
          DTPT(2)  = ZERO
       ENDIF
      ENDIF

C
      END
