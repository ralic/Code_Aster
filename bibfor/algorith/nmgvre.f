      SUBROUTINE NMGVRE (COMPOR, MAT, DPIDA2, DBDPHI)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 04/02/2008   AUTEUR GODARD V.GODARD 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*16 COMPOR
      INTEGER      MAT
      REAL*8       DPIDA2, DBDPHI

C ----------------------------------------------------------------------
C     LOIS A GRADIENTS : GRANDEURS DE REFERENCE
C ----------------------------------------------------------------------
C
C ----------------------------------------------------------------------
      
      REAL*8  VAL(5),KG,GAMMA
      CHARACTER*2 K2(5)
      CHARACTER*8 NOM(5)
C ----------------------------------------------------------------------

      IF (COMPOR.EQ.'ENDO_FRAGILE') THEN
  
        NOM(1) = 'E'
        NOM(2) = 'NU'
        NOM(3) = 'SY'
        NOM(4) = 'D_SIGM_EPSI'
        CALL RCVALA(MAT,' ','ELAS'     ,0,' ',0.D0,1,NOM(1),VAL(1),
     &    K2,'F ')
        CALL RCVALA(MAT,' ','ECRO_LINE',0,' ',0.D0,2,NOM(3),VAL(3),
     &    K2,'F ')

        GAMMA  = -VAL(4)/VAL(1)
        KG     = VAL(3)**2 / (2*VAL(1)) * (1+GAMMA)**2
        DPIDA2 = 8.D0/13.D0/(1.D0+GAMMA)*VAL(3)**2 / (2*VAL(1))
        DBDPHI = 2*KG/(1+GAMMA)**3


      ELSEIF ( (COMPOR.EQ.'VMIS_ISOT_LINE')
     &    .OR.(COMPOR.EQ.'VMIS_ISOT_TRAC')) THEN
  
        NOM(1) = 'E'
        NOM(2) = 'NU'
        NOM(3) = 'SY'
        NOM(4) = 'D_SIGM_EPSI'
        CALL RCVALA(MAT,' ','ELAS'     ,0,' ',0.D0,1,NOM(1),VAL(1),
     &    K2,'F ')
        CALL RCVALA(MAT,' ','ECRO_LINE',0,' ',0.D0,2,NOM(3),VAL(3),
     &    K2,'F ')


        DPIDA2= ABS( 4.D0/13.D0*VAL(1)*VAL(4)/(VAL(1)-VAL(4)))
        DBDPHI = 1.D0

      ELSEIF (COMPOR.EQ.'ENDO_ISOT_BETON') THEN
        NOM(1) = 'E'
        NOM(2) = 'NU'
        NOM(3) = 'SYT'
        NOM(4) = 'D_SIGM_EPSI'
        CALL RCVALA(MAT,' ','ELAS'     ,0,' ',0.D0,1,NOM(1),VAL(1),
     &    K2,'F ')
        CALL RCVALA(MAT,' ','BETON_ECRO_LINE',0,' ',0.D0,2,NOM(3),
     &    VAL(3),K2,'F ')

        GAMMA  = -VAL(4)/VAL(1)
        KG     = VAL(3)**2 / (2*VAL(1)) * (1+GAMMA)**2

        DPIDA2 = 8.D0/13.D0/(1.D0+GAMMA)*VAL(3)**2 / (2*VAL(1))
        DBDPHI = 2*KG/(1+GAMMA)**3
      END IF

      END
