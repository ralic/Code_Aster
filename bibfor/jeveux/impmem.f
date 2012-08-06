      SUBROUTINE IMPMEM()
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 06/08/2012   AUTEUR LEFEBVRE J-P.LEFEBVRE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C     RENVOIE LA VALEUR EN MEGA OCTETS DE LA MEMOIRE UTILISEE PAR JEVEUX
C
C ======================================================================
      REAL*8           RVAL(3)
      CHARACTER*8      K8TAB(3)
      INTEGER          IRET
      
      CALL R8INIR(3,-1.D0,RVAL,1)
      K8TAB(1) = 'VMPEAK'
      K8TAB(2) = 'CMAX_JV'  
      K8TAB(3) = 'CUSE_JV'  
      CALL UTGTME(3,K8TAB,RVAL,IRET)
      IF ( IRET .EQ. 0 ) THEN
        IF (RVAL(1).GT.0.D0) THEN
          CALL U2MESR('I','JEVEUX_33',3,RVAL)
        ELSE
          CALL U2MESR('I','JEVEUX_34',3,RVAL)
        ENDIF
      ELSE  
        CALL ASSERT(.FALSE.)
      ENDIF
      
      END      
