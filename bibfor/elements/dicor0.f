      SUBROUTINE DICOR0 (K0,VARIM,VARIP1,VARIP2,DNSDU,DMSDT,DNSDT)
C ----------------------------------------------------------------------
      IMPLICIT NONE
      REAL*8 K0(78),VARIM,VARIP1,VARIP2,DNSDU,DMSDT,DNSDT
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 10/01/95   AUTEUR D6BHHIV I.VAUTIER 
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
C
C     UTILITAIRE POUR LE COMPORTEMENT CORNIERE.
C
C ----------------------------------------------------------------------
C
C IN  : K0     : COEFFICIENTS DE RAIDEUR TANGENTE
C       VARIM  : VARIABLE INTERNE PRECEDENTE
C
C OUT : VARIP$ : VARIABLES INTERNES ACTUALISEES
C       DNSDU  :
C       DMSDT  :
C       DNSDT  :
C
C ----------------------------------------------------------------------
      DNSDU = K0(1)
      DMSDT = K0(15)
      DNSDT = 0.D0
      VARIP1 = VARIM
      VARIP2 = 0.D0
C ----------------------------------------------------------------------
C
 9999 CONTINUE
      END
