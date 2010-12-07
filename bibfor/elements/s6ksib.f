      SUBROUTINE S6KSIB(BKSI)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 12/08/2008   AUTEUR DESROCHES X.DESROCHES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C     ------------------------------------------------------------------
C
C    DERIVEE P/R VARIABLES CANONIQUE AU POINT D INTEGRATION
C    POUR LES ELEMENTS PRISMATIQUES A 6 NOEUDS (POINT D ORIGINE)
C     ------------------------------------------------------------------
C
C
C   ENTREES 
C     NPINT   : NBRE DE  PTS D INTEGRATION
C     XG      : COOR. CANONIQUES DES  PTS D INTEGRATION
C
C   SORTIES :  
C     BKSI(3,6) : LES DERIVEES
C
      IMPLICIT NONE
C
C   VARIABLES GLOBALES 
C
      REAL *8 XG(3)
      REAL *8 BKSI(3,6)
C
C   VARIABLES LOCALES 
C
      REAL *8 UN,ZERO,UNS2
      ZERO=0.D0
      UN=1.D0
      UNS2=1.D0/2.D0
C
C ON REMPLIT LE TABLEAU XG
C
      XG(1)=ZERO
      XG(2)=ZERO
      XG(3)=ZERO     
C CALCUL DE Ni,ksi
      BKSI(1,1)=UNS2*(XG(3)-UN)
      BKSI(1,2)=-BKSI(1,1)
      BKSI(1,3)=ZERO
      BKSI(1,4)=UNS2*(-XG(3)-UN)
      BKSI(1,5)=UNS2*(XG(3)+UN) 
      BKSI(1,6)=BKSI(1,3)

C CALUL DE Ni,eta
      BKSI(2,1)=BKSI(1,1)
      BKSI(2,2)=BKSI(1,3)
      BKSI(2,3)=BKSI(1,2)
      BKSI(2,4)=BKSI(1,4)
      BKSI(2,5)=BKSI(1,3)
      BKSI(2,6)=BKSI(1,5)

C CALCUL DE Ni,zeta
      BKSI(3,1)=UNS2*(-UN+XG(2)+XG(1))
      BKSI(3,2)=UNS2*(-XG(1))
      BKSI(3,3)=UNS2*(-XG(2))
      BKSI(3,4)=-BKSI(3,1)
      BKSI(3,5)=-BKSI(3,2)
      BKSI(3,6)=-BKSI(3,3)
C
      END
