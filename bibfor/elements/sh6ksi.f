      SUBROUTINE SH6KSI(NPINT,XXG,BKSI)
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
C     ------------------------------------------------------------------
C         DERIVEE P/R VARIABLES CANONIQUE AUX POINTS D INTEGRATION
C          POUR LES ELEMENTS PRISMATIQUES A 6 NOEUDS
C                                                    TRINH V.D     2007
C     ------------------------------------------------------------------
C
C   ENTREES 
C     NPINT   : NBRE DE  PTS D INTEGRATION
C     XG      : COOR. CANONIQUES DES  PTS D INTEGRATION
C
C   SORTIES :  
C     BKSI(3,6,NPINT) : LES DERIVEES
C
      IMPLICIT NONE
C
C   VARIABLES GLOBALES 
C
      INTEGER NPINT
      REAL *8 XG(3,5),XXG(5)
      REAL *8 BKSI(3,6,5)
C
C   VARIABLES LOCALES 
C
      REAL *8 UN,ZERO,UNS2,UNS3
      INTEGER IP
      ZERO=0.D0
      UN=1.D0
      UNS2=1.D0/2.D0
      UNS3=1.D0/3.D0
C 
C ON REMPLIT LE TABLEAU XG
C
      DO 10 IP=1,NPINT
         XG(1,IP)=UNS3
         XG(2,IP)=UNS3
         XG(3,IP)=XXG(IP)
   10 CONTINUE
C
      DO 20 IP=1,NPINT
C      
C CALCUL DE Ni,ksi
C
        BKSI(1,1,IP)=UNS2*(XG(3,IP)-UN)
        BKSI(1,2,IP)=-BKSI(1,1,IP)
        BKSI(1,3,IP)=ZERO
        BKSI(1,4,IP)=UNS2*(-XG(3,IP)-UN)
        BKSI(1,5,IP)=UNS2*(XG(3,IP)+UN) 
        BKSI(1,6,IP)=BKSI(1,3,IP)
C
C CALUL DE Ni,eta
C
        BKSI(2,1,IP)=BKSI(1,1,IP)
        BKSI(2,2,IP)=BKSI(1,3,IP)
        BKSI(2,3,IP)=BKSI(1,2,IP)
        BKSI(2,4,IP)=BKSI(1,4,IP)
        BKSI(2,5,IP)=BKSI(1,3,IP)
        BKSI(2,6,IP)=BKSI(1,5,IP)
C
C CALCUL DE Ni,zeta
C
        BKSI(3,1,IP)=UNS2*(-UN+XG(2,IP)+XG(1,IP))
        BKSI(3,2,IP)=UNS2*(-XG(1,IP))
        BKSI(3,3,IP)=UNS2*(-XG(2,IP))
        BKSI(3,4,IP)=-BKSI(3,1,IP)
        BKSI(3,5,IP)=-BKSI(3,2,IP)
        BKSI(3,6,IP)=-BKSI(3,3,IP)
C
   20 CONTINUE
C
      END
