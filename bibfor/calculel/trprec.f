      SUBROUTINE TRPREC ( MCF, IOCC, EPSI, CRIT, PREC, CRIT2 )
      IMPLICIT   NONE
      CHARACTER*(*)       MCF
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 30/09/2008   AUTEUR REZETTE C.REZETTE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C     COMMANDE:  TEST_RESU
C
C     REMARQUES:  MCF=_F( ...
C                         PRECISION: ( EPSI , PREC  )           L_R8
C                         CRITERE  : ( CRIT , CRIT2 )           L_TXM
C     EPSI ET CRIT  SONT LA PRECISION ET LE CRITERE DU TEST
C     PREC ET CRIT2 SONT LA PRECISION ET LE CRITERE DE L'EXTRACTION
C ----------------------------------------------------------------------
      INTEGER      IOCC, NP, NC
      REAL*8       EPSI, PREC, EPSIR(2)
      CHARACTER*8  CRIT, CRIT2, CRITR(2)
C     ------------------------------------------------------------------
C
      CALL GETVR8 ( MCF, 'PRECISION', IOCC,1,0, EPSI, NP )
      NP = -NP
      IF (NP.EQ.0) THEN
         EPSI = 0.001D0
         PREC = 1.D-6
      ELSE IF (NP.EQ.1) THEN
         CALL GETVR8 ( MCF, 'PRECISION', IOCC,1,1, EPSI, NP )
         PREC = EPSI
      ELSE
         CALL GETVR8 ( MCF, 'PRECISION', IOCC,1,2, EPSIR, NP )
         EPSI = EPSIR(1)
         PREC = EPSIR(2)
      END IF
C
      CALL GETVTX(MCF,'CRITERE',IOCC,1,0,CRIT,NC)
      NC = -NC
      IF (NC.EQ.0) THEN
         CRIT = 'RELATIF'
         CRIT2 = 'RELATIF'
      ELSE IF (NC.EQ.1) THEN
         CALL GETVTX(MCF,'CRITERE',IOCC,1,1,CRIT,NC)
         CRIT2 = CRIT
      ELSE
         CALL GETVTX(MCF,'CRITERE',IOCC,1,2,CRITR,NC)
         CRIT = CRITR(1)
         CRIT2 = CRITR(2)
      END IF
C
      END
