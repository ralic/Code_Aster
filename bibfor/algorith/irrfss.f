      SUBROUTINE IRRFSS ( SIG , DDFDDS )
      IMPLICIT NONE
      REAL*8   SIG(6),DDFDDS(6,6)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/02/2006   AUTEUR CIBHHPD L.SALMONA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       3D / DP / CP
C       DERIVEE / S / S DE LA FONCTION SEUIL A (SIG ) DONNES
C
C       IN  SIG    :  TENSEUR CONTRAINTE
C                                                     T
C       OUT D2FD2S :  DDFDDS = 1/S ( 3/2 ID - DFDS DFDS )
C                     DFDS   = 3 D / 2 S
C                                            T
C                     ID     = I4 - 1/3 I2 I2
C                                           T           1/2
C                     S      = (3/2(D-X1-X2) (D-X1-X2))
C                     D      = SIG - 1/3 TR(SIG) I
C       ----------------------------------------------------------------
      REAL*8 ID(6,6),D23,D13,ZERO,UN,S
      PARAMETER       ( D23  =  .66666666666666D0 )
      PARAMETER       ( D13  = -.33333333333333D0 )
      PARAMETER       ( ZERO =  0.D0              )
      PARAMETER       ( UN   =  1.D0              )
      REAL*8 LCNRTS,DEV(6),DFDS(6),DFDS2(6,6)
      DATA ID         / D23   , D13   , D13   , ZERO , ZERO , ZERO ,
     &                  D13   , D23   , D13   , ZERO , ZERO , ZERO ,
     &                  D13   , D13   , D23   , ZERO , ZERO , ZERO ,
     &                  ZERO  , ZERO  , ZERO  , UN   , ZERO , ZERO ,
     &                  ZERO  , ZERO  , ZERO  , ZERO , UN   , ZERO ,
     &                  ZERO  , ZERO  , ZERO  , ZERO , ZERO , UN /

        

      CALL LCDEVI ( SIG , DEV )
      S =  LCNRTS ( DEV )
      IF ( S.EQ.0.D0) THEN
        CALL LCINMA (0.D0,DDFDDS)
      ELSE
        CALL LCPRSV ( 1.5D0 / S , DEV , DFDS )
        CALL LCPRTE ( DFDS  , DFDS  , DFDS2 )
        CALL LCPRSM ( 1.5D0   , ID    , DDFDDS )
        CALL LCDIMA ( DDFDDS, DFDS2 , DDFDDS )
        CALL LCPRSM ( 1.D0/ S , DDFDDS, DDFDDS )
      ENDIF

      END
