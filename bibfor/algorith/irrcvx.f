      SUBROUTINE IRRCVX ( FAMI, KPG, KSP, NMAT, MATER, SIG ,VIN, 
     &                    SEUIL )
      IMPLICIT  NONE

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
      CHARACTER*(*) FAMI
      INTEGER       KPG, KSP, NMAT
      REAL*8        MATER(NMAT,2),SIG(6),VIN(*),SEUIL

C --- BUT : CONVEXE ELASTO PLASTIQUE A T+DT POUR (SIGF , VIND) DONNES --
C ----------------------------------------------------------------------
C IN  : FAMI   :  FAMILLE DES POINTS DE GAUSS  -------------------------
C --- : KPG    :  NUMERO DU POINT DE GAUSS  ----------------------------
C --- : KSP    :  NUMERO DU SOUS POINT DE GAUSS ------------------------
C --- : SIG    :  CONTRAINTE A T+DT ------------------------------------
C --- : VIN    :  VARIABLES INTERNES A T -------------------------------
C --- : NMAT   :  DIMENSION MATER --------------------------------------
C --- : MATER  :  COEFFICIENTS MATERIAU A T+DT -------------------------
C OUT : SEUIL  :  SEUIL  ELASTICITE  A T+DT ----------------------------
C ----------------------------------------------------------------------
C ======================================================================

      REAL*8 IRRAD, IRRAF, ETAI, P, DEV(6),K,N,P0,ETAIS,LCNRTS
      INTEGER IRET
      
C     RECUPERATION DE L IRRADIATION
      CALL RCVARC('F','IRRA','-',FAMI,KPG,KSP,IRRAD,IRET)
      CALL RCVARC('F','IRRA','+',FAMI,KPG,KSP,IRRAF,IRET)
C VARIABLES INTERNES      
      P=VIN(1)
C PARAMETRES MATERIAUX
      K=MATER(1,2)
      N=MATER(2,2)
      P0=MATER(3,2)
      IF ( (IRRAF-IRRAD).GT.0.D0) THEN
        SEUIL=1.D0
        GOTO 9999
      ELSE IF ( (IRRAD-IRRAF).GT.0.D0) THEN
        CALL UTMESS ('F','IRRCVX','PROBLEME DANS LA DEFINITION DE LA'//
     &  ' FLUENCE. ELLE DIMINUE AU COURS DU TEMPS!')
      ELSE
        CALL LCDEVI( SIG, DEV )
        IF ( ((P+P0).EQ.0.D0).AND.(N.EQ.0.D0)) THEN
          SEUIL=LCNRTS(DEV)-K
        ELSE
          SEUIL=LCNRTS(DEV)-K*((P+P0)**N)
        ENDIF
      ENDIF
9999  CONTINUE
      END
