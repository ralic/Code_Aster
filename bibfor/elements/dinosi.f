      SUBROUTINE DINOSI(NBT,NEQ,NNO,NC,PGL,
     &                  KLV,DUL,SIM,SIP,FONO,
     &                  NUMLOI,VARINT)
C ----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  NBT,NEQ,NNO,NC,NUMLOI
      REAL*8   PGL(3,3),KLV(NBT),DUL(NEQ),SIM(NEQ)
      REAL*8   SIP(NEQ),FONO(NEQ),VARINT(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/12/2006   AUTEUR VOLDOIRE F.VOLDOIRE 
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

C ======================================================================
C
C     CALCUL DES EFFORTS GENERALISES (REPERE LOCAL)
C     ET DES FORCES NODALES (REPERE GLOBAL). COMME ON TRAITE DES
C     ELEMENTS DISCRETS, CES QUANTITES SONT EGALES, AU REPERE PRES.
C
C ======================================================================
C
C IN  : NBT    : NOMBRE DE VALEURS POUR LA DEMI-MATRICE
C       NEQ    : NOMBRE DE DDL DE L'ELEMENT
C       NNO    : NOMBRE DE NOEUDS DE L'ELEMENT (1 OU 2)
C       NC     : NOMBRE DE DDL PAR NOEUD
C       PGL    : MATRICE DE PASSAGE REPERE GLOBAL -> LOCAL
C       KLV    : MATRICE DE "RAIDEUR TANGENTE"
C       DUL    : INCREMENT DE DEPLACEMENT LOCAL
C       SIM    : EFFORTS GENERALISES A L'INSTANT PRECEDENT
C       NUMLOI : NUMERO DE LA LOI DES DISCRETS
C       VARINT : VARIABLE INTERNE A T+
C
C OUT : SIP    : EFFORTS GENERALISES ACTUALISES
C       FONO   : FORCES NODALES
C
C**************** DECLARATION DES VARIABLES LOCALES ********************
C
      REAL*8  KLC(144),FL(12)
      INTEGER II,IFORC
C
C************ FIN DES DECLARATIONS DES VARIABLES LOCALES ***************

C     DANS LE CAS DE LA LOI 2, LES EFFORTS SONT DANS V1
C     ILS SONT EXPRIMES DANS LE REPERE LOCAL
      IF ( NUMLOI .EQ. 2 ) THEN
         IF ( NNO.EQ.1 ) THEN
            DO 110 II = 1,NC
               IFORC = 2*(II-1)+1
               SIP(II)     =  VARINT(IFORC)
               FL(II)      =  VARINT(IFORC)
110         CONTINUE
         ELSEIF ( NNO.EQ.2 ) THEN
            DO 115 II = 1,NC
               IFORC = 2*(II-1)+1
               SIP(II)     =  VARINT(IFORC)
               SIP(II+NC)  =  VARINT(IFORC)
               FL(II)      = -VARINT(IFORC)
               FL(II+NC)   = -VARINT(IFORC)
115         CONTINUE
         ENDIF
      ELSE
C ------ DEMI-MATRICE KLV TRANSFORMEE EN MATRICE PLEINE KLC
         CALL VECMA (KLV,NBT,KLC,NEQ)
C
C ------ CALCUL DE FL = KLC.DUL (INCREMENT D'EFFORT)
         CALL PMAVEC ('ZERO',NEQ,KLC,DUL,FL)
C
C ------ EFFORTS GENERALISES AUX NOEUDS 1 ET 2 (REPERE LOCAL)
C        ON CHANGE LE SIGNE DES EFFORTS SUR LE PREMIER NOEUD
C        POUR LES MECA_DIS_TR_L ET MECA_DIS_T_L
         IF ( NNO.EQ.1 ) THEN
            DO 100 II = 1,NC
               SIP(II)     =  FL(II) + SIM(II)
               FL(II)      =  FL(II) + SIM(II)
100         CONTINUE
         ELSEIF ( NNO.EQ.2 ) THEN
            DO 105 II = 1,NC
               SIP(II)     = -FL(II)    + SIM(II)
               SIP(II+NC)  =  FL(II+NC) + SIM(II+NC)
               FL(II)      =  FL(II)    - SIM(II)
               FL(II+NC)   =  FL(II+NC) + SIM(II+NC)
105         CONTINUE
         ENDIF
      ENDIF

C --- FORCES NODALES AUX NOEUDS 1 ET 2 (REPERE GLOBAL)
      IF (NC.NE.2) THEN
         CALL UTPVLG ( NNO, NC, PGL, FL, FONO )
      ELSE
         CALL UT2VLG ( NNO, NC, PGL, FL, FONO )
      ENDIF
C ----------------------------------------------------------------------
C
 9999 CONTINUE
      END
