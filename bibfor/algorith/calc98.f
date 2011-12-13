      SUBROUTINE CALC98 (NOMRES,MAILLA,NUMDDL)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 25/11/98   AUTEUR CIBHHGB G.BERTRAND 
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
      IMPLICIT REAL*8 (A-H,O-Z)
C
C***********************************************************************
C    P. RICHARD                                     DATE 09/07/91
C-----------------------------------------------------------------------
C  BUT: ROUTINE DE CALCUL DE BASE MODALE
C
C    BASE MODALE DE TYPE MIXTE CRAIG-BAMPTON MAC-NEAL OU INCONNUE
C
C-------- DEBUT COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C-----  FIN  COMMUNS NORMALISES  JEVEUX  -------------------------------
C
      CHARACTER*8 NOMRES,MAILLA
      CHARACTER*19 NUMDDL
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C---------  RECUPERATION DES NOEUDS D'INTERFACE       ------------------
C
      CALL DEFINT (MAILLA,NOMRES)
C
C-------------CREATION TABLEAU DESCRIPTION DES DEFORMES ----------------
C           ET MODIFICATION NUMEROTATION NOEUDS INTERFACES
C
      CALL CRLIDD(NOMRES,MAILLA)
C
C---------------PRISE EN COMPTE DES MASQUES DE DDL AUX NOEUDS-----------
C             ET DETERMINATION DU NOMBRE DE MODES ET DEFORMEES
C
      CALL GESDEF(NOMRES,NUMDDL)
C
C
C-----------------DETERMINATION DES DDL INTERFACE ACTIFS FINAUX---------
C
C
      CALL DDLACT(NOMRES,NUMDDL)
C
      END
