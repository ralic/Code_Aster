      SUBROUTINE MDALL2 (NOMRES,BASEMO,NUMGEN,RES,NBO,NBMODE)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*8 BASEMO,NOMRES,NUMGEN,RES,BLANC8
      CHARACTER*16 BLAN16
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/09/2012   AUTEUR ALARCON A.ALARCON 
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
C
C TOLE CRP_21
C
C     ALLOCATION DES VECTEURS DE SORTIE POUR PROJ_RESU_BASE
C     ------------------------------------------------------------------
C IN  : NOMRES : NOM DU RESULTAT DELA COMMANDE (RESU_GENE)
C IN  : BASEMO : BASE MODALE SUR LAQUELLE ON PROJETTE RES
C IN  : NUMGEN : NUME_DDL_GENE
C IN  : RES    : RESULTAT PHYSIQUE A PROJETER
C IN  : NBO    : NOMBRE DE PAS DE TEMPS
C IN  : NBMODE : NOMBRE DE MODES
C ----------------------------------------------------------------------

C     ------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER IINST,INORD,IORDR,IPTEM,JACCE,JVITE,JDEPL,JPASS 
      INTEGER IBID,JINST,JORDR,JREFE,NBMODE,NBO
      REAL*8  DTBID
      CHARACTER*4 K4BID(3)
      CHARACTER*8 K8B
C-----------------------------------------------------------------------
      BLANC8 =  '        '
      BLAN16 = '                '
      IBID = 0
      DTBID = 0.D0
C       
      CALL JEMARQ()

C----- INITIALISATION DE LA SD TYPE

      CALL MDALLO(NOMRES,BASEMO,BLANC8,BLANC8,BLANC8,NBMODE,DTBID,NBO,
     &            0,BLANC8,BLANC8,0,BLANC8,0,
     &            BLANC8,JDEPL,JVITE,JACCE,JPASS,JORDR,JINST,
     &            IBID,IBID,IBID,IBID,IBID,IBID,IBID,IBID,
     &            BLAN16,IBID,K4BID,'TRAN')

C---- MODIFICATION DU .REFD POUR Y AJOUTER LE NUMGEN
      CALL JEVEUO(NOMRES//'           .REFD','E',JREFE)
      ZK24(JREFE+3) = NUMGEN

C---- EN ABSENCE D'INFORMATION SUR LE PAS DE TEMPS, LE .PTEM EST 
C---- EST FORCE A ZERO
      IF (NBO.NE.0) THEN
           DO 66 IPTEM=0,NBO-1
                ZR(JPASS+IPTEM) = DTBID
66         CONTINUE
      ENDIF
C --- REMPLISSAGE DU .ORDR ET DU .DISC 
C
      CALL JEVEUO(RES//'           .ORDR','E',IORDR)
      DO 10 INORD=1,NBO
        ZI(JORDR-1+INORD) = ZI(IORDR-1+INORD)
        CALL RSADPA(RES,'L',1,'INST',ZI(IORDR-1+INORD),0,IINST,K8B)
        ZR(JINST-1+INORD) = ZR(IINST)
10    CONTINUE
C
C
      CALL JEDEMA()
      END
