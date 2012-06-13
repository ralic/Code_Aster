      SUBROUTINE MDALL2 (NOMRES,BASEMO,NUMGEN,RES,NBO,NBMODE)
      IMPLICIT    REAL*8 (A-H,O-Z)
      INCLUDE 'jeveux.h'
      CHARACTER*8 BASEMO,NOMRES,NUMGEN,RES,KBID
      INTEGER     NBO,NBMOD,NBSTOC
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C     ALLOCATION DES VECTEURS DE SORTIE
C     ------------------------------------------------------------------
C IN  : NOMRES : NOM DU RESULTAT DELA COMMANDE (RESU_GENE)
C IN  : BASEMO : BASE MODALE SUR LAQUELLE ON PROJETTE RES
C IN  : NUMGEN : NUME_DDL_GENE
C IN  : RES    : RESULTAT PHYSIQUE A PROJETER
C IN  : NBO    : NOMBRE DE PAS DE TEMPS
C IN  : NBMODE : NOMBRE DE MODES
C ----------------------------------------------------------------------

C     ------------------------------------------------------------------
      CALL JEMARQ()
      NBSTOC = NBMODE * NBO

C --- REMPLISSAGE DU .REFD

      CALL JEEXIN(NOMRES//'           .REFD',IRET)
      IF (IRET.EQ.0) THEN
C On recupere la numerotation generalisee
         CALL WKVECT(NOMRES//'           .REFD','G V K24',7,JREFE)
         ZK24(JREFE)   = ' '
         ZK24(JREFE+1) = ' '
         ZK24(JREFE+2) = ' '
         ZK24(JREFE+3) = NUMGEN
         ZK24(JREFE+4) = ' '
         ZK24(JREFE+5) = BASEMO(1:8)
         ZK24(JREFE+6) = ' '
      ENDIF
C
C --- REMPLSSAGE DU .DESC

      CALL JEEXIN(NOMRES//'           .DESC',IRET)
      IF (IRET.EQ.0) THEN
         CALL WKVECT(NOMRES//'           .DESC','G V I',5,JDESC)
         ZI(JDESC) = 1
         ZI(JDESC+1) = NBMODE
         ZI(JDESC+2) = 0
         ZI(JDESC+3) = 0
         ZI(JDESC+4) = 0
      ENDIF
C
      IF (NBO.NE.0) THEN
        CALL JECREO(NOMRES//'           .DEPL' ,'G V R')
        CALL JEECRA(NOMRES//'           .DEPL' ,'LONMAX',NBSTOC,KBID)
        CALL JEECRA(NOMRES//'           .DEPL' ,'LONUTI',NBSTOC,KBID)
        CALL JEVEUT(NOMRES//'           .DEPL' ,'E',JDEPL)
        CALL JECREO(NOMRES//'           .VITE' ,'G V R')
        CALL JEECRA(NOMRES//'           .VITE' ,'LONMAX',NBSTOC,KBID)
        CALL JEECRA(NOMRES//'           .VITE' ,'LONUTI',NBSTOC,KBID)
        CALL JEVEUT(NOMRES//'           .VITE' ,'E',JVITE)
        CALL JECREO(NOMRES//'           .ACCE' ,'G V R')
        CALL JEECRA(NOMRES//'           .ACCE' ,'LONMAX',NBSTOC,KBID)
        CALL JEECRA(NOMRES//'           .ACCE' ,'LONUTI',NBSTOC,KBID)
        CALL JEVEUT(NOMRES//'           .ACCE' ,'E',JACCE)
        CALL JECREO(NOMRES//'           .ORDR' ,'G V I')
        CALL JEECRA(NOMRES//'           .ORDR' ,'LONMAX',NBO,KBID)
        CALL JEECRA(NOMRES//'           .ORDR' ,'LONUTI',NBO,KBID)
        CALL JEVEUT(NOMRES//'           .ORDR' ,'E',JORDR)
        CALL JECREO(NOMRES//'           .INST' ,'G V R')
        CALL JEECRA(NOMRES//'           .INST' ,'LONMAX',NBO,KBID)
        CALL JEECRA(NOMRES//'           .INST' ,'LONUTI',NBO,KBID)
        CALL JEVEUT(NOMRES//'           .INST' ,'E',JINST)
        CALL JECREO(NOMRES//'           .PTEM' ,'G V R')
        CALL JEECRA(NOMRES//'           .PTEM' ,'LONMAX',1,KBID)
        CALL JEECRA(NOMRES//'           .PTEM' ,'LONUTI',1,KBID)
        CALL JEVEUT(NOMRES//'           .PTEM' ,'E',JPTEM)
      ENDIF

C --- REMPLISSAGE DU .ORDR
C
      CALL JEVEUO(RES//'           .RSPR','E',IINST)
      CALL JEVEUO(RES//'           .ORDR','E',IORDR)
      DO 10 INORD=1,NBO
        ZI(JORDR-1+INORD) = ZI(IORDR-1+INORD)
        ZR(JINST-1+INORD) = ZR(IINST-1+INORD)
10    CONTINUE
C
C
      CALL JEDEMA()
      END
