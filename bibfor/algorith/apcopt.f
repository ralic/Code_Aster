      SUBROUTINE APCOPT(SDAPPA,IP   ,COORPT)
C     
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      INCLUDE 'jeveux.h'
      CHARACTER*19 SDAPPA
      INTEGER      IP
      REAL*8       COORPT(3)
C      
C ----------------------------------------------------------------------
C
C ROUTINE APPARIEMENT (UTILITAIRE)
C
C COORDONNEES DU POINT
C
C ----------------------------------------------------------------------
C
C
C IN  SDAPPA : NOM DE LA SD APPARIEMENT
C IN  IP     : NUMERO DU POINT
C OUT COORPT : COORDONNEES DU POINT
C
C
C
C
      CHARACTER*24 APPOIN
      INTEGER      JPOIN
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()   
C
C --- ACCES SDAPPA
C      
      APPOIN = SDAPPA(1:19)//'.POIN'   
      CALL JEVEUO(APPOIN,'L',JPOIN)
C
C --- COORDONNEES DU NOEUD 
C
      COORPT(1) = ZR(JPOIN+3*(IP-1)+1-1)
      COORPT(2) = ZR(JPOIN+3*(IP-1)+2-1)
      COORPT(3) = ZR(JPOIN+3*(IP-1)+3-1)            
C
      CALL JEDEMA()
C 
      END
