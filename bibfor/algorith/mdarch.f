      SUBROUTINE MDARCH (ISTO1,IPAS,DISC,DT,NBMODE,TYPCAL,NBSYM,NOMSYM,
     +                   DEPGER,VITGER,ACCGER,DEPSTR,VITSTR,ACCSTR, 
     +                   DEPGEC,VITGEC,ACCGEC,DEPSTC,VITSTC,ACCSTC,
     +                   PASSTO,IORSTO,DISCST)
      IMPLICIT NONE
      INTEGER    IORSTO(*)
      REAL*8     DEPGER(*),VITGER(*),ACCGER(*),
     +           DEPSTR(*),VITSTR(*),ACCSTR(*),
     +           PASSTO(*),DISCST(*)
      COMPLEX*16 DEPGEC(*),VITGEC(*),ACCGEC(*),
     +           DEPSTC(*),VITSTC(*),ACCSTC(*)
      CHARACTER*4 TYPCAL
      CHARACTER*4 NOMSYM(3)   
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/08/2012   AUTEUR ALARCON A.ALARCON 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C TOLE CRP_21
C 
C-----------------------------------------------------------------------
C
C   ARCHIVAGE DES CHAMPS GENERALISES OBLIGATOIRES POUR LA SD_DYNA_GENE
C
C-----------------------------------------------------------------------
      INTEGER IM, IND, IPAS, ISTO1, NBSYM, ICH
      INTEGER NBMODE 
      REAL*8 DT, DISC 
C-----------------------------------------------------------------------
C
      CALL ASSERT ((TYPCAL(1:4).EQ.'TRAN').OR.(TYPCAL(1:4).EQ.'HARM'))
C 
      IORSTO(ISTO1+1) = IPAS
      DISCST(ISTO1+1) = DISC
      IND = NBMODE * ISTO1
C      
      IF (TYPCAL(1:4).EQ.'TRAN') THEN
C
          PASSTO(ISTO1+1) = DT
          DO 69 IM = 1,NBMODE
             DEPSTR(IND+IM) = DEPGER(IM)
             VITSTR(IND+IM) = VITGER(IM)
             ACCSTR(IND+IM) = ACCGER(IM)
 69      CONTINUE
C
      ELSE
C
          DO 100 ICH=1,NBSYM
C                
              IF (NOMSYM(ICH)(1:4).EQ.'DEPL') THEN
                  DO 101 IM = 1,NBMODE
                     DEPSTC(IND+IM) = DEPGEC(IM)      
 101              CONTINUE
              ELSE IF (NOMSYM(ICH)(1:4).EQ.'VITE') THEN
                  DO 102 IM = 1,NBMODE
                     VITSTC(IND+IM) = VITGEC(IM)      
 102              CONTINUE
              ELSE 
                  DO 103 IM = 1,NBMODE
                     ACCSTC(IND+IM) = ACCGEC(IM)      
 103              CONTINUE
              ENDIF
C
 100      CONTINUE         
C
      ENDIF
C
      END
