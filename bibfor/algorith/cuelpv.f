      SUBROUTINE CUELPV(NUMLIA,RESOCU,NBLIAI,LELPIV)
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
C      
      IMPLICIT     NONE
      INCLUDE 'jeveux.h'
      LOGICAL      LELPIV
      INTEGER      NUMLIA
      INTEGER      NBLIAI
      CHARACTER*24 RESOCU
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : ALGOCU
C ----------------------------------------------------------------------
C
C  SAVOIR SI LA LIAISON EST A PIVOT NUL
C
C IN  RESOCU : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  NUMLIA : NUMERO DE LA LIAISON 
C IN  NBLIAI : NOMBRE DE LIAISONS DE CONTACT 
C OUT CFELPV : .TRUE.  SI LIAISON A PIVOT NUL 
C              .FALSE. SINON
C
C
C
C
      INTEGER      IOTE
      CHARACTER*19 LIOT
      INTEGER      JLIOT
C ======================================================================
      LIOT   = RESOCU(1:14)//'.LIOT'
      CALL JEVEUO(LIOT,'L',JLIOT)
C ======================================================================
      LELPIV = .FALSE.
      DO 10 IOTE = 1, ZI(JLIOT+4*NBLIAI) 
         IF (ZI(JLIOT-1+IOTE).EQ.NUMLIA) THEN
            LELPIV = .TRUE.
            GOTO 100
         ENDIF
 10   CONTINUE 

 100  CONTINUE 
C ======================================================================
      END
