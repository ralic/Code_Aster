      SUBROUTINE CFELPV(NUMLIA,TYPLIA,RESOCO,NBLIAI,LELPIV)
C ======================================================================
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
      CHARACTER*2  TYPLIA
      CHARACTER*24 RESOCO
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : ALGOCL/ALGOCO/FRO2GD/FROLGD
C ----------------------------------------------------------------------
C
C  SAVOIR SI LA LIAISON EST A PIVOT NUL
C
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  NUMLIA : NUMERO DE LA LIAISON 
C IN  TYPLIA : TYPE DE LA LIAISON
C                'C0': CONTACT
C                'F0': FROTTEMENT SUIVANT LES DEUX DIRECTIONS
C                       SIMULTANEES (3D)
C                'F1': FROTTEMENT SUIVANT LA PREMIERE DIRECTION (3D)
C                'F2': FROTTEMENT SUIVANT LA SECONDE DIRECTION (3D)
C IN  NBLIAI : NOMBRE DE LIAISONS DE CONTACT 
C OUT CFELPV : .TRUE.  SI LIAISON A PIVOT NUL 
C              .FALSE. SINON
C
C
C
C
      INTEGER      IOTE
      CHARACTER*2  TYPEC0,TYPEF0, TYPEF1, TYPEF2
      CHARACTER*19 LIOT
      INTEGER      JLIOT
C ======================================================================
      TYPEC0 = 'C0'
      TYPEF0 = 'F0'
      TYPEF1 = 'F1'
      TYPEF2 = 'F2'
      LIOT   = RESOCO(1:14)//'.LIOT'
      CALL JEVEUO (LIOT,'L',JLIOT)
C ======================================================================
      LELPIV = .FALSE.
      IF (TYPLIA.EQ.TYPEC0) THEN
         DO 10 IOTE = 1, ZI(JLIOT+4*NBLIAI) 
            IF (ZI(JLIOT-1+IOTE).EQ.NUMLIA) THEN
               LELPIV = .TRUE.
               GOTO 100
            ENDIF
 10      CONTINUE 
      ELSE IF (TYPLIA.EQ.TYPEF0) THEN
         DO 20 IOTE = 1, ZI(JLIOT+4*NBLIAI+1)
            IF (ZI(JLIOT-1+IOTE+NBLIAI).EQ.NUMLIA) THEN
               LELPIV = .TRUE.
               GOTO 100
            ENDIF
 20      CONTINUE 
      ELSE IF (TYPLIA.EQ.TYPEF1) THEN
         DO 30 IOTE = 1, ZI(JLIOT+4*NBLIAI+2)
            IF (ZI(JLIOT-1+IOTE+2*NBLIAI).EQ.NUMLIA) THEN
               LELPIV = .TRUE.
               GOTO 100
            ENDIF
 30      CONTINUE 
      ELSE IF (TYPLIA.EQ.TYPEF2) THEN
         DO 40 IOTE = 1, ZI(JLIOT+4*NBLIAI+3)
            IF (ZI(JLIOT-1+IOTE+3*NBLIAI).EQ.NUMLIA) THEN
               LELPIV = .TRUE.
               GOTO 100
            ENDIF
 40      CONTINUE 
      ENDIF
 100  CONTINUE 
C ======================================================================
      END
