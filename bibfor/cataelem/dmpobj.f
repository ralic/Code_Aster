      SUBROUTINE DMPOBJ(IPASSE,ETAT,COMMEN)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CATAELEM  DATE 12/05/97   AUTEUR JMBHH01 J.M.PROIX 
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
      COMMON /CIMP/IMP,IULMES,IULIST,IULVIG
C
C
      INTEGER IPASSE,ETAT
      CHARACTER*(*) COMMEN
      CHARACTER*32 NOMOBJ
C
C     INCLUDE($CDEBUG)
      CHARACTER*8 CLEDBG
      CHARACTER*24 OBJDMP
      INTEGER PASDMP,TYOBDM
      COMMON /CMODBG/CLEDBG
      COMMON /CDEBUG/ICCDBG
      COMMON /CBDMPC/OBJDMP(30)
      COMMON /CBDMPN/NDMP,PASDMP(30),TYOBDM(30)
C
      CHARACTER*1 K1BID
C       NDMP : NOMBRE D OBJETS A DUMPER
C       PASDMP(IDMP)  : PASSE OU ON DUMPE L OBJET IDMP
C       OBJDMP(IDMP)  : NOM DE L OBJET IDMP
C       TYOBDM(IDMP)  : GENRE DE L OBJET IDMP :  0 OBJET SIMPLE
C                                                1 COLLECTION NUMEROTEE
C                                                2 COLLECTION NOMME
C
C     EXCLUDE($CDEBUG)
C     INCLUDE($FUNJEV)
C
C     FONCTIONS JEVEUX
C
      CHARACTER*32 JEXNUM,JEXNOM
C     EXCLUDE($FUNJEV)
C
      ITITRE = 0
      IFIN = 0
      IF (NDMP.EQ.0) GOTO 9999
C
      DO 10 IDMP = 1,NDMP
        IF (PASDMP(IDMP).EQ.IPASSE) THEN
          CALL JEEXIN(OBJDMP(IDMP),IRET)
          IF (IRET.EQ.0) THEN
            WRITE (IMP,*) ' % OBJET ',OBJDMP(IDMP),'  INEXISTANT '

          ELSE
            IF (ITITRE.EQ.0) THEN
              WRITE (IMP,*) ' %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
              WRITE (IMP,*) ' %'
              WRITE (IMP,*) ' % DUMP OBJETS ETAT ',ETAT,' ',COMMEN
              WRITE (IMP,*) ' %'
              WRITE (IMP,*) ' %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
              WRITE (IMP,*) ' %'
C
              ITITRE = 1
              IFIN = 1
            END IF

            IF (TYOBDM(IDMP).EQ.0) THEN
              WRITE (IMP,*) ' % *** CONTENU DE L OBJET ',OBJDMP(IDMP)
              CALL JEIMPO('VIGILE',OBJDMP(IDMP),'    ',' ')

            ELSE IF (TYOBDM(IDMP).EQ.1) THEN
              WRITE (IMP,*) ' % *** CONTENU DE L OBJET ',OBJDMP(IDMP)
              CALL JELIRA(OBJDMP(IDMP),'NMAXOC',NOBJ,K1BID)
              WRITE (IMP,*) ' % *** NOMBRE D OBJETS SIMPLES ',NOBJ
              DO 5 IOBJ = 1,NOBJ
                WRITE (IMP,*) ' % ***  ',IOBJ,' EME ELEMENT SIMPLE '
                CALL JEIMPO('VIGILE',JEXNUM(OBJDMP(IDMP),IOBJ),'   ',
     +                      '  ')
    5         CONTINUE

            ELSE IF (TYOBDM(IDMP).EQ.2) THEN
              WRITE (IMP,*) ' % *** CONTENU DE L OBJET ',OBJDMP(IDMP)
              CALL JELIRA(OBJDMP(IDMP),'NMAXOC',NOBJ,K1BID)
              WRITE (IMP,*) ' % *** NOMBRE D OBJETS SIMPLES ',NOBJ
              DO 7 IOBJ = 1,NOBJ
                CALL JENUNO(JEXNUM(OBJDMP(IDMP),IOBJ),NOMOBJ)
                WRITE (IMP,*) ' % ***  ',IOBJ,' EME ELEMENT DE NOM ',
     +            NOMOBJ
                CALL JEIMPO('VIGILE',JEXNUM(OBJDMP(IDMP),IOBJ),'   ',
     +                      '  ')
    7         CONTINUE
            END IF

          END IF

        END IF

   10 CONTINUE
C
      IF (IFIN.EQ.1) THEN
        WRITE (IMP,*) ' %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
        WRITE (IMP,*) ' %'
        WRITE (IMP,*) ' % FIN DUMP OBJETS ETAT ',ETAT,' ',COMMEN
        WRITE (IMP,*) ' %'
        WRITE (IMP,*) ' %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
        WRITE (IMP,*) ' %'
      END IF
C
      GOTO 9999

 9999 CONTINUE
      END
